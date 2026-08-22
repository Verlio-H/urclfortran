module ir
   use iso_fortran_env, only: real64, int8, int16, real32, int64
   use iso_c_binding, only: c_char
   use include, only: SMALL, BIG, string, location
   use ir_instructions, only: ir_instruction, ir_operand
   use data_mod, only: list

   implicit none (type, external)

   type :: ir_block
      integer(BIG), allocatable :: child_blocks(:)
      type(list) :: parent_blocks = list()
      type(list) :: content = list() ! instructions
      character(:), allocatable :: name
      integer(SMALL) :: inferred_depth = 1
   end type

   integer(SMALL), parameter :: HINT_INVALID = -1
   integer(SMALL), parameter :: HINT_INT = 0
   integer(SMALL), parameter :: HINT_FLOAT = 1
   integer(SMALL), parameter :: HINT_ADDR = 2
   integer(SMALL), parameter :: HINT_ISIMD = 3
   integer(SMALL), parameter :: HINT_FSIMD = 4

   character(*), parameter :: HINT_STRINGS(-1:4) = [ &
      'error',&
      'int  ',&
      'float',&
      'addr ',&
      'isimd',&
      'fsimd' &
   ]

   type :: ir_type
      character(:), allocatable :: name
      type(location) :: loc = location()
      type(list) :: subtypes = list() ! ir_subtype
   contains
      procedure, non_overridable :: comp_count => ir_type_comp_count
      procedure, non_overridable :: comp => ir_type_comp
   end type

   type :: full_ir_type
      logical :: unknown = .false.
      integer(SMALL) :: indirection_count = 0
      logical, allocatable :: restrict_mask(:)
      logical, allocatable :: restrictish_mask(:)
      logical, allocatable :: const_mask(:)
      integer(BIG), allocatable :: array_sizes(:) ! 0:indirection count
      integer(BIG) :: type = 0 ! pointer into array of ir types
   contains
      procedure, non_overridable :: dereference => full_ir_type_dereference
      procedure, non_overridable :: bit_count => full_ir_type_bit_count
   end type

   type :: ir_subtype
      integer(SMALL) :: size = 1
      integer(SMALL) :: hint = HINT_INVALID
      type(full_ir_type) :: type = full_ir_type() ! if non fundamental hint
      integer(BIG) :: count = 1
   end type


   type :: comptime_val
      class(base_comptime_val), allocatable :: val
   end type

   type, abstract :: base_comptime_val
   end type

   ! TODO: arbitrary size
   type, extends(base_comptime_val) :: comptime_int
      integer(BIG) :: val
      integer(BIG) :: type = 0
   end type

   ! TODO: arbitrary precision
   type, extends(base_comptime_val) :: comptime_float
      real(real64) :: val
      integer(BIG) :: type = 0
   end type

   type, extends(base_comptime_val) :: comptime_addr
      character(:), allocatable :: name
      integer(BIG) :: proc = 0
      integer(BIG) :: var = 0
      integer(BIG) :: offset = 0
   end type

   abstract interface
      subroutine comptime_eval(result, args)
         import comptime_val
         type(comptime_val), allocatable :: result(:)
         type(comptime_val), contiguous :: args(:, :)
      end subroutine
   end interface
  
  ! ordering for arguments if commutative:
  !  last: constants in order of increasing gvn number
  !  first: loop variables in order of increasing gvn number
  !  midle: other values

   type :: ir_procedure
      ! possible other properties:
      !  rreduce_1_0 (mod)
      !  negone_identity (and)
      ! (l and r properties don't necessarily have to be on 2 arg functions)
      ! (they apply to first 2 arguments)
      logical :: fundamental = .false. ! supplied by the backend
      logical :: non_fundamental = .false. ! ignored if supplied by the backend
      logical :: pure = .false. ! does not write to any external state
      logical :: simple = .false. ! does not read any external data, implies pure
      ! first 2 args
      logical :: commutative = .false. ! argument order doesn't matter (+, *)
      ! first 2 args
      logical :: associative = .false. ! f(f(x, a), b) = f(x, f(a, b)) (+, *, //)
      ! last arg or all args if commutative
      logical :: zero_identity = .false. ! f(x, 0) = x (+, -)
      ! last arg or all args if commutative
      logical :: one_identity = .false. ! f(x, 1) = x (*, /, **)
      logical :: lreduce_0_0 = .false. ! f(0, x) = 0 (*, /)
      logical :: rreduce_0_0 = .false. ! f(x, 0) = 0 (*)
      logical :: rreduce_0_1 = .false. ! f(x, 0) = 1 (**)
      logical :: lzero_illegal = .false. ! f(0, x) is illegal
      logical :: rzero_illegal = .false. ! f(x, 0) is illegal (/)
      logical :: reduce_add = .false. ! f(i, x) = f(i - n, x) + f(n, x) (*)
      logical :: reduce_mlt = .false. ! f(x, i) = f(x, i - n) * f(x, n) (**)
      logical :: evaluatable = .false. ! if true the function is gauranteed to halt, must be simple
      logical :: variadic = .false. ! if true the function allows more arguments beyond the provided list

      procedure(comptime_eval), nopass, pointer :: eval => NULL()
      type(full_ir_type), allocatable :: return_type
      integer(BIG), allocatable :: arguments(:)
      integer, allocatable :: ssa_arguments(:)
      character(:), allocatable :: name
      ! first elements are the arguments
      type(list) :: vars = list() ! big int
      type(list) :: blocks = list() ! ir_block
      type(location) :: loc = location()
      integer :: ssa_counter = 1
   contains
      procedure, non_overridable :: get_block => ir_procedure_get_block
   end type

   type :: ir_var
      character(:), allocatable :: name
      type(full_ir_type) :: type = full_ir_type()
      type(location) :: loc = location()
      logical :: static = .false. ! stored in dw/bss/data instead of stack
      logical :: export = .false. ! potentially accessible outside of this object, *must* be included in output
      logical :: extern = .false. ! defined elsewhere, don't need to allocate
      logical :: const = .false. ! only written to once and only directly, globals are the only thing that can be const
      logical :: noderef = .false. ! if an argument is never dereferenced
      logical :: active = .true.
      type(comptime_val), allocatable :: contents(:)
   end type

   type :: full_ir
      type(list) :: procedures = list() ! ir_procedure
      type(list) :: types = list() ! ir_type
      type(list) :: vars = list() ! ir_var
      type(list) :: blocks = list() ! ir_block
      type(list) :: global_vars = list() ! big
   contains
      procedure, non_overridable :: get_var_type => ir_get_var_type
   end type

   ! TODO: change slice syntax to prevent architecture dependence issues
   type, extends(ir_operand) :: operand_ir_var
      integer(BIG) :: var = 0
      integer(SMALL) :: dereference_count = 0
      ! TODO: multi dimensional slices
      logical :: slice = .false.
      integer(BIG) :: lindex = 1, loffset = 0
      integer(BIG) :: uindex = 1, uoffset = 0
   contains
      procedure, non_overridable :: equals => ir_var_equals
      procedure, non_overridable :: get_type => operand_ir_var_get_type
   end type

   type, extends(ir_operand) :: operand_ssa_var
      integer :: idx = -1
      ! TODO: multi dimensional slices
      logical :: slice = .false.
      integer(BIG) :: lindex = 1, loffset = 0
      integer(BIG) :: uindex = 1, uoffset = 0
   end type

   type, extends(ir_operand) :: operand_comptime
      class(base_comptime_val), allocatable :: val
   end type

   type, extends(ir_operand) :: operand_ir_block
      character(:), allocatable :: name
      integer(BIG) :: block_index = 0
   end type

   type, extends(ir_operand) :: operand_empty
   end type

   ! URCL mappings:
   !  0 - SP
   !  1 - FP/R1
   !  2-7 - arg0-arg5/R2-R7
   !  8-n - R8-(n - 1)
   type, extends(ir_operand) :: operand_asm_reg
      integer(SMALL) :: index
   end type
contains
   function ir_create_type(name, bits, hint) result(type)
      character(*), intent(in) :: name
      integer(SMALL), intent(in) :: bits
      integer(SMALL), intent(in) :: hint
      type(ir_type) :: type

      integer(BIG) :: i
      class(*), allocatable :: tmp

      type = ir_type_empty()
      type%name = name
      tmp = ir_subtype(size=bits, hint=hint)
      call type%subtypes%move_push(tmp)
   end function

   function ir_block_empty() result(result)
      type(ir_block) :: result
      result = ir_block(content = list(ir_instruction()), parent_blocks = list(0_BIG))
   end function

   function ir_type_empty() result(result)
      type(ir_type) :: result
      result = ir_type(subtypes = list(ir_subtype()))
   end function

   function ir_procedure_empty() result(result)
      type(ir_procedure) :: result
      result = ir_procedure(vars = list(0_BIG), blocks = list(0_BIG))
   end function

   function full_ir_empty() result(result)
      type(full_ir) :: result
      result%procedures = list(ir_procedure_empty())
      result%types = list(ir_type_empty())
      result%vars = list(ir_var())
      result%blocks = list(ir_block())

      result%global_vars = list(0_BIG)
   end function

   function full_ir_type_dereference(type) result(result)
      class(full_ir_type), intent(in) :: type
      type(full_ir_type) :: result

      result%unknown = type%unknown
      result%indirection_count = type%indirection_count - 1_SMALL
      result%restrict_mask = type%restrict_mask(2:)
      result%restrictish_mask = type%restrictish_mask(2:)
      allocate(result%const_mask(0:result%indirection_count))
      result%const_mask = type%const_mask(1:)

      allocate(result%array_sizes(0:result%indirection_count))
      result%array_sizes(:) = type%array_sizes(1:)

      result%type = type%type

   end function

   function ir_procedure_get_block(proc, curr_ir, i) result(bblock)
      class(ir_procedure), target, intent(in) :: proc
      type(full_ir), intent(in) :: curr_ir
      integer(BIG) :: i
      type(ir_block), pointer :: bblock

      select type (idx => proc%blocks%get(i))
      class default
         error stop 'invalid procedure in get block'
      type is (integer(BIG))
         select type (block => curr_ir%blocks%get(idx))
         class default
            error stop 'invalid curr_ir in get block'
         type is (ir_block)
            bblock => block
         end select
      end select
   end function

   function ir_var_equals(a, b) result(res)
      class(operand_ir_var), intent(in) :: a
      class(operand_ir_var), intent(in) :: b
      logical :: res

      res = .false.
      if (a%var /= b%var) return
      if (a%dereference_count /= b%dereference_count) return
      !if (a%slice .neqv. b%slice) return
      !if (a%slice) then
      !   if (a%lindex /= b%lindex) return
      !   if (a%loffset /= b%loffset) return
      !   if (a%uindex /= b%uindex) return
      !   if (a%uoffset /= b%uoffset) return
      !end if
      res = .true.
   end function

   function ir_get_var_type(input, idx) result(type)
      class(full_ir), intent(in) :: input
      integer(BIG), intent(in) :: idx
      type(full_ir_type) :: type

      select type (var => input%vars%get(idx))
      class default
         error stop 'invalid input to ir_get_var_type'
      type is (ir_var)
         type = var%type
      end select
   end function

   function operand_ir_var_get_type(op, input) result(type)
      class(operand_ir_var), intent(in) :: op
      type(full_ir), intent(in) :: input
      type(full_ir_type) :: type

      type = input%get_var_type(op%var)
      type%indirection_count = type%indirection_count - op%dereference_count
   end function

   function ir_type_comp_count(type) result(count)
      class(ir_type), intent(in) :: type
      integer(BIG) :: count

      integer(BIG) :: i

      count = 0
      do i = 1, type%subtypes%size
         select type (subtype => type%subtypes%get(i))
         class default
            error stop 'invalid type construction in ir_type_comp_count'
         type is (ir_subtype)
            count = count + subtype%count
         end select
      end do
   end function

   function ir_type_comp(type, idx) result(subtype)
      class(ir_type), intent(in) :: type
      integer(BIG), intent(in) :: idx
      type(ir_subtype), pointer :: subtype

      integer(BIG) :: i, count

      if (idx < 0) then
         error stop 'index out of bounds in type comp'
      end if
      count = 0
      do i = 1, type%subtypes%size
         select type (stype => type%subtypes%get(i))
         class default
            error stop 'invalid type construction in ir_type_comp'
         type is (ir_subtype)
            if (idx - count <= stype%count) then
               subtype => stype
               return
            end if
            count = count + subtype%count
         end select
      end do
      error stop 'index out of bounds in type comp'
   end function


   recursive function full_ir_type_bit_count(full_type, input) result(count)
      class(full_ir_type), intent(in) :: full_type
      type(full_ir), intent(in) :: input
      integer(BIG) :: count

      integer(BIG) :: i
      type(ir_type), pointer :: type

      if (full_type%indirection_count /= 0) then
         count = 1
         return
      end if

      select type (true_type => input%types%get(full_type%type))
      class default
         error stop 'invalid input argument to full_ir_type_bit_count'
      type is (ir_type)
         type => true_type
      end select

      count = 0
      do i = 1, type%subtypes%size
         select type (subtype => type%subtypes%get(i))
         class default
            error stop 'invalid type construction in ir_type_bit_count'
         type is (ir_subtype)
            if (subtype%hint == HINT_INVALID) then
               count = count + subtype%count * subtype%type%bit_count(input)
            else
               count = count + subtype%count * subtype%size
            end if
         end select
      end do
   end function
end module
