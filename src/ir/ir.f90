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
   integer(SMALL), parameter :: HINT_UINT = 1
   integer(SMALL), parameter :: HINT_ADDR = 2
   integer(SMALL), parameter :: HINT_FLOAT = 3
   integer(SMALL), parameter :: HINT_IVEC_8 = 4
   integer(SMALL), parameter :: HINT_IVEC_16 = 5
   integer(SMALL), parameter :: HINT_IVEC_32 = 6
   integer(SMALL), parameter :: HINT_IVEC_64 = 7
   integer(SMALL), parameter :: HINT_FVEC_16 = 8
   integer(SMALL), parameter :: HINT_FVEC_32 = 9
   integer(SMALL), parameter :: HINT_FVEC_64 = 10

   character(*), parameter :: HINT_STRINGS(-1:10) = [ &
      'error ',&
      'int   ',&
      'uint  ',&
      'addr  ',&
      'float ',&
      'ivec8 ',&
      'ivec16',&
      'ivec32',&
      'ivec64',&
      'fvec16',&
      'fvec32',&
      'fvec64' &
   ]

   type :: ir_type
      character(:), allocatable :: name
      type(location) :: loc = location()
      type(list) :: subtypes = list() ! ir_subtype
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
      procedure, non_overridable :: deference => full_ir_type_deference
   end type

   type :: ir_subtype
      integer(SMALL) :: size = 0
      integer(SMALL) :: hint = HINT_INVALID
      type(full_ir_type) :: type = full_ir_type() ! if non fundamental hint
      integer(BIG) :: count = 1
   end type


   type :: comptime_val
      class(base_comptime_val), allocatable :: val
   end type

   type, abstract :: base_comptime_val
   end type

   type, extends(base_comptime_val) :: comptime_int
      integer(BIG) :: val
   end type

   type, extends(base_comptime_val) :: comptime_uint
      integer(BIG) :: val
   end type

   type, extends(base_comptime_val) :: comptime_float
      real(real64) :: val
   end type

   type, extends(base_comptime_val) :: comptime_addr
      character(:), allocatable :: name
      integer(BIG) :: proc = 0
      integer(BIG) :: var = 0
      integer(BIG) :: offset = 0
   end type

   type, extends(base_comptime_val) :: comptime_ivec8
      integer(int8), allocatable :: val(:)
   end type

   type, extends(base_comptime_val) :: comptime_ivec16
      integer(int16), allocatable :: val(:)
   end type

   type, extends(base_comptime_val) :: comptime_ivec32
      integer, allocatable :: val(:)
   end type

   type, extends(base_comptime_val) :: comptime_ivec64
      integer(int64), allocatable :: val(:)
   end type

   type, extends(base_comptime_val) :: comptime_fvec16
      real(real32), allocatable :: val(:)
   end type

   type, extends(base_comptime_val) :: comptime_fvec32
      real(real32), allocatable :: val(:)
   end type

   type, extends(base_comptime_val) :: comptime_fvec64
      real(real64), allocatable :: val(:)
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
      logical :: fundamental = .false. ! supplied by the backend
      logical :: non_fundamental = .false. ! ignored if supplied by the backend
      logical :: pure = .false. ! does not write to any external state
      logical :: simple = .false. ! does not read any external data, implies pure
      logical :: commutative = .false. ! argument order doesn't matter (+, *)
      logical :: associative = .false. ! f(f(x, a), b) = f(x, f(a, b)) (+, *, -, //)
      logical :: zero_identity = .false. ! f(x, 0) = x (+, -)
      logical :: one_identity = .false. ! f(x, 1) = x (*, /, **)
      logical :: lreduce_0_0 = .false. ! f(0, x) = 0 (*, /)
      logical :: rreduce_0_0 = .false. ! f(x, 0) = 0 (*)
      logical :: rreduce_0_1 = .false. ! f(x, 0) = 1 (**)
      logical :: rzero_illegal = .false. ! f(x, 0) is illegal (/)
      logical :: reduce_add = .false. ! f(i, x) = f(i - n, x) + f(n, x) (*)
      logical :: reduce_mlt = .false. ! f(x, i) = f(x, i - n) * f(x, n) (**)
      logical :: evaluatable = .false. ! if true the function is gauranteed to halt, must be simple
      logical :: variadic = .false. ! if true the function allows more arguments beyond the provided list
      procedure(comptime_eval), nopass, pointer :: eval => NULL()
      type(full_ir_type), allocatable :: return_type
      integer(BIG), allocatable :: arguments(:)
      character(:), allocatable :: name
      type(list) :: vars = list() ! big int
      type(list) :: blocks = list() ! ir_block
      type(location) :: loc = location()
   end type

   type :: ir_var
      character(:), allocatable :: name
      type(full_ir_type) :: type = full_ir_type()
      type(location) :: loc = location()
      logical :: static = .false. ! stored in dw/bss/data instead of stack
      logical :: export = .false. ! potentially accessible outside of this object, *must* be included in output
      logical :: extern = .false. ! defined elsewhere, don't need to allocate
      logical :: const = .false. ! only written to once and only directly, globals are the only thing that can be const
      type(comptime_val), allocatable :: contents(:)
   end type

   type :: full_ir
      type(list) :: procedures = list() ! ir_procedure
      type(list) :: types = list() ! ir_type
      type(list) :: vars = list() ! ir_var
      type(list) :: blocks = list() ! ir_block
      type(list) :: global_vars = list() ! big
   end type

   type, extends(ir_operand) :: operand_ir_var
      integer(BIG) :: var = 0
      integer(SMALL) :: dereference_count = 0
      ! TODO: multi dimensional slices
      logical :: slice = .false.
      integer(BIG) :: lindex, loffset
      integer(BIG) :: uindex, uoffset
   end type

   type, extends(ir_operand) :: operand_ssa_var
      integer :: idx
      logical :: deferenced
      ! TODO: multi dimensional slices
      logical :: slice
      integer(BIG) :: lindex, loffset
      integer(BIG) :: uindex, uoffset
   end type

   type, extends(ir_operand) :: operand_comptime
      class(base_comptime_val), allocatable :: val
   end type

   type, extends(ir_operand) :: operand_ir_block
      character(:), allocatable :: name
      integer(BIG) :: block_index = 0
   end type

   ! URCL mappings:
   !  -1 - SP
   !  0 - R0
   !  1 - FP/R1
   !  2-7 - arg0-arg5/R2-R7
   !  8-n - R8-(n - 1)
   type, extends(ir_operand) :: operand_asm_reg
      integer(SMALL) :: index
   end type
contains
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

   function full_ir_type_deference(type) result(result)
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
end module
