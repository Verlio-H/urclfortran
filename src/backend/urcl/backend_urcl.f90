module backend_urcl
   use include, only: SMALL, BIG
   use ir, only: full_ir, ir_procedure, HINT_INT, HINT_FLOAT, ir_type, ir_subtype
   use ir_graph, only: proc_stats
   use ir_write, only: write_ir
   use ir_backend_tools, only: phi_removal
   use data_mod, only: list
   use backend_type, only: backend_base_type
   use backend_lower_bits, only: ir_lower_bits, ir_convert_hint
   use backend_calling_convention, only: type_corrected_bit_count, fix_large_values
   use urcl_inst_select, only: instruction_select
   use urcl_init_ir, only: setup_builtin, ASM_JMP, ASM_MOV
   implicit none (type, external)

   type, extends(backend_base_type) :: backend_urcl_type
      integer(SMALL) :: bits = 16
      integer(SMALL) :: regs = 16

      ! overlapping starting from r1, caller saved
      ! return_reg_count <= argument_reg_count
      integer(SMALL) :: arg_reg_count = 6
      integer(SMALL) :: ret_reg_count = 4

      ! immediately after argument regs
      integer(SMALL) :: caller_reg_count = 4

      ! immediately after caller saved regs
      integer(SMALL) :: callee_reg_count = 6
      
      logical :: iris = .false.
      integer(BIG) :: asm_func_offset = 0
      ! TODO: switch to a non square 2d array
      type(list), allocatable :: reg_associations(:)
   contains
      procedure :: full_init => urcl_init
      procedure :: ir_init => urcl_ir_init
      procedure :: pre_ssa => urcl_pre_ssa
      procedure :: pre_lowering => urcl_size_lowering
      procedure :: compile => urcl_compile
      procedure :: write => ir_write_wrapper
   end type

contains
   subroutine urcl_init(this)
      class(backend_urcl_type), intent(inout) :: this
   end subroutine

   subroutine urcl_ir_init(this, intermediate)
      class(backend_urcl_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      call setup_builtin(this%bits, intermediate)
   end subroutine

   function urcl_size_map(subtype, userptr) result(result)
      type(ir_subtype), intent(in) :: subtype
      class(*), intent(in) :: userptr
      integer(BIG) :: result

      select type (userptr)
      class default
         error stop 'incorrect userptr in urcl_size_map'
      type is (backend_urcl_type)
         ! TODO: remove division here
         result = (subtype%size + userptr%bits - 1) / userptr%bits * userptr%bits
      end select
   end function

   function urcl_ret_size_cutoff(userptr, intermediate, proc, type, isret) result(result)
      class(*), intent(in) :: userptr
      type(full_ir), intent(in) :: intermediate
      type(ir_procedure), intent(in) :: proc
      type(ir_type), intent(in) :: type
      logical, value, intent(in) :: isret
      logical :: result

      integer(BIG) :: bit_count

      select type (userptr)
      class default
         error stop 'incorrect userptr in urcl_ret_size_cutoff'
      type is (backend_urcl_type)
         bit_count = type_corrected_bit_count(type, urcl_size_map, userptr%bits, intermediate, userptr)
         result = bit_count > userptr%bits * userptr%ret_reg_count
      end select
   end function

   subroutine urcl_pre_ssa(this, intermediate)
      class(backend_urcl_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate

      ! deal with calling convention
      call fix_large_values(intermediate, urcl_ret_size_cutoff, this)
   end subroutine

   subroutine urcl_size_lowering(this, intermediate, associations, stats)
      class(backend_urcl_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)

      call ir_lower_bits(intermediate, this%bits, HINT_FLOAT, HINT_FLOAT)
      if (this%iris) then
         call ir_lower_bits(intermediate, 16_SMALL, HINT_FLOAT, HINT_INT)
      else
         call ir_convert_hint(intermediate, HINT_FLOAT, HINT_INT)
      end if
      call ir_lower_bits(intermediate, this%bits, HINT_INT, HINT_INT)
   end subroutine

   subroutine urcl_compile(this, intermediate, associations, stats)
      class(backend_urcl_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)

      integer(BIG) :: i
      integer :: j
      class(*), allocatable :: val
      integer(SMALL) :: args, caller, callee

      ! init register associations
      allocate(this%reg_associations(intermediate%procedures%size), source=list(0_SMALL))
      do i = 1, intermediate%procedures%size
         select type (proc => intermediate%procedures%get(i))
         type is (ir_procedure)
            do j = 1, proc%ssa_counter
               val = 0_SMALL
               call this%reg_associations(i)%move_push(val)
            end do
         end select
      end do

      args = max(this%regs / 2, 6)
      caller = (this%regs - args) / 2
      callee = this%regs - caller
      !call instruction_select(this%bits, args, caller, callee, intermediate, this%reg_associations, associations, &
      !   this%arg_reg_count, this%ret_reg_count)

      call phi_removal(intermediate, associations, stats, ASM_JMP, ASM_MOV)
   end subroutine

   subroutine urcl_pre_write(this, intermediate, associations, stats)
      class(backend_urcl_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)
   end subroutine

   subroutine ir_write_wrapper(this, output, intermediate, associations)
      class(backend_urcl_type), intent(inout) :: this
      type(list), intent(inout) :: output
      type(full_ir), intent(in) :: intermediate
      type(list), intent(in), optional :: associations(:)

      call write_ir(output, intermediate, associations)
   end subroutine
end module
