module backend_urcl
   use include, only: SMALL, BIG
   use ir, only: full_ir, ir_procedure, HINT_INT, HINT_FLOAT
   use ir_graph, only: proc_stats
   use ir_write, only: write_ir
   use data_mod, only: list
   use backend_type, only: backend_base_type
   use backend_lower_bits, only: ir_lower_bits, ir_convert_hint
   use urcl_inst_select, only: instruction_select
   implicit none (type, external)

   type, extends(backend_base_type) :: backend_urcl_type
      integer(SMALL) :: bits = 16
      integer(SMALL) :: regs = 8
      logical :: iris = .false.
      ! TODO: switch to a non square 2d array
      type(list), allocatable :: reg_associations(:)
   contains
      procedure :: full_init => urcl_init
      procedure :: pre_lowering => urcl_size_lowering
      procedure :: instruction_selection => urcl_instruction_selection
      procedure :: pre_write => urcl_pre_write
      procedure :: write => ir_write_wrapper
   end type
contains
   subroutine urcl_init(this)
      class(backend_urcl_type), intent(inout) :: this
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

   subroutine urcl_instruction_selection(this, intermediate, associations, stats)
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
      call instruction_select(this%bits, args, caller, callee, intermediate, this%reg_associations)
   end subroutine

   subroutine urcl_pre_write(this, intermediate, associations, stats)
      class(backend_urcl_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)
   end subroutine

   subroutine ir_write_wrapper(this, output, curr_ir)
      class(backend_urcl_type), intent(inout) :: this
      type(list), intent(inout) :: output
      type(full_ir), intent(in) :: curr_ir

      call write_ir(output, curr_ir)
   end subroutine
end module
