module backend_arm
   use include, only: SMALL
   use ir, only: full_ir, HINT_INT, HINT_FLOAT
   use ir_graph, only: proc_stats
   use ir_write, only: write_ir
   use data_mod, only: list
   use backend_type, only: backend_base_type
   use backend_lower_bits, only: ir_lower_bits, ir_convert_hint
   implicit none (type, external)

   type, extends(backend_base_type) :: backend_arm_type
      integer(SMALL) :: bits = 64
      integer :: version_major = 8
      integer :: version_minor = 4
      character :: version_profile = 'a'
      logical :: single_floats = .true.
      logical :: double_floats = .true.
      logical :: neon = .true.
   contains
      procedure :: full_init => arm_init
      procedure :: pre_lowering => arm_size_lowering
      procedure :: instruction_selection => arm_instruction_selection
      procedure :: pre_write => arm_pre_write
      procedure :: write => ir_write_wrapper
   end type
contains
   subroutine arm_init(this)
      class(backend_arm_type), intent(inout) :: this
   end subroutine

   subroutine arm_size_lowering(this, intermediate, associations, stats)
      class(backend_arm_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)

      if (this%double_floats) then
         call ir_lower_bits(intermediate, 64_SMALL, HINT_FLOAT, HINT_INT)
      else if (this%single_floats) then
         call ir_lower_bits(intermediate, 32_SMALL, HINT_FLOAT, HINT_INT)
      else
         call ir_convert_hint(intermediate, HINT_FLOAT, HINT_INT)
      end if

      call ir_lower_bits(intermediate, this%bits, HINT_INT, HINT_INT)
   end subroutine

   subroutine arm_instruction_selection(this, intermediate, associations, stats)
      class(backend_arm_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)
   end subroutine

   subroutine arm_pre_write(this, intermediate, associations, stats)
      class(backend_arm_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)
   end subroutine

   subroutine ir_write_wrapper(this, output, curr_ir)
      class(backend_arm_type), intent(inout) :: this
      type(list), intent(inout) :: output
      type(full_ir), intent(in) :: curr_ir

      call write_ir(output, curr_ir)
   end subroutine
end module
