module backend_ir
   use ir, only: full_ir
   use ir_graph, only: proc_stats
   use ir_write, only: write_ir
   use data_mod, only: list
   use backend_type, only: backend_base_type
   implicit none (type, external)

   type, extends(backend_base_type) :: backend_ir_type
   contains
      procedure :: full_init => ir_full_init
      procedure :: ir_init => ir_ir_init
      procedure :: pre_ssa => ir_pre_ssa
      procedure :: pre_lowering => ir_empty_pass
      procedure :: instruction_selection => ir_empty_pass
      procedure :: pre_write => ir_empty_pass
      procedure :: write => ir_write_wrapper
   end type
contains
   subroutine ir_full_init(this)
      class(backend_ir_type), intent(inout) :: this
   end subroutine

   subroutine ir_ir_init(this, intermediate)
      class(backend_ir_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
   end subroutine

   subroutine ir_pre_ssa(this, intermediate)
      class(backend_ir_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
   end subroutine

   subroutine ir_empty_pass(this, intermediate, associations, stats)
      class(backend_ir_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)
   end subroutine

   subroutine ir_write_wrapper(this, output, intermediate, associations)
      class(backend_ir_type), intent(inout) :: this
      type(list), intent(inout) :: output
      type(full_ir), intent(in) :: intermediate
      type(list), intent(in), optional :: associations(:)

      call write_ir(output, intermediate, associations)
   end subroutine
end module
