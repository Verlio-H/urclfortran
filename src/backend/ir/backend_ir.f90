module backend_ir
   use ir, only: full_ir
   use ir_graph, only: proc_stats
   use ir_write, only: write_ir
   use data_mod, only: list
   use backend_type, only: backend_base_type
   implicit none (type, external)

   type, extends(backend_base_type) :: backend_ir_type
   contains
      procedure :: full_init => ir_init
      procedure :: pre_lowering => ir_empty_pass
      procedure :: instruction_selection => ir_empty_pass
      procedure :: pre_write => ir_empty_pass
      procedure :: write => ir_write_wrapper
   end type
contains
   subroutine ir_init(this)
      class(backend_ir_type), intent(inout) :: this
   end subroutine

   subroutine ir_empty_pass(this, intermediate, associations, stats)
      class(backend_ir_type), intent(inout) :: this
      type(full_ir), intent(inout) :: intermediate
      type(list), intent(inout) :: associations(:)
      type(proc_stats), intent(inout) :: stats(:)
   end subroutine

   subroutine ir_write_wrapper(this, output, curr_ir)
      class(backend_ir_type), intent(inout) :: this
      type(list), intent(inout) :: output
      type(full_ir), intent(in) :: curr_ir

      call write_ir(output, curr_ir)
   end subroutine
end module
