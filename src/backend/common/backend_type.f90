module backend_type
   use ir, only: full_ir
   use ir_graph, only: proc_stats
   use data_mod, only: list
   implicit none (type, external)

   type, abstract :: backend_base_type
   contains
      procedure(init), deferred :: full_init
      procedure(ir_init), deferred :: ir_init
      procedure(pass), deferred :: pre_lowering
      procedure(pass), deferred :: instruction_selection
      procedure(pass), deferred :: pre_write
      procedure(write), deferred :: write
   end type

   abstract interface
      subroutine init(this)
         import backend_base_type
         class(backend_base_type), intent(inout) :: this
      end subroutine

      subroutine ir_init(this, intermediate)
         import backend_base_type, full_ir
         class(backend_base_type), intent(inout) :: this
         type(full_ir), intent(inout) :: intermediate
      end subroutine

      subroutine pass(this, intermediate, associations, stats)
         import backend_base_type, full_ir, list, proc_stats
         class(backend_base_type), intent(inout) :: this
         type(full_ir), intent(inout) :: intermediate
         type(list), intent(inout) :: associations(:)
         type(proc_stats), intent(inout) :: stats(:)
      end subroutine

      subroutine write(this, output, intermediate, associations)
         import backend_base_type, list, full_ir
         class(backend_base_type), intent(inout) :: this
         type(list), intent(inout) :: output
         type(full_ir), intent(in) :: intermediate
         type(list), intent(in), optional :: associations(:)
      end subroutine
   end interface
end module
