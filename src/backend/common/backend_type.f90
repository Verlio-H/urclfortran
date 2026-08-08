module backend_type
   use ir, only: full_ir
   use ir_graph, only: proc_stats
   use data_mod, only: list
   implicit none (type, external)

   type, abstract :: backend_base_type
   contains
      ! performed at the very beginning of execution
      ! intended for one time internal setup
      procedure(init), deferred :: full_init

      ! performed prior to ir being generated 
      ! intended for resetting state, and for initializing ir with fundamental types, functions, etc.
      procedure(nossa_pass), deferred :: ir_init

      ! performed prior to the ir being ssaified
      ! intended for doing high level transformations
      ! for example, converting functions to use hidden pointers for large return values
      procedure(nossa_pass), deferred :: pre_ssa

      ! performed prior to compound values being lowered
      ! intended for transforming integer types to compound types
      ! this is for supporting type sizes beyond those provided by the architecture
      procedure(pass), deferred :: pre_lowering

      ! converts code into solely machine ops (as function calls)
      ! leaves phi nodes alone
      ! may internally consist of several passes
      ! pre ra instruction scheduling can happen here too
      ! after this stage, any inst at the end of a block can be a branch
      procedure(pass), deferred :: instruction_selection

      ! happens after phi removal
      ! intended for:
      !  cleaning up removed phis (which are now extraneous assignment instructions)
      !  register allocation
      !  peephole optimization
      !  post ra scheduling
      procedure(pass), deferred :: pre_write

      ! converts the code into the final compiled assembly as a string
      procedure(write), deferred :: write
   end type

   abstract interface
      subroutine init(this)
         import backend_base_type
         class(backend_base_type), intent(inout) :: this
      end subroutine

      subroutine nossa_pass(this, intermediate)
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
