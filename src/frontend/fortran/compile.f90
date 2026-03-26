module fort_compile
   use include, only: SMALL
   use fort_lexer, only: lexical_analysis, print_tokengroup
   use fort_astgen, only: syntactic_analysis, ast_node
   use data_mod, only: list
   !use astgen, only: ast, genast, NODE_PROGRAM, NODE_MODULE, print_ast
   !use semantic, only: sem_module, genmodfile
   !use irgen, only: ir_ptr, gen_ir, ir_finalize, ir_print
   !use backend_urcl16, only: gen_asm
   !use optimize
   implicit none (type, external)

contains
   function compiledata(input) result(result)
      character(:), allocatable :: result
      type(list), intent(in) :: input

      type(list) :: lexed
      
      type(ast_node) :: ast
      !type(sem_module), allocatable :: symbol_info(:)
      !type(ir_ptr), allocatable :: ssa(:)
      ! integer :: maxvar
      !type(siarr) :: varsizes

      ! integer :: i
      ! lexical analysis
      ! turns program text into array of tokens

      call lexical_analysis(lexed, input)

      call print_tokengroup(lexed)

      ! syntactic analysis
      ! turns array of tokens into a syntactic tree
      call syntactic_analysis(ast, lexed)
      !call print_ast(ast, 0)
      ! semantic analysis
      ! generates module files
      ! allocate(symbol_info(asted%nodes(1)%subnodes%size - 1))
      ! do i = 1, asted%nodes(1)%subnodes%size - 1
      !     associate(type => asted%nodes(asted%nodes(1)%subnodes%array(i))%type)
      !         if (type == NODE_MODULE .or. type == NODE_PROGRAM) then
      !             call genmodfile(asted, asted%nodes(1)%subnodes%array(i), symbol_info(i))
      !         end if
      !     end associate
      ! end do
      
      ! ssa generation
      ! generates ssa code using a combination of the module files and the ast and automatically creates cfg
      ! call gen_ir(asted, symbol_info, ssa, maxvar, varsizes)

      ! if (allocated(ssa)) then
      !     do i = 1, size(ssa)
      !         call ir_optimize(ssa(i)%ptr, maxvar, varsizes)
      !         !call ir_print(ssa(i)%ptr)
      !     end do
      ! end if

      ! result = gen_asm(ssa, maxvar, varsizes)

      ! if (allocated(ssa)) then
      !     do i = 1, size(ssa)
      !         call ir_finalize(ssa(i)%ptr)
      !     end do
      ! end if

      result = ''
   end function
end module
