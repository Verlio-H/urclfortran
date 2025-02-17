module compile
    use backend_urcl16
    implicit none

contains
    function compiledata(input, fname, startline) result(result)
        character(:), allocatable :: result
        character(:), allocatable, intent(in) :: input
        character(*), intent(in) :: fname
        integer(SMALL), allocatable, intent(in), optional :: startline
        
        type(ast) :: asted
        type(sem_module), allocatable :: symbol_info(:)
        type(ir_ptr), allocatable :: ssa(:)
        integer :: maxvar
        type(siarr) :: varsizes

        integer :: i
        ! lexical analysis
        ! turns program text into array of tokens
        call lex(input, fname, startline)

        ! syntactic analysis
        ! turns array of tokens into a syntactic tree
        asted = genast(lexed, fname)
        ! call print_ast(asted%nodes(1),asted,0)
        ! semantic analysis
        ! generates module files
        allocate(symbol_info(asted%nodes(1)%subnodes%size - 1))
        do i = 1, asted%nodes(1)%subnodes%size - 1
            associate(type => asted%nodes(asted%nodes(1)%subnodes%array(i))%type)
                if (type == NODE_MODULE .or. type == NODE_PROGRAM) then
                    call genmodfile(asted, asted%nodes(1)%subnodes%array(i), symbol_info(i))
                end if
            end associate
        end do
        
        ! ssa generation
        ! generates ssa code using a combination of the module files and the ast and automatically creates cfg
        call gen_ir(asted, symbol_info, ssa, maxvar, varsizes)

        if (allocated(ssa)) then
            do i = 1, size(ssa)
                !call ir_print(ssa(i)%ptr)
            end do
        end if

        result = gen_asm(ssa,maxvar,varsizes)

        if (allocated(ssa)) then
            do i = 1, size(ssa)
                call ir_finalize(ssa(i)%ptr)
            end do
        end if
    end function
end module