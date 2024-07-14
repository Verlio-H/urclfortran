module compile
    use semantic
    implicit none

contains
    function compiledata(input, fname, startline) result(result)
        character(len=:), allocatable :: result
        character(len=:), allocatable, intent(in) :: input
        character(len=*), intent(in) :: fname
        integer(SMALL), allocatable, intent(in), optional :: startline
        
        type(ast) :: asted

        integer :: i
        ! lexical analysis
        ! turns program text into array of tokens
        call lex(input,fname,startline)

        ! syntactic analysis
        ! turns array of tokens into a syntactic tree
        asted = genast(lexed,fname)
        call print_ast(asted%nodes(1),asted,0)
        
        ! semantic analysis
        ! generates module files
        do i=1,asted%nodes(1)%subnodes%size-1
            call genmodfile(asted,asted%nodes(1)%subnodes%array(i))
        end do
        

        ! code gen
        ! generates code using a combination of the module files and the ast
        !result = generate(asted,functbl)
        result = ''
    end function
end module