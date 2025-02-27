submodule (astgen) nodes_if
    use lexer, only: TOKEN_LGROUP, TOKEN_RGROUP
    implicit none

contains
    module subroutine astnode_if(result, input, t, currentnode, fname, i, else)
        type(ast), intent(inout) :: result
        type(tokengroup), intent(in) :: input
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i
        logical, optional, intent(in) :: else

        integer :: tempi
        integer :: depth
        type(node) :: tempnode
        integer :: currentnode2


        i = i + 1
        if (input%tokens(i)%type /= TOKEN_LGROUP) then
            call throw('expected open parenthesis in if statement', fname, input%tokens(i)%line, input%tokens(i)%char)
        end if


        tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
        if (present(else)) then
            tempnode%type = NODE_ELSE_IF
        else
            tempnode%type = NODE_IF
        end if
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%parentnode = currentnode
        tempnode%value = ''

        call result%append(tempnode, currentnode2)
        call result%nodes(currentnode)%subnodes2%append(currentnode2)

        tempi = i
        depth = 1
        do while (depth /= 0 .or. input%tokens(i)%type /= TOKEN_RGROUP)
            i = i + 1
            associate (tok => input%tokens(i))
                if (tok%type == TOKEN_LGROUP) depth = depth + 1
                if (tok%type == TOKEN_RGROUP) depth = depth - 1
            end associate
        end do

        call parse_expr(result, currentnode2, input, tempi + 1, i - 1, fname, .false.)
        i = i + 1

        if (input%tokens(i)%type /= TOKEN_IDENTIFIER .or. input%tokens(i)%value /= 'THEN') then
            call throw('single line if is currently unsupported', fname, input%tokens(i)%line, input%tokens(i)%char)
        end if

        i = i + 1

        if (input%tokens(i)%type /= TOKEN_NEXTLINE) then
            call throw('unexpected stuff after if statement', fname, input%tokens(i)%line, input%tokens(i)%char)
        end if

        currentnode = currentnode2
    end subroutine

    module subroutine astnode_else(result, input, t, currentnode, fname, i)
        type(ast), intent(inout) :: result
        type(tokengroup), intent(in) :: input
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode
        integer :: currentnode2
        

        i = i - 1
        call astnode_end(result, t, currentnode, fname, i)
        i = i + 2

        associate (funcnodes => result%nodes(currentnode)%subnodes2)
            associate (lastnode => result%nodes(funcnodes%array(funcnodes%size - 1)))
                print*,lastnode%type
                lastnode%value = 'e'//lastnode%value
            end associate
        end associate

        if (input%tokens(i)%type == TOKEN_NEXTLINE) then
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
        
            tempnode%type = NODE_ELSE
            tempnode%startlnum = t(i)%line
            tempnode%startchar = t(i)%char
            tempnode%fname = fname
            tempnode%parentnode = currentnode
            tempnode%value = ''

            call result%append(tempnode, currentnode2)
            call result%nodes(currentnode)%subnodes2%append(currentnode2)

            currentnode = currentnode2
        else if (input%tokens(i)%type == TOKEN_IDENTIFIER .and. input%tokens(i)%value == 'IF') then
            call astnode_if(result, input, t, currentnode, fname, i, .true.)
        else
            call throw('unexpected junk following else statement', fname, input%tokens(i)%line, input%tokens(i)%char)
        end if
    end subroutine
end submodule