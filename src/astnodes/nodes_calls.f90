submodule (astgen) nodes_calls
    implicit none

contains
    module subroutine astnode_call(result, input, t, currentnode, currentnode2, fname, i)
        type(ast), intent(inout) :: result
        type(tokengroup), intent(in) :: input
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        integer, intent(inout) :: currentnode2
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode

        if (t(i + 1)%type /= TOKEN_IDENTIFIER) then
            call throw('subroutine name must be an identifier', fname, t(i + 1)%line, t(i + 1)%char)
        end if
        if (t(i + 2)%type /= TOKEN_LGROUP) then
            call throw('subroutine call must include parenthesis', fname, t(i + 2)%line, t(i + 2)%char)
        end if

        tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
        tempnode%type = NODE_CALL
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%parentnode = currentnode2
        tempnode%value = t(i + 1)%value
        
        call result%append(tempnode, currentnode2)
        call result%nodes(currentnode)%subnodes2%append(currentnode2)

        block
            integer :: tempi
            integer(SMALL) :: depth
            depth = 0_SMALL
            i = i + 2
            if (t(i + 1)%type == TOKEN_RGROUP) then
                i = i + 2
                if (t(i)%type == TOKEN_NEXTLINE) return
                call throw('expected new line after call', fname, t(i)%line, t(i)%char)
            end if
            do while (t(i)%type /= TOKEN_RGROUP)
                i = i + 1
                tempi = i
                do while (depth /= 0 .or. t(i)%type /= TOKEN_OPERATOR .or. t(i)%value /= ',')
                    if (t(i)%type == TOKEN_NEXTLINE) then
                        call throw('expected closing parentheis in call', fname, t(i)%line, t(i)%char)
                    end if
                    if (depth == 0 .and. t(i)%type == TOKEN_RGROUP) exit
                    if (t(i)%type == TOKEN_LGROUP) depth = depth + 1_SMALL
                    if (t(i)%type == TOKEN_RGROUP) depth = depth - 1_SMALL
                    i = i + 1
                end do
                i = i - 1
                call parse_expr(result, currentnode2, input, tempi, i, fname, .false.)
                i = i + 1
            end do
            i = i + 1
            if (t(i)%type /= TOKEN_NEXTLINE) call throw('expected new line after call', fname, t(i)%line, t(i)%char)
        end block
    end subroutine
end submodule