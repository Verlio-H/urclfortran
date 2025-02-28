submodule (astgen) nodes_do
    use lexer, only: TOKEN_LGROUP, TOKEN_RGROUP
    implicit none
contains
    module subroutine astnode_do(result, input, t, currentnode, fname, i)
        type(ast), intent(inout) :: result
        type(tokengroup), intent(in) :: input
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        integer :: depth
        integer :: tmpi
        type(node) :: tempnode
        integer :: currentnode2


        tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
        tempnode%type = NODE_DO
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%parentnode = currentnode
        tempnode%value = ''

        call result%append(tempnode, currentnode2)
        call result%nodes(currentnode)%subnodes2%append(currentnode2)

        currentnode = currentnode2

        i = i + 1
        tmpi = i
        depth = 0

        do while (depth /= 0 .or. t(i)%type /= TOKEN_ASSIGN)
            i = i + 1
            if (t(i)%type == TOKEN_LGROUP) depth = depth + 1
            if (t(i)%type == TOKEN_RGROUP) depth = depth - 1
            if (t(i)%type == TOKEN_NEXTLINE) then
                call throw('unexpected new line in do statemeent', fname, t(i)%line, t(i)%char)
            end if
        end do

        call parse_expr(result, currentnode, input, tmpi, i - 1, fname, .false.)

        i = i + 1
        tmpi = i

        if (t(i)%type == TOKEN_LGROUP) then
            depth = depth + 1
            i = i + 1
        end if
        do while (depth /= 0 .or. t(i)%value /= ',')
            if (t(i)%type == TOKEN_LGROUP) depth = depth + 1
            if (t(i)%type == TOKEN_RGROUP) depth = depth - 1
            if (t(i)%type == TOKEN_NEXTLINE) then
                call throw('missing end condition in do statemeent', fname, t(i)%line, t(i)%char)
            end if
            i = i + 1
        end do

        call parse_expr(result, currentnode, input, tmpi, i - 1, fname, .false.)

        i = i + 1
        tmpi = i

        if (t(i)%type == TOKEN_LGROUP) then
            depth = depth + 1
            i = i + 1
        end if
        do while (depth /= 0 .or. (t(i)%value /= ',' .and. t(i)%type /= TOKEN_NEXTLINE))
            if (t(i)%type == TOKEN_NEXTLINE) then
                call throw('unmatched parenthesis in do statement', fname, t(i)%line, t(i)%char)
            end if
            if (t(i)%type == TOKEN_LGROUP) depth = depth + 1
            if (t(i)%type == TOKEN_RGROUP) depth = depth - 1
            i = i + 1
        end do
        
        call parse_expr(result, currentnode, input, tmpi, i - 1, fname, .false.)
        
        if (t(i)%value /= ',') then
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_INT_VAL
            tempnode%value = '1'
            call result%append(tempnode, currentnode2)
            call result%nodes(currentnode)%subnodes%append(currentnode2)
            return
        end if

        i = i + 1
        tmpi = i

        do while (t(i)%type /= TOKEN_NEXTLINE)
            i = i + 1
        end do

        call parse_expr(result, currentnode, input, tmpi, i - 1, fname, .false.)
    end subroutine
end submodule