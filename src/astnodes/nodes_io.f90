submodule (astgen) nodes_io
    implicit none

contains
    subroutine astnode_write(result, input, t, currentnode, currentnode2, childnode, fname, i)
        type(ast), intent(inout) :: result
        type(tokengroup), intent(in) :: input
        type(token), intent(in) :: t(:)
        integer, intent(in) :: currentnode
        integer, intent(inout) :: currentnode2
        integer, intent(inout) :: childnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode

        select case (result%nodes(currentnode)%type)
        case (NODE_MODULE, NODE_ROOT)
            call throw('invalid spot for write statement', fname, t(i)%line, t(i)%char)
        case (NODE_PROGRAM)
            if (result%nodes(currentnode)%secondpart) then
                call throw('invalid spot for write statement', fname, t(i)%line, t(i)%char)
            end if
        end select

        tempnode = node()
        tempnode%type = NODE_CALL
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%parentnode = currentnode
        tempnode%value = 'ifunc_write'

        call result%append(tempnode,currentnode2)
        call result%nodes(currentnode)%subnodes2%append(currentnode2)

        i = i + 2
        block
            integer :: tempi
            integer(SMALL) :: depth
            tempi = i
            depth = 0
            if (t(i)%type == TOKEN_ASTERISK) then
                tempnode = node()
                tempnode%type = NODE_INT_VAL
                tempnode%startlnum = t(i)%line
                tempnode%startchar = t(i)%char
                tempnode%fname = fname
                tempnode%parentnode = currentnode2
                tempnode%value = '6'
                call result%append(tempnode, childnode)
                call result%nodes(currentnode2)%subnodes%append(childnode)
                i = i + 1
                if (t(i)%type /= TOKEN_OPERATOR .or. t(i)%value /= ',') then
                    call throw('syntax error in write statement', fname, t(i)%line, t(i)%char)
                end if
                i = i + 1
            else
                do while (depth /= 0 .or. t(i)%type /= TOKEN_OPERATOR .or. t(i)%value /= ',')
                    if (t(i)%type == TOKEN_LGROUP) depth = depth + 1_2
                    if (t(i)%type == TOKEN_RGROUP) depth = depth - 1_2
                    i = i + 1
                end do
                i = i - 1
                call parse_expr(result, currentnode2, input, tempi, i, fname, .false.)
                i = i + 2
            end if

            tempi = i
            do while (depth /= 0 .or. t(i)%type /= TOKEN_RGROUP)
                if (t(i)%type == TOKEN_LGROUP) depth = depth + 1_2
                if (t(i)%type == TOKEN_RGROUP) depth = depth - 1_2
                i = i + 1
            end do
            i = i - 1
            call parse_expr(result, currentnode2, input, tempi, i, fname, .false.)
            i = i + 1
            do while (t(i)%type /= TOKEN_NEXTLINE)
                i = i + 1
                tempi = i
                do while (depth /= 0 .or. t(i)%type /= TOKEN_OPERATOR .or. t(i)%value /= ',')
                    if (t(i)%type == TOKEN_NEXTLINE) exit
                    if (t(i)%type == TOKEN_LGROUP) depth = depth + 1_2
                    if (t(i)%type == TOKEN_RGROUP) depth = depth - 1_2
                    i = i + 1
                end do
                i = i - 1
                call parse_expr(result, currentnode2, input, tempi, i, fname, .false.)
                i = i + 1
            end do
        end block
    end subroutine
end submodule