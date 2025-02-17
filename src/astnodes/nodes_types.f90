submodule (astgen) nodes_types
    implicit none

contains
    subroutine astnode_type(result, input, t, currentnode, fname, i)
        type(ast), intent(inout) :: result
        type(tokengroup), intent(in) :: input
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        i = i + 1
        block
            integer :: tempi, tempi2, depth
            integer :: typeof
            tempi = i
            do while (tempi < size(t))
                if (t(tempi)%type == TOKEN_NEXTLINE) then
                    depth = 0
                    do tempi2 = i, tempi
                        if (depth == 0 .and. t(tempi2)%type == TOKEN_IDENTIFIER) exit
                        if (t(tempi2)%type == TOKEN_LGROUP) depth = depth + 1
                        if (t(tempi2)%type == TOKEN_RGROUP) depth = depth - 1
                    end do
                    tempi = tempi2 - 1
                    exit
                else if (t(tempi)%type == TOKEN_OPERATOR .and. t(tempi)%value == '::') then
                    tempi = tempi - 1
                    exit
                end if
                tempi = tempi + 1
            end do
            tempi2 = i - 1
            i = tempi + 1
            if (t(i)%type == TOKEN_OPERATOR .and. t(i)%value == '::') i = i + 1
            do while (t(i)%type /= TOKEN_NEXTLINE)
                if (t(i)%type == TOKEN_OPERATOR .and. t(i)%value == ',') then
                    i = i + 1
                    cycle
                else if (t(i)%type == TOKEN_ASSIGN) then
                    i = i + 1
                    tempi = i
                    do while (t(i)%type /= TOKEN_NEXTLINE)
                        i = i + 1
                    end do
                    i = i - 1
                    call parse_expr(result, typeof, input, tempi, i, fname, .false.)
                    exit
                end if
                if (t(i)%type /= TOKEN_IDENTIFIER) then
                    call throw('expected identifier', fname, t(i)%line, t(i)%char)
                end if

                typeof = parse_bigtype(result, input, tempi2, tempi, fname)
                result%nodes(typeof)%value = t(i)%value
                call result%nodes(currentnode)%subnodes2%append(typeof)
                i = i + 1
                if (t(i)%type == TOKEN_LGROUP) then
                    call throw('arrays not yet supported', fname, t(i)%line, t(i)%char)
                end if
            end do
        end block
    end subroutine
end submodule