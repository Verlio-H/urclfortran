submodule (astgen) nodes_implicit
    implicit none

contains
    module subroutine astnode_implicit(result, t, currentnode, currentnode2, fname, i)
        type(ast), intent(inout) :: result
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        integer, intent(inout) :: currentnode2
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode

        i = i + 1
        do while (t(i)%type /= TOKEN_NEXTLINE)
            if (t(i)%type /= TOKEN_IDENTIFIER) then
                call throw('expected identifier in implicit statement', fname, t(i)%line, t(i)%char)
            end if
            block
                integer :: end, end2
                integer(SMALL) :: depth
                integer :: resulttype
                end = i
                depth = 0
                tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
                tempnode%type = NODE_IMPLICIT
                tempnode%startlnum = t(i)%line
                tempnode%startchar = t(i)%char
                tempnode%fname = fname
                tempnode%parentnode = currentnode
                tempnode%value = ''
                do while (.not.(((t(end)%type == TOKEN_OPERATOR .and. t(end)%value == ',') .or. &
                            (t(end)%type == TOKEN_NEXTLINE)) .and. depth == 0))
                    
                    if (t(end)%type == TOKEN_NEXTLINE) then
                        call throw('expected closing parenthesis', fname, t(end)%line, t(end)%char)
                    end if
                    if (t(end)%type == TOKEN_LGROUP) depth = depth + 1_SMALL
                    if (t(end)%type == TOKEN_RGROUP) then
                        if (depth == 0) call throw('missing left parenthesis', fname, t(end)%line, t(end)%char)
                        depth = depth - 1_SMALL
                    end if
                    end = end + 1
                end do
                end = end - 1
                end2 = end
                ! find opening parenthesis
                if (.not.(t(end)%type == TOKEN_IDENTIFIER .and. t(end)%value == 'NONE')) then
                    do while (t(end)%type /= TOKEN_LGROUP)
                        end = end - 1
                        if (end <= i) then
                            call throw('expected parenthesis', fname, t(i)%line, t(i)%char)
                        end if
                    end do
                    end = end - 1
                end if
                call result%append(tempnode, currentnode)
                if (t(end)%value == 'NONE') then
                    tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
                    tempnode%type = NODE_TYPE
                    tempnode%value = ''
                    tempnode%value2 = TYPE_NONE
                    tempnode%parentnode = currentnode
                    call result%append(tempnode, resulttype)
                    call result%nodes(resulttype)%subnodes%append(0)
                else
                    resulttype = parse_type(result, lexed, i, end, fname)
                end if
                call result%nodes(currentnode)%subnodes%append(resulttype)
                i = end + 1
                ! (i:end2) = range (end2 and i == parens)
                end2 = end2 - 1
                i = i + 1
                do while (i<=end2)
                    tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
                    tempnode%type = NODE_STRING
                    tempnode%startlnum = t(i)%line
                    tempnode%startchar = t(i)%char
                    tempnode%fname = fname
                    tempnode%parentnode = currentnode
                    if (t(i)%type/=TOKEN_IDENTIFIER) then
                        call throw('expected letter', fname, t(i)%line, t(i)%char)
                    end if
                    if ((t(i + 1)%type == TOKEN_RGROUP .and. t(i + 1)%value == ')') .or. &
                        (t(i + 1)%type == TOKEN_OPERATOR .and. t(i + 1)%value == ',')) then

                        tempnode%value = t(i)%value
                        i = i + 2
                    else if (t(i + 1)%type == TOKEN_OPERATOR .and. t(i + 1)%value == '-') then
                        if (t(i + 2)%type /= TOKEN_IDENTIFIER) then
                            call throw('expected letter', fname, t(i + 2)%line, t(i + 2)%char)
                        end if
                        tempnode%value = t(i)%value//'-'//t(i + 2)%value
                        i = i + 4
                    else
                        call throw('syntax errror', fname, t(i)%line, t(i)%char)
                    end if
                    call result%append(tempnode, currentnode2)
                    call result%nodes(currentnode)%subnodes2%append(currentnode2)
                end do
                i = end2 + 2
                if (t(i)%type == TOKEN_OPERATOR) i = i + 1
                call result%nodes(result%nodes(currentnode)%parentnode)%subnodes2%append(currentnode)
                currentnode = result%nodes(currentnode)%parentnode
            end block
        end do
        if (t(i)%type /= TOKEN_NEXTLINE) then
            call throw('expected new line following implicit statement', fname, t(i)%line, t(i)%char)
        end if
    end subroutine
end submodule