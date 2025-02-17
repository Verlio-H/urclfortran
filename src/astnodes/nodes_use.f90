submodule (astgen) nodes_use
    implicit none

contains
    module subroutine astnode_use(result, t, currentnode, currentnode2, fname, childnode, i)
        type(ast), intent(inout) :: result
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        integer, intent(inout) :: currentnode2
        character(*), intent(in) :: fname
        integer, intent(inout) :: childnode
        integer, intent(inout) :: i

        type(node) :: tempnode

        select case (result%nodes(currentnode)%type)
        case (NODE_PROGRAM, NODE_MODULE)
            if (result%nodes(currentnode)%secondpart) then
                call throw('invalid spot for use statement', fname, t(i)%line, t(i)%char)
            end if
        case default
            call throw('invalid spot for use statement', fname, t(i)%line, t(i)%char)
        end select
        i = i + 1
        if (t(i)%type /= TOKEN_IDENTIFIER) then
            call throw('expected identifier in use statement', fname, t(i)%line, t(i)%char)
        end if

        tempnode = node()
        tempnode%type = NODE_USE
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%parentnode = currentnode
        tempnode%value = t(i)%value

        i = i + 1
        call result%append(tempnode, currentnode2)
        call result%nodes(currentnode)%subnodes2%append(currentnode2)
        if (t(i)%type == TOKEN_OPERATOR .and. t(i)%value == ',') then
            i = i + 1
            if (t(i)%type /= TOKEN_IDENTIFIER .or. t(i)%value /= 'ONLY') then
                call throw('syntax error in use statement',fname,t(i)%line,t(i)%char)
            end if
            i = i + 1
            if (t(i)%type /= TOKEN_OPERATOR .or. t(i)%value /= ':') then
                call throw('syntax error in use statement', fname, t(i)%line, t(i)%char)
            end if
            i = i + 1
            do while (t(i)%type /= TOKEN_NEXTLINE)
                tempnode = node()
                tempnode%type = NODE_STRING
                tempnode%startlnum = t(i)%line
                tempnode%startchar = t(i)%char
                tempnode%fname = fname
                tempnode%parentnode = currentnode2
                if (t(i)%type /= TOKEN_IDENTIFIER) then
                    call throw('syntax error in use statement', fname, t(i)%line, t(i)%char)
                end if
                if (t(i + 1)%type == TOKEN_OPERATOR .and. t(i + 1)%value == '=>') then
                    if (t(i + 2)%type /= TOKEN_IDENTIFIER) then
                        call throw('syntax error in use statement', fname, t(i)%line, t(i)%char)
                    end if
                    tempnode%value = t(i)%value//'-'//t(i + 2)%value
                    i = i + 3
                else
                    tempnode%value = t(i)%value
                    i = i + 1
                end if
                if (t(i)%type == TOKEN_OPERATOR .and. t(i)%value == ',') then
                    i = i + 1
                end if
                call result%append(tempnode, childnode)
                call result%nodes(currentnode2)%subnodes%append(childnode)
            end do
        end if
    end subroutine
end submodule