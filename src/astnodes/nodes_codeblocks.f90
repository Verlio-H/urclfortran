submodule (astgen) nodes_codeblocks
    implicit none

contains
    module subroutine astnode_program(result, t, currentnode, fname, i)
        type(ast), intent(inout) :: result
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode

        if (currentnode /= 1) then
            call throw('program statement must be in global scope', fname, t(i)%line, t(i)%char)
        end if
        if (t(i + 1)%type /= TOKEN_IDENTIFIER) then
            call throw('program name must be an identifier', fname, t(i + 1)%line, t(i + 1)%char)
        end if
        tempnode = node()
        tempnode%type = NODE_PROGRAM
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%value = t(i + 1)%value
        tempnode%parentnode = 1
        
        call result%append(tempnode,currentnode)
        call result%nodes(1)%subnodes%append(currentnode)
        i = i + 1
        if (t(i + 1)%type /= TOKEN_NEXTLINE) then
            call throw('expected newline after module statement', fname, t(i + 1)%line, t(i + 1)%char)
        end if
    end subroutine

    module subroutine astnode_module(result, t, currentnode, fname, i)
        type(ast), intent(inout) :: result
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode

        if (currentnode /= 1) then
            call throw('module statement must be in global scope', fname, t(i)%line, t(i)%char)
        end if
        if (t(i + 1)%type /= TOKEN_IDENTIFIER) then
            call throw('module name must be an identifier', fname, t(i + 1)%line, t(i + 1)%char)
        end if
        tempnode = node()
        tempnode%type = NODE_MODULE
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%value = t(i + 1)%value
        tempnode%parentnode = 1
        
        call result%append(tempnode,currentnode)
        call result%nodes(1)%subnodes%append(currentnode)
        i = i + 1
        if (t(i + 1)%type /= TOKEN_NEXTLINE) then
            call throw('expected newline after module statement', fname, t(i + 1)%line, t(i + 1)%char)
        end if
    end subroutine

    module subroutine astnode_subroutine(result, t, currentnode, fname, childnode, i)
        type(ast), intent(inout) :: result
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        integer, intent(inout) :: childnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        type(node) :: tempnode

        select case (result%nodes(currentnode)%type)
        case (NODE_PROGRAM,NODE_MODULE)
            if (.not.result%nodes(currentnode)%secondpart) then
                call throw('invalid spot for subroutine statement', fname, t(i)%line, t(i)%char)
            end if
        case (NODE_ROOT)
        case default
            call throw('invalid spot for subroutine statement', fname, t(i)%line, t(i)%char)
        end select

        if (t(i + 1)%type /= TOKEN_IDENTIFIER) then
            call throw('expected subroutine name', fname, t(i + 1)%line, t(i + 1)%char)
        end if
        if (t(i + 2)%type /= TOKEN_LGROUP .or. t(i + 2)%value /= '(') then
            call throw('expected left parenthesis', fname, t(i + 2)%line, t(i + 2)%char)
        end if

        tempnode = node()
        tempnode%type = NODE_SUBROUTINE
        tempnode%startlnum = t(i)%line
        tempnode%startchar = t(i)%char
        tempnode%fname = fname
        tempnode%value = t(i + 1)%value
        tempnode%parentnode = currentnode

        call result%append(tempnode, currentnode)
        allocate(result%nodes(currentnode)%attributes(2))
        call result%nodes(tempnode%parentnode)%subnodes%append(currentnode)

        i = i + 3
        do while (t(i)%type /= TOKEN_RGROUP .or. t(i)%value /= ')')
            select case (t(i)%type)
            case (TOKEN_RGROUP)
                call throw('expected right parenthesis', fname, t(i)%line, t(i)%char)
            case (TOKEN_NEXTLINE)
                call throw('expected closing parenthesis', fname, t(i)%line, t(i)%char)
            case (TOKEN_OPERATOR)
                if (t(i)%value /= ',') then
                    call throw('unexpected token in subroutine declaration', fname, t(i)%line, t(i)%char)
                end if
                if (t(i - 1)%type /= TOKEN_IDENTIFIER) then
                    call throw('unexpected identifier in subroutine declaration', fname, t(i)%line, t(i)%char)
                end if
            case (TOKEN_IDENTIFIER)
                tempnode = node()
                tempnode%type = NODE_STRING
                tempnode%startlnum = t(i)%line
                tempnode%startchar = t(i)%char
                tempnode%fname = fname
                tempnode%value = t(i)%value
                tempnode%parentnode = currentnode
                call result%append(tempnode, childnode)
                call result%nodes(currentnode)%subnodes%append(childnode)
            case default
                call throw('unexpected token in subroutine declaration', fname, t(i)%line, t(i)%char)
            end select
            i = i + 1
        end do
        i = i + 1
        if (t(i)%type /= TOKEN_NEXTLINE) then
            call throw('expected new line following subroutine definition', fname, t(i)%line, t(i)%char)
        end if
    end subroutine
end submodule