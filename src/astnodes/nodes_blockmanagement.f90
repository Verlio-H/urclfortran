submodule (astgen) nodes_blockmanagement
    implicit none

contains
    module subroutine astnode_end(result, t, currentnode, fname, i)
        type(ast), intent(inout) :: result
        type(token), intent(in) :: t(:)
        integer, intent(inout) :: currentnode
        character(*), intent(in) :: fname
        integer, intent(inout) :: i

        select case (t(i + 1)%type)
        case (TOKEN_NEXTLINE)
            select case (result%nodes(currentnode)%type)
            case (NODE_PROGRAM, NODE_SUBROUTINE, NODE_MODULE)
                currentnode = result%nodes(currentnode)%parentnode
                i = i + 1
            case default
                call throw('block type must be stated in end statement', fname, t(i + 1)%line, t(i + 1)%char)
            end select
        case (TOKEN_IDENTIFIER)
            select case (t(i + 1)%value)
            case ('PROGRAM', 'ENDPROGRAM')
                if (result%nodes(currentnode)%type /= NODE_PROGRAM) then
                    call throw('incorrect block type in end statement', fname, t(i + 1)%line, t(i + 1)%char)
                end if
            case ('MODULE', 'ENDMODULE')
                if (result%nodes(currentnode)%type /= NODE_MODULE) then
                    call throw('incorrect block type in end statement', fname, t(i + 1)%line, t(i + 1)%char)
                end if
            case ('SUBROUTINE', 'ENDSUBROUTINE')
                if (result%nodes(currentnode)%type /= NODE_SUBROUTINE) then
                    call throw('incorrect block type in end statement', fname, t(i + 1)%line, t(i + 1)%char)
                end if
            case ('IF', 'ENDIF', 'ELSE')
                if (result%nodes(currentnode)%type /= NODE_IF .and. result%nodes(currentnode)%type /= NODE_ELSE .and. &
                    result%nodes(currentnode)%type /= NODE_ELSE_IF) then
                    call throw('incorrect block type in end statement', fname, t(i + 1)%line, t(i + 1)%char)
                end if
            case default
                call throw('unknown block type in end statement', fname, t(i + 1)%line, t(i + 1)%char)
            end select
            if (t(i + 1)%value == 'ELSE') then
                currentnode = result%nodes(currentnode)%parentnode
                return
            end if
            select case (t(i + 2)%type)
            case (TOKEN_NEXTLINE)
                ! TODO: deal with named non program unit blocks
                currentnode = result%nodes(currentnode)%parentnode
                i = i + 2
            case (TOKEN_IDENTIFIER)
                if (t(i + 2)%value /= result%nodes(currentnode)%value) then
                    call throw('incorrect block name in end statement', fname, t(i + 2)%line, t(i + 2)%char)
                end if
                currentnode = result%nodes(currentnode)%parentnode
                i = i + 3
            case default
                call throw('expected block name in end statement', fname, t(i + 2)%line, t(i + 2)%char)
            end select
        case default
            call throw('unknown block type in end statement', fname, t(i + 1)%line, t(i + 1)%char)
        end select
    end subroutine
end submodule