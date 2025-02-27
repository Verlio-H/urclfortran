submodule (astgen) astprint
    implicit none

contains
    module recursive subroutine print_ast(currnode, fullast, depth)
        type(node), intent(in) :: currnode
        type(ast), intent(in) :: fullast
        integer, intent(in) :: depth

        integer :: i

        select case (currnode%type)
        case (NODE_ROOT)
            print '(A)', 'program:'
            do i = 1, currnode%subnodes%size - 1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, 2)
            end do
        case (NODE_PROGRAM)
            print '(A)', repeat(' ', depth)//'program statement:'
            print '(A)', repeat(' ', depth + 1)//'name: '//currnode%value
            print '(A)', repeat(' ', depth + 1)//'procedures:'
            do i = 1, currnode%subnodes%size - 1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, depth + 2)
            end do
            print '(A)', repeat(' ', depth + 1)//'statements:'
            do i = 1, currnode%subnodes2%size - 1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)), fullast, depth + 2)
            end do
        case (NODE_MODULE)
            print '(A)', repeat(' ', depth)//'module:'
            print '(A)', repeat(' ', depth + 1)//'name: '//currnode%value
            print '(A)', repeat(' ', depth + 1)//'procedures:'
            do i = 1, currnode%subnodes%size - 1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, depth + 2)
            end do
            print '(A)', repeat(' ', depth + 1)//'statements:'
            do i = 1, currnode%subnodes2%size - 1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)), fullast, depth + 2)
            end do
        case (NODE_SUBROUTINE)
            print '(A)', repeat(' ', depth)//'subroutine:'
            print '(A)', repeat(' ', depth + 1)//'name: '//currnode%value
            print '(A)', repeat(' ', depth + 1)//'arguments:'
            do i = 1, currnode%subnodes%size - 1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, depth + 2)
            end do
            print '(A)', repeat(' ', depth + 1)//'statements:'
            do i = 1, currnode%subnodes2%size - 1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)), fullast, depth + 2)
            end do
        case (NODE_ASSIGNMENT)
            print '(A)', repeat(' ', depth)//'assign:'
            call print_ast(fullast%nodes(currnode%subnodes%array(1)), fullast, depth + 1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)), fullast, depth + 1)
        case (NODE_IMPLICIT)
            print '(A)', repeat(' ', depth)//'implicit:'
            print '(A)', repeat(' ', depth + 1)//'type:'
            if (.not.allocated(currnode%subnodes%array)) then
                print '(A)', 'expected type in implicit statement'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)), fullast, depth + 2)
            print '(A)', repeat(' ', depth + 1)//'letters:'
            do i = 1, currnode%subnodes2%size - 1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)), fullast, depth + 2)
            end do
        case (NODE_USE)
            print '(A)', repeat(' ', depth)//'use:'
            print '(A)', repeat(' ', depth + 1)//currnode%value
            if (allocated(currnode%subnodes%array)) then
                print '(A)', repeat(' ', depth)//'only:'
                do i = 1, currnode%subnodes%size - 1
                    call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, depth + 2)
                end do
            end if
        case (NODE_CALL)
            print '(A)', repeat(' ', depth)//'call '//trim(currnode%value)//':'
            if (allocated(currnode%subnodes%array)) then
                do i = 1, currnode%subnodes%size - 1
                    call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, depth + 1)
                end do
            end if
        case (NODE_INT_VAL)
            print '(A)', repeat(' ', depth)//'int: '//currnode%value
        case (NODE_REAL_VAL)
            print '(A)', repeat(' ', depth)//'real: '//currnode%value
        case (NODE_LOGICAL_VAL)
            print '(A)', repeat(' ', depth)//'logical: '//currnode%value
        case (NODE_CHAR_VAL)
            select type (val => currnode%value2)
            type is (character(*))
                print '(A)', repeat(' ', depth)//'char: '//val
            end select 
        case (NODE_ADD, NODE_SUB, NODE_MLT, NODE_DIV, NODE_EXP, NODE_MEMBER, NODE_EQ, NODE_NE, NODE_LT, NODE_LE, NODE_GT, NODE_GE, &
                NODE_AND, NODE_OR)
            select case (currnode%type)
            case (NODE_ADD)
                print '(A)', repeat(' ', depth)//'add:'
            case (NODE_SUB)
                print '(A)', repeat(' ', depth)//'sub:'
            case (NODE_MLT)
                print '(A)', repeat(' ', depth)//'mlt:'
            case (NODE_DIV)
                print '(A)', repeat(' ', depth)//'div:'
            case (NODE_EXP)
                print '(A)', repeat(' ', depth)//'pow:'
            case (NODE_MEMBER)
                print '(A)', repeat(' ', depth)//'member access:'
            case (NODE_EQ)
                print '(A)', repeat(' ', depth)//'eq:'
            case (NODE_NE)
                print '(A)', repeat(' ', depth)//'ne:'
            case (NODE_LT)
                print '(A)', repeat(' ', depth)//'lt:'
            case (NODE_LE)
                print '(A)', repeat(' ', depth)//'le:'
            case (NODE_GT)
                print '(A)', repeat(' ', depth)//'gt:'
            case (NODE_GE)
                print '(A)', repeat(' ', depth)//'ge:'
            case (NODE_NOT)
                print '(A)', repeat(' ', depth)//'not:'
            case (NODE_AND)
                print '(A)', repeat(' ', depth)//'and:'
            case (NODE_OR)
                print '(A)', repeat(' ', depth)//'or:'
            end select
            if (.not.allocated(currnode%subnodes%array) .or. .not.allocated(currnode%subnodes2%array)) then
                print '(A)', 'expected 2 arguments in operation'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)), fullast, depth + 1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)), fullast, depth + 1)
        case (NODE_NOT)
            print '(A)', repeat(' ', depth)//'not:'
            if (.not.allocated(currnode%subnodes2%array)) then
                print '(A)', 'expected 1 argument in not'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)), fullast, depth + 1)
        case (NODE_FNC_ARR)
            print '(A)', repeat(' ', depth)//'funccall or array: '//currnode%value
            if (allocated(currnode%subnodes%array)) then
                do i = 1, currnode%subnodes%size - 1
                    call print_ast(fullast%nodes(currnode%subnodes%array(i)), fullast, depth + 1)
                end do
            end if
        case (NODE_IF, NODE_ELSE_IF)
            if (currnode%type == NODE_IF) then
                print '(A)', repeat(' ', depth)//'if:'
            else
                print '(A)', repeat(' ', depth)//'else if:'
            end if
            print '(A)', repeat(' ', depth + 1)//trim(currnode%value)
            if (.not.allocated(currnode%subnodes%array)) then
                print '(A)', 'missing condition in if statement'
                stop
            end if
            if (.not.allocated(currnode%subnodes2%array)) then
                print '(A)', 'missing body in if statement'
                stop
            end if
            print '(A)', repeat(' ', depth + 1)//'condition:'
            call print_ast(fullast%nodes(currnode%subnodes%array(1)), fullast, depth + 2)
            print '(A)', repeat(' ', depth + 1)//'contents:'
            do i = 1, currnode%subnodes2%size - 1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)), fullast, depth + 2)
            end do
        case (NODE_ELSE)
            print '(A)', repeat(' ', depth)//'else:'
            do i = 1, currnode%subnodes2%size - 1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)), fullast, depth + 1)
            end do
        case (NODE_TYPE)
            select type (a => currnode%value2)
            type is (integer(SMALL))
                select case (a)
                case (TYPE_INTEGER)
                    print '(A)', repeat(' ', depth)//'type: integer'
                case (TYPE_REAL)
                    print '(A)', repeat(' ', depth)//'type: real'
                case (TYPE_COMPLEX)
                    print '(A)', repeat(' ', depth)//'type: complex'
                case (TYPE_NONE)
                    print '(A)', repeat(' ', depth)//'type: none'
                case default
                    print '(A)', repeat(' ', depth)//'type: unknown'
                end select
                if (a /= TYPE_NONE) then
                    print '(A)', repeat(' ', depth)//'kind:'
                    call print_ast(fullast%nodes(currnode%subnodes2%array(1)), fullast, depth + 1)
                    print '(A)', repeat(' ', depth)//'len:'
                    call print_ast(fullast%nodes(currnode%subnodes2%array(2)), fullast, depth + 1)
                    if (currnode%value /= '') print '(A)', repeat(' ', depth)//'name: '//currnode%value
                    associate (b => currnode%subnodes%array(1))
                        if (b /= 0) then
                            print '(A)', repeat(' ', depth + 1)//'properties:'
                            if (iand(b, PROP_VALUE) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'value'
                            end if
                            if (iand(b, PROP_INTENTIN) /= 0) then
                                if (iand(b, PROP_INTENTOUT) /= 0) then
                                    print '(A)', repeat(' ', depth + 2)//'intent(inout)'
                                else
                                    print '(A)', repeat(' ', depth + 2)//'intent(in)'
                                end if
                            else if (iand(b, PROP_INTENTOUT) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'intent(out)'
                            end if
                            if (iand(b, PROP_ALLOCATABLE) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'allocatable'
                            end if
                            if (iand(b, PROP_POINTER) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'pointer'
                            end if
                            if (iand(b, PROP_PARAMETER) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'parameter'
                            end if
                            if (iand(b, PROP_SAVE) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'save'
                            end if
                            if (iand(b, PROP_OPTIONAL) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'optional'
                            end if
                            if (iand(b, PROP_TARGET) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'target'
                            end if
                            if (iand(b, PROP_PRIVATE) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'private'
                            end if
                            if (iand(b, PROP_PROTECTED) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'protected'
                            end if
                            if (iand(b, PROP_EXTERNAL) /= 0) then
                                print '(A)', repeat(' ', depth + 2)//'external'
                            end if
                        end if 
                    end associate
                    if (currnode%subnodes%size - 1 >= 2) then
                        print '(A)', repeat(' ', depth + 1)//'value:'
                        call print_ast(fullast%nodes(currnode%subnodes%array(2)), fullast, depth + 2)
                    end if
                end if
            class default
                print '(A)', 'unexpected type'
                stop
            end select
        case (NODE_STRING)
            print '(A)', repeat(' ', depth)//'string: '//currnode%value
        case default
            print '(A)', repeat(' ', depth)//'unknown node'
        end select
    end subroutine
end submodule