submodule (astgen) parsing
    use include, only: itoa2, precedence
    use lexer, only: TOKEN_LGROUP, TOKEN_RGROUP, TOKEN_OPERATOR, TOKEN_VALUE_CHAR, TOKEN_VALUE_INT, TOKEN_VALUE_LOGICAL, &
                        TOKEN_VALUE_REAL, TOKEN_ASTERISK
    implicit none
    
contains
    module function isop(op) result(result)
        logical :: result
        integer(SMALL), intent(in) :: op

        if (op < RPN_LGROUP .and. op > RPN_CHAR) then
            result = .true.
        else
            result = .false.
        end if
    end function

    module function parse_type(tree, tokens, start, end, fname) result(resulttype)
        integer :: resulttype
        type(ast), intent(inout) :: tree
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start
        integer, intent(in) :: end
        character(*), intent(in) :: fname

        integer :: currnode
        type(node) :: tempnode, tempnode2
        integer :: i, idx

        tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
        tempnode%type = NODE_TYPE
        i = start
        idx = 1
        do while (i <= end)
            associate (tok => tokens%tokens(i))
                if (tok%type == TOKEN_IDENTIFIER) then
                    if (allocated(tempnode%value2)) then
                        call throw('type already specified', fname, tok%line, tok%char)
                    end if
                    select case (tok%value)
                    case ('INTEGER')
                        tempnode%value2 = TYPE_INTEGER
                        call tree%append(tempnode, resulttype)
                    case ('REAL')
                        tempnode%value2 = TYPE_REAL
                        call tree%append(tempnode, resulttype)
                    case ('COMPLEX')
                        tempnode%value2 = TYPE_COMPLEX
                        call tree%append(tempnode, resulttype)
                    case ('LOGICAL')
                        tempnode%value2 = TYPE_LOGICAL
                        call tree%append(tempnode, resulttype)
                    case ('CHARACTER')
                        tempnode%value2 = TYPE_CHARACTER
                        call tree%append(tempnode, resulttype)
                        idx = 2
                    case default
                        call throw('unknown type: '//tok%value, fname, tok%line, tok%char)
                    end select

                    call tree%nodes(resulttype)%subnodes2%append(0)
                    call tree%nodes(resulttype)%subnodes2%append(0)
                else
                    block
                        integer :: depth, tmpi

                        if (tok%value == ',') then
                            i = i + 1
                            tmpi = i
                        else
                            tmpi = i + 1
                        end if
                        
                        associate(kindtok => tokens%tokens(tmpi))
                            if (tokens%tokens(tmpi + 1)%type == TOKEN_ASSIGN) then
                                if (kindtok%value /= 'KIND' .and. kindtok%value /= 'LEN') then
                                    call throw('unknown parameter for type', fname, kindtok%line, kindtok%char)
                                end if
                                if (kindtok%value == 'KIND') then
                                    idx = 1
                                else ! kindtok%value == 'LEN'
                                    idx = 2
                                end if
                                i = i + 2
                                tmpi = tmpi + 2
                            end if
                        end associate
                        depth = 1
                        do while (depth /= 0 .or. tokens%tokens(i)%type /= TOKEN_RGROUP)
                            if (depth == 1 .and. tokens%tokens(i)%type == TOKEN_OPERATOR .and. tokens%tokens(i)%value == ',') exit
                            i = i + 1
                            if (tokens%tokens(i)%type == TOKEN_LGROUP) depth = depth + 1
                            if (tokens%tokens(i)%type == TOKEN_RGROUP) depth = depth - 1
                        end do
                        ! find end
                        i = i - 1
                        call parse_expr(tree, resulttype, tokens, tmpi, i, fname, .true., idx)
                        idx = 1
                        !i = i + 1
                        if (tokens%tokens(i + 1)%type == TOKEN_RGROUP) i = i + 1
                    end block
                end if
            end associate
            i = i + 1
        end do

        if (tree%nodes(resulttype)%subnodes2%array(1) == 0) then
            tempnode2 = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode2%type = NODE_INT_VAL
            tempnode2%value = '4'
            call tree%append(tempnode2, currnode)
            tree%nodes(resulttype)%subnodes2%array(1) = currnode
        end if
        if (tree%nodes(resulttype)%subnodes2%array(2) == 0) then
            tempnode2 = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode2%type = NODE_INT_VAL
            tempnode2%value = '1'
            call tree%append(tempnode2, currnode)
            tree%nodes(resulttype)%subnodes2%array(2) = currnode
        end if
        call tree%nodes(resulttype)%subnodes%append(0)
    end function

    module function parse_bigtype(tree,tokens,start,end,fname) result(resulttype)
        integer :: resulttype
        type(ast), intent(inout) :: tree
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start
        integer, intent(in) :: end
        character(*), intent(in) :: fname

        integer :: i, depth, final

        depth = 0
        final = end

        big: &
        do i = start,end
            associate (tok=>tokens%tokens(i))
                select case (tok%type)
                case (TOKEN_LGROUP)
                    depth = depth + 1
                case (TOKEN_RGROUP)
                    depth = depth - 1
                case (TOKEN_OPERATOR)
                    if (depth == 0) then
                        select case (tok%value)
                        case (',')
                            final = i - 1
                            exit big
                        end select
                    end if
                end select
            end associate
        end do big
        resulttype = parse_type(tree, tokens, start, final, fname)
        if (final == end) return
        i = final + 2
        do while (i <= end)
            ! check for attributes
            associate (tok => tokens%tokens(i))
                if (tok%type == TOKEN_OPERATOR .and. tok%value == ',') then
                else if (tok%type == TOKEN_IDENTIFIER) then
                    select case (tok%value)
                    case ('VALUE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_VALUE)
                    case ('ALLOCATABLE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_ALLOCATABLE)
                    case ('POINTER')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_POINTER)
                    case ('PARAMETER')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_PARAMETER)
                    case ('SAVE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_SAVE)
                    case ('OPTIONAL')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_OPTIONAL)
                    case ('TARGET')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_TARGET)
                    case ('PRIVATE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_PRIVATE)
                    case ('PUBLIC')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_PUBLIC)
                    case ('PROTECTED')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_PROTECTED)
                    case ('EXTERNAL')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_EXTERNAL)
                    case ('INTENT')
                        i = i + 1
                        if (tokens%tokens(i)%type /= TOKEN_LGROUP) then
                            call throw('invalid syntax in intent attribute', fname, tok%line, tok%char)
                        end if
                        i = i + 1
                        if (tokens%tokens(i)%type /= TOKEN_IDENTIFIER) then
                            call throw('invalid syntax in intent attribute', fname, tok%line, tok%char)
                        end if
                        select case (tokens%tokens(i)%value)
                        case ('IN')
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_INTENTIN)
                        case ('OUT')
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_INTENTOUT)
                        case ('INOUT')
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_INTENTIN)
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1), PROP_INTENTOUT)
                        case default
                            call throw('invalid syntax in intent attribute', fname, tok%line, tok%char)
                        end select
                        i = i + 1
                        if (tokens%tokens(i)%type /= TOKEN_RGROUP) then
                            call throw('invalid syntax in intent attribute', fname, tok%line, tok%char)
                        end if
                    case default
                        call throw('unknown attribute', fname, tok%line, tok%char)
                    end select
                else
                    call throw('unexpected symbol in type declaration', fname, tok%line, tok%char)
                end if
            end associate
            i = i + 1
        end do
    end function


    ! TODO: list for shunting yard:
    !   logicals
    !   characters
    !   ranges
    !   array constructors
    !   ternarys (f2023)
    module function shunting(tokens, start, end, fname)
        type(rpn) :: shunting
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start
        integer, intent(in) :: end
        character(*), intent(in) :: fname

        type(rpnnode) :: stack(64)
        integer(SMALL) :: sp

        integer :: i
        character(64) :: lastident
        logical :: isident

        isident = .false.
        sp = 0

        associate (t=>tokens%tokens)
           do i = start, end
                if (isident .and. t(i)%type /= TOKEN_LGROUP) then
                    isident = .false.
                    call shunting%things%append(RPN_IDENT)
                    call shunting%vals%append(lastident)
                end if
                select case (t(i)%type)
                case (TOKEN_IDENTIFIER)
                    isident = .true.
                    lastident = t(i)%value
                    cycle
                case (TOKEN_VALUE_CHAR)
                    call shunting%things%append(RPN_CHAR)
                    call shunting%vals%append(t(i)%value)
                case (TOKEN_VALUE_INT)
                    call shunting%things%append(RPN_INT)
                    call shunting%vals%append(t(i)%value)
                case (TOKEN_VALUE_REAL)
                    call shunting%things%append(RPN_REL)
                    call shunting%vals%append(t(i)%value)
                case (TOKEN_VALUE_LOGICAL)
                    call shunting%things%append(RPN_LOG)
                    call shunting%vals%append(t(i)%value)
                case (TOKEN_LGROUP)
                    sp = sp + 1_SMALL
                    if (isident) then
                        stack(sp) = rpnnode(RPN_LGROUP,string(lastident))
                        call shunting%things%append(RPN_RGROUP)
                        call shunting%vals%append(')')
                        isident = .false.
                    else
                        stack(sp)=rpnnode(RPN_LGROUP, string('('))
                    end if
                case (TOKEN_RGROUP)
                    do while (stack(sp)%thing /= RPN_LGROUP)
                        if (sp <= 0) call throw('mismatched parenthesis', fname, t(i)%line, t(i)%char)
                        call shunting%things%append(stack(sp)%thing)
                        call shunting%vals%append(stack(sp)%val%value)
                        sp = sp - 1_SMALL
                    end do
                    if (stack(sp)%val%value /= '(') then
                        call shunting%things%append(RPN_LGROUP)
                        call shunting%vals%append(stack(sp)%val%value)
                    end if
                    sp = sp - 1_SMALL
                case (TOKEN_OPERATOR, TOKEN_ASTERISK)
                    if (t(i)%value /= ',') then
                        if (t(i)%value == '**') then
                            do while (sp > 0)
                                if (.not.(isop(stack(sp)%thing) .and. &
                                    precedence(stack(sp)%val%value) > precedence(t(i)%value))) exit
                                call shunting%things%append(stack(sp)%thing)
                                call shunting%vals%append(stack(sp)%val%value)
                                sp = sp - 1_SMALL
                            end do
                        else
                            do while (sp > 0)
                                if (.not.(isop(stack(sp)%thing) .and. &
                                    precedence(stack(sp)%val%value) >= precedence(t(i)%value))) exit
                                call shunting%things%append(stack(sp)%thing)
                                call shunting%vals%append(stack(sp)%val%value)
                                sp = sp - 1_SMALL
                            end do
                        end if
                        sp = sp + 1_SMALL
                        select case (t(i)%value)
                        case ('**')
                            stack(sp) = rpnnode(RPN_POW, string('**'))
                        case ('*')
                            stack(sp) = rpnnode(RPN_MLT, string('*'))
                        case ('/')
                            stack(sp) = rpnnode(RPN_DIV, string('/'))
                        case ('+')
                            stack(sp) = rpnnode(RPN_ADD, string('+'))
                        case ('-')
                            stack(sp) = rpnnode(RPN_SUB, string('-'))
                        case ('%')
                            stack(sp) = rpnnode(RPN_MEMBER, string('%'))
                        case ('.EQ.')
                            stack(sp) = rpnnode(RPN_EQ, string('.EQ.'))
                        case ('.NE.')
                            stack(sp) = rpnnode(RPN_NE, string('.NE.'))
                        case ('.LT.')
                            stack(sp) = rpnnode(RPN_LT, string('.LT.'))
                        case ('.LE.')
                            stack(sp) = rpnnode(RPN_LE, string('.LE.'))
                        case ('.GT.')
                            stack(sp) = rpnnode(RPN_GT, string('.GT.'))
                        case ('.GE.')
                            stack(sp) = rpnnode(RPN_GE, string('.GE.'))
                        case ('.NOT.')
                            stack(sp) = rpnnode(RPN_NOT, string('.NOT.'))
                        case ('.AND.')
                            stack(sp) = rpnnode(RPN_AND, string('.AND.'))
                        case ('.OR.')
                            stack(sp) = rpnnode(RPN_OR, string('.OR.'))
                        case ('.EQV.')
                            stack(sp) = rpnnode(RPN_EQV, string('.EQV.'))
                        case ('.NEQV.')
                            stack(sp) = rpnnode(RPN_NEQV, string('.NEQV.'))
                        case default
                            call throw('unknown operator', fname, t(i)%line, t(i)%char)
                        end select
                    else
                        do while (sp > 0 .and. stack(sp)%thing /= RPN_LGROUP)
                            call shunting%things%append(stack(sp)%thing)
                            call shunting%vals%append(stack(sp)%val%value)
                            sp = sp - 1_SMALL
                        end do
                    end if
                end select
            end do
            if (isident) then
                call shunting%things%append(RPN_IDENT)
                call shunting%vals%append(lastident)
            end if
            do sp = sp, 1_SMALL, -1_SMALL
                if (stack(sp)%thing == RPN_LGROUP) call throw('mismatched parenthesis', fname, t(i)%line, t(i)%char)
                call shunting%things%append(stack(sp)%thing)
                call shunting%vals%append(stack(sp)%val%value)
            end do
        end associate
    end function

    module subroutine print_rpn(input)
        type(rpn), intent(in) :: input

        integer :: i

        do i = 1, input%things%size - 1
            select case (input%things%array(i))
            case (RPN_INT)
                write(* , '(A)', advance='no') 'i:'//input%vals%array(i)%value//' '
            case (RPN_REL)
                write(* , '(A)', advance='no') 'r:'//input%vals%array(i)%value//' '
            case (RPN_LOG)
                 write(* , '(A)', advance='no') 'l:'//input%vals%array(i)%value//' '
            case (RPN_ADD)
                write(* , '(A)', advance='no') '+ '
            case (RPN_SUB)
                write(* , '(A)', advance='no') '- '
            case (RPN_MLT)
                write(* , '(A)', advance='no') '* '
            case (RPN_DIV)
                write(* , '(A)', advance='no') '/ '
            case (RPN_POW)
                write(* , '(A)', advance='no') '** '
            case (RPN_IDENT)
                write(* , '(A)', advance='no') 'ident:'//trim(input%vals%array(i)%value)//' '
            case (RPN_LGROUP)
                write(* , '(A)', advance='no') trim(input%vals%array(i)%value)//'( '
            case (RPN_RGROUP)
                write(* , '(A)', advance='no') ') '
            case (RPN_MEMBER)
                write(* , '(A)', advance='no') '% '
            case (RPN_EQ)
                write(* , '(A)', advance='no') '== '
            case (RPN_NE)
                write(* , '(A)', advance='no') '!= '
            case (RPN_LT)
                write(* , '(A)', advance='no') '< '
            case (RPN_LE)
                write(* , '(A)', advance='no') '<= '
            case (RPN_GT)
                write(* , '(A)', advance='no') '> '
            case (RPN_GE)
                write(* , '(A)', advance='no') '>= '
            case (RPN_NOT)
                write(* , '(A)', advance='no') '! '
            case (RPN_AND)
                write(* , '(A)', advance='no') '&& '
            case (RPN_OR)
                write(* , '(A)', advance='no') '|| '
            case (RPN_EQV)
                write(* , '(A)', advance='no') '=== '
            case (RPN_NEQV)
                write(* , '(A)', advance='no') '!== '
            end select
        end do
        write(* , '(A)') ''
    end subroutine

    module subroutine parse_expr(tree, currnode, tokens, start, end, fname, two, idx)
        type(ast), intent(inout) :: tree
        integer, intent(in) :: currnode
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start
        integer, intent(in) :: end
        character(*), intent(in) :: fname
        logical, intent(in) :: two
        integer, optional, intent(in) :: idx

        type(rpn) :: postfix
        type(rpn) :: prefix
        integer :: i

        postfix = shunting(tokens,start,end,fname)

        do i = postfix%things%size - 1, 1, -1
            call prefix%things%append(postfix%things%array(i))
            call prefix%vals%append(postfix%vals%array(i)%value)
        end do

        i = 1

        if (allocated(prefix%things%array)) then
            if (present(idx)) then
                call parse_expr_add(tree, currnode, prefix, i, two, idx)
            else
                call parse_expr_add(tree, currnode, prefix, i, two)
            end if
            if (i /= prefix%things%size) then
                call throw('syntax error in expression', fname, tokens%tokens(end)%line, tokens%tokens(end)%char)
            end if
        end if
    end subroutine

    subroutine parse_append(tree, currnode, value, two, idx)
        type(ast), intent(inout) :: tree
        integer, intent(in) :: currnode
        integer, intent(in) :: value
        logical, intent(in) :: two
        integer, optional, intent(in) :: idx

        if (two) then
            if (present(idx)) then
                tree%nodes(currnode)%subnodes2%array(idx) = value
            else
                call tree%nodes(currnode)%subnodes2%append(value)
            end if
        else
            if (present(idx)) then
                tree%nodes(currnode)%subnodes%array(idx) = value
            else
                call tree%nodes(currnode)%subnodes%append(value)
            end if
        end if
    end subroutine

    recursive subroutine parse_expr_add(tree, currnode, prefix, i, two, idx)
        type(ast), intent(inout) :: tree
        integer, intent(in) :: currnode
        type(rpn), intent(in) :: prefix
        integer, intent(inout) :: i
        logical, intent(in) :: two
        integer, optional, intent(in) :: idx

        type(node) :: tempnode
        integer :: currnode2,currnode3

        if (i > prefix%things%size - 1) then
            call throw('outside of bounds', 'unknown', 0_SMALL, 0_SMALL)
        end if
        select case (prefix%things%array(i))
        case (RPN_INT)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_INT_VAL
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
        case (RPN_REL)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_REAL_VAL
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
        case (RPN_CHAR)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_CHAR_VAL
            tempnode%parentnode = currnode
            tempnode%value2 = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
        case (RPN_LOG)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_LOGICAL_VAL
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
        case (RPN_LGROUP)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_FNC_ARR
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
            do while (prefix%things%array(i) /= RPN_RGROUP)
                call parse_expr_add(tree, currnode2, prefix, i, .false.)
            end do
            i = i + 1
        case (RPN_IDENT)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            tempnode%type = NODE_STRING
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
        case (RPN_ADD, RPN_SUB, RPN_MLT, RPN_DIV, RPN_POW, RPN_MEMBER, RPN_EQ, RPN_NE, RPN_LT, RPN_LE, RPN_GT, RPN_GE, RPN_NOT, &
                RPN_AND, RPN_OR, RPN_EQV, RPN_NEQV)
            tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
            select case (prefix%things%array(i))
            case (RPN_ADD)
                tempnode%type = NODE_ADD
            case (RPN_SUB)
                tempnode%type = NODE_SUB
            case (RPN_MLT)
                tempnode%type = NODE_MLT
            case (RPN_DIV)
                tempnode%type = NODE_DIV
            case (RPN_POW)
                tempnode%type = NODE_EXP
            case (RPN_MEMBER)
                tempnode%type = NODE_MEMBER
            case (RPN_EQ, RPN_EQV)
                tempnode%type = NODE_EQ
            case (RPN_NE, RPN_NEQV)
                tempnode%type = NODE_NE
            case (RPN_LT)
                tempnode%type = NODE_LT
            case (RPN_LE)
                tempnode%type = NODE_LE
            case (RPN_GT)
                tempnode%type = NODE_GT
            case (RPN_GE)
                tempnode%type = NODE_GE
            case (RPN_NOT)
                tempnode%type = NODE_NOT
            case (RPN_AND)
                tempnode%type = NODE_AND
            case (RPN_OR)
                tempnode%type = NODE_OR
            end select
            tempnode%parentnode = currnode
            call tree%append(tempnode,currnode2)
            if (present(idx)) then
                call parse_append(tree, currnode, currnode2, two, idx)
            else
                call parse_append(tree, currnode, currnode2, two)
            end if
            i = i + 1
            call parse_expr_add(tree, currnode2, prefix, i, .true.)
            if (tempnode%type == NODE_NOT) return
            if (i >= prefix%things%size) then
                tempnode = node(0, 0, 0, '', null(), 0, .false., null(), iarr(), iarr(), null())
                tempnode%type = NODE_INT_VAL
                tempnode%parentnode = currnode2
                tempnode%value = '0'
                call tree%append(tempnode, currnode3)
                call tree%nodes(currnode2)%subnodes%append(currnode3)
                return
            end if
            call parse_expr_add(tree, currnode2, prefix, i, .false.)
        case default
            if (allocated(tree%nodes(currnode)%fname)) then
                call throw('unknown rpn node', tree%nodes(currnode)%fname, &
                            tree%nodes(currnode)%startlnum, tree%nodes(currnode)%startchar)
            else
                call throw('unknown rpn node '//itoa2(prefix%things%array(i)), 'unknown', &
                            tree%nodes(currnode)%startlnum,tree%nodes(currnode)%startchar)
            end if
        end select
    end subroutine
end submodule