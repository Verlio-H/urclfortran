submodule (astgen) parsing
    implicit none
    
contains
    module logical function isop(op)
        integer(SMALL), intent(in) :: op
        if (op<RPN_LGROUP.and.op>RPN_CHAR) then
            isop = .true.
        else
            isop = .false.
        end if
    end function

    module function parseType(tree,tokens,start,end,fname) result(resulttype)
        type(ast), intent(inout) :: tree
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start, end
        integer :: resulttype, currnode
        type(node) :: tempnode, tempnode2

        character(len=*), intent(in) :: fname

        integer :: i

        tempnode = node()
        tempnode%type = NODE_TYPE
        i = start
        do while (i<=end)
            associate (tok=>tokens%tokens(i))
                if (tok%type == TOKEN_IDENTIFIER) then
                    if (allocated(tempnode%value2)) then
                        call throw('type already specified',fname,tok%line,tok%char)
                    end if
                    select case (tok%value)
                    case ('INTEGER')
                        tempnode%value2 = TYPE_INTEGER
                        call tree%append(tempnode,resulttype)
                    case ('REAL')
                        tempnode%value2 = TYPE_REAL
                        call tree%append(tempnode,resulttype)
                    case ('COMPLEX')
                        tempnode%value2 = TYPE_COMPLEX
                        call tree%append(tempnode,resulttype)
                    case default
                        call throw('unknown type: '//tok%value,fname,tok%line,tok%char)
                    end select
                else if (tok%type == TOKEN_LGROUP .and. tok%value == '(') then
                    block
                        integer :: depth, tmpi
                        tmpi = i + 1
                        depth = 1
                        do while (depth/=0.or.tokens%tokens(i)%type/=TOKEN_RGROUP)
                            i = i + 1
                            if (tokens%tokens(i)%type==TOKEN_LGROUP) depth = depth + 1
                            if (tokens%tokens(i)%type==TOKEN_RGROUP) depth = depth - 1
                        end do
                        ! find end
                        i = i - 1
                        call parseExpr(tree,resulttype,tokens,tmpi,i,fname,.true.)
                        i = i + 1
                    end block
                end if
            end associate
            i = i + 1
        end do
        ! todo: add different kind type options
        if (.not.allocated(tree%nodes(resulttype)%subnodes2%array)) then
            tempnode2 = node()
            tempnode2%type = NODE_INT_VAL
            tempnode2%value = '4'
            call tree%append(tempnode2,currnode)
            call tree%nodes(resulttype)%subnodes2%append(currnode)
        end if
        call tree%nodes(resulttype)%subnodes%append(0)
    end function

    module function parseBigType(tree,tokens,start,end,fname) result(resulttype)
        type(ast), intent(inout) :: tree
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start, end
        integer :: resulttype

        character(len=*), intent(in) :: fname

        integer :: i, depth, final
        depth = 0
        final = end
        big: do i=start,end
            associate (tok=>tokens%tokens(i))
                select case (tok%type)
                case (TOKEN_LGROUP)
                    depth = depth + 1
                case (TOKEN_RGROUP)
                    depth = depth - 1
                case (TOKEN_OPERATOR)
                    if (depth==0) then
                        select case (tok%value)
                        case (',')
                            final = i-1
                            exit big
                        end select
                    end if
                end select
            end associate
        end do big
        resulttype = parseType(tree,tokens,start,final,fname)
        if (final==end) return
        i = final+2
        do while (i<=end)
            ! check for attributes
            associate (tok=>tokens%tokens(i))
                if (tok%type==TOKEN_OPERATOR.and.tok%value==',') then
                else if (tok%type==TOKEN_IDENTIFIER) then
                    select case (tok%value)
                    case ('VALUE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_VALUE)
                    case ('ALLOCATABLE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_ALLOCATABLE)
                    case ('POINTER')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_POINTER)
                    case ('PARAMETER')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_PARAMETER)
                    case ('SAVE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_SAVE)
                    case ('OPTIONAL')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_OPTIONAL)
                    case ('TARGET')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_TARGET)
                    case ('PRIVATE')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_PRIVATE)
                    case ('PUBLIC')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_PUBLIC)
                    case ('PROTECTED')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_PROTECTED)
                    case ('EXTERNAL')
                        tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_EXTERNAL)
                    case ('INTENT')

                        i = i + 1
                        if (tokens%tokens(i)%type/=TOKEN_LGROUP) then
                            call throw('invalid syntax in intent attribute',fname,tok%line,tok%char)
                        end if
                        i = i + 1
                        if (tokens%tokens(i)%type/=TOKEN_IDENTIFIER) then
                            call throw('invalid syntax in intent attribute',fname,tok%line,tok%char)
                        end if
                        select case (tokens%tokens(i)%value)
                        case ('IN')
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_INTENTIN)
                        case ('OUT')
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_INTENTOUT)
                        case ('INOUT')
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_INTENTIN)
                            tree%nodes(resulttype)%subnodes%array(1) = ior(tree%nodes(resulttype)%subnodes%array(1),PROP_INTENTOUT)
                        case default
                            call throw('invalid syntax in intent attribute',fname,tok%line,tok%char)
                        end select
                        i = i + 1
                        if (tokens%tokens(i)%type/=TOKEN_RGROUP) then
                            call throw('invalid syntax in intent attribute',fname,tok%line,tok%char)
                        end if
                    case default
                        call throw('unknown attribute',fname,tok%line,tok%char)
                    end select
                else
                    call throw('unexpected symbol in type declaration',fname,tok%line,tok%char)
                end if
            end associate
            i = i + 1
        end do
    end function


    ! todo list for shunting yard:
    !   logicals
    !   characters
    !   ranges
    !   array constructors
    !   ternarys (f2023)
    module function shunting(tokens,start,end,fname)
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start, end
        type(rpn) :: shunting
        character(len=*), intent(in) :: fname

        type(rpnnode) :: stack(64)
        integer(SMALL) :: sp

        integer :: i
        character(len=64) :: lastident
        logical :: isident

        isident = .false.
        sp = 0
        associate (t=>tokens%tokens)

           do i=start,end

                if (isident.and.t(i)%type/=TOKEN_LGROUP) then
                    isident = .false.
                    call shunting%things%append(RPN_IDENT)
                    call shunting%vals%append(lastident)
                end if
                select case (t(i)%type)
                case (TOKEN_IDENTIFIER)
                    isident = .true.
                    lastident = t(i)%value
                    cycle
                case (TOKEN_VALUE_INT)
                    call shunting%things%append(RPN_INT)
                    call shunting%vals%append(t(i)%value)
                case (TOKEN_VALUE_REAL)
                    call shunting%things%append(RPN_REL)
                    call shunting%vals%append(t(i)%value)
                case (TOKEN_LGROUP)
                    sp = sp + 1_SMALL
                    if (isident) then
                        stack(sp) = rpnnode(RPN_LGROUP,string(lastident))
                        call shunting%things%append(RPN_RGROUP)
                        call shunting%vals%append(')')
                        isident = .false.
                    else
                        stack(sp)=rpnnode(RPN_LGROUP,string('('))
                    end if
                case (TOKEN_RGROUP)
                    do while (stack(sp)%thing/=RPN_LGROUP)
                        if (sp<=0) call throw('mismatched parenthesis',fname,t(i)%line,t(i)%char)
                        call shunting%things%append(stack(sp)%thing)
                        call shunting%vals%append(stack(sp)%val%value)
                        sp = sp - 1_SMALL
                    end do
                    if (stack(sp)%val%value/='(') then
                        call shunting%things%append(RPN_LGROUP)
                        call shunting%vals%append(stack(sp)%val%value)
                    end if
                    sp = sp - 1_SMALL
                case (TOKEN_OPERATOR,TOKEN_ASTERISK)
                    if (t(i)%value/=',') then
                        if (t(i)%value=='**') then
                            do while (sp>0)
                                if (.not.(isop(stack(sp)%thing).and.precedence(stack(sp)%val%value)>precedence(t(i)%value))) exit
                                call shunting%things%append(stack(sp)%thing)
                                call shunting%vals%append(stack(sp)%val%value)
                                sp = sp - 1_SMALL
                            end do
                        else
                            do while (sp>0)
                                if (.not.(isop(stack(sp)%thing).and.precedence(stack(sp)%val%value)>=precedence(t(i)%value))) exit
                                call shunting%things%append(stack(sp)%thing)
                                call shunting%vals%append(stack(sp)%val%value)
                                sp = sp - 1_SMALL
                            end do
                        end if
                        sp = sp + 1_SMALL
                        select case (t(i)%value)
                        case ('**')
                            stack(sp)=rpnnode(RPN_POW,string('**'))
                        case ('*')
                            stack(sp)=rpnnode(RPN_MLT,string('*'))
                        case ('/')
                            stack(sp)=rpnnode(RPN_DIV,string('/'))
                        case ('+')
                            stack(sp)=rpnnode(RPN_ADD,string('+'))
                        case ('-')
                            stack(sp)=rpnnode(RPN_SUB,string('-'))
                        case ('%')
                            stack(sp)=rpnnode(RPN_MEMBER,string('%'))
                        case default
                            call throw('unknown operator',fname,t(i)%line,t(i)%char)
                        end select
                    else
                        do while (sp>0.and.stack(sp)%thing/=RPN_LGROUP)
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
            do sp=sp,1_SMALL,-1_SMALL
                if (stack(sp)%thing==RPN_LGROUP) call throw('mismatched parenthesis',fname,t(i)%line,t(i)%char)
                call shunting%things%append(stack(sp)%thing)
                call shunting%vals%append(stack(sp)%val%value)
            end do
        end associate
    end function

    module subroutine print_rpn(input)
        type(rpn), intent(in) :: input
        integer :: i
        do i=1,input%things%size-1
            select case (input%things%array(i))
            case (RPN_INT)
                write(*,'(A)',advance='no') 'i:'//input%vals%array(i)%value//' '
            case (RPN_REL)
                write(*,'(A)',advance='no') 'r:'//input%vals%array(i)%value//' '
            case (RPN_ADD)
                write(*,'(A)',advance='no') '+ '
            case (RPN_SUB)
                write(*,'(A)',advance='no') '- '
            case (RPN_MLT)
                write(*,'(A)',advance='no') '* '
            case (RPN_DIV)
                write(*,'(A)',advance='no') '/ '
            case (RPN_POW)
                write(*,'(A)',advance='no') '** '
            case (RPN_IDENT)
                write(*,'(A)',advance='no') 'ident:'//trim(input%vals%array(i)%value)//' '
            case (RPN_LGROUP)
                write(*,'(A)',advance='no') trim(input%vals%array(i)%value)//'( '
            case (RPN_RGROUP)
                write(*,'(A)',advance='no') ') '
            case (RPN_MEMBER)
                write(*,'(A)',advance='no') '% '
            end select
        end do
        print*,''
    end subroutine

    module subroutine parseExpr(tree,currnode,tokens,start,end,fname,two)
        type(ast), intent(inout) :: tree
        integer, intent(in) :: currnode
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start, end
        character(len=*), intent(in) :: fname
        logical, intent(in) :: two

        type(rpn) :: postfix
        type(rpn) :: prefix

        integer :: i
        postfix = shunting(tokens,start,end,fname)

        do i=postfix%things%size-1,1,-1
            call prefix%things%append(postfix%things%array(i))
            call prefix%vals%append(postfix%vals%array(i)%value)
        end do

        i = 1
        call parseExprAdd(tree,currnode,prefix,i,two)
        if (i/=prefix%things%size) call throw('syntax error in expression',fname,tokens%tokens(end)%line,tokens%tokens(end)%char)

    end subroutine

    recursive subroutine parseExprAdd(tree,currnode,prefix,i,two)
        type(ast), intent(inout) :: tree
        integer, intent(in) :: currnode
        type(rpn), intent(in) :: prefix
        integer, intent(inout) :: i
        logical, intent(in) :: two

        type(node) :: tempnode
        integer :: currnode2,currnode3

        if (.not.allocated(prefix%things%array)) then
            call throw('strange error','unknown',0_2,0_2)
        end if
        if (i>prefix%things%size-1) then
            call throw('outside of bounds','unknown',0_2,0_2)
        end if
        select case (prefix%things%array(i))
        case (RPN_INT)
            tempnode = node()
            tempnode%type = NODE_INT_VAL
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (two) then
                call tree%nodes(currnode)%subnodes2%append(currnode2)
            else
                call tree%nodes(currnode)%subnodes%append(currnode2)
            end if
            i = i + 1
        case (RPN_REL)
            tempnode = node()
            tempnode%type = NODE_REAL_VAL
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (two) then
                call tree%nodes(currnode)%subnodes2%append(currnode2)
            else
                call tree%nodes(currnode)%subnodes%append(currnode2)
            end if
            i = i + 1
        case (RPN_LGROUP)
            tempnode = node()
            tempnode%type = NODE_FNC_ARR
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (two) then
                call tree%nodes(currnode)%subnodes2%append(currnode2)
            else
                call tree%nodes(currnode)%subnodes%append(currnode2)
            end if
            i = i + 1
            do while (prefix%things%array(i)/=RPN_RGROUP)
                call parseExprAdd(tree,currnode2,prefix,i,.false.)
            end do
            i = i + 1
        case (RPN_IDENT)
            tempnode = node()
            tempnode%type = NODE_STRING
            tempnode%parentnode = currnode
            tempnode%value = prefix%vals%array(i)%value
            call tree%append(tempnode,currnode2)
            if (two) then
                call tree%nodes(currnode)%subnodes2%append(currnode2)
            else
                call tree%nodes(currnode)%subnodes%append(currnode2)
            end if
            i = i + 1
        case (RPN_ADD,RPN_SUB,RPN_MLT,RPN_DIV,RPN_POW,RPN_MEMBER)
            tempnode = node()
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
            end select
            tempnode%parentnode = currnode
            call tree%append(tempnode,currnode2)
            if (two) then
                call tree%nodes(currnode)%subnodes2%append(currnode2)
            else
                call tree%nodes(currnode)%subnodes%append(currnode2)
            end if
            i = i + 1
            call parseExprAdd(tree,currnode2,prefix,i,.true.)
            if (i>=prefix%things%size) then
                tempnode = node()
                tempnode%type = NODE_INT_VAL
                tempnode%parentnode = currnode2
                tempnode%value = '0'
                call tree%append(tempnode,currnode3)
                call tree%nodes(currnode2)%subnodes%append(currnode3)
                return
            end if
            call parseExprAdd(tree,currnode2,prefix,i,.false.)
        case default
            if (allocated(tree%nodes(currnode)%fname)) then
                call throw('unknown rpn node',tree%nodes(currnode)%fname,&
                tree%nodes(currnode)%startlnum,tree%nodes(currnode)%startchar)
            else
                print*,i
                call throw('unknown rpn node '//itoa2(prefix%things%array(i)),'unknown',&
                tree%nodes(currnode)%startlnum,tree%nodes(currnode)%startchar)
            end if
        end select
    end subroutine
end submodule