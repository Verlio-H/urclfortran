module astgen
    use lexer
    implicit none

    type :: node
        integer(SMALL) :: type = 0
        integer(SMALL) :: startlnum = 0
        integer(SMALL) :: startchar = 0
        character(len=64) :: value = ''
        class(*), allocatable :: value2
        integer :: parentnode = 0
        logical :: secondpart = .false.
        integer, allocatable :: attributes(:)
        type(iarr) :: subnodes
        type(iarr) :: subnodes2
        character(len=:), allocatable :: fname
    end type

    type :: ast
        type(node), allocatable :: nodes(:)
        integer :: size = 0
    contains
        procedure :: append => ast_append
    end type

    type rpn
        type(siarr) :: things
        type(carr) :: vals
    end type

    type rpnnode
        integer(SMALL) :: thing
        type(string) :: val
    end type


    integer(SMALL), parameter :: TYPE_NONE = 0
    integer(SMALL), parameter :: TYPE_INTEGER = 1
    integer(SMALL), parameter :: TYPE_REAL = 2
    integer(SMALL), parameter :: TYPE_COMPLEX = 3
    integer(SMALL), parameter :: TYPE_DERIVED = -1

    integer, parameter :: PROP_VALUE = 2**0
    integer, parameter :: PROP_INTENTIN = 2**1
    integer, parameter :: PROP_INTENTOUT = 2**2
    integer, parameter :: PROP_ALLOCATABLE = 2**3
    integer, parameter :: PROP_POINTER = 2**4
    integer, parameter :: PROP_PARAMETER = 2**5
    integer, parameter :: PROP_SAVE = 2**6
    integer, parameter :: PROP_OPTIONAL = 2**8
    integer, parameter :: PROP_TARGET = 2**9
    integer, parameter :: PROP_PRIVATE = 2**10
    integer, parameter :: PROP_PUBLIC = 2**11
    integer, parameter :: PROP_PROTECTED = 2**12
    integer, parameter :: PROP_EXTERNAL = 2**13 !one MUST specify external if they are to use an external procedure
    ! todo: procedure pointers
    !asynchronous and volatile attributes are not applicable
    
    integer(SMALL), parameter :: KIND_DEF_INTEGER = 4
    integer(SMALL), parameter :: KIND_DEF_REAL = 4

    integer(SMALL), parameter :: RPN_INT = 0
    integer(SMALL), parameter :: RPN_REL = 1
    integer(SMALL), parameter :: RPN_LOG = 2
    integer(SMALL), parameter :: RPN_CHAR = 3
    integer(SMALL), parameter :: RPN_ADD = 4
    integer(SMALL), parameter :: RPN_SUB = 5
    integer(SMALL), parameter :: RPN_MLT = 6
    integer(SMALL), parameter :: RPN_DIV = 7
    integer(SMALL), parameter :: RPN_POW = 8
    integer(SMALL), parameter :: RPN_CAT = 9
    integer(SMALL), parameter :: RPN_MEMBER = 10
    integer(SMALL), parameter :: RPN_IDENT = 11
    integer(SMALL), parameter :: RPN_LGROUP = 12
    integer(SMALL), parameter :: RPN_RGROUP = 13

! blocks
    ! todo: add necessary 202y blocks
    integer(SMALL), parameter :: NODE_MODULE = 0
    integer(SMALL), parameter :: NODE_PROGRAM = 1
    integer(SMALL), parameter :: NODE_FUNCTION = 2
    integer(SMALL), parameter :: NODE_SUBROUTINE = 3
    integer(SMALL), parameter :: NODE_BLOCK_DATA = 4
    integer(SMALL), parameter :: NODE_TYPE_BLOCK = 5
    integer(SMALL), parameter :: NODE_INTERFACE = 6
    integer(SMALL), parameter :: NODE_BLOCK = 7
    integer(SMALL), parameter :: NODE_ASSOCIATE = 8
    integer(SMALL), parameter :: NODE_SELECT_CASE = 9
    integer(SMALL), parameter :: NODE_CASE = 10
    integer(SMALL), parameter :: NODE_FORALL = 11
    integer(SMALL), parameter :: NODE_WHERE = 12
    integer(SMALL), parameter :: NODE_DO = 13
    integer(SMALL), parameter :: NODE_DO_WHILE = 14
    integer(SMALL), parameter :: NODE_DO_CONCURRENT = 15
    integer(SMALL), parameter :: NODE_IF = 16
    integer(SMALL), parameter :: NODE_ELSE = 17
    integer(SMALL), parameter :: NODE_ELSE_IF = 18
    integer(SMALL), parameter :: NODE_SELECT_TYPE = 19
    integer(SMALL), parameter :: NODE_TYPE_GAURD = 20
! statements
    integer(SMALL), parameter :: NODE_ASSIGNMENT = 1000
    integer(SMALL), parameter :: NODE_IMPLICIT = 1001
! operators
    integer(SMALL), parameter :: NODE_ADD = 2000
    integer(SMALL), parameter :: NODE_SUB = 2001
    integer(SMALL), parameter :: NODE_MLT = 2002
    integer(SMALL), parameter :: NODE_DIV = 2003
    integer(SMALL), parameter :: NODE_EXP = 2004
    integer(SMALL), parameter :: NODE_MEMBER = 2005 ! technically shouldn't be operator by the standard but simpler and better
! values
    integer(SMALL), parameter :: NODE_INT_VAL = 3000
    integer(SMALL), parameter :: NODE_REAL_VAL = 3001
    integer(SMALL), parameter :: NODE_CHAR_VAL = 3002
    integer(SMALL), parameter :: NODE_LOGICAL_VAL = 3003
! other
    integer(SMALL), parameter :: NODE_FNC_ARR = 4000
    integer(SMALL), parameter :: NODE_LABEL = 4001
    integer(SMALL), parameter :: NODE_ROOT = 4002
    integer(SMALL), parameter :: NODE_STRING = 4003
    integer(SMALL), parameter :: NODE_TYPE = 4004

    interface
        module function isop(op)
            logical :: isop
            integer(SMALL), intent(in) :: op
        end function

        module function parseType(tree,tokens,start,end,fname) result(resultnode)
            type(ast), intent(inout) :: tree
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start, end
            integer :: resultnode
    
            character(len=*), intent(in) :: fname
        end function

        module function parseBigType(tree,tokens,start,end,fname) result(resulttype)
            type(ast), intent(inout) :: tree
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start, end
            integer :: resulttype
    
            character(len=*), intent(in) :: fname
        end function

        module function shunting(tokens,start,end,fname)
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start, end
            type(rpn) :: shunting
            character(len=*), intent(in) :: fname
        end function

        module subroutine print_rpn(input)
            type(rpn), intent(in) :: input
        end subroutine
        
        module subroutine parseExpr(tree,currnode,tokens,start,end,fname,two)
            type(ast), intent(inout) :: tree
            integer, intent(in) :: currnode
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start, end
            character(len=*), intent(in) :: fname
            logical, intent(in) :: two
        end subroutine    
    end interface
    
contains
    function genast(input,fname,startline,startchar) result(result)
        type(ast) :: result
        type(tokengroup), intent(in) :: input
        character(len=*), intent(in) :: fname
        integer(SMALL), allocatable, intent(in), optional :: startline
        integer(SMALL), allocatable, intent(in), optional :: startchar

        type(node) :: tempnode
        integer :: currentnode, currentnode2, childnode

        integer :: i
        integer(SMALL) :: lnum, char

        if (present(startline)) then
            lnum = startline
        else
            lnum = 1_2
        end if
        if (present(startchar)) then
            char = startchar
        else
            char = 1_2
        end if
        i = 1
        currentnode = 1
        tempnode = node()
        tempnode%type = NODE_ROOT
        tempnode%parentnode = 0
        tempnode%fname = fname
        tempnode%value = ''
        call result%append(tempnode)
        do while (i<input%size-1)
            associate(t=>input%tokens)
                ! check for assignment
                ! todo: deal with arrays
                if (t(i+1)%type==TOKEN_ASSIGN) then
                    tempnode = node()
                    tempnode%type = NODE_ASSIGNMENT
                    tempnode%parentnode = currentnode
                    call result%append(tempnode,currentnode2)
                    call result%nodes(currentnode)%subnodes2%append(currentnode2)
                    call parseExpr(result,currentnode2,input,i,i,fname,.false.)
                    ! find end of line
                    block
                        integer :: tempi
                        i = i + 2
                        tempi = i
                        do while (t(i)%type/=TOKEN_NEXTLINE)
                            i = i + 1
                        end do
                        i = i - 1
                        call parseExpr(result,currentnode2,input,tempi,i,fname,.true.)
                    end block
                else
                    select case (t(i)%type)
                    case (TOKEN_IDENTIFIER)
                        select case (t(i)%value)
                        case ('PROGRAM')
                            if (currentnode/=1) then
                                call throw('program statement must be in global scope',fname,t(i)%line,t(i)%char)
                            end if
                            if (t(i+1)%type/=TOKEN_IDENTIFIER) then
                                call throw('program name must be an identifier',fname,t(i+1)%line,t(i+1)%char)
                            end if
                            tempnode = node()
                            tempnode%type = NODE_PROGRAM
                            tempnode%startlnum = t(i)%line
                            tempnode%startchar = t(i)%char
                            tempnode%fname = fname
                            tempnode%value = t(i+1)%value
                            tempnode%parentnode = 1
                            
                            call result%append(tempnode,currentnode)
                            call result%nodes(1)%subnodes%append(currentnode)
                            i = i + 1
                            if (t(i+1)%type/=TOKEN_NEXTLINE) then
                                call throw('expected newline after module statement',fname,t(i+1)%line,t(i+1)%char)
                            end if
                        case ('MODULE')
                            if (currentnode/=1) then
                                call throw('module statement must be in global scope',fname,t(i)%line,t(i)%char)
                            end if
                            if (t(i+1)%type/=TOKEN_IDENTIFIER) then
                                call throw('program name must be an identifier',fname,t(i+1)%line,t(i+1)%char)
                            end if
                            tempnode = node()
                            tempnode%type = NODE_MODULE
                            tempnode%startlnum = t(i)%line
                            tempnode%startchar = t(i)%char
                            tempnode%fname = fname
                            tempnode%value = t(i+1)%value
                            tempnode%parentnode = 1
                            
                            call result%append(tempnode,currentnode)
                            call result%nodes(1)%subnodes%append(currentnode)
                            i = i + 1
                            if (t(i+1)%type/=TOKEN_NEXTLINE) then
                                call throw('expected newline after module statement',fname,t(i+1)%line,t(i+1)%char)
                            end if
                        case ('END')
                            select case (t(i+1)%type)
                            case (TOKEN_NEXTLINE)
                                select case (result%nodes(currentnode)%type)
                                case (NODE_PROGRAM,NODE_SUBROUTINE,NODE_MODULE)
                                    currentnode = result%nodes(currentnode)%parentnode
                                    i = i + 1
                                case default
                                    call throw('block type must be stated in end statement',fname,t(i+1)%line,t(i+1)%char)
                                end select
                            case (TOKEN_IDENTIFIER)
                                select case (t(i+1)%value)
                                case ('PROGRAM')
                                    if (result%nodes(currentnode)%type/=NODE_PROGRAM) then
                                        call throw('incorrect block type in end statement',fname,t(i+1)%line,t(i+1)%char)
                                    end if
                                case ('MODULE')
                                    if (result%nodes(currentnode)%type/=NODE_MODULE) then
                                        call throw('incorrect block type in end statement',fname,t(i+1)%line,t(i+1)%char)
                                    end if
                                case ('SUBROUTINE')
                                    if (result%nodes(currentnode)%type/=NODE_SUBROUTINE) then
                                        call throw('incorrect block type in end statement',fname,t(i+1)%line,t(i+1)%char)
                                    end if
                                case default
                                    call throw('unknown block type in end statement',fname,t(i+1)%line,t(i+1)%char)
                                end select
                                select case (t(i+2)%type)
                                case (TOKEN_NEXTLINE)
                                    ! todo: deal with named non program unit blocks
                                    currentnode = result%nodes(currentnode)%parentnode
                                    i = i + 2
                                case (TOKEN_IDENTIFIER)
                                    if (t(i+2)%value/=result%nodes(currentnode)%value) then
                                        call throw('incorrect block name in end statement',fname,t(i+2)%line,t(i+2)%char)
                                    end if
                                    currentnode = result%nodes(currentnode)%parentnode
                                    i = i + 3
                                case default
                                    call throw('expected block name in end statement',fname,t(i+2)%line,t(i+2)%char)
                                end select
                            case default
                                call throw('unknown block type in end statement',fname,t(i+1)%line,t(i+1)%char)
                            end select
                        case ('CONTAINS')
                            select case (result%nodes(currentnode)%type)
                            case (NODE_PROGRAM,NODE_MODULE)
                                if (result%nodes(currentnode)%secondpart) then
                                    call throw('already in contains section',fname,t(i)%line,t(i)%char)
                                end if
                                result%nodes(currentnode)%secondpart = .true.
                            case default
                                call throw('contains statement cannot be in this block type',fname,t(i)%line,t(i)%char)
                            end select
                        case ('SUBROUTINE')
                            select case (result%nodes(currentnode)%type)
                            case (NODE_PROGRAM,NODE_MODULE)
                                if (.not.result%nodes(currentnode)%secondpart) then
                                    call throw('invalid spot for subroutine statement',fname,t(i)%line,t(i)%char)
                                end if
                            case (NODE_ROOT)
                            case default
                                call throw('invalid spot for subroutine statement',fname,t(i)%line,t(i)%char)
                            end select
                            if (t(i+1)%type/=TOKEN_IDENTIFIER) then
                                call throw('expected subroutine name',fname,t(i+1)%line,t(i+1)%char)
                            end if
                            if (t(i+2)%type/=TOKEN_LGROUP.or.t(i+2)%value/='(') then
                                call throw('expected left parenthesis',fname,t(i+2)%line,t(i+2)%char)
                            end if
                            tempnode = node()
                            tempnode%type = NODE_SUBROUTINE
                            tempnode%startlnum = t(i)%line
                            tempnode%startchar = t(i)%char
                            tempnode%fname = fname
                            tempnode%value = t(i+1)%value
                            tempnode%parentnode = currentnode
                            call result%append(tempnode,currentnode)
                            allocate(result%nodes(currentnode)%attributes(2))
                            call result%nodes(tempnode%parentnode)%subnodes%append(currentnode)
                            i = i + 3
                            do while (t(i)%type/=TOKEN_RGROUP.or.t(i)%value/=')')
                                select case (t(i)%type)
                                case (TOKEN_RGROUP)
                                    call throw('expected right parenthesis',fname,t(i)%line,t(i)%char)
                                case (TOKEN_NEXTLINE)
                                    call throw('expected closing parenthesis',fname,t(i)%line,t(i)%char)
                                case (TOKEN_OPERATOR)
                                    if (t(i)%value/=',') then
                                        call throw('unexpected token in subroutine declaration',fname,t(i)%line,t(i)%char)
                                    end if
                                    if (t(i-1)%type/=TOKEN_IDENTIFIER) then
                                        call throw('unexpected identifier in subroutine declaration',fname,t(i)%line,t(i)%char)
                                    end if
                                case (TOKEN_IDENTIFIER)
                                    tempnode = node()
                                    tempnode%type = NODE_STRING
                                    tempnode%startlnum = t(i)%line
                                    tempnode%startchar = t(i)%char
                                    tempnode%fname = fname
                                    tempnode%value = t(i)%value
                                    tempnode%parentnode = currentnode
                                    call result%append(tempnode,childnode)
                                    call result%nodes(currentnode)%subnodes%append(childnode)
                                case default
                                    call throw('unexpected token in subroutine declaration',fname,t(i)%line,t(i)%char)
                                end select
                                i = i + 1
                            end do
                            i = i + 1
                            if (t(i)%type/=TOKEN_NEXTLINE) then
                                call throw('expected new line following subroutine definition',fname,t(i)%line,t(i)%char)
                            end if
                        case ('IMPLICIT')
                            i = i + 1
                            do while (t(i)%type/=TOKEN_NEXTLINE)
                                if (t(i)%type/=TOKEN_IDENTIFIER) then
                                    call throw('expected identifier in implicit statement',fname,t(i)%line,t(i)%char)
                                end if
                                block
                                    integer :: end, end2
                                    integer(SMALL) :: depth
                                    integer :: resulttype
                                    end = i
                                    depth = 0
                                    tempnode = node()
                                    tempnode%type = NODE_IMPLICIT
                                    tempnode%startlnum = t(i)%line
                                    tempnode%startchar = t(i)%char
                                    tempnode%fname = fname
                                    tempnode%parentnode = currentnode
                                    tempnode%value = ''
                                    do while(.not.(((t(end)%type==TOKEN_OPERATOR.and.t(end)%value==',').or.&
                                        (t(end)%type==TOKEN_NEXTLINE)).and.depth==0))
                                        if (t(end)%type==TOKEN_NEXTLINE) then
                                            call throw('expected closing parenthesis',fname,t(end)%line,t(end)%char)
                                        end if
                                        if (t(end)%type==TOKEN_LGROUP) depth = depth + 1_2
                                        if (t(end)%type==TOKEN_RGROUP) then
                                            if (depth==0) call throw('missing left parenthesis',fname,t(end)%line,t(end)%char)
                                            depth = depth - 1_2
                                        end if
                                        end = end + 1
                                    end do
                                    end = end - 1
                                    end2 = end
                                    ! find opening parenthesis
                                    if (.not.(t(end)%type==TOKEN_IDENTIFIER.and.t(end)%value=='NONE')) then
                                        do while (t(end)%type/=TOKEN_LGROUP)
                                            end = end - 1
                                            if (end<=i) then
                                                call throw('expected parenthesis',fname,t(i)%line,t(i)%char)
                                            end if
                                        end do
                                        end = end - 1
                                    end if
                                    call result%append(tempnode,currentnode)
                                    if (t(end)%value=='NONE') then
                                        tempnode = node()
                                        tempnode%type = NODE_TYPE
                                        tempnode%value = ''
                                        tempnode%value2 = TYPE_NONE
                                        tempnode%parentnode = currentnode
                                        call result%append(tempnode,resulttype)
                                        call result%nodes(resulttype)%subnodes%append(0)

                                    else
                                        resulttype = parseType(result,lexed,i,end,fname)
                                    end if
                                    call result%nodes(currentnode)%subnodes%append(resulttype)
                                    i = end + 1
                                    ! (i:end2) = range (end2 and i == parens)
                                    end2 = end2 - 1
                                    i = i + 1
                                    do while (i<=end2)
                                        tempnode = node()
                                        tempnode%type = NODE_STRING
                                        tempnode%startlnum = t(i)%line
                                        tempnode%startchar = t(i)%char
                                        tempnode%fname = fname
                                        tempnode%parentnode = currentnode
                                        if (t(i)%type/=TOKEN_IDENTIFIER) then
                                            call throw('expected letter',fname,t(i)%line,t(i)%char)
                                        end if
                                        if ((t(i+1)%type==TOKEN_RGROUP.and.t(i+1)%value==')').or.&
                                        (t(i+1)%type==TOKEN_OPERATOR.and.t(i+1)%value==',')) then
                                            tempnode%value = t(i)%value
                                            i = i + 2
                                        else if (t(i+1)%type==TOKEN_OPERATOR.and.t(i+1)%value=='-') then
                                            if (t(i+2)%type/=TOKEN_IDENTIFIER) then
                                                call throw('expected letter',fname,t(i+2)%line,t(i+2)%char)
                                            end if
                                            tempnode%value = t(i)%value//'-'//t(i+2)%value
                                            i = i + 4
                                        else
                                            call throw('syntax errror',fname,t(i)%line,t(i)%char)
                                        end if
                                        call result%append(tempnode,currentnode2)
                                        call result%nodes(currentnode)%subnodes2%append(currentnode2)
                                    end do
                                    i = end2 + 2
                                    if (t(i)%type==TOKEN_OPERATOR) i = i + 1
                                    call result%nodes(result%nodes(currentnode)%parentnode)%subnodes2%append(currentnode)
                                    currentnode = result%nodes(currentnode)%parentnode
                                end block
                            end do
                            if (t(i)%type/=TOKEN_NEXTLINE) then
                                call throw('expected new line following implicit statement',fname,t(i)%line,t(i)%char)
                            end if
                        case ('INTEGER','REAL','COMPLEX')
                            i = i + 1
                            block
                                integer :: tempi, tempi2, depth
                                integer :: typeof
                                tempi = i
                                do while (tempi<size(t))
                                    if (t(tempi)%type==TOKEN_NEXTLINE) then
                                        depth = 0
                                        do tempi2=i,tempi
                                            if (depth==0.and.t(tempi2)%type==TOKEN_IDENTIFIER) exit
                                            if (t(tempi2)%type==TOKEN_LGROUP) depth = depth + 1
                                            if (t(tempi2)%type==TOKEN_RGROUP) depth = depth - 1
                                        end do
                                        tempi = tempi2-1
                                        exit
                                    else if (t(tempi)%type==TOKEN_OPERATOR.and.t(tempi)%value=='::') then
                                        tempi = tempi - 1
                                        exit
                                    end if
                                    tempi = tempi + 1
                                end do
                                tempi2 = i-1
                                i = tempi + 1
                                if (t(i)%type==TOKEN_OPERATOR.and.t(i)%value=='::') i = i + 1
                                do while (t(i)%type/=TOKEN_NEXTLINE)
                                    if (t(i)%type==TOKEN_OPERATOR.and.t(i)%value==',') then
                                        i = i + 1
                                        cycle
                                    else if (t(i)%type==TOKEN_ASSIGN) then
                                        i = i + 1
                                        tempi = i
                                        do while (t(i)%type/=TOKEN_NEXTLINE)
                                            i = i + 1
                                        end do
                                        i = i - 1
                                        call parseExpr(result,typeof,input,tempi,i,fname,.false.)
                                        exit
                                    end if
                                    if (t(i)%type/=TOKEN_IDENTIFIER) then
                                        call throw('expected identifier',fname,t(i)%line,t(i)%char)
                                    end if

                                    typeof = parseBigType(result,input,tempi2,tempi,fname)
                                    result%nodes(typeof)%value = t(i)%value
                                    call result%nodes(currentnode)%subnodes2%append(typeof)
                                    i = i + 1
                                    if (t(i)%type==TOKEN_LGROUP) then
                                        call throw('arrays not yet supported',fname,t(i)%line,t(i)%char)
                                    end if
                                end do
                            end block
                        case default
                            call throw('unknown identifier "'//input%tokens(i)%value//'"',fname,t(i)%line,t(i)%char)
                        end select
                    case (TOKEN_NEXTLINE)
                    case default
                        call throw('unclassifiable statement',fname,t(i)%line,t(i)%char)
                    end select
                end if
                i = i + 1
            end associate
        end do
    end function

    recursive subroutine print_ast(currnode,fullast,depth)
        type(node), intent(in) :: currnode
        type(ast), intent(in) :: fullast
        integer, intent(in) :: depth
        integer :: i
        select case (currnode%type)
        case (NODE_ROOT)
            print'(A)','program:'
            do i=1,currnode%subnodes%size-1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)),fullast,2)
            end do
        case (NODE_PROGRAM)
            print'(A)',repeat(' ',depth)//'program statement:'
            print'(A)',repeat(' ',depth+1)//'name: '//currnode%value
            print'(A)',repeat(' ',depth+1)//'procedures:'
            do i=1,currnode%subnodes%size-1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)),fullast,depth+2)
            end do
            print'(A)',repeat(' ',depth+1)//'statements:'
            do i=1,currnode%subnodes2%size-1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)),fullast,depth+2)
            end do
        case (NODE_MODULE)
            print'(A)',repeat(' ',depth)//'module:'
            print'(A)',repeat(' ',depth+1)//'name: '//currnode%value
            print'(A)',repeat(' ',depth+1)//'procedures:'
            do i=1,currnode%subnodes%size-1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)),fullast,depth+2)
            end do
            print'(A)',repeat(' ',depth+1)//'statements:'
            do i=1,currnode%subnodes2%size-1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)),fullast,depth+2)
            end do
        case (NODE_SUBROUTINE)
            print'(A)',repeat(' ',depth)//'subroutine:'
            print'(A)',repeat(' ',depth+1)//'name: '//currnode%value
            print'(A)',repeat(' ',depth+1)//'arguments:'
            do i=1,currnode%subnodes%size-1
                call print_ast(fullast%nodes(currnode%subnodes%array(i)),fullast,depth+2)
            end do
            print'(A)',repeat(' ',depth+1)//'statements:'
            do i=1,currnode%subnodes2%size-1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)),fullast,depth+2)
            end do
        case (NODE_ASSIGNMENT)
            print'(A)',repeat(' ',depth)//'assign:'
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_IMPLICIT)
            print'(A)',repeat(' ',depth)//'implicit:'
            print'(A)',repeat(' ',depth+1)//'type:'
            if (.not.allocated(currnode%subnodes%array)) then
                print'(A)','expected type in implicit statement'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+2)
            print'(A)',repeat(' ',depth+1)//'letters:'
            do i=1,currnode%subnodes2%size-1
                call print_ast(fullast%nodes(currnode%subnodes2%array(i)),fullast,depth+2)
            end do
        case (NODE_INT_VAL)
            print'(A)',repeat(' ',depth)//'int: '//currnode%value
        case (NODE_REAL_VAL)
            print'(A)',repeat(' ',depth)//'real: '//currnode%value
        case (NODE_ADD)
            print'(A)',repeat(' ',depth)//'add:'
            if (.not.allocated(currnode%subnodes%array).or..not.allocated(currnode%subnodes2%array)) then
                print'(A)','expected 2 arguments in add'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_SUB)
            print'(A)',repeat(' ',depth)//'sub:'
            if (.not.allocated(currnode%subnodes%array).or..not.allocated(currnode%subnodes2%array)) then
                print'(A)','expected 2 arguments in sub'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_MLT)
            print'(A)',repeat(' ',depth)//'mlt:'
            if (.not.allocated(currnode%subnodes%array).or..not.allocated(currnode%subnodes2%array)) then
                print'(A)','expected 2 arguments in mlt'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_DIV)
            print'(A)',repeat(' ',depth)//'div:'
            if (.not.allocated(currnode%subnodes%array).or..not.allocated(currnode%subnodes2%array)) then
                print'(A)','expected 2 arguments in div'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_EXP)
            print'(A)',repeat(' ',depth)//'pow:'
            if (.not.allocated(currnode%subnodes%array).or..not.allocated(currnode%subnodes2%array)) then
                print'(A)','expected 2 arguments in pow'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_MEMBER)
            print'(A)',repeat(' ',depth)//'member access:'
            if (.not.allocated(currnode%subnodes%array).or..not.allocated(currnode%subnodes2%array)) then
                print'(A)','expected 2 arguments for member access'
                stop
            end if
            call print_ast(fullast%nodes(currnode%subnodes%array(1)),fullast,depth+1)
            call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
        case (NODE_FNC_ARR)
            print'(A)',repeat(' ',depth)//'funccall or array: '//currnode%value
            if (allocated(currnode%subnodes%array)) then
                do i=1,currnode%subnodes%size-1
                    call print_ast(fullast%nodes(currnode%subnodes%array(i)),fullast,depth+1)
                end do
            end if
        case (NODE_TYPE)
            block
                select type (a=>currnode%value2)
                type is (integer(SMALL))
                    select case (a)
                    case (TYPE_INTEGER)
                        print'(A)',repeat(' ',depth)//'type: integer'
                    case (TYPE_REAL)
                        print'(A)',repeat(' ',depth)//'type: real'
                    case (TYPE_COMPLEX)
                        print'(A)',repeat(' ',depth)//'type: complex'
                    case (TYPE_NONE)
                        print'(A)',repeat(' ',depth)//'type: none'
                    case default
                        print'(A)',repeat(' ',depth)//'type: unknown'
                    end select
                    if (a/=TYPE_NONE) then
                        print'(A)',repeat(' ',depth)//'kind:'
                        call print_ast(fullast%nodes(currnode%subnodes2%array(1)),fullast,depth+1)
                        if (currnode%value/='') print'(A)',repeat(' ',depth)//'name: '//currnode%value
                        associate (b=>currnode%subnodes%array(1))
                            if (b/=0) then
                                print'(A)',repeat(' ',depth+1)//'properties:'
                                if (iand(b,PROP_VALUE)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'value'
                                end if
                                if (iand(b,PROP_INTENTIN)/=0) then
                                    if (iand(b,PROP_INTENTOUT)/=0) then
                                        print'(A)',repeat(' ',depth+2)//'intent(inout)'
                                    else
                                        print'(A)',repeat(' ',depth+2)//'intent(in)'
                                    end if
                                else if (iand(b,PROP_INTENTOUT)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'intent(out)'
                                end if
                                if (iand(b,PROP_ALLOCATABLE)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'allocatable'
                                end if
                                if (iand(b,PROP_POINTER)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'pointer'
                                end if
                                if (iand(b,PROP_PARAMETER)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'parameter'
                                end if
                                if (iand(b,PROP_SAVE)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'save'
                                end if
                                if (iand(b,PROP_OPTIONAL)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'optional'
                                end if
                                if (iand(b,PROP_TARGET)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'target'
                                end if
                                if (iand(b,PROP_PRIVATE)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'private'
                                end if
                                if (iand(b,PROP_PROTECTED)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'protected'
                                end if
                                if (iand(b,PROP_EXTERNAL)/=0) then
                                    print'(A)',repeat(' ',depth+2)//'external'
                                end if
                            end if 
                        end associate
                        if (currnode%subnodes%size-1>=2) then
                            print'(A)',repeat(' ',depth+1)//'value:'
                            call print_ast(fullast%nodes(currnode%subnodes%array(2)),fullast,depth+2)
                        end if
                    end if
                class default
                    print'(A)','unexpected type'
                    stop
                end select
            end block
        case (NODE_STRING)
            print'(A)',repeat(' ',depth)//'string: '//currnode%value
        case default
            print'(A)',repeat(' ',depth)//'unknown node'
        end select
    end subroutine

    subroutine ast_append(this,value,index)
        class(ast), intent(inout) :: this
        type(node), intent(inout) :: value
        integer, intent(out), optional :: index

        type(node), allocatable :: tmp(:)
        if (iand(this%size,15)==0) then
            if (this%size==0) then
                allocate(this%nodes(15))
                this%size = 1
            else
                allocate(tmp(this%size+15))
                tmp(:this%size-1) = this%nodes(:this%size-1) !copy the data
                call move_alloc(tmp, this%nodes) !rename
            end if
        end if
        this%nodes(this%size)%type = value%type
        this%nodes(this%size)%startlnum = value%startlnum
        this%nodes(this%size)%startchar = value%startchar
        if (allocated(value%fname)) then
            this%nodes(this%size)%fname = value%fname
        end if
        this%nodes(this%size)%value = value%value
        ! literally only combo that gfortran would work with
        ! only reason that value is inout, remove inout if gfortran compiler bug gets fixed
        if (allocated(value%value2)) then
            allocate(this%nodes(this%size)%value2,mold=value%value2)
            call move_alloc(value%value2,this%nodes(this%size)%value2)
        end if
        this%nodes(this%size)%parentnode = value%parentnode
        
        if (present(index)) index = this%size
        this%size = this%size+1
        
    end subroutine

    pure integer function findendln(tokens,start)
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start
        integer :: i
        do i=start,tokens%size-1
            if (tokens%tokens(i)%type==TOKEN_NEXTLINE) then
                findendln = i
                return
            end if
        end do
        findendln = i
    end function
end module