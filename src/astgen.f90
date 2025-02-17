module astgen
    use lexer
    implicit none

    type :: node
        integer(SMALL) :: type = 0
        integer(SMALL) :: startlnum = 0
        integer(SMALL) :: startchar = 0
        character(64) :: value = ''
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

    type, private :: rpn
        type(siarr) :: things
        type(carr) :: vals
    end type

    type, private :: rpnnode
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
    integer, parameter :: PROP_EXTERNAL = 2**13
    integer, parameter :: PROP_INDIRECT = 2**14
    ! TODO: procedure pointers
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
    ! TODO: add necessary 202y blocks
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
    integer(SMALL), parameter :: NODE_USE = 1002
    integer(SMALL), parameter :: NODE_CALL = 1003
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
        module function isop(op) result(result)
            logical :: result
            integer(SMALL), intent(in) :: op
        end function

        module function parse_type(tree, tokens, start, end, fname) result(resultnode)
            integer :: resultnode
            type(ast), intent(inout) :: tree
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start
            integer, intent(in) :: end
            character(*), intent(in) :: fname
        end function

        module function parse_bigtype(tree, tokens, start, end, fname) result(resulttype)
            integer :: resulttype
            type(ast), intent(inout) :: tree
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start
            integer, intent(in) :: end
            character(*), intent(in) :: fname
        end function

        module function shunting(tokens, start, end, fname)
            type(rpn) :: shunting
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start
            integer, intent(in) :: end
            character(*), intent(in) :: fname
        end function

        module subroutine print_rpn(input)
            type(rpn), intent(in) :: input
        end subroutine
        
        module subroutine parse_expr(tree, currnode, tokens, start, end, fname, two)
            type(ast), intent(inout) :: tree
            integer, intent(in) :: currnode
            type(tokengroup), intent(in) :: tokens
            integer, intent(in) :: start, end
            character(*), intent(in) :: fname
            logical, intent(in) :: two
        end subroutine

        module subroutine astnode_write(result, input, t, currentnode, currentnode2, childnode, fname, i)
            type(ast), intent(inout) :: result
            type(tokengroup), intent(in) :: input
            type(token), intent(in) :: t(:)
            integer, intent(in) :: currentnode
            integer, intent(inout) :: currentnode2
            integer, intent(inout) :: childnode
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_program(result, t, currentnode, fname, i)
            type(ast), intent(inout) :: result
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_module(result, t, currentnode, fname, i)
            type(ast), intent(inout) :: result
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_subroutine(result, t, currentnode, fname, childnode, i)
            type(ast), intent(inout) :: result
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            integer, intent(inout) :: childnode
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_end(result, t, currentnode, fname, i)
            type(ast), intent(inout) :: result
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_implicit(result, t, currentnode, currentnode2, fname, i)
            type(ast), intent(inout) :: result
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            integer, intent(inout) :: currentnode2
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_use(result, t, currentnode, currentnode2, fname, childnode, i)
            type(ast), intent(inout) :: result
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            integer, intent(inout) :: currentnode2
            character(*), intent(in) :: fname
            integer, intent(inout) :: childnode
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_type(result, input, t, currentnode, fname, i)
            type(ast), intent(inout) :: result
            type(tokengroup), intent(in) :: input
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module subroutine astnode_call(result, input, t, currentnode, currentnode2, fname, i)
            type(ast), intent(inout) :: result
            type(tokengroup), intent(in) :: input
            type(token), intent(in) :: t(:)
            integer, intent(inout) :: currentnode
            integer, intent(inout) :: currentnode2
            character(*), intent(in) :: fname
            integer, intent(inout) :: i
        end subroutine

        module recursive subroutine print_ast(currnode, fullast, depth)
            type(node), intent(in) :: currnode
            type(ast), intent(in) :: fullast
            integer, intent(in) :: depth
        end subroutine
    end interface
contains
    function genast(input, fname, startline, startchar) result(result)
        type(ast) :: result
        type(tokengroup), intent(in) :: input
        character(*), intent(in) :: fname
        integer(SMALL), allocatable, optional, intent(in) :: startline
        integer(SMALL), allocatable, optional, intent(in) :: startchar

        type(node) :: tempnode
        integer :: currentnode, currentnode2, childnode
        integer :: i
        integer(SMALL) :: lnum, char

        if (present(startline)) then
            lnum = startline
        else
            lnum = 1_SMALL
        end if
        if (present(startchar)) then
            char = startchar
        else
            char = 1_SMALL
        end if

        i = 1
        currentnode = 1
        tempnode = node()
        tempnode%type = NODE_ROOT
        tempnode%parentnode = 0
        tempnode%fname = fname
        tempnode%value = ''
        call result%append(tempnode)

        do while (i < input%size - 1)
            associate(t => input%tokens)
                ! check for assignment
                ! TODO: deal with arrays
                if (t(i + 1)%type == TOKEN_ASSIGN) then
                    tempnode = node()
                    tempnode%type = NODE_ASSIGNMENT
                    tempnode%parentnode = currentnode
                    call result%append(tempnode,currentnode2)
                    call result%nodes(currentnode)%subnodes2%append(currentnode2)
                    call parse_expr(result, currentnode2, input, i, i, fname, .false.)
                    ! find end of line
                    block
                        integer :: tempi
                        i = i + 2
                        tempi = i
                        do while (t(i)%type /= TOKEN_NEXTLINE)
                            i = i + 1
                        end do
                        i = i - 1
                        call parse_expr(result, currentnode2, input, tempi, i, fname, .true.)
                    end block
                else
                    select case (t(i)%type)
                    case (TOKEN_IDENTIFIER)
                        select case (t(i)%value)
                        case ('WRITE')
                            call astnode_write(result, input, t, currentnode, currentnode2, childnode, fname, i)
                        case ('PROGRAM')
                            call astnode_program(result, t, currentnode, fname, i)
                        case ('MODULE')
                            call astnode_module(result, t, currentnode, fname, i)
                        case ('SUBROUTINE')
                            call astnode_subroutine(result, t, currentnode, fname, childnode, i)
                        case ('END')
                            call astnode_end(result, t, currentnode, fname, i)
                        case ('CONTAINS')
                            select case (result%nodes(currentnode)%type)
                            case (NODE_PROGRAM, NODE_MODULE)
                                if (result%nodes(currentnode)%secondpart) then
                                    call throw('already in contains section', fname, t(i)%line, t(i)%char)
                                end if
                                result%nodes(currentnode)%secondpart = .true.
                            case default
                                call throw('contains statement cannot be in this block type', fname, t(i)%line, t(i)%char)
                            end select
                        case ('USE')
                            call astnode_use(result, t, currentnode, currentnode2, fname, childnode, i)
                        case ('IMPLICIT')
                            call astnode_implicit(result, t, currentnode, currentnode2, fname, i)
                        case ('INTEGER','REAL','COMPLEX')
                            call astnode_type(result, input, t, currentnode, fname, i)
                        case ('CALL')
                            call astnode_call(result, input, t, currentnode, currentnode2, fname, i)
                        case default
                            call throw('unknown identifier "'//input%tokens(i)%value//'"', fname, t(i)%line, t(i)%char)
                        end select
                    case (TOKEN_NEXTLINE)
                    case default
                        call throw('unclassifiable statement', fname, t(i)%line, t(i)%char)
                    end select
                end if
                i = i + 1
            end associate
        end do
    end function

    subroutine ast_append(this,value,index)
        class(ast), intent(inout) :: this
        type(node), intent(inout) :: value
        integer, optional, intent(out) :: index

        type(node), allocatable :: tmp(:)

        if (iand(this%size, 15) == 0) then
            if (this%size == 0) then
                allocate(this%nodes(15))
                this%size = 1
            else
                allocate(tmp(this%size + 15))
                tmp(:this%size - 1) = this%nodes(:this%size - 1) !copy the data
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
            allocate(this%nodes(this%size)%value2, mold=value%value2)
            call move_alloc(value%value2, this%nodes(this%size)%value2)
        end if
        this%nodes(this%size)%parentnode = value%parentnode
        
        if (present(index)) index = this%size
        this%size = this%size + 1
        
    end subroutine

    pure elemental function findendln(tokens,start) result(result)
        integer :: result
        type(tokengroup), intent(in) :: tokens
        integer, intent(in) :: start
        integer :: i
        do i=start,tokens%size - 1
            if (tokens%tokens(i)%type == TOKEN_NEXTLINE) then
                result = i
                return
            end if
        end do
        result = i
    end function
end module