module fort_asttypes
   use include, only: SMALL

   type :: ast_node
      integer(SMALL) :: type !NODE_
      character(:), allocatable :: content_str
      integer :: content_int
      type(ast_node_arr), allocatable :: children(:)
      integer(SMALL) :: lnum
      integer(SMALL) :: char
   end type

   type :: ast_node_arr
      type(ast_node), allocatable :: array(:)
      integer :: size
   contains
      procedure, non_overridable :: append => ast_node_arr_append
   end type

! blocks
   ! content_str = name
   ! children(1) = declaration section
   ! children(2) = definition section
   integer(SMALL), parameter :: NODE_MODULE = 0
   integer(SMALL), parameter :: NODE_PROGRAM = 1

   ! content_str = name
   ! children(1) = attributes
   ! children(2) = template args
   ! children(3) = args
   ! children(4) = contents
   ! children(5) = return type for functions
   integer(SMALL), parameter :: NODE_FUNCTION = 2
   integer(SMALL), parameter :: NODE_SUBROUTINE = 3

   ! content_str = name
   ! children(1) = contents
   integer(SMALL), parameter :: NODE_BLOCK_DATA = 4

   ! content_str = name
   ! children(1) = flags
   ! children(2) = contenst
   integer(SMALL), parameter :: NODE_TYPE_BLOCK = 5

   ! content_str = name
   ! content_int = flags
   ! children(1) = contents
   integer(SMALL), parameter :: NODE_INTERFACE = 6

   ! content_str = name
   ! children(1) = contents
   integer(SMALL), parameter :: NODE_BLOCK = 7

   ! content_str = name
   ! children(1) = contents
   integer(SMALL), parameter :: NODE_ASSOCIATE = 8

   ! content_str = name
   ! children(1) = expr
   ! children(2) = contents (cases)
   integer(SMALL), parameter :: NODE_SELECT_CASE = 9

   ! content_str = name
   ! children(1) = exprs
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_CASE = 10

   ! content_str = name
   ! children(1) = ranges
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_FORALL = 11

   ! content_str = name
   ! children(1) = expr
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_WHERE = 12

   ! content_str = name
   ! children(1) = loop arguments (in <var>=<start>,<end>,<inc> this becomes <var>, <start>, <end>, <inc>)
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_DO = 13

   ! content_str = name
   ! children(1) = expr
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_DO_WHILE = 14

   ! content_str = name
   ! children(1) = ranges
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_DO_CONCURRENT = 15

   ! content_str = name
   ! children(1) = expr
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_IF = 16

   ! children(1) = expr
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_ELSE_IF = 17

   ! content_str = name
   ! children(1) = expr
   integer(SMALL), parameter :: NODE_SELECT_TYPE = 18

   ! content_str = name
   ! content_int = 2 if default, 1 if class, 0 if type
   ! children(1) = type
   ! children(2) = contents
   integer(SMALL), parameter :: NODE_TYPE_GAURD = 19

   integer(SMALL), parameter :: NODE_TEMPLATE = 20
   integer(SMALL), parameter :: NODE_REQUIREMENT = 21
! statements
   ! children(1,1) = left
   ! children(1,2) = right
   integer(SMALL), parameter :: NODE_ASSIGNMENT = 1000

   ! content_int = 1 if "implicit none (external)"
   ! children(1,1) = type
   ! children(1,2) = range
   integer(SMALL), parameter :: NODE_IMPLICIT = 1001

   ! content_str = module name
   ! children(1) = symbols
   integer(SMALL), parameter :: NODE_USE = 1002

   ! content_str = subroutine name
   ! children(1) = arguments
   integer(SMALL), parameter :: NODE_CALL = 1003

   integer(SMALL), parameter :: NODE_RETURN = 1004
   integer(SMALL), parameter :: NODE_INSTANTIATE = 1005
   integer(SMALL), parameter :: NODE_WRITE = 1006
   integer(SMALL), parameter :: NODE_READ = 1007
   integer(SMALL), parameter :: NODE_ALLOCATE = 1008
   integer(SMALL), parameter :: NODE_DEALLOCATE = 1009
! operators
   ! children(1,1) = left
   ! children(1,2) = right
   integer(SMALL), parameter :: NODE_ADD = 2000
   integer(SMALL), parameter :: NODE_SUB = 2001
   integer(SMALL), parameter :: NODE_MLT = 2002
   integer(SMALL), parameter :: NODE_DIV = 2003
   integer(SMALL), parameter :: NODE_EXP = 2004
   integer(SMALL), parameter :: NODE_MEMBER = 2005 ! not actually an operator
   integer(SMALL), parameter :: NODE_EQ = 2006
   integer(SMALL), parameter :: NODE_NE = 2007
   integer(SMALL), parameter :: NODE_LT = 2008
   integer(SMALL), parameter :: NODE_LE = 2009
   integer(SMALL), parameter :: NODE_GT = 2010
   integer(SMALL), parameter :: NODE_GE = 2011
   integer(SMALL), parameter :: NODE_NOT = 2012
   integer(SMALL), parameter :: NODE_AND = 2013
   integer(SMALL), parameter :: NODE_OR = 2014
   ! children(1,1) = left
   ! children(1,2) = middle
   ! children(1,3) = right
   integer(SMALL), parameter :: NODE_TERNARY = 2015
! values
   ! content_str = value
   ! children(1,1) = kind
   integer(SMALL), parameter :: NODE_INT_VAL = 3000
   integer(SMALL), parameter :: NODE_REAL_VAL = 3001
   integer(SMALL), parameter :: NODE_CHAR_VAL = 3002
   integer(SMALL), parameter :: NODE_LOGICAL_VAL = 3003
! other
   ! content_str = name
   ! children(0) = args
   integer(SMALL), parameter :: NODE_FNC_ARR = 4000
   ! content_int = label id
   integer(SMALL), parameter :: NODE_LABEL = 4001
   integer(SMALL), parameter :: NODE_ROOT = 4002

   ! content_str = string
   integer(SMALL), parameter :: NODE_STRING = 4003

   ! content_int = extended type array
   integer(SMALL), parameter :: NODE_TYPE = 4004


contains
   pure elemental subroutine ast_node_arr_append(this, value)
        class(ast_node_arr), intent(inout) :: this
        type(ast_node), intent(in) :: value

        type(ast_node), allocatable :: tmp(:)

        if (.not.allocated(this%array)) then
            allocate(this%array(16))
            this%size = 0
        else if (this%size == size(this%array)) then
            allocate(tmp(this%size * 2))
            tmp(:this%size) = this%array(:this%size) ! copy the data
            call move_alloc(tmp, this%array) ! rename
        end if
        this%size = this%size + 1
        this%array(this%size) = value
    end subroutine
end module