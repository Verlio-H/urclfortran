module include
   use iso_c_binding, only: c_sizeof, c_size_t, c_char
   use data_mod, only: list
   implicit none (type, external)

   integer, parameter :: BIG = c_size_t

   type :: string
      character(:), allocatable :: val
   end type

   type :: file_span
      character(:), allocatable :: file_name
      integer(BIG) :: start_line = 0, end_line = 0
      integer :: start_column = 0, end_column = 0
   end type

   type :: location
      type(file_span), allocatable :: loc_chain(:)
   end type

   type :: annotated_string
      character(:), allocatable :: val
      type(location) :: loc = location()
   end type
   
   integer, parameter :: SMALL = selected_int_kind(4)
   integer, parameter :: INT_KIND = kind(1)

   logical :: output = .true.

contains
   subroutine poly_assign_real(l, r)
      class(*), allocatable, intent(out) :: l
      real, intent(in) :: r

      if (allocated(l)) deallocate(l)
      allocate(l, source=r)
   end subroutine

   subroutine poly_assign_int(l, r)
      class(*), allocatable, intent(out) :: l
      integer, intent(in) :: r

      if (allocated(l)) deallocate(l)
      allocate(l, source=r)
   end subroutine

   subroutine poly_assign_cmplx(l, r)
      class(*), allocatable, intent(out) :: l
      complex, intent(in) :: r

      if (allocated(l)) deallocate(l)
      allocate(l, source=r)
   end subroutine

   subroutine poly_assign_poly(l, r)
      class(*), allocatable, intent(out) :: l
      class(*), intent(in) :: r

      l = r
   end subroutine

   subroutine read_file(result, fname, original_loc)
      type(list), intent(inout) :: result
      type(location), intent(in), optional :: original_loc
      character(*), intent(in) :: fname

      type(location) :: loc
      type(location), target :: line_loc
      type(file_span), pointer :: span
      character(:), allocatable :: line

      integer :: lnum
      logical :: done

      if (present(original_loc)) then
         loc = original_loc
      else
         allocate(loc%loc_chain(0))
      end if

      allocate(line_loc%loc_chain(size(loc%loc_chain) + 1))
      span => line_loc%loc_chain(size(line_loc%loc_chain))
      span%file_name = fname

      lnum = 1
      open(file=fname, unit=1)
      do
         line = trim(getline(fname, 1, done))
         span%start_line = lnum
         span%start_column = 1
         span%end_line = lnum
         span%end_column = len(line)
         call result%push(annotated_string(line, line_loc))
         if (done) exit
         lnum = lnum + 1
      end do
      close(3)
   end subroutine

   pure function file_char(fname, lnum, char) result(span)
      character(*), intent(in) :: fname
      integer(BIG), intent(in) :: lnum
      integer, intent(in) :: char
      type(file_span) :: span

      span = file_span(fname, lnum, lnum, char, char)
   end function

   pure function itoa(i) result(result)
      character(:), allocatable :: result
      integer, intent(in) :: i

      result = repeat(' ', 12)
      write(result(:), '(I12)') i
      result = trim(adjustl(result))
   end function

   pure function sitoa(i) result(result)
      character(:), allocatable :: result
      integer(SMALL), intent(in) :: i

      result = itoa(int(i))
   end function

   pure function bitoa(i) result(result)
      character(:), allocatable :: result
      integer(BIG), intent(in) :: i

      result = repeat(' ', 20)
      write(result(:), '(I20)') i
      result = trim(result(trim_index(result):))
   end function

   pure function rtoa(r) result(result)
      character(:), allocatable :: result
      real, intent(in) :: r

      result = repeat(' ', 32)
      write(result(:), '(1PE14.7E2)') r
      result = trim(adjustl(result))
   end function

   pure function ltoa(l) result(result)
      character(:), allocatable :: result
      logical, intent(in) :: l

      if (l) then
         result = '.true.'
      else
         result = '.false.'
      end if
   end function

   pure function atosi(a) result(result)
      integer(SMALL) :: result
      character(*), intent(in) :: a

      read(a, *) result
   end function

   pure function atobi(a) result(result)
      integer(BIG) :: result
      character(*), intent(in) :: a

      read(a, *) result
   end function

   pure function atoi(a) result(result)
      integer :: result
      character(*), intent(in) :: a

      read(a, *) result
   end function

   pure function ator(a) result(result)
      real :: result
      character(*), intent(in) :: a

      read(a, *) result
   end function

   pure function atoc(a) result(result)
      complex :: result
      character(*), intent(in) :: a

      read(a, *) result%re
      read(a(index(a, ' ') + 1:), *) result%im
   end function

   pure function atol(a) result(result)
      logical :: result
      character(*), intent(in) :: a

      if (a == '.TRUE.') then
         result = .true.
      else
         result = .false.
      end if
   end function

   function safe_index(str, char, loc) result(pos)
      character(*), intent(in) :: str
      character(1), intent(in) :: char
      type(location), intent(in) :: loc
      integer(BIG) :: pos

      pos = index(str, char)
      if (pos == 1) then
         call throw('Empty value', loc)
      end if
   end function

   pure function to_upper(input) result(out)
      character(:), allocatable :: out
      character(*), intent(in) :: input

      integer :: i

      allocate(character(len(input)) :: out)
      do i = 1, len(input)
         if (input(i:i) >= 'a' .and. input(i:i) <= 'z') then
               out(i:i) = achar(iachar(input(i:i)) - 32)
         else
               out(i:i) = input(i:i)
         end if
      end do
   end function

   pure function to_lower(input) result(out)
      character(:), allocatable :: out
      character(*), intent(in) :: input

      integer :: i

      allocate(character(len(input)) :: out)
      do i = 1, len(input)
         if (input(i:i) >= 'A' .and. input(i:i) <= 'Z') then
               out(i:i) = achar(iachar(input(i:i)) + 32)
         else
               out(i:i) = input(i:i)
         end if
      end do
   end function

   function count_char(str, char) result(count)
      character(*), target, intent(in) :: str
      character, value, intent(in) :: char
      integer :: count

      character(:), pointer :: current

      current => str
      count = 0
      do while (index(current, char) /= 0)
         current => current(index(current, char) + 1:)
         count = count + 1
      end do 
   end function

   pure function trim_index(str) result(out)
      character(*), intent(in) :: str
      integer :: out

      do out = 1, len(str)
         if (str(out:out) /= ' ') return
      end do
   end function

   subroutine throw(err, loc, stop)
      character(*), intent(in) :: err
      type(location), intent(in) :: loc
      logical, optional :: stop

      if (allocated(loc%loc_chain)) then
         print '(A,I0,A,I0,A)', loc%loc_chain(1)%file_name//':', &
                                loc%loc_chain(1)%start_line, ':', &
                                loc%loc_chain(1)%start_column, ': '// &
                                err
      else
         print '(A)', 'Unknown: '//err
      end if
      if (present(stop)) then
         if (stop) then
               stop -1, quiet=.true.
         end if
      else
         stop -1, quiet=.true.
      end if
      output = .false.
   end subroutine

   function getline(fname, unit, end) result(line)
      character(:), allocatable :: line
      character(*), intent(in) :: fname
      integer, intent(in) :: unit
      logical, intent(out), optional :: end
      
      character(256) :: readline

      line = ''
      do
        read (unit, '(A)', advance='no', eor=3, end=999) readline
        line = line//readline
      end do
   3  line = line//readline
      if (present(end)) end = .false.
      return
  999 if (present(end)) then
         end = .true.
      else
         call throw('unexpected EOF', location([file_span(fname)]))
      end if
   end

   integer function precedence(operator)
      character(*), intent(in) :: operator

      select case (operator)
      case ('.EQV.', '.NEQV.')
         precedence = 1
      case ('.OR.')
         precedence = 2
      case ('.AND.')
         precedence = 3
      case ('.NOT.')
         precedence = 4
      case ('.EQ.', '.NE', '.LT.', '.LE.', '.GT.', '.GE.')
         precedence = 5
      case ('//')
         precedence = 6
      case ('+', '-')
         precedence = 7
      case ('*', '/')
         precedence = 8
      case ('**')
         precedence = 9
      case ('(')
         precedence = -1
      case default
         call throw('unknown precedence for operator '//operator, location())
      end select
   end function
end module
