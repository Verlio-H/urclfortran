module include
    implicit none
    integer, parameter :: SMALL = selected_int_kind(4)
    logical :: output = .true.

    type iarr
        integer, allocatable :: array(:)
        integer :: size = 0
    contains
        procedure :: append => iarr_append
    end type

    type siarr
        integer(SMALL), allocatable :: array(:)
        integer :: size = 0
    contains
        procedure :: append => siarr_append
    end type

    type string
        character(:), allocatable :: value
    end type

    type carr
        type(string), allocatable :: array(:)
        integer :: size = 0
    contains
        procedure :: append => carr_append
        procedure :: append_carr => carr_append_carr
    end type
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

    function read_file(fname) result(result)
        character(:), allocatable :: result
        character(*), intent(in) :: fname

        logical :: done

        open(file=fname, unit=1)
        result = ''
        do
            result = result//trim(getline(fname, 1, done))//achar(10)
            if (done) exit
        end do
        close(3)
    end function

    pure function itoa(i) result(result)
        character(:), allocatable :: result
        integer, intent(in) :: i

        result = repeat(' ', 12)
        write(result, '(I12)') i
        result = trim(adjustl(result))
    end function

    pure function itoa2(i) result(result)
        character(:), allocatable :: result
        integer(SMALL), intent(in) :: i

        result = itoa(int(i))
    end function

    pure function rtoa(r) result(result)
        character(:), allocatable :: result
        real, intent(in) :: r

        result = repeat(' ', 32)
        write(result, '(1PE14.7E2)') r
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

    pure function atoi2(a) result(result)
        integer(SMALL) :: result
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

    pure function tocaps(input) result(out)
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

    pure function tolower(input) result(out)
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

    subroutine throw(err, fname, lnum, char, stop)
        character(*), intent(in) :: err
        character(*), intent(in) :: fname
        integer(SMALL), intent(in) :: lnum
        integer(SMALL), intent(in) :: char
        logical, optional :: stop

        print '(A,I0,A,I0,A)', fname//':', lnum, ':', char, ': '//err
        if (present(stop)) then
            if (stop) then
                stop -1, quiet=.true.
            end if
        else
            stop -1, quiet=.true.
        end if
        output = .false.
    end subroutine

    pure elemental subroutine iarr_append(this, value)
        class(iarr), intent(inout) :: this
        integer, intent(in) :: value

        integer, allocatable :: tmp(:)

        if (iand(this%size, 15) == 0) then
            if (this%size == 0) then
                allocate(this%array(15))
                this%size = 1
            else
                allocate(tmp(this%size + 15))
                tmp(:this%size - 1) = this%array(:this%size - 1) !copy the data
                call move_alloc(tmp, this%array) !rename
            end if
        end if
        this%array(this%size) = value
        this%size = this%size + 1
    end subroutine
    
    pure elemental subroutine siarr_append(this, value)
        class(siarr), intent(inout) :: this
        integer(SMALL), intent(in) :: value

        integer(SMALL), allocatable :: tmp(:)

        if (iand(this%size, 15) == 0) then
            if (this%size == 0) then
                allocate(this%array(15))
                this%size = 1
            else
                allocate(tmp(this%size + 15))
                tmp(:this%size - 1) = this%array(:this%size - 1) !copy the data
                call move_alloc(tmp, this%array) !rename
            end if
        end if
        this%array(this%size) = value
        this%size = this%size + 1
    end subroutine

    pure elemental subroutine carr_append(this, value)
        class(carr), intent(inout) :: this
        character(*), intent(in) :: value

        type(string), allocatable :: tmp(:)

        if (iand(this%size, 15) == 0) then
            if (this%size == 0) then
                allocate(this%array(15))
                this%size = 1
            else
                allocate(tmp(this%size + 15))
                tmp(:this%size - 1) = this%array(:this%size - 1) !copy the data
                call move_alloc(tmp, this%array) !rename
            end if
        end if
        this%array(this%size)%value = value
        this%size = this%size + 1
    end subroutine

    pure elemental subroutine carr_append_carr(this, value)
        class(carr), intent(inout) :: this
        class(carr), intent(in) :: value

        integer :: i
        do i = 1, value%size - 1
            call this%append(value%array(i)%value)
        end do
    end subroutine

    function getline(fname, unit, end) result(line)
        character(:), allocatable :: line
        character(*), intent(in) :: fname
        integer, intent(in) :: unit
        logical, intent(out), optional :: end
        
        character(256) :: readline

        line = ''
      2 read (unit, '(A)', advance='no', eor=3, end=999) readline
        line = line//readline
        goto 2
      3 line = line//readline
        if (present(end)) end = .false.
        return
    999 if (present(end)) then
            end = .true.
        else
            call throw('unexpected EOF', fname, 0_SMALL, 0_SMALL)
        end if
    end

    integer function precedence(operator)
        character(*), intent(in) :: operator
        print*,operator
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
        case ('%')
            precedence = 11
        case ('(', 'NINT')
            precedence = -1
        case default
            call throw('unknown precedence for operator '//operator, '', 0_SMALL, 0_SMALL)
        end select
    end function
end module