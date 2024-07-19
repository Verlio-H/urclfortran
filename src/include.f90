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
        character(len=:), allocatable :: value
    end type

    type carr
        type(string), allocatable :: array(:)
        integer :: size = 0
    contains
        procedure :: append => carr_append
    end type
contains
    subroutine poly_assign_real(l,r)
        class(*), allocatable, intent(out) :: l
        real, intent(in) :: r
        if (allocated(l)) deallocate(l)
        allocate(l,source=r)
    end subroutine

    subroutine poly_assign_int(l,r)
        class(*), allocatable, intent(out) :: l
        integer, intent(in) :: r
        if (allocated(l)) deallocate(l)
        allocate(l,source=r)
    end subroutine

    subroutine poly_assign_cmplx(l,r)
        class(*), allocatable, intent(out) :: l
        complex, intent(in) :: r
        if (allocated(l)) deallocate(l)
        allocate(l,source=r)
    end subroutine

    subroutine poly_assign_poly(l,r)
        class(*), allocatable, intent(out) :: l
        class(*), intent(in) :: r
        l = r
    end subroutine

    function read_file(fname)
        character(len=*), intent(in) :: fname
        character(len=:), allocatable :: read_file
        logical :: done
        open(file=fname,unit=1,action='read')
        read_file = ''
        do
            read_file = read_file//trim(getline(fname,1,done))//achar(10)
            if (done) exit
        end do
        close(3)
    end function

    pure function itoa(i)
        integer, intent(in) :: i
        character(len=:), allocatable :: itoa
        itoa = repeat(' ',12)
        write(itoa,'(I12)') i
        itoa = trim(adjustl(itoa))
    end function

    pure function itoa2(i)
        integer(SMALL), intent(in) :: i
        character(len=:), allocatable :: itoa2
        integer :: tmp
        tmp = i ! cursed way of avoiding the cast
        itoa2 = itoa(tmp)
    end function

    pure function rtoa(r)
        real, intent(in) :: r
        character(len=:), allocatable :: rtoa
        rtoa = repeat(' ',32)
        write(rtoa,'(1PE14.7E2)') r
        rtoa = trim(adjustl(rtoa))
    end function

    pure function atoi2(a)
        character(len=63), intent(in) :: a
        integer(SMALL) :: atoi2
        read(a,*) atoi2
    end function

    pure function atoi(a)
        character(len=63), intent(in) :: a
        integer :: atoi
        read(a,*) atoi
    end function

    pure function ator(a)
        character(len=63), intent(in) :: a
        real :: ator
        read(a,*) ator
    end function

    pure function atoc(a)
        character(len=63), intent(in) :: a
        complex :: atoc
        read(a,*) atoc%re
        read(a(index(a,' ')+1:),*) atoc%im
    end function

    pure function tocaps(input) result(out)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: out

        integer :: i
        allocate(character(len=len(input)) :: out)
        do i=1,len(input)
            if (input(i:i)>='a'.and.input(i:i)<='z') then
                out(i:i) = achar(iachar(input(i:i))-32)
            else
                out(i:i) = input(i:i)
            end if
        end do
    end function

    subroutine throw(err,fname,lnum,char,stop)
        character(len=*), intent(in) :: err
        character(len=*), intent(in) :: fname
        integer(SMALL), intent(in) :: lnum
        integer(SMALL), intent(in) :: char
        logical, optional :: stop
        print'(A,I0,A,I0,A)',fname//':',lnum,':',char,': '//err
        if (present(stop)) then
            if (stop) then
                stop -1, quiet=.true.
            end if
        else
            stop -1, quiet=.true.
        end if
        output = .false.
    end subroutine

    pure elemental subroutine iarr_append(this,value)
        class(iarr), intent(inout) :: this
        integer, intent(in) :: value

        integer, allocatable :: tmp(:)
        if (iand(this%size,15)==0) then
            if (this%size==0) then
                allocate(this%array(15))
                this%size = 1
            else
                allocate(tmp(this%size+15))
                tmp(:this%size-1) = this%array(:this%size-1) !copy the data
                call move_alloc(tmp, this%array) !rename
            end if
        end if
        this%array(this%size) = value
        this%size = this%size+1
    end subroutine
    
    pure elemental subroutine siarr_append(this,value)
        class(siarr), intent(inout) :: this
        integer(SMALL), intent(in) :: value

        integer(SMALL), allocatable :: tmp(:)
        if (iand(this%size,15)==0) then
            if (this%size==0) then
                allocate(this%array(15))
                this%size = 1
            else
                allocate(tmp(this%size+15))
                tmp(:this%size-1) = this%array(:this%size-1) !copy the data
                call move_alloc(tmp, this%array) !rename
            end if
        end if
        this%array(this%size) = value
        this%size = this%size+1
    end subroutine

    pure elemental subroutine carr_append(this,value)
        class(carr), intent(inout) :: this
        character(len=*), intent(in) :: value

        type(string), allocatable :: tmp(:)
        if (iand(this%size,15)==0) then
            if (this%size==0) then
                allocate(this%array(15))
                this%size = 1
            else
                allocate(tmp(this%size+15))
                tmp(:this%size-1) = this%array(:this%size-1) !copy the data
                call move_alloc(tmp, this%array) !rename
            end if
        end if
        this%array(this%size)%value = value
        this%size = this%size+1
    end subroutine

    function getline(fname,unit,end) result(line)
        character(len=*), intent(in) :: fname
        integer, intent(in) :: unit
        logical, intent(out), optional :: end
        character(len=:), allocatable :: line
        character(len=256) :: readline
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
            call throw('unexpected EOF',fname,0_2,0_2)
        end if
    end

    integer function precedence(operator)
        character(len=*), intent(in) :: operator

        select case (operator)
        case ('+','-')
            precedence = 4
        case ('//')
            precedence = 5
        case ('*','/')
            precedence = 6
        case ('**')
            precedence = 7
        case ('%')
            precedence = 8
        case ('(','NINT')
            precedence = -1
        case default
            call throw('unknown precedence for operator '//operator,'',0_2,0_2)
        end select
    end function
end module