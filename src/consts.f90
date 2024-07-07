module constants
    use astgen
    implicit none
    type type
        integer(SMALL) :: type = TYPE_NONE
        integer(SMALL) :: kind = 0
        integer(SMALL) :: properties = 0
        integer(SMALL) :: dimcount = 0
        integer(SMALL), allocatable :: dims(:)
    end type

    type const
        type(type) :: typeof
        class(*), allocatable :: value
    end type

    interface assignment(=)
        module procedure :: const_assignment
        module procedure :: type_assignment
    end interface
contains
    impure elemental subroutine type_assignment(l,r)
        type(type), intent(out) :: l
        type(type), intent(in) :: r
        l%type = r%type
        l%kind = r%kind
        l%properties = r%properties
        l%dimcount = r%dimcount
        if (allocated(l%dims)) deallocate(l%dims)
        if (allocated(r%dims)) allocate(l%dims,source=r%dims)
    end subroutine

    impure elemental subroutine const_assignment(l,r)
        type(const), intent(out) :: l
        type(const), intent(in) :: r
        l%typeof = r%typeof
        if (allocated(l%value)) deallocate(l%value)
        allocate(l%value,source=r%value)
    end subroutine

    type(const) function constadd(a,b) result(result)
        type(const) :: a, b
        result%typeof = castSame(a,b)
        select type (av=>a%value)
        type is (integer)
            select type (bv=>b%value)
            type is (integer)
                result%value = av+bv
            end select
        type is (real)
            select type (bv=>b%value)
            type is (real)
                result%value = av+bv
            end select
        type is (complex)
            select type (bv=>b%value)
            type is (complex)
                result%value = av+bv
            end select
        end select
    end function

    type(const) function constsub(a,b) result(result)
        type(const) :: a, b
        result%typeof = castSame(a,b)
        select type (av=>a%value)
        type is (integer)
            select type (bv=>b%value)
            type is (integer)
                result%value = av-bv
            end select
        type is (real)
            select type (bv=>b%value)
            type is (real)
                result%value = av-bv
            end select
        type is (complex)
            select type (bv=>b%value)
            type is (complex)
                result%value = av-bv
            end select
        end select
    end function

    type(const) function constmlt(a,b) result(result)
        type(const) :: a, b
        result%typeof = castSame(a,b)
        select type (av=>a%value)
        type is (integer)
            select type (bv=>b%value)
            type is (integer)
                result%value = av*bv
            end select
        type is (real)
            select type (bv=>b%value)
            type is (real)
                result%value = av*bv
            end select
        type is (complex)
            select type (bv=>b%value)
            type is (complex)
                result%value = av*bv
            end select
        end select
    end function

    type(const) function constdiv(a,b) result(result)
        type(const) :: a, b
        result%typeof = castSame(a,b)
        select type (av=>a%value)
        type is (integer)
            select type (bv=>b%value)
            type is (integer)
                result%value = av/bv
            end select
        type is (real)
            select type (bv=>b%value)
            type is (real)
                result%value = av/bv
            end select
        type is (complex)
            select type (bv=>b%value)
            type is (complex)
                result%value = av/bv
            end select
        end select
    end function

    type(const) function constexp(a,b) result(result)
        type(const) :: a, b
        result%typeof = castSame(a,b)
        select type (av=>a%value)
        type is (integer)
            select type (bv=>b%value)
            type is (integer)
                result%value = av**bv
            end select
        type is (real)
            select type (bv=>b%value)
            type is (real)
                result%value = av**bv
            end select
        type is (complex)
            select type (bv=>b%value)
            type is (complex)
                result%value = av**bv
            end select
        end select
    end function

    type(type) function castSame(a,b) result(result)
        type(const), intent(inout) :: a,b
        if (a%typeof%type==b%typeof%type) then
            result%type = a%typeof%type
            result%kind = max(a%typeof%kind,b%typeof%kind)
        else if (a%typeof%type==TYPE_COMPLEX) then
            result%type = TYPE_COMPLEX
            result%kind = a%typeof%kind
            select type(bv=>b%value)
            type is (real)
                b%value = cmplx(bv,0)
            type is (integer)
                b%value = cmplx(bv,0)
            end select
        else if (b%typeof%type==TYPE_COMPLEX) then
            result%type = TYPE_COMPLEX
            result%kind = b%typeof%kind
            block
                complex :: tmp
                select type(av=>a%value)
                type is (real)
                    tmp = cmplx(av,0)
                type is (integer)
                    tmp = cmplx(av,0)
                end select
                call poly_assign_cmplx(a%value,tmp)
            end block
        else if (a%typeof%type==TYPE_REAL) then
            result%type = TYPE_REAL
            result%kind = a%typeof%kind
            select type (bv=>b%value)
            type is (integer)
                block
                    real :: tmp
                    tmp = float(bv)
                    call poly_assign_real(a%value,tmp)
                    if (allocated(b%value)) deallocate(b%value)
                    allocate(b%value,source=tmp)
                end block
            end select
        else if (b%typeof%type==TYPE_REAL) then
            result%type = TYPE_REAL
            result%kind = b%typeof%kind
            select type (av=>a%value)
            type is (integer)
                block
                    real :: tmp
                    tmp = float(av)
                    if (allocated(a%value)) deallocate(a%value)
                    allocate(a%value,source=tmp)
                end block
            end select
        else
            ! TODO: handle invalid type combo
            result%type = TYPE_NONE
            result%kind = 0
        end if
    end function
end module