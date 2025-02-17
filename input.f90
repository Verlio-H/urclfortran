module module
    implicit complex(2**3) (B,A)
    integer, parameter :: SMALL = 2
    real(SMALL+2), parameter :: foo = (3/2+0.1)/SMALL**3.14*100+0.1
    complex, parameter :: i = cmplx(0,1)
    real, parameter :: e = 2.718281828
    complex, parameter :: bar = e**(0.3*i)
    integer, parameter :: abooga = nint(foo+bar%im*100)
contains
    subroutine amogus(a,abooga2)
        integer, intent(in) :: a
        integer :: abooga2
        abooga2 = a + abooga
    end subroutine
end module

module mod2
    use module, only: foo=>abooga,i,amogus
    implicit none
    integer, parameter :: goop = foo**2
    integer :: g
end module

program f
    use mod2
    use module
    integer :: goop2, goop3
    call amogus(4, goop2)
    goop3 = goop2 + 2
    g = goop3
    goop3 = g
contains
    subroutine subroutine(sub)
    end subroutine
end program