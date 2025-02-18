module a
contains
    subroutine foo(a, b)
        integer :: a
        integer, intent(in) :: b
        a = b
    end subroutine
end module

program b
    use a
    call foo(2, 3)
    call bar()
    call bar2()
contains
    subroutine bar()
    end subroutine
end program
    