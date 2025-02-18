module a
    implicit none
    integer :: foo
    integer :: bar

contains
    subroutine b()
    end subroutine
end module

module z
end module

program e
    use z
    use a, only: w => b, bar
end program