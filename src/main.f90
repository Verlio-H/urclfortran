program compiler
    use include
    use compile
    implicit none

    character(len=:), allocatable :: input
    ! initialize data
    input = read_file('input.f90')
    ! call compile function
    input = compiledata(input,'input.f90')
    ! output result
    ! cleanup
end program