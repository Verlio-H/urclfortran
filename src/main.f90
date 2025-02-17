program compiler
    use include
    use compile
    implicit none

    character(len=:), allocatable :: input
    integer :: newunit
    ! initialize data
    input = read_file('input.f90')
    ! call compile function
    input = compiledata(input,'input.f90')
    ! output result
    open(newunit=newunit, file='output.urcl')
    write(newunit, '(A)') input
    ! cleanup
end program