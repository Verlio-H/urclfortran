program compiler
    use include, only: read_file, carr
    use compile, only: compiledata
    implicit none

    character(:), allocatable :: input, output
    character(256) :: argument, libloc
    character(:), allocatable :: ofname
    type(carr) :: ifnames
    logical :: link
    integer :: newunit, argnum, argcnt

    ! read arguments
    link = .true.
    argnum = 1
    ofname = 'output.urcl'
    argcnt = command_argument_count()
    do while (argnum <= argcnt)
        call get_command_argument(argnum, argument)
        if (argument == '-o') then
            argnum = argnum + 1
            call get_command_argument(argnum, argument)
            ofname = trim(argument)
        else if (argument == '-c') then
            link = .false.
        else
            call ifnames%append(trim(argument))
        end if
        argnum = argnum + 1
    end do

    if (.not.allocated(ifnames%array)) then
        write(*, '(A)') 'error: no input files provided'
        stop
    end if

    output = ''
    do argnum = 1, ifnames%size - 1
        ! initialize data
        input = read_file(ifnames%array(argnum)%value)
        
        ! call compile function
        output = output//compiledata(input, ifnames%array(argnum)%value)
    end do

    ! output result
    open(newunit=newunit, file=ofname)
    write(newunit, '(A)') output
    close(newunit)

    if (link) then
        call get_environment_variable('LIBFORT_PATH',libloc)
        call execute_command_line('urcl-ld -o '//ofname//' '//&
                                    trim(libloc)//'/urcl16/main.urcl '//&
                                    trim(libloc)//'/urcl16/math.urcl '//ofname)
    end if
end program