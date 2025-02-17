module semantic
    use consts
    implicit none

    type :: sem_variable
        type(type) :: vartype
        character(:), allocatable :: name
        class(*), allocatable :: value ! only applies to parameters
        integer :: offset ! only used in backend
        character(:), allocatable :: srcmod
    end type

    type :: sem_proc
        logical :: subrout
        character(:), allocatable :: name
        type(sem_variable) :: return
        type(sem_variable), allocatable :: arguments(:)
    end type

    type :: sem_inter
        character(len=:), allocatable :: name
        type(sem_proc), allocatable :: functions(:)
    end type

    ! TODO: add proper inheritance stuff
    type :: sem_type
        character(:), allocatable :: name
        type(type) :: inherits
        type(type), allocatable :: components(:)
    end type

    type :: sem_module
        character(:), allocatable :: name
        type(sem_variable), allocatable :: vartbl(:) ! contains all public heap allocated variables
        type(sem_inter), allocatable :: functbl(:) ! contains all public interfaces
        type(sem_type), allocatable :: typetbl(:) ! contains all public types
    end type

    interface assignment(=)
        procedure :: sem_module_assignment
    end interface
contains
    ! given an index in the ast, generate a module file
    subroutine genmodfile(input, index, outputmod)
        type(ast), intent(in) :: input
        integer, intent(in) :: index
        type(sem_module), optional, intent(out) :: outputmod

        type(sem_module) :: result
        integer(SMALL) :: i
        type(type) :: implicit(26)
        integer :: unit

        ! default implicit typing rules
        implicit(1:8) = type(TYPE_REAL, 4, 0, 0)
        implicit(9:15) = type(TYPE_INTEGER, 4, 0, 0)
        implicit(16:26) = type(TYPE_REAL, 4, 0, 0)

        select case (input%nodes(index)%type)
        case (NODE_MODULE,NODE_PROGRAM)
            result = sem_module()
            result%name = trim(input%nodes(index)%value)
            ! get semantic info
            i = 1
            do while (i <= input%nodes(index)%subnodes2%size - 1)
                associate (subnode => input%nodes(input%nodes(index)%subnodes2%array(i)))
                    select case (subnode%type)
            !  - import other modules
                    case (NODE_USE)
                        block
                            if (allocated(subnode%subnodes%array)) then
                                result = combine(result, import_module(subnode%value, &
                                        input%nodes(subnode%subnodes%array(:subnode%subnodes%size - 1))%value))
                            else
                                result = combine(result, import_module(subnode%value, [repeat(' ', 64)]))
                            end if
                        end block
            !  - get implicit rules
                    case (NODE_IMPLICIT)
                        associate (typenode => input%nodes(subnode%subnodes%array(1)))
                            if (typenode%type == NODE_TYPE) then
                                select type (a => typenode%value2)
                                type is (integer(SMALL))
                                    if (a == TYPE_NONE) then
                                        implicit(:) = type(TYPE_NONE, 0, 0, 0)
                                        i = i + 1_SMALL
                                        cycle
                                    end if
                                end select
                            end if
                            block
                                type(type) :: implicittype
                                integer :: j
                                implicittype = eval_type(input,subnode%subnodes%array(1),result)
                                ! parse range
                                do j = 1, subnode%subnodes2%size - 1
                                    associate (str=>input%nodes(subnode%subnodes2%array(j))%value)
                                        if (str(:1) < 'A') then
                                            call throw('error in implicit statement', &
                                                        subnode%fname, subnode%startlnum, subnode%startchar)
                                        end if
                                        if (str(2:2) == '-') then
                                            if (str(3:3) < 'A') then
                                                call throw('error in implicit statement', &
                                                            subnode%fname,subnode%startlnum,subnode%startchar)
                                            end if
                                            implicit(iachar(str(:1)) - iachar('@'):iachar(str(3:3)) - iachar('@')) = implicittype
                                        else
                                            implicit(iachar(str(:1)) - iachar('@')) = implicittype
                                        end if
                                    end associate
                                end do
                            end block
                        end associate
            !  - iterate over all types
                    end select
                end associate
                i = i + 1_SMALL
            end do

            i = 1
            do while (i <= input%nodes(index)%subnodes%size - 1)
            !  - iterate over all functions
                block
                    type(sem_proc) :: func
                    type(sem_inter) :: resultinter
                    integer :: j, varloc
                    character(64), allocatable :: names(:)
                    associate (tidx => input%nodes(index)%subnodes%array(i))
                        associate (t => input%nodes(tidx))
                            select case (t%type)
                            case (NODE_SUBROUTINE)
                                func%subrout = .true.
                                if (t%subnodes%size > 0) then
                                    ! get name of arguments
                                    allocate(func%arguments(t%subnodes%size - 1))
                                    allocate(names(t%subnodes%size - 1))
                                    do j = 1, t%subnodes%size - 1
                                        associate (r => input%nodes(t%subnodes%array(j)))
                                            select case (r%type)
                                            case (NODE_STRING)
                                                func%arguments(j)%name = trim(r%value)
                                                names(j) = r%value
                                            case default
                                                call throw('expected string for var name', r%fname, r%startlnum, r%startchar)
                                            end select
                                        end associate
                                    end do
                                    ! get set of variable types
                                    do j = 1, t%subnodes2%size - 1
                                        associate (ridx => t%subnodes2%array(j))
                                            associate (r => input%nodes(ridx))
                                                select case (r%type)
                                                case (NODE_TYPE)
                                                    varloc = findloc(names, r%value, dim=1)
                                                    if (varloc /= 0) then
                                                        !update var info
                                                        func%arguments(varloc)%vartype = eval_type(input, ridx, result)
                                                    end if
                                                end select
                                            end associate
                                        end associate
                                    end do
                                    ! fill in implicit types
                                    do j = 1, size(func%arguments)
                                        associate (r => func%arguments(j))
                                            if (r%vartype%type == TYPE_NONE) then
                                                r%vartype = implicit(iachar(r%name(:1)) - iachar('@'))
                                                if (r%vartype%type == TYPE_NONE) then
                                                    call throw('variable '''//r%name//''' has no implicit type', &
                                                                t%fname, t%startlnum, t%startchar)
                                                end if
                                            end if
                                        end associate
                                    end do
                                end if
                                ! add to module
                                func%name = trim(t%value)
                                resultinter%name = trim(t%value)
                                resultinter%functions = [func]
                                if (allocated(result%functbl)) then
                                    block
                                        type(sem_inter), allocatable :: tmp(:)
                                        call move_alloc(result%functbl, tmp)
                                        allocate(result%functbl(size(tmp) + 1))
                                        result%functbl(:size(tmp)) = tmp
                                        result%functbl(size(result%functbl)) = resultinter
                                    end block
                                else
                                    result%functbl = [resultinter]
                                end if
                            end select
                        end associate
                    end associate
                end block
                i = i + 1_SMALL
            end do
            i = 1
            
            if (input%nodes(index)%type == NODE_MODULE) then
                ! iterate over all variables
                do while (i <= input%nodes(index)%subnodes2%size - 1)
                    associate (tidx => input%nodes(index)%subnodes2%array(i))
                        associate (t => input%nodes(tidx))
                            select case (t%type)
                            case (NODE_TYPE)
                                select type (a => t%value2)
                                type is (integer(SMALL))
                                    block
                                        type(type) :: vartype
                                        type(sem_variable) :: variable
                                        type(const) :: evalresult
                                        vartype = eval_type(input,tidx,result)
                                        variable%vartype = vartype
                                        variable%name = t%value
                                        variable%srcmod = result%name
                                        if (t%subnodes%size - 1 >= 2) then
                                            evalresult = eval_constexpr(input, t%subnodes%array(2), result)
                                            if (allocated(variable%value)) deallocate(variable%value)
                                            allocate(variable%value, source=evalresult%value)
                                        end if
                                        if (allocated(result%vartbl)) then
                                            block
                                                type(sem_variable), allocatable :: tmp(:)
                                                call move_alloc(result%vartbl, tmp)
                                                allocate(result%vartbl(size(tmp) + 1))
                                                result%vartbl(:size(tmp)) = tmp
                                                result%vartbl(size(result%vartbl)) = variable
                                            end block
                                        else
                                            result%vartbl = [variable]
                                        end if
                                    end block
                                end select
                            end select
                        end associate
                    end associate
                    i = i + 1_SMALL
                end do
                ! generate module file
                open(newunit=unit, file=result%name//'.fmod')
                ! output all variable names
                write(unit, '(A)') 'VARS'
                if (allocated(result%vartbl)) then
                    do i = 1_SMALL, int(size(result%vartbl), SMALL) ! possibly do something about the cast here
                        associate (v => result%vartbl(i))
                            if (iand(v%vartype%properties, int(PROP_PRIVATE, SMALL)) /= 0) cycle
                            write(unit, '(A)') '-'//trim(v%name)
                            write(unit, '(A)') ' '//v%srcmod
                            call writesemvar(unit, v)
                        end associate
                    end do
                end if
                write(unit, '(A)') 'FUNCS'
                if (allocated(result%functbl)) then

                    do i = 1_SMALL, int(size(result%functbl), SMALL) ! possibly do something about the cast here
                        associate (v=>result%functbl(i))
                            write(unit, '(A)') '-'//trim(v%name)
                            block
                                integer :: j, k
                                do j = 1, size(v%functions)
                                    associate(r => v%functions(j))
                                        write(unit, '(A)') ' '//r%name
                                        if (r%subrout) then
                                            write(unit, '(A)') ' S'
                                        else
                                            write(unit, '(A)') ' F'
                                            write(unit, '(A)') ' ='//r%return%name
                                            call writesemvar(unit, r%return)
                                        end if
                                        do k = 1, size(r%arguments)
                                            write(unit, '(A)') ' -'//trim(r%arguments(k)%name)
                                            call writesemvar(unit, r%arguments(k))
                                        end do
                                    end associate
                                end do
                            end block
                        end associate
                    end do
                end if
                write(unit, '(A)') 'END'
                close(unit)
            end if
            if (present(outputmod)) then
                ! TODO: change to move_alloc tomfoolery
                outputmod = result
            end if
        case default
        call throw('expected module or program', &
                    input%nodes(index)%fname, input%nodes(index)%startlnum, input%nodes(index)%startchar)
        end select
    end subroutine

    subroutine writesemvar(unit, v)
        integer, value, intent(in) :: unit
        type(sem_variable), intent(in) :: v

        associate (t => v%vartype)
            write(unit, '(A)') ' '//itoa2(t%type)
            write(unit, '(A)') ' '//itoa2(t%kind)
            write(unit, '(A)') ' '//itoa2(t%properties)
            write(unit, '(A)') ' '//itoa2(t%dimcount)
            write(unit, '(A)', advance='no')
            if (allocated(t%dims)) then
                write(unit, '(I6)') t%dims
            else
                write(unit, '(A)') ' NONE'
            end if
        end associate
        if (allocated(v%value)) then
            select type (vv => v%value)
            type is (integer)
                write(unit, '(A)') ' I'//itoa(vv)
            type is (real)
                write(unit, '(A)') ' R'//rtoa(vv)
            type is (complex)
                write(unit, '(A)') ' C'//rtoa(vv%re)//' '//rtoa(vv%im)
            end select
        else
            write(unit, '(A)') ' N'
        end if
    end subroutine

    function combine(a, b) result(result)
        type(sem_module) :: result
        type(sem_module), intent(in) :: a, b

        result%name = a%name
        if (allocated(a%vartbl)) then
            if (allocated(b%vartbl)) then
                result%vartbl = [a%vartbl, b%vartbl]
            else
                result%vartbl = a%vartbl
            end if
        else if (allocated(b%vartbl)) then
            result%vartbl = b%vartbl
        end if
        if (allocated(a%functbl)) then
            if (allocated(b%functbl)) then
                result%functbl = [a%functbl, b%functbl]
            else
                result%functbl = a%functbl
            end if
        else if (allocated(b%functbl)) then
            result%functbl = b%functbl
        end if
        if (allocated(a%typetbl)) then
            if (allocated(b%typetbl)) then
                result%typetbl = [a%typetbl, b%typetbl]
            else
                result%typetbl = a%typetbl
            end if
        else if (allocated(b%typetbl)) then
            result%typetbl = b%typetbl
        end if
    end function

    function eval_type(input, index, semmod) result(result)
        type(type) :: result
        type(ast), intent(in) :: input
        integer, intent(in) :: index
        type(sem_module), intent(in) :: semmod

        type(const) :: kind

        associate (t => input%nodes(index))
            result%properties = int(t%subnodes%array(1), SMALL)
            ! TODO: arrays
            ! evaluate kind
            kind = eval_constexpr(input, t%subnodes2%array(1), semmod)
            if (kind%typeof%type /= TYPE_INTEGER) then
                call throw('expected integer as kind value', t%fname, t%startlnum, t%startchar)
            end if
            select type (a => kind%value)
            type is (integer)
                result%kind = int(a, SMALL)
            end select
            select type (a => t%value2)
            type is (integer(SMALL))
                result%type = a
            end select
        end associate
    end function

    recursive function eval_constexpr(input, i, semmod) result(result)
        type(const) :: result
        type(ast), intent(in) :: input
        integer, intent(in) :: i
        type(sem_module), intent(in) :: semmod

        type(const) :: a, b

        associate (t => input%nodes(i))
            select case (t%type)
            case (NODE_INT_VAL)
                block
                    integer(SMALL) :: iunder
                    integer :: value ! make this a large int later on
                    result%typeof%type = TYPE_INTEGER
                    result%typeof%kind = 4
                    iunder = int(index(t%value, '_'), SMALL)
                    if (iunder /= 0) then
                        read(t%value(:iunder - 1), *) value
                        if (t%value(iunder + 1:iunder + 1) >= '0' .and. t%value(iunder + 1:iunder + 1) <= '9') then
                            read(t%value(iunder + 1:), *) result%typeof%kind
                        else
                            call throw('kinds from identifiers are currently unsupported', t%fname, t%startlnum, t%startchar)
                        end if
                    else
                        read(t%value, *) value
                    end if
                    result%value = value ! can't be read into directly prior to fortran 2023
                end block
            case (NODE_REAL_VAL)
                block
                    integer(SMALL) :: iunder
                    real :: value ! make this a large real later on
                    result%typeof%type = TYPE_REAL
                    result%typeof%kind = 4
                    iunder = int(index(t%value, '_'), SMALL)
                    if (iunder /= 0) then
                        read(t%value(:iunder - 1), *) value
                        if (t%value(iunder + 1:iunder + 1) >= '0'.and.t%value(iunder + 1:iunder + 1) <= '9') then
                            read(t%value(iunder + 1:),*) result%typeof%kind
                        else
                            call throw('kinds from identifiers are currently unsupported', t%fname, t%startlnum, t%startchar)
                        end if
                    else
                        read(t%value, *) value
                    end if
                    result%value = value ! can't be read into directly prior to fortran 2023
                end block
            case (NODE_STRING)
                block
                    ! find variable
                    integer :: j
                    if (allocated(semmod%vartbl)) then
                        do j = 1, size(semmod%vartbl)
                            if (semmod%vartbl(j)%name == t%value) exit
                        end do
                        if (j > size(semmod%vartbl)) then
                            if (allocated(t%fname)) then
                                call throw('variable name not found', t%fname, t%startlnum, t%startchar)
                            else
                                call throw('variable name not found: '//t%value, 'unknown', t%startlnum, t%startchar)
                            end if
                        end if
                    else
                        call throw('variable name not found', t%fname, t%startlnum, t%startchar) 
                    end if
                    if (iand(semmod%vartbl(j)%vartype%properties, int(PROP_PARAMETER, SMALL)) == 0) then
                        call throw('variables in constant expressions must be parameters', t%fname, t%startlnum, t%startchar)
                    end if
                    result%typeof = semmod%vartbl(j)%vartype
                    if (allocated(result%value)) deallocate(result%value)
                    !allocate(result%value,source=semmod%vartbl(j)%value) ! for some reason this no work in gfortran
                    !result%value = semmod%vartbl(j)%value ! for some reason this no work in ifort
                    call poly_assign_poly(result%value, semmod%vartbl(j)%value) ! works in both
                end block
            case (NODE_FNC_ARR)
                ! TODO: arrays
                result = eval_constfunc(input, semmod, t%value, t%subnodes)
            case (NODE_ADD)
                a = eval_constexpr(input, t%subnodes%array(1), semmod)
                b = eval_constexpr(input, t%subnodes2%array(1), semmod)
                call const_assignment(result, const_add(a,b))
            case (NODE_SUB)
                a = eval_constexpr(input, t%subnodes%array(1), semmod)
                b = eval_constexpr(input, t%subnodes2%array(1), semmod)
                call const_assignment(result, const_sub(a, b))
            case (NODE_MLT)
                a = eval_constexpr(input, t%subnodes%array(1), semmod)
                b = eval_constexpr(input, t%subnodes2%array(1), semmod)
                call const_assignment(result, const_mlt(a,b))
            case (NODE_DIV)
                a = eval_constexpr(input, t%subnodes%array(1), semmod)
                b = eval_constexpr(input, t%subnodes2%array(1), semmod)
                call const_assignment(result, const_div(a,b))
            case (NODE_EXP)
                a = eval_constexpr(input, t%subnodes%array(1), semmod)
                b = eval_constexpr(input, t%subnodes2%array(1), semmod)
                call const_assignment(result, const_exp(a,b))
            case (NODE_MEMBER)
                a = eval_constexpr(input, t%subnodes%array(1), semmod)
                if (input%nodes(t%subnodes2%array(1))%type /= NODE_STRING) then
                    call throw('second argument to member access must be string', t%fname, t%startlnum, t%startchar)
                end if
                associate (val => input%nodes(t%subnodes2%array(1))%value)
                    ! TODO: derived type components
                    select case (a%typeof%type)
                    case (TYPE_COMPLEX)
                        select type (c => a%value)
                        type is (complex)
                            select case (val)
                            case ('IM')
                                ! todo: arrays
                                result%typeof%type = TYPE_REAL
                                result%typeof%kind = a%typeof%kind
                                call poly_assign_real(result%value, c%im)
                            case ('RE')
                                ! todo: arrays
                                result%typeof%type = TYPE_REAL
                                result%typeof%kind = a%typeof%kind
                                result%value = c%re
                            case default
                                call throw('unknown component', t%fname, t%startlnum, t%startchar)
                            end select
                        end select
                    case default
                        call throw('member access not allowed for this type of value', t%fname, t%startlnum, t%startchar)
                    end select
                end associate
            case default
                call throw('unexpected identifier in constant expression', t%fname, t%startlnum, t%startchar)
            end select
        end associate
    end function

    function eval_constfunc(input, semmod, name, args) result(result)
        type(const) :: result
        type(ast), intent(in) :: input
        type(sem_module), intent(in) :: semmod
        character(64), intent(in) :: name
        type(iarr), intent(in) :: args

        type(const), allocatable :: evaledargs(:)
        type(type), parameter :: empty = type(TYPE_NONE,0,0,0)
        type(const), parameter :: none = const(empty)
        integer :: i

        if (args%size > 0) then
            ! might be some way to make this use an elemental function
            ! TODO: allow named arguments

            allocate(evaledargs(args%size - 1))

            do i=1,args%size - 1
                evaledargs(i) = eval_constexpr(input, args%array(i), semmod)
            end do

        end if
        if (args%size <= 0) then
            result = none
            return
        end if

        if (args%size - 1 == 1) then
            select case (name)
            case ('NINT')
                if (evaledargs(1)%typeof%type /= TYPE_REAL) then
                    result = none
                    return
                end if
                result%typeof%type = TYPE_INTEGER
                result%typeof%kind = 4
                select type (v => evaledargs(1)%value)
                type is (real)
                    call poly_assign_int(result%value, nint(v))
                end select
            case default
                result = none
            end select
        else if (args%size - 1 == 2) then
            select case (name)
            case ('CMPLX')
                if (evaledargs(1)%typeof%type /= TYPE_REAL .and. evaledargs(1)%typeof%type /= TYPE_INTEGER) then
                    result = none
                    return
                end if
                if (evaledargs(2)%typeof%type/=TYPE_REAL .and. evaledargs(2)%typeof%type /= TYPE_INTEGER) then
                    result = none
                    return
                end if
                result%typeof%type = TYPE_COMPLEX
                result%typeof%kind = 4
                block
                    complex :: tmp
                    select type (b => evaledargs(1)%value)
                    type is (real)
                        select type (a => evaledargs(2)%value)
                        type is (real)
                            tmp = cmplx(a, b)
                        type is (integer)
                            tmp = cmplx(a, b)
                        end select
                    type is (integer)
                        select type (a => evaledargs(2)%value)
                        type is (real)
                            tmp = cmplx(a, b)
                        type is (integer)
                            tmp = cmplx(a, b)
                        end select
                    end select
                    call poly_assign_cmplx(result%value, tmp)
                end block
            case default
                result = none
            end select
        else
            result = none
        end if
    end function

    !TODO: types
    function import_module(name, restrict) result(result)
        type(sem_module) :: result
        character(64), intent(in) :: name
        character(64), intent(in) :: restrict(:)
        
        integer :: unit
        character(65) :: tmp
        integer(SMALL) :: i

        open(newunit=unit, file=trim(name)//'.fmod', status='old')

        read(unit, '(A)') tmp
        if (tmp /= 'VARS') then
            call throw('error reading module '//trim(name), '', 0_SMALL, 0_SMALL)
        end if
        read(unit,'(A)') tmp
        do while (tmp /= 'FUNCS')
            if (tmp(:1) /= '-') call throw('error reading module '//trim(name), '', 0_SMALL, 0_SMALL)
            if (restrict(1) /= '') then
                if (.not.symbol_included(tmp(2:), restrict)) then
                    do i = 1, 8
                        read(unit, '(A)') tmp
                    end do
                    cycle
                end if
            end if

            ! lengthen variable array
            if (allocated(result%vartbl)) then
                block
                    type(sem_variable), allocatable :: tmpvar(:)
                    call move_alloc(result%vartbl, tmpvar)
                    allocate(result%vartbl(size(tmpvar) + 1))
                    result%vartbl(:size(tmpvar)) = tmpvar
                end block
            else
                allocate(result%vartbl(1))
            end if

            result%vartbl(size(result%vartbl))%name = trim(tmp(2:))
            read(unit, '(A)') tmp
            result%vartbl(size(result%vartbl))%srcmod = trim(tmp(2:))

            call read_var(unit, result%vartbl(size(result%vartbl)))

            read(unit,'(A)') tmp
        end do
        read(unit,'(A)') tmp
        do while (tmp /= 'END')
            if (tmp(:1) /= '-') call throw('error reading module '//trim(name), '', 0_SMALL, 0_SMALL)
            if (restrict(1) /= '') then
                if (.not.symbol_included(tmp(2:), restrict)) then
                    i = 1
                    read(unit, '(A)') tmp
                    do while (tmp(:1) /= '-' .and. tmp /= 'END')
                        read(unit, '(A)') tmp
                    end do
                    cycle
                end if
            end if

            if (allocated(result%functbl)) then
                block
                    type(sem_inter), allocatable :: tmpinter(:)
                    call move_alloc(result%functbl,tmpinter)
                    allocate(result%functbl(size(tmpinter) + 1))
                    result%functbl(:size(tmpinter)) = tmpinter
                end block
            else
                allocate(result%functbl(1))
            end if

            associate (intertmp => result%functbl(size(result%functbl)))
                intertmp%name = trim(tmp(2:))
                read(unit, '(A)') tmp
                do while (tmp(:1) /= '-' .and. tmp /= 'END')
                    if (allocated(intertmp%functions)) then
                        block
                            type(sem_proc), allocatable :: tmpproc(:)
                            call move_alloc(intertmp%functions, tmpproc)
                            allocate(intertmp%functions(size(tmpproc) + 1))
                            intertmp%functions(:size(tmpproc)) = tmpproc
                        end block
                    else
                        allocate(intertmp%functions(1))
                    end if
    
                    associate (functmp => intertmp%functions(size(intertmp%functions)))
                        functmp%name = trim(tmp(2:))
                        read(unit, '(A)') tmp

                        if (tmp == ' S') then
                            functmp%subrout = .true.
                        else
                            functmp%subrout = .false.
                        end if
                        
                        ! TODO: functions
    
                        read(unit, '(A)') tmp
                        do while (tmp(:1) /= '-' .and. tmp /= 'END')
                            if (allocated(functmp%arguments)) then
                                block
                                    type(sem_variable), allocatable :: tmpvar(:)
                                    call move_alloc(functmp%arguments, tmpvar)
                                    allocate(functmp%arguments(size(tmpvar) + 1))
                                    functmp%arguments(:size(tmpvar)) = tmpvar
                                end block
                            else
                                allocate(functmp%arguments(1))
                            end if

                            functmp%arguments(size(functmp%arguments))%name = trim(tmp(3:))
                            call read_var(unit, functmp%arguments(size(functmp%arguments)))

                            read(unit, '(A)') tmp
                        end do
                    end associate
                end do
            end associate
        end do

        close(unit)
    end function

    impure elemental subroutine sem_module_assignment(l, r)
        type(sem_module), intent(out) :: l
        type(sem_module), intent(in) :: r

        if (allocated(r%name)) l%name = r%name
        if (allocated(r%vartbl)) l%vartbl = r%vartbl
        if (allocated(r%functbl)) l%functbl = r%functbl
        if (allocated(r%typetbl)) l%typetbl = r%typetbl
    end subroutine

    function symbol_included(name, restrict) result(result)
        logical :: result
        character(64), intent(inout) :: name
        character(64), intent(in) :: restrict(1:)

        character(64) :: str
        integer :: i
        do i = 1, size(restrict)
            str = restrict(i)
            if (str(index(str, '-') + 1:) == name) then
                result = .true.
                if (index(str, '-') /= 0) then
                    name = str(:index(str, '-') - 1)
                end if
                return
            end if
        end do
        result = .false.
    end function

    subroutine read_var(unit, dest)
        integer, intent(in) :: unit
        type(sem_variable), intent(inout) :: dest

        character(65) :: tmp

        associate (typetmp => dest%vartype)
            read(unit, '(A)') tmp
            typetmp%type = atoi2(tmp)
            read(unit, '(A)') tmp
            typetmp%kind = atoi2(tmp)
            read(unit, '(A)') tmp
            typetmp%properties = atoi2(tmp)
            read(unit, '(A)') tmp
            typetmp%dimcount = atoi2(tmp)
            read(unit, '(A)') tmp
            ! TODO: arrays
            !typetmp%dims
        end associate
        read(unit, '(A)') tmp
        select case (tmp(2:2))
        case ('I')
            call poly_assign_int(dest%value, atoi(tmp(3:)))
        case ('R')
            call poly_assign_real(dest%value, ator(tmp(3:)))
        case ('C')
            call poly_assign_cmplx(dest%value, atoc(tmp(3:)))
        end select
    end subroutine
end module