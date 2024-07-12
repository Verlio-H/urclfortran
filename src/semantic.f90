module semantic
    use consts
    implicit none

    type, private :: sem_variable
        type(type) :: vartype
        character(len=:), allocatable :: name
        class(*), allocatable :: value ! only applies to parameters
        integer :: offset
    end type

    type, private :: sem_proc
        logical :: subrout
        character(len=:), allocatable :: name
        type(sem_variable) :: return
        type(sem_variable), allocatable :: arguments(:)
    end type

    type, private :: sem_inter
        character(len=:), allocatable :: name
        type(sem_proc), allocatable :: functions(:)
    end type

    ! TODO: add proper inheritance stuff
    type, private :: sem_type
        character(len=:), allocatable :: name
        type(type), allocatable :: components(:)
    end type

    type, private :: sem_module
        character(len=:), allocatable :: name
        type(sem_variable), allocatable :: vartbl(:) ! contains all public heap allocated variables
        type(sem_inter), allocatable :: functbl(:) ! contains all public interfaces
        type(sem_type), allocatable :: typetbl(:) ! contains all public types
        integer :: totaloffset ! total offset of all variables
    end type

    private :: writesemvar
    private :: combine
    private :: evaltype
    private :: eval_constexpr
    private :: eval_constfunc

contains
    ! given an index in the ast, generate a module file
    subroutine genmodfile(input,index)
        type(ast), intent(in) :: input
        integer :: index

        type(sem_module) :: result
        integer(SMALL) :: i
        type(type) :: implicit(26)
        integer :: curroffset
        integer :: unit

        ! default implicit typing rules
        implicit(1:8) = type(TYPE_REAL,4,0,0)
        implicit(9:15) = type(TYPE_INTEGER,4,0,0)
        implicit(16:26) = type(TYPE_REAL,4,0,0)

        ! global variable offset
        curroffset = 0

        select case (input%nodes(index)%type)
        case (NODE_MODULE)
            result = sem_module(totaloffset=curroffset)
            result%name = trim(input%nodes(index)%value)
            ! get semantic info
            i = 1
            do while (i<=input%nodes(index)%subnodes2%size-1)
                associate (subnode=>input%nodes(input%nodes(index)%subnodes2%array(i)))
                    select case (subnode%type)
            !  - import other modules
            !  - get implicit rules
                    case (NODE_IMPLICIT)
                        associate (typenode=>input%nodes(subnode%subnodes%array(1)))
                            if (typenode%type==NODE_TYPE) then
                                select type (a=>typenode%value2)
                                type is (integer(SMALL))
                                    if (a==TYPE_NONE) then
                                        implicit(:) = type(TYPE_NONE,0,0,0)
                                        i = i + 1_SMALL
                                        cycle
                                    end if
                                end select
                            end if
                            block
                                type(type) :: implicittype
                                integer :: j
                                implicittype = evaltype(input,subnode%subnodes%array(1),result)
                                ! parse range
                                do j=1,subnode%subnodes2%size-1
                                    associate (str=>input%nodes(subnode%subnodes2%array(j))%value)
                                        if (str(:1)<'A') then
                                            call throw('error in implicit statement', &
                                             subnode%fname,subnode%startlnum,subnode%startchar)
                                        end if
                                        if (str(2:2)=='-') then
                                            if (str(3:3)<'A') then
                                                call throw('error in implicit statement', &
                                                 subnode%fname,subnode%startlnum,subnode%startchar)
                                            end if
                                            implicit(iachar(str(:1))-iachar('@'):iachar(str(3:3))-iachar('@')) = implicittype
                                        else
                                            implicit(iachar(str(:1))-iachar('@')) = implicittype
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
            do while (i<=input%nodes(index)%subnodes%size-1)
            !  - iterate over all functions
                block
                    type(sem_proc) :: func
                    type(sem_inter) :: resultinter
                    integer :: j, varloc
                    character(len=64), allocatable :: names(:)
                    associate (tidx=>input%nodes(index)%subnodes%array(i))
                        associate (t=>input%nodes(tidx))
                            select case (t%type)
                            case (NODE_SUBROUTINE)
                                func%subrout = .true.
                                if (t%subnodes%size>0) then
                                    ! get name of arguments
                                    allocate(func%arguments(t%subnodes%size-1))
                                    allocate(names(t%subnodes%size-1))
                                    do j=1,t%subnodes%size-1
                                        associate (r=>input%nodes(t%subnodes%array(j)))
                                            select case (r%type)
                                            case (NODE_STRING)
                                                func%arguments(j)%name = trim(r%value)
                                                names(j) = r%value
                                            case default
                                                call throw('expected string for var name',r%fname,r%startlnum,r%startchar)
                                            end select
                                        end associate
                                    end do
                                    ! get set of variable types
                                    do j=1,t%subnodes2%size-1
                                        associate (ridx=>t%subnodes2%array(j))
                                            associate (r=>input%nodes(ridx))
                                                select case (r%type)
                                                case (NODE_TYPE)
                                                    varloc = findloc(names,r%value,dim=1)
                                                    if (varloc/=0) then
                                                        !update var info
                                                        func%arguments(varloc)%vartype = evaltype(input,ridx,result)
                                                    end if
                                                end select
                                            end associate
                                        end associate
                                    end do
                                    ! fill in implicit types
                                    do j=1,size(func%arguments)
                                        associate (r=>func%arguments(j))
                                            if (r%vartype%type==TYPE_NONE) then
                                                r%vartype = implicit(iachar(r%name(:1))-iachar('@'))
                                                if (r%vartype%type==TYPE_NONE) then
                                                    call throw('variable '''//r%name//''' has no implicit type',&
                                                     t%fname,t%startlnum,t%startchar)
                                                end if
                                            end if
                                        end associate
                                    end do
                                end if
                                ! add to module
                                resultinter%name = t%value
                                resultinter%functions = [func]
                                if (allocated(result%functbl)) then
                                    block
                                        type(sem_inter), allocatable :: tmp(:)
                                        call move_alloc(result%functbl,tmp)
                                        allocate(result%functbl(size(tmp)+1))
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
            
            ! iterate over all variables
            do while (i<=input%nodes(index)%subnodes2%size-1)
                associate (tidx=>input%nodes(index)%subnodes2%array(i))
                    associate (t=>input%nodes(tidx))
                        select case (t%type)
                        case (NODE_TYPE)
                            select type (a=>t%value2)
                            type is (integer(SMALL))
                                block
                                    type(type) :: vartype
                                    type(sem_variable) :: variable
                                    type(const) :: evalresult
                                    vartype = evaltype(input,tidx,result)
                                    variable%vartype = vartype
                                    variable%name = t%value
                                    variable%offset = curroffset
                                    curroffset = curroffset + vartype%kind/2
                                    if (t%subnodes%size-1>=2) then
                                        evalresult = eval_constexpr(input,t%subnodes%array(2),result)
                                        if (allocated(variable%value)) deallocate(variable%value)
                                        allocate(variable%value,source=evalresult%value)
                                    end if
                                    if (allocated(result%vartbl)) then
                                        block
                                            type(sem_variable), allocatable :: tmp(:)
                                            call move_alloc(result%vartbl,tmp)
                                            allocate(result%vartbl(size(tmp)+1))
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
            open(newunit=unit,file=result%name//'.fmod')
            ! output all variable names
            write(unit,'(A)') 'VARS'
            if (allocated(result%vartbl)) then
                do i=1,int(size(result%vartbl),SMALL) ! possibly do something about the cast here
                    associate (v=>result%vartbl(i))
                        if (iand(v%vartype%properties,int(PROP_PRIVATE,SMALL))/=0) cycle
                        write(unit,'(A)') '-'//trim(v%name)
                        call writesemvar(unit,v)
                    end associate
                end do
            end if
            write(unit,'(A)') 'FUNCS'
            if (allocated(result%functbl)) then

                do i=1,int(size(result%functbl),SMALL) ! possibly do something about the cast here
                    associate (v=>result%functbl(i))
                        write(unit,'(A)') '-'//trim(v%name)
                        block
                            integer :: j, k
                            do j=1,size(v%functions)
                                associate(r=>v%functions(j))
                                    if (r%subrout) then
                                        write(unit,'(A)') ' S'
                                    else
                                        write(unit,'(A)') ' F'
                                        write(unit,'(A)') ' ='//r%return%name
                                        call writesemvar(unit,r%return)
                                    end if
                                    do k=1,size(r%arguments)
                                        write(unit,'(A)') ' -'//trim(r%arguments(k)%name)
                                        call writesemvar(unit,r%arguments(k))
                                    end do
                                end associate
                            end do
                        end block
                    end associate
                end do
            else

            end if
            close(unit)
        case (NODE_PROGRAM)
        case default
        call throw('expected module or program',input%nodes(index)%fname,input%nodes(index)%startlnum,input%nodes(index)%startchar)
        end select
    end subroutine

    subroutine writesemvar(unit,v)
        integer, value, intent(in) :: unit
        type(sem_variable), intent(in) :: v
        associate (t=>v%vartype)
            write(unit,'(A)') ' '//itoa2(t%type)
            write(unit,'(A)') ' '//itoa2(t%kind)
            write(unit,'(A)') ' '//itoa2(t%properties)
            write(unit,'(A)') ' '//itoa2(t%dimcount)
            write(unit,'(A)',advance='no')
            if (allocated(t%dims)) then
                write(unit,'(I6)') t%dims
            else
                write(unit,'(A)') ' NONE'
            end if
        end associate
        if (allocated(v%value)) then
            select type (vv=>v%value)
            type is (integer)
                write(unit,'(A)') ' I'//itoa(vv)
            type is (real)
                write(unit,'(A)') ' R'//rtoa(vv)
            type is (complex)
                write(unit,'(A)') ' C'//rtoa(vv%re)//' '//rtoa(vv%im)
            end select
        else
            write(unit,'(A)') ' N'
        end if
        write(unit,'(A)') ' '//itoa(v%offset)
    end subroutine

    type(sem_module) function combine(a,b) result(result)
        type(sem_module), intent(in) :: a, b


        result%name = a%name
        result%vartbl = [a%vartbl, b%vartbl]
        result%functbl = [a%functbl, b%functbl]
        result%typetbl = [a%typetbl, b%typetbl]

        result%totaloffset = b%totaloffset

    end function

    type(type) function evaltype(input,index,semmod) result(result)
        type(ast), intent(in) :: input
        integer, intent(in) :: index
        type(sem_module), intent(in) :: semmod
        type(const) :: kind
        associate (t=>input%nodes(index))
            result%properties = int(t%subnodes%array(1),SMALL)
            ! TODO: arrays
            ! evaluate kind
            kind = eval_constexpr(input,t%subnodes2%array(1),semmod)
            if (kind%typeof%type/=TYPE_INTEGER) then
                call throw('expected integer as kind value',t%fname,t%startlnum,t%startchar)
            end if
            select type (a=>kind%value)
            type is (integer)
                result%kind = int(a,SMALL)
            end select
            select type (a=>t%value2)
            type is (integer(SMALL))
                result%type = a
            end select
        end associate
    end function

    recursive type(const) function eval_constexpr(input,i,semmod) result(result)
        type(ast), intent(in) :: input
        integer, intent(in) :: i
        type(sem_module), intent(in) :: semmod
        type(const) :: a, b
        associate (t=>input%nodes(i))
            select case (t%type)
            case (NODE_INT_VAL)
                block
                    integer(SMALL) :: iunder
                    integer :: value ! make this a large int later on
                    result%typeof%type = TYPE_INTEGER
                    result%typeof%kind = 4
                    iunder = int(index(t%value,'_'),2)
                    if (iunder/=0) then
                        read(t%value(:iunder-1),*) value
                        if (t%value(iunder+1:iunder+1)>='0'.and.t%value(iunder+1:iunder+1)<='9') then
                            read(t%value(iunder+1:),*) result%typeof%kind
                        else
                            call throw('kinds from identifiers are currently unsupported',t%fname,t%startlnum,t%startchar)
                        end if
                    else
                        read(t%value,*) value
                    end if
                    result%value = value ! can't be read into directly prior to fortran 2023
                end block
            case (NODE_REAL_VAL)
                block
                    integer(SMALL) :: iunder
                    real :: value ! make this a large real later on
                    result%typeof%type = TYPE_REAL
                    result%typeof%kind = 4
                    iunder = int(index(t%value,'_'),SMALL)
                    if (iunder/=0) then
                        read(t%value(:iunder-1),*) value
                        if (t%value(iunder+1:iunder+1)>='0'.and.t%value(iunder+1:iunder+1)<='9') then
                            read(t%value(iunder+1:),*) result%typeof%kind
                        else
                            call throw('kinds from identifiers are currently unsupported',t%fname,t%startlnum,t%startchar)
                        end if
                    else
                        read(t%value,*) value
                    end if
                    result%value = value ! can't be read into directly prior to fortran 2023
                end block
            case (NODE_STRING)
                block
                    ! find variable
                    integer :: j
                    if (allocated(semmod%vartbl)) then
                        do j=1,size(semmod%vartbl)
                            if (semmod%vartbl(j)%name==t%value) exit
                        end do
                        if (j>size(semmod%vartbl)) then
                            if (allocated(t%fname)) then
                                call throw('variable name not found',t%fname,t%startlnum,t%startchar)
                            else
                                call throw('variable name not found: '//t%value,'unknown',t%startlnum,t%startchar)
                            end if
                        end if
                    else
                        call throw('variable name not found',t%fname,t%startlnum,t%startchar) 
                    end if
                    if (iand(semmod%vartbl(j)%vartype%properties,int(PROP_PARAMETER,SMALL))==0) then
                        call throw('variables in constant expressions must be parameters',t%fname,t%startlnum,t%startchar)
                    end if
                    result%typeof = semmod%vartbl(j)%vartype
                    if (allocated(result%value)) deallocate(result%value)

                    allocate(result%value,source=semmod%vartbl(j)%value)
                end block
            case (NODE_FNC_ARR)
                ! todo: arrays
                result = eval_constfunc(input,semmod,t%value,t%subnodes)
            case (NODE_ADD)
                a = eval_constexpr(input,t%subnodes%array(1),semmod)
                b = eval_constexpr(input,t%subnodes2%array(1),semmod)
                call const_assignment(result,const_add(a,b))
            case (NODE_SUB)
                a = eval_constexpr(input,t%subnodes%array(1),semmod)
                b = eval_constexpr(input,t%subnodes2%array(1),semmod)
                result = const_sub(a,b)
            case (NODE_MLT)
                a = eval_constexpr(input,t%subnodes%array(1),semmod)
                b = eval_constexpr(input,t%subnodes2%array(1),semmod)
                result = const_mlt(a,b)
            case (NODE_DIV)
                a = eval_constexpr(input,t%subnodes%array(1),semmod)
                b = eval_constexpr(input,t%subnodes2%array(1),semmod)
                result = const_div(a,b)
            case (NODE_EXP)
                a = eval_constexpr(input,t%subnodes%array(1),semmod)
                b = eval_constexpr(input,t%subnodes2%array(1),semmod)
                result = const_exp(a,b)
            case (NODE_MEMBER)
                a = eval_constexpr(input,t%subnodes%array(1),semmod)
                if (input%nodes(t%subnodes2%array(1))%type/=NODE_STRING) then
                    call throw('second argument to member access must be string',t%fname,t%startlnum,t%startchar)
                end if
                associate (val=>input%nodes(t%subnodes2%array(1))%value)
                    ! todo: derived type components
                    select case (a%typeof%type)
                    case (TYPE_COMPLEX)
                        select type (c=>a%value)
                        type is (complex)
                            select case (val)
                            case ('IM')
                                ! todo: arrays
                                result%typeof%type = TYPE_REAL
                                result%typeof%kind = a%typeof%kind
                                call poly_assign_real(result%value,c%im)
                            case ('RE')
                                ! todo: arrays
                                result%typeof%type = TYPE_REAL
                                result%typeof%kind = a%typeof%kind
                                result%value = c%re
                            case default
                                call throw('unknown component',t%fname,t%startlnum,t%startchar)
                            end select
                        end select
                    case default
                        call throw('member access not allowed for this type of value',t%fname,t%startlnum,t%startchar)
                    end select
                end associate
            case default
                call throw('unexpected identifier in constant expression',t%fname,t%startlnum,t%startchar)
            end select
        end associate
    end function

    type(const) function eval_constfunc(input,semmod,name,args) result(result)
        type(ast), intent(in) :: input
        type(sem_module), intent(in) :: semmod
        character(len=64), intent(in) :: name
        type(iarr), intent(in) :: args

        type(const), allocatable :: evaledargs(:)
        type(type), parameter :: empty = type(TYPE_NONE,0,0,0)
        type(const), parameter :: none = const(empty)
        integer :: i

        if (args%size>0) then
            ! might be some way to make this use an elemental function
            ! TODO: allow named arguments

            allocate(evaledargs(args%size-1))

            do i=1,args%size-1
                evaledargs(i) = eval_constexpr(input,args%array(i),semmod)
            end do

        end if
        if (args%size<=0) then
            result = none
            return
        end if

        if (args%size-1==1) then
            select case (name)
            case ('NINT')
                if (evaledargs(1)%typeof%type/=TYPE_REAL) then
                    result = none
                    return
                end if
                result%typeof%type = TYPE_INTEGER
                result%typeof%kind = 4
                select type (v=>evaledargs(1)%value)
                type is (real)
                    call poly_assign_int(result%value,nint(v))
                end select
            case default
                result = none
            end select
        else if (args%size-1==2) then
            select case (name)
            case ('CMPLX')
                if (evaledargs(1)%typeof%type/=TYPE_REAL.and.evaledargs(1)%typeof%type/=TYPE_INTEGER) then
                    result = none
                    return
                end if
                if (evaledargs(2)%typeof%type/=TYPE_REAL.and.evaledargs(2)%typeof%type/=TYPE_INTEGER) then
                    result = none
                    return
                end if
                result%typeof%type = TYPE_COMPLEX
                result%typeof%kind = 4
                block
                    complex :: tmp
                    select type (b=>evaledargs(1)%value)
                    type is (real)
                        select type (a=>evaledargs(2)%value)
                        type is (real)
                            tmp = cmplx(a,b)
                        type is (integer)
                            tmp = cmplx(a,b)
                        end select
                    type is (integer)
                        select type (a=>evaledargs(2)%value)
                        type is (real)
                            tmp = cmplx(a,b)
                        type is (integer)
                            tmp = cmplx(a,b)
                        end select
                    end select
                    call poly_assign_cmplx(result%value,tmp)
                end block
            case default
                result = none
            end select
        else
            result = none
        end if

        
    end function
end module