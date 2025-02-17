module backend_urcl16
    use backend_common
    implicit none

    type linked_string
        character(len=:), allocatable :: value
        type(linked_string), pointer :: next
    end type

    integer :: tempnum
contains
    function gen_asm(irinput,maxvar,varsizes) result(result)
        character(len=:), allocatable :: result
        type(ir_ptr), intent(inout) :: irinput(:)
        integer, intent(inout) :: maxvar
        type(siarr), intent(inout) :: varsizes
 
        integer :: i
        integer, allocatable :: varcounts(:)
        type(iarr), allocatable :: varconnections(:)
        logical, allocatable :: livevars(:)
        type(linked_string), pointer :: tempresult, prevstrptr
        integer(SMALL), allocatable :: varlocs(:)
        integer :: strsize

        tempnum = 0



        print*,'EEEEEEEEEEEEEEEEEEEEEEEEEEE'
        do i=1,size(irinput)
            call resolve_offsets(irinput(i)%ptr, ptr=1, int8=1, int16=1, int32=2, int64=4, int128=8, float=1, double=2)
        end do

        do i=1,size(irinput)
            call lower16(irinput(i)%ptr,maxvar,varsizes)
            call ir_print(irinput(i)%ptr)
        end do

        allocate(varcounts(maxvar))
        allocate(varconnections(maxvar))
        allocate(livevars(maxvar))

        do i=1,size(varcounts)
            varcounts(i) = 0
            livevars(i) = .false.
        end do
        ! build live vars graph
        do i=1,size(irinput)
            call countrefs(irinput(i)%ptr,varcounts)
        end do

        do i=1,size(irinput)
            call updatelivevars(irinput(i)%ptr,varcounts,livevars,varconnections)
        end do
        ! call print_livevars(varconnections)

        call allocr(varlocs, varconnections, maxvar)
        ! do i=1,size(varlocs)
        !     write(*,'(A)',advance='no') itoa2(varlocs(i))//' '
        ! end do

        nullify(tempresult)
        allocate(tempresult)
        tempresult%value = ''
        nullify(tempresult%next)

        do i=1,size(irinput)
            call internal_gen_asm(tempresult,irinput(i)%ptr,varsizes,varlocs)
        end do

        strsize = 0
        prevstrptr => tempresult
        do while (associated(tempresult))
            if (allocated(tempresult%value)) then
                strsize = strsize + len(tempresult%value)
            end if
            tempresult => tempresult%next
        end do
        tempresult => prevstrptr

        allocate(character(len=strsize) :: result)

        strsize = 1
        prevstrptr => tempresult
        do while (associated(tempresult))
            if (allocated(tempresult%value)) then
                result(strsize:strsize+len(tempresult%value)-1) = tempresult%value
                strsize = strsize + len(tempresult%value)
            end if
            tempresult => tempresult%next
        end do
        tempresult => prevstrptr

        ! free string memory
        prevstrptr => tempresult
        do while (associated(prevstrptr))
            tempresult => tempresult%next
            deallocate(prevstrptr)
            prevstrptr => tempresult
        end do
    end function

    pure function calculate_arg(op,varlocs) result(result)
        character(:), allocatable :: result
        type(operand), intent(in) :: op
        integer(SMALL), intent(in) :: varlocs(:)
        
        if (op%type == V_BP) then
            result = 'R15'
        else if (op%type == V_SP) then
            result = 'SP'
        else if (op%type == V_IMM) then
            select type (val => op%value)
            type is (integer)
                result = itoa(val)
            end select
        else if (op%type == V_VAR) then
            select type (val => op%value)
            type is (integer)
                result = 'R'//itoa2(varlocs(val))
            end select
        else if (op%type == V_SYMB) then
            select type (var => op%value)
            type is (character(*))
                result = '!'//var
            end select
        else
            result = ''
        end if
    end function

    recursive subroutine internal_gen_asm(result,irinput,varsizes,varlocs)
        type(linked_string), pointer, intent(inout) :: result
        type(ir), intent(in) :: irinput
        type(siarr), intent(in) :: varsizes
        integer(SMALL), intent(in) :: varlocs(:)

        type(linked_string), pointer :: current_strpointer
        type(ir_instruction), pointer :: current_instruction
        integer :: i, j
        character(:), allocatable :: inst
        character(:), allocatable :: arg1, arg2, arg3

        current_strpointer => result
        if (associated(current_strpointer)) then
            do while (associated(current_strpointer%next))
                current_strpointer => current_strpointer%next
            end do
        end if

        ! insert prologs
        select case (irinput%block_type)
        case (BLOCK_PROGRAM)
            current_strpointer%value = '!_main'//achar(10)//'._main'//achar(10)//'MOV R15 SP'//achar(10)
        case (BLOCK_SUBROUTINE)
            current_strpointer%value = '!s_'//irinput%name//achar(10)//'.s_'//irinput%name//achar(10)//&
                'PSH R15'//achar(10)//'MOV R15 SP'//achar(10)
        case (BLOCK_ROOT)
            ! insert globals
            if (allocated(irinput%module%vartbl)) then
                do i=1, size(irinput%module%vartbl)
                    associate (var => irinput%module%vartbl(i))
                        if (var%srcmod /= irinput%module%name) cycle
                        if (iand(var%vartype%properties, int(PROP_PARAMETER, SMALL)) /= 0) cycle
                        allocate(current_strpointer%next)
                        current_strpointer => current_strpointer%next
                        current_strpointer%value = '!g_'//var%srcmod//'_'//var%name//achar(10)// &
                            '.g_'//var%srcmod//'_'//var%name//achar(10)//'DW 0'//achar(10)
                        if (var%vartype%type == TYPE_INTEGER .and. var%vartype%kind == 4) then
                            current_strpointer%value = current_strpointer%value//'DW 0'//achar(10)
                        end if
                        current_strpointer%value = current_strpointer%value//achar(10)
                        nullify(current_strpointer%next)
                    end associate
                end do
            end if
        end select

        select case (irinput%block_type)
        case (BLOCK_PROGRAM, BLOCK_SUBROUTINE)
            block
                integer :: min
                min = 0
                do i=1, size(irinput%variables)
                    if (irinput%variables(i)%var%offset < min) min = irinput%variables(i)%var%offset
                end do
                if (min /= 0) current_strpointer%value = current_strpointer%value//'ADD SP SP '//itoa(min)//achar(10)
            end block
        end select

        current_instruction => irinput%instruction
        do while (associated(current_instruction))
            if (current_instruction%instruction == OP_NOP) then
                current_instruction => current_instruction%next
                cycle
            end if
            if (associated(current_strpointer)) then
                allocate(current_strpointer%next)
                current_strpointer => current_strpointer%next
            else
                allocate(result)
                current_strpointer => result
            end if
            ! todo: handle register spilling
            select case (current_instruction%instruction)
            case (OP_MOV)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                select case (current_instruction%operands(2)%type)
                case (V_IMM)
                    select type (val => current_instruction%operands(2)%value)
                    type is (integer)
                        current_strpointer%value = 'IMM '//arg1//' '//itoa(val)//achar(10)
                    end select
                case default
                    arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                    current_strpointer%value = 'MOV '//arg1//' '//arg2//achar(10)
                end select
            case (OP_ADD, OP_SUB, OP_SETL)
                select case (current_instruction%instruction)
                case (OP_SUB)
                    inst = 'SUB '
                case (OP_ADD)
                    inst = 'ADD '
                case (OP_SETL)
                    inst = 'SETL '
                end select
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                arg3 = calculate_arg(current_instruction%operands(3), varlocs)
                current_strpointer%value = inst//arg1//' '//arg2//' '//arg3//achar(10)
            case (OP_LOD)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                current_strpointer%value = 'LOD '//arg1//' '//arg2//achar(10)
            case (OP_STR)
                arg1 = calculate_arg(current_instruction%operands(2), varlocs)
                arg2 = calculate_arg(current_instruction%operands(3), varlocs)
                current_strpointer%value = 'STR '//arg1//' '//arg2//achar(10)
            case (OP_PSH)
                arg1 = calculate_arg(current_instruction%operands(2), varlocs)
                current_strpointer%value = 'PSH '//arg1//achar(10)
            case (OP_ADRLV)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                select type (var => current_instruction%operands(2)%value)
                type is (integer)
                    current_strpointer%value = 'ADD '//arg1//' R15 '//itoa(irinput%variables(var)%var%offset)//achar(10)
                end select
            case (OP_ADRGV)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                select type (varidx => current_instruction%operands(2)%value)
                type is (integer)
                    associate (var => irinput%module%vartbl(varidx))
                        current_strpointer%value = 'IMM '//arg1//' !g_'//var%srcmod//'_'//var%name//achar(10)
                    end associate
                end select
            case (OP_LODLV)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                i = 0
                if (size(current_instruction%operands) >= 3) then
                    select type (var3 => current_instruction%operands(3)%value)
                    type is (integer)
                        i = var3
                    end select
                end if
                select type (var => current_instruction%operands(2)%value)
                type is (integer)
                    current_strpointer%value = 'LLOD '//arg1//' R15 '//itoa(irinput%variables(var)%var%offset + i)//achar(10)
                end select
            case (OP_LODGV)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                i = 0
                if (size(current_instruction%operands) >= 3) then
                    select type (var3 => current_instruction%operands(3)%value)
                    type is (integer)
                        i = var3
                    end select
                end if
                select type (varidx => current_instruction%operands(2)%value)
                type is (integer)
                    associate (var => irinput%module%vartbl(varidx))
                        if (i == 0) then
                            current_strpointer%value = 'LOD '//arg1//' !g_'//var%srcmod//'_'//var%name//achar(10)
                        else
                            current_strpointer%value = 'LLOD '//arg1//' !g_'//var%srcmod//'_'//var%name//' '//itoa(i)//achar(10)
                        end if
                    end associate
                end select
            case (OP_STRLV)
                arg1 = calculate_arg(current_instruction%operands(3), varlocs)
                select type (var => current_instruction%operands(2)%value)
                type is (integer)
                    current_strpointer%value = 'LSTR R15 '//itoa(irinput%variables(var)%var%offset)//' '//arg1//achar(10)
                end select
            case (OP_STRGV)
                arg1 = calculate_arg(current_instruction%operands(3), varlocs)
                select type (varidx => current_instruction%operands(2)%value)
                type is (integer)
                    associate (var => irinput%module%vartbl(varidx))
                        current_strpointer%value = 'STR !g_'//var%srcmod//'_'//var%name//' '//arg1//achar(10)
                    end associate
                end select
            case (OP_CALL)
                j = 0
                do i=size(current_instruction%operands),3,-1
                    arg1 = calculate_arg(current_instruction%operands(i), varlocs)
                    current_strpointer%value = 'PSH '//arg1//achar(10)
                    j = j + 1
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                end do
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                current_strpointer%value = 'CAL '//arg2//achar(10)
                if (arg1 /= '') then
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                    current_strpointer%value = 'POP '//arg1//achar(10)
                end if
                allocate(current_strpointer%next)
                current_strpointer => current_strpointer%next
                current_strpointer%value = 'ADD SP SP '//itoa(j)//achar(10)
            end select
            nullify(current_strpointer%next)
            current_instruction => current_instruction%next
        end do

        ! insert epilogs
        select case (irinput%block_type)
        case (BLOCK_PROGRAM, BLOCK_SUBROUTINE, BLOCK_ROOT)
            allocate(current_strpointer%next)
            current_strpointer => current_strpointer%next
            nullify(current_strpointer%next)
        end select

        select case (irinput%block_type)
        case (BLOCK_PROGRAM)
            current_strpointer%value = 'HLT'//achar(10)//achar(10)
            allocate(current_strpointer%next)
            current_strpointer => current_strpointer%next
            nullify(current_strpointer%next)
        case (BLOCK_SUBROUTINE)
            current_strpointer%value = 'MOV SP R15'//achar(10)//'POP R15'//achar(10)//'RET'//achar(10)//achar(10)
            allocate(current_strpointer%next)
            current_strpointer => current_strpointer%next
            nullify(current_strpointer%next)
        end select

        if (allocated(irinput%functions)) then
            do i=size(irinput%functions),1,-1
                if (associated(current_strpointer)) then
                    call internal_gen_asm(current_strpointer,irinput%functions(i)%ptr,varsizes,varlocs)
                else
                    call throw('internal error: unassociated current_strpointer','',0_SMALL,0_SMALL,.true.)
                    !call internal_gen_asm(current_strpointer,irinput%functions(i)%ptr,varsizes,varlocs)
                end if
            end do
        end if
        
    end subroutine

    pure subroutine allocr(result,graph,maxvar)
        integer(SMALL), allocatable, intent(out) :: result(:)
        type(iarr), allocatable, intent(in) :: graph(:)
        integer, intent(in) :: maxvar

        ! r15 - bp
        ! r0 - 0 reg

        integer, allocatable :: stack(:)
        type(iarr), allocatable :: tmpgraph(:)
        integer :: i, j, k, sp, count, remainingvar, maxcount, maxcountvar
        integer, parameter :: n = 14
        integer :: currval
        integer(SMALL) :: currentreg
        integer, allocatable :: counts(:)

        tmpgraph = graph ! pray to the gfortran overlords that this works

        allocate(stack(maxvar))
        allocate(result(maxvar))
        allocate(counts(maxvar))
        sp = 0

        remainingvar = maxvar

        do while (remainingvar /= 0)
            maxcount = 0
            do i=1,size(tmpgraph)
                ! count neighbors
                count = 0
                do j=1,tmpgraph(i)%size-1
                    if (tmpgraph(i)%array(j) /= -1) count = count + 1
                end do
                counts(i) = count
            end do
            do i=1,size(tmpgraph)
                count = counts(i)
                ! remove if less than n neighbors
                if (count < n) then
                    remainingvar = remainingvar - 1
                    sp = sp + 1
                    stack(sp) = i
                    tmpgraph(i) = iarr()
                    do j=1,size(tmpgraph)
                        if (j /= i) then
                            do k=1,tmpgraph(j)%size-1
                                if (tmpgraph(j)%array(k) == i) tmpgraph(j)%array(k) = -1
                            end do
                        end if
                    end do
                else if (count > maxcount) then
                    maxcount = count
                    maxcountvar = i
                end if
            end do
            if (remainingvar /= 0) then
                ! remove max count
                remainingvar = remainingvar - 1
                sp = sp + 1
                stack(sp) = maxcountvar
                tmpgraph(maxcountvar) = iarr()
                do i=1,size(tmpgraph)
                    if (i /= maxcountvar) then
                        do j=1,tmpgraph(i)%size-1
                            if (tmpgraph(i)%array(j) == maxcountvar) tmpgraph(i)%array(j) = -1
                        end do
                    end if
                end do
            end if
        end do

        do i=1,size(result)
            result(i) = -1
        end do

        do while (sp > 0)
            currval = stack(sp)
            sp = sp - 1
            currentreg = 1
            outer: do
                do i=1,graph(currval)%size-1
                    associate(reg => result(graph(currval)%array(i)))
                        if (reg == currentreg) then
                            currentreg = currentreg + 1_SMALL
                            cycle outer
                        end if
                    end associate
                end do
                exit
            end do outer
            result(currval) = currentreg
        end do
    end subroutine


end module