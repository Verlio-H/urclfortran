module backend_urcl16
    use include, only: SMALL, throw, siarr, iarr, itoa, itoa2, rtoa
    use astgen, only: TYPE_INTEGER, PROP_PARAMETER
    use irgen, only: ir, ir_instruction, ir_ptr, operand, V_BP, V_SP, V_IMM, V_VAR, V_SYMB, BLOCK_PROGRAM, BLOCK_ROOT, &
                    BLOCK_SUBROUTINE, OP_NOP, OP_ADD, OP_SUB, OP_MLT, OP_UMLT, OP_ADRLV, OP_ADRGV, OP_LOD, OP_LODGV, OP_LODLV, &
                    OP_STR, OP_STRLV, OP_STRGV, OP_MOV, OP_SETL, OP_SSETL, OP_PSH, OP_CALL, OP_DIV, OP_CAST, OP_EQ, OP_NE, OP_LT, &
                    OP_LE, OP_GT, OP_GE, OP_NOT, OP_AND, OP_OR, OP_XOR, OP_BR, OP_RET, ir_print, BLOCK_CONTINUE, BLOCK_IF, &
                    BLOCK_ELSE, BLOCK_DO
    use backend_common, only: resolve_offsets, lower16, countrefs, updatelivevars, print_livevars
    implicit none

    type linked_string
        character(:), allocatable :: value
        type(linked_string), pointer :: next
    end type


    integer, parameter :: regn = 14
contains
    function gen_asm(irinput, maxvar, varsizes) result(result)
        character(:), allocatable :: result
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

        do i = 1, size(irinput)
            call resolve_offsets(irinput(i)%ptr, ptr=1, int8=1, int16=1, int32=2, int64=4, int128=8, float=1, double=2)
        end do

        do i = 1, size(irinput)
            call lower16(irinput(i)%ptr, maxvar, varsizes)
            ! call ir_print(irinput(i)%ptr)
        end do

        allocate(varcounts(maxvar))
        allocate(varconnections(maxvar))
        allocate(livevars(maxvar))

        do i = 1, size(varcounts)
            varcounts(i) = 0
            livevars(i) = .false.
        end do

        ! build live vars graph
        do i = 1, size(irinput)
            call countrefs(irinput(i)%ptr, varcounts)
        end do

        do i = 1, size(irinput)
            call updatelivevars(irinput(i)%ptr, varcounts, livevars, varconnections)
        end do
        ! call print_livevars(varconnections)

        call allocr(varlocs, varconnections, maxvar)
        ! do i=1,size(varlocs)
        !    write(*,'(A)',advance='no') itoa2(varlocs(i))//' '
        ! end do

        nullify(tempresult)
        allocate(tempresult)
        tempresult%value = ''
        nullify(tempresult%next)

        do i = 1, size(irinput)
            call internal_gen_asm(tempresult,irinput(i)%ptr, varsizes, varlocs)
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

        allocate(character(strsize) :: result)

        strsize = 1
        prevstrptr => tempresult
        do while (associated(tempresult))
            if (allocated(tempresult%value)) then
                result(strsize:strsize + len(tempresult%value) - 1) = tempresult%value
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

    pure function calculate_arg(op, varlocs) result(result)
        character(:), allocatable :: result
        type(operand), intent(in) :: op
        integer(SMALL), intent(in) :: varlocs(:)
        
        if (op%type == V_BP) then
            result = 'R1'
        else if (op%type == V_SP) then
            result = 'SP'
        else if (op%type == V_IMM) then
            select type (val => op%value)
            type is (integer)
                result = itoa(val)
            type is (real)
                result = rtoa(val)
            type is (logical)
                if (val) then
                    result = '-1'
                else
                    result = '0'
                end if
            end select
        else if (op%type == V_VAR) then
            select type (val => op%value)
            type is (integer)
                result = 'R'//itoa2(varlocs(val) + 1_SMALL)
            end select
        else if (op%type == V_SYMB) then
            select type (var => op%value)
            type is (character(*))
                result = '!'//trim(var)
            end select
        else
            result = ''
        end if
    end function

    recursive subroutine internal_gen_asm(result, irinput, varsizes, varlocs)
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

        select case (irinput%block_type)
        case (BLOCK_ROOT, BLOCK_PROGRAM)
            ! insert globals
            if (allocated(irinput%module%vartbl)) then
                do i = 1, size(irinput%module%vartbl)
                    associate (var => irinput%module%vartbl(i))
                        if (var%srcmod /= irinput%module%name) cycle
                        if (iand(var%vartype%properties, int(PROP_PARAMETER, SMALL)) /= 0) cycle
                        allocate(current_strpointer%next)
                        current_strpointer => current_strpointer%next
                        current_strpointer%value = '!g_'//var%srcmod//'_'//trim(var%name)//achar(10)//&
                                                    '.g_'//var%srcmod//'_'//trim(var%name)//achar(10)//'DW 0'//achar(10)
                        if (var%vartype%type == TYPE_INTEGER .and. var%vartype%kind == 4) then
                            current_strpointer%value = current_strpointer%value//'DW 0'//achar(10)
                        end if
                        current_strpointer%value = current_strpointer%value//achar(10)
                        nullify(current_strpointer%next)
                    end associate
                end do
            end if
        end select
        ! insert prologs
        select case (irinput%block_type)
        case (BLOCK_PROGRAM)
            allocate(current_strpointer%next)
            current_strpointer => current_strpointer%next
            current_strpointer%value = '!_main'//achar(10)//'._main'//achar(10)//'MOV R1 SP'//achar(10)
            nullify(current_strpointer%next)
        case (BLOCK_SUBROUTINE)
            current_strpointer%value = '!s_'//irinput%name//achar(10)//'.s_'//irinput%name//achar(10)//&
                                        'PSH R1'//achar(10)//'MOV R1 SP'//achar(10)
        case (BLOCK_IF, BLOCK_ELSE, BLOCK_CONTINUE, BLOCK_DO)
            current_strpointer%value = '.'//irinput%name//achar(10)
        end select

        select case (irinput%block_type)
        case (BLOCK_PROGRAM, BLOCK_SUBROUTINE)
            block
                integer :: min
                min = 0
                if (allocated(irinput%variables)) then
                    do i = 1, size(irinput%variables)
                        if (irinput%variables(i)%var%offset < min) min = irinput%variables(i)%var%offset
                    end do
                end if
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
            case (OP_MOV, OP_CAST)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                select case (current_instruction%operands(2)%type)
                case (V_IMM)
                    current_strpointer%value = 'IMM '//arg1//' '//arg2//achar(10)
                case default
                    if (current_instruction%operands(1)%kind /= current_instruction%operands(2)%kind) then
                        if (current_instruction%operands(1)%kind == 24) then
                            current_strpointer%value = 'ITOF '//arg1//' '//arg2//achar(10)
                        else
                            current_strpointer%value = 'FTOI '//arg1//' '//arg2//achar(10)
                        end if
                    else
                        if (arg1 /= arg2) current_strpointer%value = 'MOV '//arg1//' '//arg2//achar(10)
                    end if
                end select
            case (OP_ADD, OP_SUB, OP_SETL, OP_SSETL, OP_MLT, OP_UMLT, OP_DIV, OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE, OP_AND, &
                    OP_OR, OP_XOR)
                select case (current_instruction%instruction)
                case (OP_ADD)
                    inst = 'ADD '
                case (OP_SUB)
                    inst = 'SUB '
                case (OP_SETL)
                    inst = 'SETL '
                case (OP_SSETL)
                    inst = 'SSETL '
                case (OP_MLT)
                    inst = 'MLT '
                case (OP_UMLT)
                    inst = 'UMLT '
                case (OP_DIV)
                    inst = 'SDIV '
                case (OP_EQ)
                    inst = 'SETE '
                case (OP_NE)
                    inst = 'SETNE '
                case (OP_LT)
                    inst = 'SSETL '
                case (OP_LE)
                    inst = 'SSETLE '
                case (OP_GT)
                    inst = 'SSETG '
                case (OP_GE)
                    inst = 'SSETGE '
                case (OP_AND)
                    inst = 'AND '
                case (OP_OR)
                    inst = 'OR '
                case (OP_XOR)
                    inst = 'XOR '
                end select
                if (current_instruction%operands(1)%kind == 24) then
                    select case (current_instruction%instruction)
                    case (OP_ADD)
                        inst = 'FADD '
                    case (OP_SUB)
                        inst = 'FSUB '
                    case (OP_MLT)
                        inst = 'FMLT '
                    case (OP_DIV)
                        inst = 'FDIV '
                    end select
                end if
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                arg3 = calculate_arg(current_instruction%operands(3), varlocs)
                current_strpointer%value = inst//arg1//' '//arg2//' '//arg3//achar(10)
            case (OP_NOT)
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                current_strpointer%value = 'NOT '//arg1//' '//arg2//achar(10)
            case (OP_BR)
                arg1 = calculate_arg(current_instruction%operands(2), varlocs)
                select type (arg3i => current_instruction%operands(3)%value)
                type is (integer)
                    if (irinput%children_dup(arg3i)) then
                        current_strpointer%value = 'BNZ .'//irinput%children(arg3i)%ptr%name//' '//arg1//achar(10)
                    else
                        select type (arg4 => current_instruction%operands(4)%value)
                        type is (integer)
                            current_strpointer%value = 'BRZ .'//irinput%children(arg4)%ptr%name//' '//arg1//achar(10)
                        end select
                    end if
                end select
            case (OP_RET)
                current_strpointer%value = 'MOV SP R1'//achar(10)//'POP R1'//achar(10)//'RET'//achar(10)//achar(10)
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
                    current_strpointer%value = 'ADD '//arg1//' R1 '//itoa(irinput%variables(var)%var%offset)//achar(10)
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
                    current_strpointer%value = 'LLOD '//arg1//' R1 '//itoa(irinput%variables(var)%var%offset + i)//achar(10)
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
                            current_strpointer%value = 'LOD '//arg1//' !g_'//var%srcmod//'_'//trim(var%name)//achar(10)
                        else
                            current_strpointer%value = 'LLOD '//arg1//' !g_'//var%srcmod//'_'//trim(var%name)//' '//itoa(i)//&
                                                        achar(10)
                        end if
                    end associate
                end select
            case (OP_STRLV)
                arg1 = calculate_arg(current_instruction%operands(3), varlocs)
                select type (var => current_instruction%operands(2)%value)
                type is (integer)
                    current_strpointer%value = 'LSTR R1 '//itoa(irinput%variables(var)%var%offset)//' '//arg1//achar(10)
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
                do i = size(current_instruction%operands), 3, -1
                    arg1 = calculate_arg(current_instruction%operands(i), varlocs)
                    current_strpointer%value = 'PSH '//arg1//achar(10)
                    j = j + 1
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                end do
                arg1 = calculate_arg(current_instruction%operands(1), varlocs)
                arg2 = calculate_arg(current_instruction%operands(2), varlocs)
                if (arg1 /= '') then
                    current_strpointer%value = 'PSH R0'//achar(10)
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                end if
                current_strpointer%value = 'CAL '//arg2//achar(10)
                if (arg1 /= '') then
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                    current_strpointer%value = 'POP '//arg1//achar(10)
                end if

                if (j /= 0) then
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                    current_strpointer%value = 'ADD SP SP '//itoa(j)//achar(10)
                end if
            end select
            nullify(current_strpointer%next)
            current_instruction => current_instruction%next
        end do


        allocate(current_strpointer%next)
        current_strpointer => current_strpointer%next
        nullify(current_strpointer%next)

        if (allocated(irinput%children)) then
            do i = 1, size(irinput%children)
                if (irinput%children_dup(i)) cycle
                if (associated(current_strpointer)) then
                    call internal_gen_asm(current_strpointer, irinput%children(i)%ptr, varsizes, varlocs)
                else
                    call throw('internal error: unassociated current_strpointer', '', 0_SMALL, 0_SMALL)
                end if
            end do
        end if


        select case (irinput%block_type)
        case (BLOCK_IF, BLOCK_CONTINUE, BLOCK_ELSE, BLOCK_DO)
            if (allocated(irinput%children)) then
                if (irinput%children_dup(1)) then
                    current_strpointer%value = 'JMP .'//irinput%children(1)%ptr%name//achar(10)
                    allocate(current_strpointer%next)
                    current_strpointer => current_strpointer%next
                    nullify(current_strpointer%next)
                end if
            end if
            result => current_strpointer
        end select

        ! insert epilogs

        select case (irinput%block_type)
        case (BLOCK_PROGRAM)
            current_strpointer%value = 'HLT'//achar(10)//achar(10)
            allocate(current_strpointer%next)
            current_strpointer => current_strpointer%next
            nullify(current_strpointer%next)
        case (BLOCK_SUBROUTINE)
            current_strpointer%value = 'MOV SP R1'//achar(10)//'POP R1'//achar(10)//'RET'//achar(10)//achar(10)
            allocate(current_strpointer%next)
            current_strpointer => current_strpointer%next
            nullify(current_strpointer%next)
        end select

        if (allocated(irinput%functions)) then
            do i = size(irinput%functions), 1, -1
                if (associated(current_strpointer)) then
                    call internal_gen_asm(current_strpointer, irinput%functions(i)%ptr, varsizes, varlocs)
                else
                    call throw('internal error: unassociated current_strpointer', '', 0_SMALL, 0_SMALL)
                end if
            end do
        end if
    end subroutine

    pure subroutine allocr(result, graph, maxvar)
        integer(SMALL), allocatable, intent(out) :: result(:)
        type(iarr), allocatable, intent(in) :: graph(:)
        integer, intent(in) :: maxvar

        ! r1 - bp
        ! r0 - 0 reg

        integer, allocatable :: stack(:)
        type(iarr), allocatable :: tmpgraph(:)
        integer :: i, j, k, sp, count, remainingvar, maxcount, maxcountvar
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
            do i = 1, size(tmpgraph)
                ! count neighbors
                count = 0
                do j = 1, tmpgraph(i)%size - 1
                    if (tmpgraph(i)%array(j) /= -1) count = count + 1
                end do
                counts(i) = count
            end do
            do i = 1, size(tmpgraph)
                count = counts(i)
                ! remove if less than n neighbors
                if (count < regn) then
                    remainingvar = remainingvar - 1
                    sp = sp + 1
                    stack(sp) = i
                    tmpgraph(i) = iarr()
                    do j = 1, size(tmpgraph)
                        if (j /= i) then
                            do k = 1, tmpgraph(j)%size - 1
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
                do i = 1, size(tmpgraph)
                    if (i /= maxcountvar) then
                        do j = 1, tmpgraph(i)%size - 1
                            if (tmpgraph(i)%array(j) == maxcountvar) tmpgraph(i)%array(j) = -1
                        end do
                    end if
                end do
            end if
        end do

        do i = 1, size(result)
            result(i) = -1
        end do

        do while (sp > 0)
            currval = stack(sp)
            sp = sp - 1
            currentreg = 1
            outer: &
            do
                do i = 1, graph(currval)%size - 1
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