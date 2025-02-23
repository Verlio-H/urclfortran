module backend_common
    use include, only: SMALL, throw, iarr, siarr, itoa, poly_assign_poly
    use astgen, only: TYPE_NONE, TYPE_INTEGER, TYPE_REAL, TYPE_COMPLEX, TYPE_LOGICAL, PROP_INDIRECT
    use irgen, only: ir, ir_instruction, operand, V_NONE, V_VAR, V_IMM, OP_MOV, OP_PSH, OP_ADD, OP_SUB, OP_MLT, OP_LOD, OP_STR, &
                    OP_LODLV, OP_STRLV, OP_LODGV, OP_STRGV, OP_CAST, OP_SETL, OP_SSETL, OP_UMLT, insert_inst2, insert_inst3
    implicit none

contains
    recursive subroutine countrefs(irinput, varcounts)
        type(ir), pointer, intent(in) :: irinput
        integer, intent(inout) :: varcounts(:)

        type(ir_instruction), pointer :: current_instruction
        integer :: i

        current_instruction => irinput%instruction
        do while (associated(current_instruction))
            if (allocated(current_instruction%operands)) then
                do i = 2, size(current_instruction%operands)
                    associate(op => current_instruction%operands(i))
                        if (op%type == V_VAR) then
                            select type (value => op%value)
                            type is (integer)
                                if (value == 0) cycle
                                varcounts(value) = varcounts(value) + 1
                            end select
                        end if
                    end associate
                end do
            end if
            current_instruction => current_instruction%next
        end do
        if (allocated(irinput%functions)) then
            do i = 1, size(irinput%functions)
                call countrefs(irinput%functions(i)%ptr, varcounts)
            end do
        end if
    end subroutine

    recursive subroutine updatelivevars(irinput, varcounts, livevars, varconnections)
        type(ir), intent(in) :: irinput
        integer, intent(inout) :: varcounts(:)
        logical, intent(inout) :: livevars(:)
        type(iarr), intent(inout) :: varconnections(:)

        integer :: i, j
        type(ir_instruction), pointer :: current_instruction

        current_instruction => irinput%instruction
        do while (associated(current_instruction))
            if (allocated(current_instruction%operands)) then
                do i = 2, size(current_instruction%operands)
                    associate(op => current_instruction%operands(i))
                        if (op%type == V_VAR) then
                            select type (value => op%value)
                            type is (integer)
                                if (value /= 0) then
                                    varcounts(value) = varcounts(value) - 1
                                    if (varcounts(value) == 0) livevars(value) = .false.
                                end if
                            end select
                        end if
                    end associate
                end do
                if (size(current_instruction%operands) >= 1) then
                    associate(op => current_instruction%operands(1))
                        if (op%type == V_VAR) then
                            select type (value => op%value)
                            type is (integer)
                                if (value /= 0) then
                                    do j=1,size(livevars)
                                        if (j == value) cycle
                                        if (livevars(j)) then
                                            call varconnections(j)%append(value)
                                            call varconnections(value)%append(j)
                                        end if
                                    end do
                                    livevars(value) = .true.
                                end if
                            end select
                        end if
                    end associate
                end if
            end if
            current_instruction => current_instruction%next
        end do
        if (allocated(irinput%functions)) then
            do i = 1, size(irinput%functions)
                call updatelivevars(irinput%functions(i)%ptr, varcounts, livevars, varconnections)
            end do
        end if
    end subroutine

    subroutine print_livevars(varconnections)
        type(iarr), intent(in) :: varconnections(:)

        integer :: i, j

        do i = 1, size(varconnections)
            write(*, '(A)') itoa(i)//':'
            do j = 1, varconnections(i)%size - 1
                write(*, '(A)') '    '//itoa(varconnections(i)%array(j))
            end do
        end do
    end subroutine


    recursive subroutine removeimms(irinput,maxvar,varsizes)
        type(ir), intent(inout) :: irinput
        integer, intent(inout) :: maxvar
        type(siarr), intent(inout) :: varsizes

        integer :: i
        type(ir_instruction), pointer :: current_instruction
        type(ir_instruction), pointer :: previous_instruction

        nullify(previous_instruction)
        current_instruction => irinput%instruction
        do while (associated(current_instruction))
            if (current_instruction%instruction /= OP_MOV .and. allocated(current_instruction%operands)) then
                do i = 2, size(current_instruction%operands)
                    associate(op => current_instruction%operands(i))
                        if (op%type == V_IMM) then
                            if (.not.associated(previous_instruction)) then
                                nullify(irinput%instruction)
                                allocate(irinput%instruction)
                                previous_instruction => irinput%instruction
                            else
                                nullify(previous_instruction%next)
                                allocate(previous_instruction%next)
                                previous_instruction => previous_instruction%next
                            end if
                            previous_instruction%next => current_instruction
                            previous_instruction%instruction = OP_MOV
                            allocate(previous_instruction%operands(2))
                            previous_instruction%operands(1)%type = V_VAR
                            maxvar = maxvar + 1
                            previous_instruction%operands(1)%value = maxvar
                            previous_instruction%operands(1)%kind = op%kind
                            previous_instruction%operands(2)%type = V_IMM
                            call varsizes%append(op%kind)
                            call poly_assign_poly(previous_instruction%operands(2)%value, op%value)
                            previous_instruction%operands(2)%kind = op%kind
                            op%type = V_VAR
                            op%value = maxvar
                        end if
                    end associate
                end do
            end if
            previous_instruction => current_instruction
            current_instruction => current_instruction%next
        end do
        if (allocated(irinput%functions)) then
            do i=1,size(irinput%functions)
                call removeimms(irinput%functions(i)%ptr, maxvar, varsizes)
            end do
        end if
    end subroutine

    recursive subroutine resolve_offsets(irinput, ptr, int8, int16, int32, int64, int128, float, double)
        type(ir), intent(inout) :: irinput
        integer, value, intent(in) :: ptr
        integer, value, intent(in) :: int8
        integer, value, intent(in) :: int16
        integer, value, intent(in) :: int32
        integer, value, intent(in) :: int64
        integer, value, intent(in) :: int128
        integer, value, intent(in) :: float
        integer, value, intent(in) :: double

        integer :: i
        integer :: current_offset
            
        current_offset = 2 * ptr

        if (allocated(irinput%children)) then
            do i = 1, size(irinput%functions)
                call resolve_offsets(irinput%children(i)%ptr, ptr, int8, int16, int32, int64, int128, float, double)
            end do
        end if
        if (allocated(irinput%functions)) then
            do i = 1, size(irinput%functions)
                call resolve_offsets(irinput%functions(i)%ptr, ptr, int8, int16, int32, int64, int128, float, double)
            end do
        end if

        if (.not.allocated(irinput%variables)) return

        do i = 1, size(irinput%variables)
            associate (var => irinput%variables(i)%var)
                if (iand(var%vartype%properties, int(PROP_INDIRECT, SMALL)) /= 0) then
                    var%offset = current_offset
                    current_offset = current_offset + ptr
                    cycle
                else if (current_offset > 0) then
                    current_offset = -ptr
                end if

                select case (var%vartype%type)
                case (TYPE_INTEGER)
                    select case (var%vartype%kind)
                    case (1_SMALL)
                        current_offset = current_offset - int8
                    case (2_SMALL)
                        current_offset = current_offset - int16
                    case (4_SMALL)
                        current_offset = current_offset - int32
                    case (8_SMALL)
                        current_offset = current_offset - int64
                    case (16_SMALL)
                        current_offset = current_offset - int128
                    end select
                case (TYPE_REAL)
                    select case (var%vartype%kind)
                    case (4_SMALL)
                        current_offset = current_offset - float
                    case (8_SMALL)
                        current_offset = current_offset - double
                    end select
                case (TYPE_COMPLEX)
                    select case (var%vartype%kind)
                    case (4_SMALL)
                        current_offset = current_offset - 2 * float
                    case (8_SMALL)
                        current_offset = current_offset - 2 * double
                    end select
                case (TYPE_LOGICAL)
                    current_offset = current_offset - int8
                end select

                var%offset = current_offset + 1
            end associate
        end do
    end subroutine

    recursive subroutine lower16(irinput, maxvar, varsizes)
        type(ir), intent(inout) :: irinput
        integer, intent(inout) :: maxvar
        type(siarr), intent(inout) :: varsizes

        type(ir_instruction), pointer :: current_instruction
        type(ir_instruction), pointer :: previous_instruction

        integer :: i
        type(iarr) :: newvars
        type(iarr) :: associations
        type(ir_instruction), pointer :: temp_instruction
        integer(SMALL) :: temp_op, temp_type
        integer :: temp_var1, temp_var2, temp_var3
        type(operand) :: temp_operand

        nullify(previous_instruction)
        nullify(temp_instruction)
        current_instruction => irinput%instruction
        do while (associated(current_instruction))
            if (.not.allocated(current_instruction%operands)) then
            else if (size(current_instruction%operands) < 2) then
            else if (current_instruction%instruction == OP_PSH .and. current_instruction%operands(2)%kind == 4) then
                select type (var => current_instruction%operands(2)%value)
                type is (integer)
                    allocate(temp_instruction)
                    temp_instruction%instruction = OP_PSH
                    allocate(temp_instruction%operands(2))
                    temp_instruction%operands(1)%type = TYPE_NONE
                    temp_instruction%operands(2)%type = V_VAR
                    temp_instruction%operands(2)%value = var
                    temp_instruction%operands(2)%kind = 2_SMALL

                    current_instruction%operands(2)%value = newvars%array(newvars_index(associations, var))
                    current_instruction%operands(2)%kind = 2_SMALL

                    temp_instruction%next => current_instruction%next
                    current_instruction%next => temp_instruction
                    current_instruction => temp_instruction
                    nullify(temp_instruction)
                end select
            else if ((current_instruction%instruction == OP_MOV .or. &
                     current_instruction%instruction == OP_LOD .or. &
                     current_instruction%instruction == OP_LODLV .or. &
                     current_instruction%instruction == OP_LODGV .or. &
                     current_instruction%instruction == OP_CAST) &
                     .and. current_instruction%operands(1)%kind == 4) then
                select type (var => current_instruction%operands(1)%value)
                type is (integer)
                    select case (current_instruction%instruction)
                    case (OP_MOV)
                        maxvar = maxvar + 1
                        call varsizes%append(2_SMALL)
                        call newvars%append(maxvar)
                        call associations%append(var)
                        varsizes%array(var) = 2_SMALL

                        current_instruction%operands(1)%kind = 2_SMALL
                        select type (var2 => current_instruction%operands(2)%value)
                        type is (integer)
                            current_instruction%operands(2)%value = mod(var2, 2**16 - 1)
                            current_instruction%operands(2)%kind = 0_SMALL

                            call insert_inst2(current_instruction, OP_MOV, &
                                                V_VAR, maxvar, 2_SMALL, &
                                                V_IMM, var2 / 2**16, 0_SMALL)
                        end select
                    case (OP_LOD)
                        select type (var2 => current_instruction%operands(2)%value)
                        type is (integer)
                            maxvar = maxvar + 1
                            call varsizes%append(-1_SMALL)
                            current_instruction%operands(1)%kind = 2_SMALL
                            varsizes%array(var) = 2_SMALL

                            temp_operand = current_instruction%operands(2)
                            call insert_inst3(current_instruction, OP_ADD, &
                                                V_VAR, maxvar, -1_SMALL, &
                                                V_NONE, 0, 0_SMALL, &
                                                V_IMM, 1, 0_SMALL)
                            current_instruction%operands(2) = temp_operand

                            maxvar = maxvar + 1
                            call varsizes%append(2_SMALL)
                            call newvars%append(maxvar)
                            call associations%append(var)

                            call insert_inst2(current_instruction, OP_LOD, &
                                                V_VAR, maxvar, 2_SMALL, &
                                                V_VAR, maxvar - 1, -1_SMALL)
                        end select
                    case (OP_LODLV, OP_LODGV)
                        if (size(current_instruction%operands) == 3) then
                            select type (val3 => current_instruction%operands(3)%value)
                            type is (integer)
                                current_instruction%operands(3)%value = val3 * 2 ! account for offset doubling in 32 => 16 stride
                            end select
                        end if
                        current_instruction%operands(1)%kind = 2_SMALL

                        maxvar = maxvar + 1
                        call varsizes%append(2_SMALL)
                        call newvars%append(maxvar)
                        call associations%append(var)

                        allocate (temp_instruction)
                        temp_instruction%instruction = current_instruction%instruction
                        allocate(temp_instruction%operands(3))
                        temp_instruction%operands(1)%type = V_VAR
                        temp_instruction%operands(1)%value = maxvar
                        temp_instruction%operands(1)%kind = 2_SMALL
                        temp_instruction%operands(2) = current_instruction%operands(2)
                        temp_instruction%operands(3)%type = V_IMM
                        if (size(current_instruction%operands) == 3) then
                            select type (val3 => current_instruction%operands(3)%value)
                            type is (integer)
                                temp_instruction%operands(3)%value = val3 + 1
                            end select
                        else
                            temp_instruction%operands(3)%value = 1
                        end if
                        temp_instruction%operands(3)%kind = 0_SMALL
                        temp_instruction%next => current_instruction%next
                        current_instruction%next => temp_instruction
                        current_instruction => temp_instruction
                        nullify(temp_instruction)
                    case (OP_CAST)
                        current_instruction%instruction = OP_MOV
                        current_instruction%operands(1)%kind = 2_SMALL
                        maxvar = maxvar + 1
                        call varsizes%append(2_SMALL)
                        call newvars%append(maxvar)
                        call associations%append(var)

                        temp_operand = current_instruction%operands(2)
                        call insert_inst3(current_instruction, OP_SSETL, &
                                            V_VAR, maxvar, 2_SMALL, &
                                            V_NONE, 0, 0_SMALL, &
                                            V_IMM, 0, 0_SMALL)
                        current_instruction%operands(2) = temp_operand
                    end select
                end select
            else if (current_instruction%instruction == OP_CAST .and. current_instruction%operands(2)%kind == 4) then
                current_instruction%operands(2)%kind = 2_SMALL
                current_instruction%instruction = OP_MOV
            else if (size(current_instruction%operands) < 3) then
            else if (current_instruction%instruction == OP_STR .and. current_instruction%operands(3)%kind == 4) then
                select type (var1 => current_instruction%operands(2)%value)
                type is (integer)
                    select type (var2 => current_instruction%operands(3)%value)
                    type is (integer)
                        maxvar = maxvar + 1
                        call varsizes%append(-1_SMALL)
                        
                        current_instruction%operands(3)%kind = 2_SMALL

                        call insert_inst3(current_instruction, OP_ADD, &
                                            V_VAR, maxvar, -1_SMALL, &
                                            V_VAR, var1, -1_SMALL, &
                                            V_IMM, 1, 0_SMALL)

                        call insert_inst3(current_instruction, OP_STR, &
                                            V_NONE, 0, 0_SMALL, &
                                            V_VAR, maxvar, -1_SMALL, &
                                            V_VAR, newvars%array(newvars_index(associations, var2)), 2_SMALL)
                    end select
                end select
            else if (current_instruction%operands(1)%kind == 4) then
                select type (var1 => current_instruction%operands(1)%value)
                type is (integer)
                    select type (var2 => current_instruction%operands(2)%value)
                    type is (integer)
                        select type (var3 => current_instruction%operands(3)%value)
                        type is (integer)
                            select case (current_instruction%instruction)
                            case (OP_ADD, OP_SUB)
                                temp_op = current_instruction%instruction
                                temp_type = current_instruction%operands(3)%type
                                current_instruction%operands(1)%kind = 2_SMALL
                                current_instruction%operands(2)%kind = 2_SMALL
                                current_instruction%operands(3)%kind = 2_SMALL

                                if (temp_type == V_IMM) then
                                    current_instruction%operands(3)%value = mod(var3, 2**16)
                                end if

                                maxvar = maxvar + 1
                                call varsizes%append(0_SMALL)

                                allocate(temp_instruction)
                                allocate(temp_instruction%operands(3))
                                temp_instruction%instruction = OP_SETL
                                temp_instruction%operands(1)%type = V_VAR
                                temp_instruction%operands(1)%value = maxvar
                                temp_instruction%operands(1)%kind = 0_SMALL
                                if (temp_op == OP_ADD) then
                                    temp_instruction%operands(2)%type = V_VAR
                                    temp_instruction%operands(2)%value = var1
                                    temp_instruction%operands(2)%kind = 2_SMALL
                                    temp_instruction%operands(3) = current_instruction%operands(3)
                                else
                                    temp_instruction%operands(2) = current_instruction%operands(3)
                                    temp_instruction%operands(3)%type = V_VAR
                                    temp_instruction%operands(3)%value = var1
                                    temp_instruction%operands(3)%kind = 2_SMALL
                                end if
                                temp_instruction%next => current_instruction%next
                                current_instruction%next => temp_instruction
                                current_instruction => temp_instruction
                                nullify(temp_instruction)

                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)

                                allocate(temp_instruction)
                                allocate(temp_instruction%operands(3))
                                temp_instruction%instruction = temp_op
                                temp_instruction%operands(1)%type = V_VAR
                                temp_instruction%operands(1)%value = maxvar
                                temp_instruction%operands(1)%kind = 2_SMALL
                                temp_instruction%operands(2)%type = V_VAR
                                temp_instruction%operands(2)%value = newvars%array(newvars_index(associations, var2))
                                temp_instruction%operands(2)%kind = 2_SMALL

                                if (temp_type == V_VAR) then
                                    temp_instruction%operands(3)%type = V_VAR
                                    temp_instruction%operands(3)%value = newvars%array(newvars_index(associations, var3))
                                    temp_instruction%operands(3)%kind = 2_SMALL
                                else if (temp_type == V_IMM) then
                                    temp_instruction%operands(3)%type = V_IMM
                                    temp_instruction%operands(3)%value = var3 / 2**16
                                end if
                                temp_instruction%next => current_instruction%next
                                current_instruction%next => temp_instruction
                                current_instruction => temp_instruction
                                nullify(temp_instruction)

                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)
                                call newvars%append(maxvar)
                                call associations%append(var1)
                                
                                call insert_inst3(current_instruction, OP_ADD, &
                                                    V_VAR, maxvar, 2_SMALL, &
                                                    V_VAR, maxvar - 1, 2_SMALL, &
                                                    V_VAR, maxvar - 2, 2_SMALL)
                                if (temp_op == OP_ADD) then
                                    current_instruction%instruction = OP_SUB
                                end if
                            case (OP_MLT)
                                ! %1 = u1 * l2
                                ! %2 = l1 * u2
                                ! %3 = %1 + %2
                                ! %4 = l1 umlt l2 (unsigned)
                                ! l = l1 * l2
                                ! u = %3 + %4

                                ! for now makes the assumption of variable for op2
                                temp_type = current_instruction%operands(3)%type
                                temp_var1 = var1
                                temp_var2 = var2
                                temp_var3 = var3
                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)
                                current_instruction%operands(1)%value = maxvar
                                current_instruction%operands(1)%kind = 2_SMALL
                                current_instruction%operands(2)%kind = 2_SMALL
                                current_instruction%operands(3)%kind = 2_SMALL

                                if (temp_type == V_IMM) then
                                    current_instruction%operands(3)%value = mod(var3, 2**16)
                                end if

                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)

                                current_instruction%operands(2)%value = newvars%array(newvars_index(associations, var2))
                                call insert_inst3(current_instruction, OP_MLT, &
                                                    V_VAR, maxvar, 2_SMALL, &
                                                    V_VAR, temp_var2, 2_SMALL, &
                                                    V_VAR, newvars%array(newvars_index(associations, var3)), 2_SMALL)
                                if (temp_type == V_IMM) then
                                    temp_instruction%operands(3)%type = V_IMM
                                    temp_instruction%operands(3)%value = temp_var3 / 2**16
                                    temp_instruction%operands(3)%kind = 0_SMALL
                                end if

                                ! %3 = %1 + %2
                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)
                                call insert_inst3(current_instruction, OP_ADD, &
                                                    V_VAR, maxvar, 2_SMALL, &
                                                    V_VAR, maxvar - 2, 2_SMALL, &
                                                    V_VAR, maxvar - 1, 2_SMALL)

                                ! %4 = l1 umlt l2 (unsigned)
                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)

                                call insert_inst3(current_instruction, OP_UMLT, &
                                                    V_VAR, maxvar, 2_SMALL, &
                                                    V_VAR, temp_var2, 2_SMALL, &
                                                    temp_type, temp_var3, 2_SMALL)
                                if (temp_type == V_IMM) then
                                    temp_instruction%operands(3)%value = mod(temp_var3, 2**16)
                                    temp_instruction%operands(3)%kind = 0_SMALL
                                end if

                                ! u = %3 + %4
                                maxvar = maxvar + 1
                                call varsizes%append(2_SMALL)
                                call newvars%append(maxvar)
                                call associations%append(temp_var1)

                                call insert_inst3(current_instruction, OP_ADD, &
                                                    V_VAR, maxvar, 2_SMALL, &
                                                    V_VAR, maxvar - 2, 2_SMALL, &
                                                    V_VAR, maxvar - 1, 2_SMALL)
                                
                                ! l = l1 * l2
                                call insert_inst3(current_instruction, OP_MLT, &
                                                    V_VAR, temp_var1, 2_SMALL, &
                                                    V_VAR, temp_var2, 2_SMALL, &
                                                    temp_type, temp_var3, 2_SMALL)
                                if (temp_type == V_IMM) then
                                    temp_instruction%operands(3)%value = mod(temp_var3, 2**16)
                                    temp_instruction%operands(3)%kind = 0_SMALL
                                end if
                            end select
                        end select
                    end select
                end select
            end if
            previous_instruction => current_instruction
            current_instruction => current_instruction%next
        end do

        if (allocated(irinput%children)) then
            do i=1,size(irinput%children)
                call lower16(irinput%children(i)%ptr, maxvar, varsizes)
            end do
        end if
        if (allocated(irinput%functions)) then
            do i=1,size(irinput%functions)
                call lower16(irinput%functions(i)%ptr, maxvar, varsizes)
            end do
        end if
    end subroutine

    function newvars_index(associations, var) result(result)
        integer :: result
        type(iarr), intent(in) :: associations
        integer, intent(in) :: var

        integer :: i
        
        do i = 1, associations%size - 1
            if (associations%array(i) == var) then
                result = i
                return
            end if
        end do
        call throw('unassociated var: '//itoa(var), 'unknown', 0_SMALL, 0_SMALL)
        result = 1
    end function
end module