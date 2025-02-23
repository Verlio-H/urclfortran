submodule (irgen) ircast
    implicit none

contains
    module subroutine gen_ir_insert_cast(lvar, lkind, rvar, rkind, varsizes, current_instruction, currnum)
        integer, intent(inout) :: lvar
        integer(SMALL), intent(inout) :: lkind
        integer, intent(inout) :: rvar
        integer(SMALL), intent(inout) :: rkind
        type(siarr), intent(inout) :: varsizes
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer, intent(inout) :: currnum

        if (lkind == rkind) then
            return
        end if

        if (lkind == 44 .or. rkind == 44) then
            call throw('cannot cast to or from logical types', 'unknown', 0_SMALL, 0_SMALL)
        end if

        allocate(current_instruction%next)
        current_instruction => current_instruction%next
        current_instruction%instruction = OP_CAST
        allocate(current_instruction%operands(2))
        nullify(current_instruction%next)

        if (lkind > rkind) then
            current_instruction%operands(1)%type = V_VAR
            current_instruction%operands(1)%value = currnum
            current_instruction%operands(1)%kind = lkind
            current_instruction%operands(2)%type = V_VAR
            current_instruction%operands(2)%value = rvar
            current_instruction%operands(2)%kind = rkind
            rkind = lkind
            rvar = currnum
            currnum = currnum + 1
            call varsizes%append(lkind)
        else
            current_instruction%operands(1)%type = V_VAR
            current_instruction%operands(1)%value = currnum
            current_instruction%operands(1)%kind = rkind
            current_instruction%operands(2)%type = V_VAR
            current_instruction%operands(2)%value = lvar
            current_instruction%operands(2)%kind = lkind
            lkind = rkind
            lvar = currnum
            currnum = currnum + 1
            call varsizes%append(rkind)
        end if
    end subroutine

    module subroutine gen_ir_cast_to(dkind, rvar, rkind, varsizes, current_instruction, currnum)
        integer(SMALL), intent(in) :: dkind
        integer, intent(inout) :: rvar
        integer(SMALL), intent(inout) :: rkind
        type(siarr), intent(inout) :: varsizes
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer, intent(inout) :: currnum

        if (dkind == rkind) then
            return
        end if

        if (dkind == 44 .or. rkind == 44) then
            call throw('cannot cast to or from logical types', 'unknown', 0_SMALL, 0_SMALL)
        end if

        allocate(current_instruction%next)
        current_instruction => current_instruction%next
        current_instruction%instruction = OP_CAST
        allocate(current_instruction%operands(2))
        nullify(current_instruction%next)

        current_instruction%operands(1)%type = V_VAR
        current_instruction%operands(1)%value = currnum
        current_instruction%operands(1)%kind = dkind
        current_instruction%operands(2)%type = V_VAR
        current_instruction%operands(2)%value = rvar
        current_instruction%operands(2)%kind = rkind
        rkind = dkind
        rvar = currnum
        currnum = currnum + 1
        call varsizes%append(dkind)

    end subroutine
end submodule