submodule (irgen) irutil
    implicit none

contains
    module pure subroutine insert_inst1(current_instruction, inst, op1_type, op1_value, op1_kind)
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer(SMALL), intent(in) :: inst
        integer(SMALL), intent(in) :: op1_type
        integer, intent(in) :: op1_value
        integer(SMALL), intent(in) :: op1_kind

        type(ir_instruction), pointer :: temp_instruction

        allocate(temp_instruction)
        temp_instruction%instruction = inst
        allocate(temp_instruction%operands(1))
        temp_instruction%operands(1)%type = op1_type
        temp_instruction%operands(1)%value = op1_value
        temp_instruction%operands(1)%kind = op1_kind
        temp_instruction%next => current_instruction%next
        current_instruction%next => temp_instruction
        current_instruction => temp_instruction
    end subroutine

    module pure subroutine insert_inst2(current_instruction, inst, op1_type, op1_value, op1_kind, op2_type, op2_value, op2_kind)
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer(SMALL), intent(in) :: inst
        integer(SMALL), intent(in) :: op1_type, op2_type
        integer, intent(in) :: op1_value, op2_value
        integer(SMALL), intent(in) :: op1_kind, op2_kind

        type(ir_instruction), pointer :: temp_instruction

        allocate(temp_instruction)
        temp_instruction%instruction = inst
        allocate(temp_instruction%operands(2))
        temp_instruction%operands(1)%type = op1_type
        temp_instruction%operands(1)%value = op1_value
        temp_instruction%operands(1)%kind = op1_kind
        temp_instruction%operands(2)%type = op2_type
        temp_instruction%operands(2)%value = op2_value
        temp_instruction%operands(2)%kind = op2_kind
        temp_instruction%next => current_instruction%next
        current_instruction%next => temp_instruction
        current_instruction => temp_instruction
    end subroutine

    module pure subroutine insert_inst3(current_instruction, inst, op1_type, op1_value, op1_kind, op2_type, op2_value, op2_kind, &
                                        op3_type, op3_value, op3_kind)
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer(SMALL), intent(in) :: inst
        integer(SMALL), intent(in) :: op1_type, op2_type, op3_type
        integer, intent(in) :: op1_value, op2_value, op3_value
        integer(SMALL), intent(in) :: op1_kind, op2_kind, op3_kind

        type(ir_instruction), pointer :: temp_instruction

        allocate(temp_instruction)
        temp_instruction%instruction = inst
        allocate(temp_instruction%operands(3))
        temp_instruction%operands(1)%type = op1_type
        temp_instruction%operands(1)%value = op1_value
        temp_instruction%operands(1)%kind = op1_kind
        temp_instruction%operands(2)%type = op2_type
        temp_instruction%operands(2)%value = op2_value
        temp_instruction%operands(2)%kind = op2_kind
        temp_instruction%operands(3)%type = op3_type
        temp_instruction%operands(3)%value = op3_value
        temp_instruction%operands(3)%kind = op3_kind
        temp_instruction%next => current_instruction%next
        current_instruction%next => temp_instruction
        current_instruction => temp_instruction
    end subroutine
end submodule