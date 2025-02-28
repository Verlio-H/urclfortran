! Info for backends:
! Kinds:
!  -1: pointer
!  0: most efficient int kind
! For Ints:
!  1: 8 bit int
!  2: 16 bit int
!  4: 32 bit int
!  8: 64 bit int
! 16: 128 bit int
! For Chars:
! 64: only size
! For logical:
! 44: only size
! For floats:
! 24: arch default (32 bit ideally)
! 28: double arch default (64 bit ideally)
! 36: quad arch default (128 bit ideally)

! Operations >=10000 can be used by backends as arch dependent ops

module irgen
    use include, only: SMALL, throw, atoi, ator, atol, itoa, siarr, carr, tolower, poly_assign_poly
    use astgen, only: ast, NODE_MODULE, NODE_PROGRAM, NODE_TYPE, NODE_SUBROUTINE, NODE_USE, NODE_ASSIGNMENT, NODE_STRING, &
                        NODE_CALL, NODE_ADD, NODE_SUB, NODE_MLT, NODE_DIV, NODE_EQ, NODE_NE, NODE_LT, NODE_LE, NODE_GT, NODE_GE, &
                        NODE_NOT, NODE_AND, NODE_OR, NODE_IF, NODE_INT_VAL, NODE_REAL_VAL, NODE_LOGICAL_VAL, NODE_CHAR_VAL, &
                        NODE_FNC_ARR, NODE_ELSE, NODE_ELSE_IF, NODE_DO, TYPE_NONE, TYPE_REAL, TYPE_LOGICAL, TYPE_INTEGER, &
                        TYPE_CHARACTER, PROP_INDIRECT, PROP_PARAMETER
    use semantic, only: sem_module, sem_variable, sem_proc, eval_type, type
    implicit none

    integer(SMALL), parameter :: OP_NOP = 0
    integer(SMALL), parameter :: OP_MOV = 1
    integer(SMALL), parameter :: OP_ADD = 2
    integer(SMALL), parameter :: OP_SUB = 3
    integer(SMALL), parameter :: OP_MLT = 4
    integer(SMALL), parameter :: OP_UMLT = 5
    integer(SMALL), parameter :: OP_DIV = 6

    integer(SMALL), parameter :: OP_EQ = 10
    integer(SMALL), parameter :: OP_NE = 11
    integer(SMALL), parameter :: OP_LT = 12
    integer(SMALL), parameter :: OP_LE = 13
    integer(SMALL), parameter :: OP_GT = 14
    integer(SMALL), parameter :: OP_GE = 15
    integer(SMALL), parameter :: OP_NOT = 16
    integer(SMALL), parameter :: OP_AND = 17
    integer(SMALL), parameter :: OP_OR = 18
    integer(SMALL), parameter :: OP_XOR = 19

    integer(SMALL), parameter :: OP_CAST = 100
    integer(SMALL), parameter :: OP_SETL = 1000
    integer(SMALL), parameter :: OP_SSETL = 1001
    integer(SMALL), parameter :: OP_PSH = 2000
    integer(SMALL), parameter :: OP_STR = 2001
    integer(SMALL), parameter :: OP_LOD = 2002
    integer(SMALL), parameter :: OP_CALL = 2003
    integer(SMALL), parameter :: OP_END = 3000
    integer(SMALL), parameter :: OP_ADRLV = 4000
    integer(SMALL), parameter :: OP_ADRGV = 4001
    integer(SMALL), parameter :: OP_STRLV = 4002
    integer(SMALL), parameter :: OP_STRGV = 4003
    integer(SMALL), parameter :: OP_LODLV = 4004
    integer(SMALL), parameter :: OP_LODGV = 4005

    integer(SMALL), parameter :: OP_ASOCMEM = 5000

    integer(SMALL), parameter :: OP_BR = 6000

    integer(SMALL), parameter :: V_NONE = 0
    integer(SMALL), parameter :: V_IMM = 1
    integer(SMALL), parameter :: V_VAR = 2
    integer(SMALL), parameter :: V_SYMB = 3
    integer(SMALL), parameter :: V_BP = 4
    integer(SMALL), parameter :: V_SP = 5
    integer(SMALL), parameter :: V_STR = 6

    integer(SMALL), parameter :: BLOCK_ROOT = 0
    integer(SMALL), parameter :: BLOCK_PROGRAM = 1
    integer(SMALL), parameter :: BLOCK_SUBROUTINE = 2
    integer(SMALL), parameter :: BLOCK_IF = 3
    integer(SMALL), parameter :: BLOCK_CONTINUE = 4
    integer(SMALL), parameter :: BLOCK_ELSE = 5
    integer(SMALL), parameter :: BLOCK_DO = 6


    type operand
        integer(SMALL) :: type
        integer(SMALL) :: kind
        class(*), allocatable :: value
    end type

    type ir_instruction
        integer(SMALL) :: instruction = 0
        type(operand), allocatable :: operands(:)
        type(ir_instruction), pointer :: next
    end type

    type ir
        integer(SMALL) :: block_type = 0
        character(:), allocatable :: name
        type(ir_instruction), pointer :: instruction
        type(sem_module), pointer :: module
        type(ir_ptr), allocatable :: children(:)
        logical, allocatable :: children_dup(:)
        type(ir_ptr), allocatable :: parents(:)
        type(ir_ptr), allocatable :: functions(:)
        type(ssa_var), allocatable :: variables(:)
    end type

    type ir_ptr
        type(ir), pointer :: ptr
    end type


    type ssa_var
        type(sem_variable) :: var
        logical :: global
    end type

    interface
        module recursive subroutine ir_print(input,printed)
            type(ir), pointer, intent(in) :: input
            type(carr), pointer, optional, intent(in) :: printed
        end subroutine

        module subroutine gen_ir_insert_cast(lvar, lkind, rvar, rkind, varsizes, current_instruction, currnum)
            integer, intent(inout) :: lvar
            integer(SMALL), intent(inout) :: lkind
            integer, intent(inout) :: rvar
            integer(SMALL), intent(inout) :: rkind
            type(siarr), intent(inout) :: varsizes
            type(ir_instruction), pointer, intent(inout) :: current_instruction
            integer, intent(inout) :: currnum
        end subroutine

        module subroutine gen_ir_cast_to(dkind, rvar, rkind, varsizes, current_instruction, currnum)
            integer(SMALL), intent(in) :: dkind
            integer, intent(inout) :: rvar
            integer(SMALL), intent(inout) :: rkind
            type(siarr), intent(inout) :: varsizes
            type(ir_instruction), pointer, intent(inout) :: current_instruction
            integer, intent(inout) :: currnum
        end subroutine

        module subroutine insert_inst1(current_instruction, inst, op1_type, op1_value, op1_kind)
            type(ir_instruction), pointer, intent(inout) :: current_instruction
            integer(SMALL), intent(in) :: inst
            integer(SMALL), intent(in) :: op1_type
            integer, intent(in) :: op1_value
            integer(SMALL), intent(in) :: op1_kind
        end subroutine

        module subroutine insert_inst2(current_instruction, inst, op1_type, op1_value, op1_kind, op2_type, op2_value, op2_kind)
            type(ir_instruction), pointer, intent(inout) :: current_instruction
            integer(SMALL), intent(in) :: inst
            integer(SMALL), intent(in) :: op1_type, op2_type
            integer, intent(in) :: op1_value, op2_value
            integer(SMALL), intent(in) :: op1_kind, op2_kind
        end subroutine

        module subroutine insert_inst3(current_instruction, inst, op1_type, op1_value, op1_kind, op2_type, op2_value, &
            op2_kind, op3_type, op3_value, op3_kind)
            type(ir_instruction), pointer, intent(inout) :: current_instruction
            integer(SMALL), intent(in) :: inst
            integer(SMALL), intent(in) :: op1_type, op2_type, op3_type
            integer, intent(in) :: op1_value, op2_value, op3_value
            integer(SMALL), intent(in) :: op1_kind, op2_kind, op3_kind
        end subroutine

        module subroutine insert_inst4(current_instruction, inst, op1_type, op1_value, op1_kind, op2_type, op2_value, &
            op2_kind, op3_type, op3_value, op3_kind, op4_type, op4_value, op4_kind)
            type(ir_instruction), pointer, intent(inout) :: current_instruction
            integer(SMALL), intent(in) :: inst
            integer(SMALL), intent(in) :: op1_type, op2_type, op3_type, op4_type
            integer, intent(in) :: op1_value, op2_value, op3_value, op4_value
            integer(SMALL), intent(in) :: op1_kind, op2_kind, op3_kind, op4_kind
        end subroutine
    end interface

    integer :: unused_num = 0
contains
    subroutine gen_ir(tree, symbols, result, maxvar, varsizes)
        type(ast), intent(in) :: tree
        type(sem_module), intent(in) :: symbols(:)
        type(ir_ptr), allocatable, intent(inout) :: result(:)
        integer, intent(out) :: maxvar
        type(siarr), intent(out) :: varsizes
        
        integer :: currnum
        integer :: i
        integer :: symbolidx
        type(ir_instruction), pointer :: current_instruction
        type(ir), pointer :: tmp_ir

        currnum = 1
        symbolidx = 0

        nullify(current_instruction)
        allocate(result(tree%nodes(1)%subnodes%size - 1))
        do i = 1, tree%nodes(1)%subnodes%size - 1
            allocate(result(i)%ptr)
            tmp_ir => result(i)%ptr
            call internal_gen_ir(tree, tree%nodes(1)%subnodes%array(i), currnum, symbols, symbolidx, result(i)%ptr, &
                                current_instruction, varsizes)
            result(i)%ptr => tmp_ir
        end do

        maxvar = currnum - 1
    end subroutine

    recursive subroutine internal_gen_ir(tree, currnode, currnum, symbols, symbolidx, result_block, current_instruction, &
                                        varsizes)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        type(sem_module), target, intent(in) :: symbols(:)
        integer, intent(inout) :: symbolidx
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        type(siarr), intent(inout) :: varsizes

        integer :: i, j
        type(ir), pointer :: sub_block1, sub_block2, sub_block3
        type(ir_instruction), pointer :: tmp_irinst

        associate (node => tree%nodes(currnode))
            select case (node%type)
            ! do stuff with node
            case (NODE_PROGRAM, NODE_MODULE)
                result_block%name = trim(node%value)
                do symbolidx = 1, size(symbols)
                    if (symbols(symbolidx)%name == result_block%name) exit
                end do
                if (symbolidx > size(symbols)) then
                    call throw("symbol table does not exist for module "//result_block%name, node%fname, node%startlnum, &
                                node%startchar)
                end if
                result_block%module => symbols(symbolidx)

                if (allocated(node%subnodes%array)) then
                    allocate(result_block%functions(node%subnodes%size - 1))
                end if
                do i = 1, node%subnodes%size - 1
                    allocate(result_block%functions(i)%ptr)
                    associate(new_node => node%subnodes%array(i))
                        sub_block1 => result_block%functions(i)%ptr
                        call internal_gen_ir(tree, new_node, currnum, symbols, symbolidx, result_block%functions(i)%ptr, &
                                            current_instruction, varsizes)
                        result_block%functions(i)%ptr => sub_block1
                    end associate
                end do
                nullify(result_block%instruction)
            case (NODE_SUBROUTINE)
                result_block%name = symbols(symbolidx)%name//'_'//tolower(trim(node%value))
                if (node%subnodes%size /= 0) then
                    allocate(result_block%variables(node%subnodes%size - 1))
                end if
                do i = 1, node%subnodes%size - 1
                    ! TODO: implicit type
                    result_block%variables(i)%var%name = trim(tree%nodes(node%subnodes%array(i))%value)
                    result_block%variables(i)%var%vartype%type = TYPE_NONE
                    result_block%variables(i)%var%vartype%properties = PROP_INDIRECT
                end do
                nullify(result_block%instruction)
            case (NODE_IF, NODE_ELSE_IF)
                block
                    integer :: result
                    type(type) :: resulttype
                    logical :: elseblock
                    type(ir_ptr), allocatable :: tmp_children(:)
                    logical, allocatable :: tmp_children_dup(:)
                    
                    call internal_gen_rval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result, resulttype, varsizes)
                    
                    if (allocated(result_block%children)) then
                        call move_alloc(result_block%children, tmp_children)
                        call move_alloc(result_block%children_dup, tmp_children_dup)
                    end if
                    allocate(result_block%children(2), result_block%children_dup(2))

                    call insert_inst4(current_instruction, OP_BR, &
                                        V_NONE, 0, 0_SMALL, &
                                        V_VAR, result, resulttype%kind, &
                                        V_IMM, size(result_block%children) - 1, 0_SMALL, &
                                        V_IMM, size(result_block%children), 0_SMALL)

                    allocate(sub_block1)
                    if (node%value(:1) == 'e') then
                        elseblock = .true.
                        sub_block1%name = 'if_'//trim(node%value(2:))//'_'//itoa(unused_num)
                    else
                        elseblock = .false.
                        sub_block1%name = 'if_'//trim(node%value)//'_'//itoa(unused_num)
                    end if

                    sub_block1%block_type = BLOCK_IF
                    allocate(sub_block1%parents(1))
                    sub_block1%parents(1)%ptr => result_block
                    allocate(sub_block1%children(1), sub_block1%children_dup(1))
                    sub_block1%module => symbols(symbolidx)
                    sub_block1%variables = result_block%variables
                    allocate(sub_block1%instruction)
                    tmp_irinst => sub_block1%instruction
                    tmp_irinst%instruction = OP_NOP
                    nullify(tmp_irinst%next)

                    do i = 1, node%subnodes2%size - 1
                        call internal_gen_ir(tree, node%subnodes2%array(i), currnum, symbols, symbolidx, sub_block1, tmp_irinst, &
                                                varsizes)
                    end do

                    allocate(sub_block2)
                    sub_block2%name = 'cont_'//result_block%name
                    sub_block2%block_type = BLOCK_CONTINUE
                    allocate(sub_block2%parents(2))
                    sub_block2%parents(2)%ptr => sub_block1
                    sub_block2%module => symbols(symbolidx)
                    sub_block2%variables = result_block%variables
                    allocate(sub_block2%instruction)
                    sub_block2%instruction%instruction = OP_NOP
                    nullify(sub_block2%instruction%next)

                    sub_block1%children(1)%ptr => sub_block2

                    result_block%children(1)%ptr => sub_block1
                    result_block%children_dup(1) = .false.
                    
                    call move_alloc(tmp_children, sub_block2%children)
                    call move_alloc(tmp_children_dup, sub_block2%children_dup)

                    if (.not.elseblock) then
                        sub_block1%children_dup(1) = .false.
                        sub_block2%parents(1)%ptr => result_block
                        current_instruction => sub_block2%instruction
                        result_block%children(2)%ptr => sub_block2
                        result_block%children_dup(2) = .true.
                        result_block => sub_block2
                        unused_num = unused_num + 1
                        return
                    end if

                    sub_block1%children_dup(1) = .true.

                    allocate(sub_block3)
                    sub_block2%parents(1)%ptr => sub_block3

                    result_block%children(2)%ptr => sub_block3
                    result_block%children_dup(2) = .false.
                    
                    sub_block3%name = 'else_'//trim(node%value(2:))//'_'//itoa(unused_num)
                    sub_block3%block_type = BLOCK_ELSE
                    allocate(sub_block3%parents(1))
                    sub_block3%parents(1)%ptr => result_block
                    allocate(sub_block3%children(1), sub_block3%children_dup(1))
                    sub_block3%children(1)%ptr => sub_block2
                    sub_block3%children_dup(1) = .false.
                    sub_block3%module => symbols(symbolidx)
                    sub_block3%variables = result_block%variables
                    allocate(sub_block3%instruction)
                    sub_block3%instruction%instruction = OP_NOP
                    current_instruction => sub_block3%instruction
                    nullify(current_instruction%next)


                    result_block => sub_block3
                    unused_num = unused_num + 1

                    return
                end block
            case (NODE_ELSE)
                do i = 1, node%subnodes2%size - 1
                    call internal_gen_ir(tree, node%subnodes2%array(i), currnum, symbols, symbolidx, result_block, &
                                            current_instruction, varsizes)
                end do
                result_block => result_block%children(1)%ptr
                return
            case (NODE_DO)
                block
                    integer :: result1, result2, result3, result4, result5
                    type(type) :: resulttype1, resulttype2, resulttype3
                    type(ir_ptr), allocatable :: tmp_children(:)
                    logical, allocatable :: tmp_children_dup(:)
                    
                    ! initialization expression
                    call internal_gen_lval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result1, resulttype1, varsizes)
                    call internal_gen_rval_ir(tree, node%subnodes%array(2), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result2, resulttype2, varsizes)
                    call gen_ir_cast_to(resulttype1%kind, result2, resulttype2%kind, &
                                                varsizes, current_instruction, currnum)
                    call insert_inst3(current_instruction, OP_STR, &
                                                V_NONE, 0, 0_SMALL, &
                                                V_VAR, result1, varsizes%array(result1), &
                                                V_VAR, result2, resulttype2%kind)
                    nullify(current_instruction%next)
                    
                    ! end value
                    call internal_gen_rval_ir(tree, node%subnodes%array(3), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result2, resulttype2, varsizes)
                    ! get sign of increment
                    call internal_gen_rval_ir(tree, node%subnodes%array(4), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result3, resulttype3, varsizes)
                    call gen_ir_cast_to(resulttype1%kind, result3, resulttype3%kind, varsizes, current_instruction, currnum)
                    
                    call insert_inst3(current_instruction, OP_SSETL, &
                                        V_VAR, currnum, 44_SMALL, &
                                        V_VAR, result3, resulttype3%kind, &
                                        V_IMM, 0, 0_SMALL)
                    call varsizes%append(44_SMALL)
                    currnum = currnum + 1
                    ! if the increment is negative, need to invert current value and end value in order to check if out of do (xor)
                    call internal_gen_rval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result1, resulttype1, varsizes)
                    call gen_ir_insert_cast(result1, resulttype1%kind, result2, resulttype2%kind, varsizes, current_instruction, &
                                            currnum)
                    call insert_inst3(current_instruction, OP_XOR, &
                                        V_VAR, currnum, resulttype1%kind, &
                                        V_VAR, result1, resulttype1%kind, &
                                        V_VAR, currnum - 1, 44_SMALL)
                    call varsizes%append(resulttype1%kind)
                    currnum = currnum + 1
                    call insert_inst3(current_instruction, OP_XOR, &
                                        V_VAR, currnum, resulttype2%kind, &
                                        V_VAR, result2, resulttype2%kind, &
                                        V_VAR, currnum - 2, 44_SMALL)
                    call varsizes%append(resulttype2%kind)
                    currnum = currnum + 1

                    call insert_inst3(current_instruction, OP_LE, &
                                        V_VAR, currnum, 44_SMALL, &
                                        V_VAR, currnum - 2, resulttype1%kind, &
                                        V_VAR, currnum - 1, resulttype2%kind)
                    call varsizes%append(44_SMALL)
                    currnum = currnum + 1

                    call insert_inst4(current_instruction, OP_BR, &
                                        V_NONE, 0, 0_SMALL, &
                                        V_VAR, currnum - 1, 44_SMALL, &
                                        V_IMM, 1, 0_SMALL, &
                                        V_IMM, 2, 0_SMALL)
                    nullify(current_instruction%next)
                    
                    ! create actual cfg stuff
                    if (allocated(result_block%children)) then
                        call move_alloc(result_block%children, tmp_children)
                        call move_alloc(result_block%children_dup, tmp_children_dup)
                    end if
                    allocate(result_block%children(2), result_block%children_dup(2))

                    allocate(sub_block1)
                    sub_block1%name = 'do_'//trim(node%value(2:))//'_'//itoa(unused_num)
                    unused_num = unused_num + 1
                    sub_block1%block_type = BLOCK_DO
                    allocate(sub_block1%parents(2))
                    sub_block1%parents(1)%ptr => result_block
                    sub_block1%parents(2)%ptr => sub_block1
                    allocate(sub_block1%children(2), sub_block1%children_dup(2))
                    sub_block1%children(2)%ptr => sub_block1
                    sub_block1%children_dup(2) = .true.
                    sub_block1%module => symbols(symbolidx)
                    sub_block1%variables = result_block%variables
                    allocate(sub_block1%instruction)
                    sub_block1%instruction%instruction = OP_NOP
                    tmp_irinst => sub_block1%instruction
                    nullify(tmp_irinst%next)

                    do i = 1, node%subnodes2%size - 1
                        call internal_gen_ir(tree, node%subnodes2%array(i), currnum, symbols, symbolidx, sub_block1, tmp_irinst, &
                                                varsizes)
                    end do

                    call internal_gen_lval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, sub_block1, &
                                                tmp_irinst, result1, resulttype1, varsizes)
                    ! load value, calc inc, add inc
                    call insert_inst2(tmp_irinst, OP_LOD, &
                                        V_VAR, currnum, resulttype1%kind, &
                                        V_VAR, result1, varsizes%array(result1))
                    call varsizes%append(resulttype1%kind)
                    result5 = currnum
                    currnum = currnum + 1

                    call internal_gen_rval_ir(tree, node%subnodes%array(4), currnum, symbols, symbolidx, sub_block1, &
                                                tmp_irinst, result3, resulttype3, varsizes)
                    call gen_ir_cast_to(resulttype1%kind, result3, resulttype3%kind, varsizes, tmp_irinst, currnum)
                    call insert_inst3(tmp_irinst, OP_SSETL, &
                                        V_VAR, currnum, 44_SMALL, &
                                        V_VAR, result3, resulttype3%kind, &
                                        V_IMM, 0, 0_SMALL)
                    result4 = currnum
                    call varsizes%append(44_SMALL)
                    currnum = currnum + 1
                    call gen_ir_cast_to(resulttype1%kind, result3, resulttype3%kind, varsizes, tmp_irinst, currnum)
                    
                    call insert_inst3(tmp_irinst, OP_ADD, &
                                        V_VAR, currnum, resulttype1%kind, &
                                        V_VAR, result5, resulttype1%kind, &
                                        V_VAR, result3, resulttype1%kind)
                    call insert_inst3(tmp_irinst, OP_STR, &
                                        V_NONE, 0, 0_SMALL, &
                                        V_VAR, result1, varsizes%array(result1), &
                                        V_VAR, currnum, resulttype1%kind)
                    call varsizes%append(resulttype1%kind)
                    result1 = currnum
                    currnum = currnum + 1

                    call internal_gen_rval_ir(tree, node%subnodes%array(3), currnum, symbols, symbolidx, sub_block1, &
                                                tmp_irinst, result2, resulttype2, varsizes)
                    call gen_ir_insert_cast(result1, resulttype1%kind, result2, resulttype2%kind, varsizes, tmp_irinst, currnum)

                    call insert_inst3(tmp_irinst, OP_XOR, &
                                        V_VAR, currnum, resulttype1%kind, &
                                        V_VAR, result1, resulttype1%kind, &
                                        V_VAR, result4, 44_SMALL)
                    call varsizes%append(resulttype1%kind)
                    result1 = currnum
                    currnum = currnum + 1

                    call insert_inst3(tmp_irinst, OP_XOR, &
                                        V_VAR, currnum, resulttype2%kind, &
                                        V_VAR, result2, resulttype2%kind, &
                                        V_VAR, result4, 44_SMALL)
                    call varsizes%append(resulttype2%kind)
                    result2 = currnum
                    currnum = currnum + 1

                    call insert_inst3(tmp_irinst, OP_LE, &
                                        V_VAR, currnum, 44_SMALL, &
                                        V_VAR, result1, resulttype1%kind, &
                                        V_VAR, result2, resulttype2%kind)
                    call insert_inst4(tmp_irinst, OP_BR, &
                                        V_NONE, 0, 0_SMALL, &
                                        V_VAR, currnum, 44_SMALL, &
                                        V_IMM, 2, 0_SMALL, &
                                        V_IMM, 1, 0_SMALL)
                    call varsizes%append(44_SMALL)
                    currnum = currnum + 1
                    nullify(tmp_irinst%next)
                    
                    allocate(sub_block2)
                    sub_block1%children(1)%ptr => sub_block2
                    sub_block1%children_dup(1) = .false.
                    result_block%children(2)%ptr => sub_block2
                    result_block%children_dup(2) = .true.
                    result_block%children(1)%ptr => sub_block1
                    result_block%children_dup(1) = .false.

                    sub_block2%name = 'cont_'//result_block%name

                    sub_block2%block_type = BLOCK_CONTINUE
                    allocate(sub_block2%parents(2))
                    sub_block2%parents(1)%ptr => result_block
                    sub_block2%parents(2)%ptr => sub_block1
                    sub_block2%module => symbols(symbolidx)
                    sub_block2%variables = result_block%variables
                    allocate(sub_block2%instruction)
                    sub_block2%instruction%instruction = OP_NOP
                    nullify(sub_block2%instruction%next)
                    current_instruction => sub_block2%instruction
                    
                    call move_alloc(tmp_children, sub_block2%children)
                    call move_alloc(tmp_children_dup, sub_block2%children_dup)

                    result_block => sub_block2
                    return
                end block
            case (NODE_TYPE, NODE_USE, NODE_ASSIGNMENT, NODE_CALL)
            case default
                call throw('unexpected node type in ir generation', node%fname, node%startlnum, node%startchar)
            end select

            select case (node%type)
            case (NODE_PROGRAM)
                result_block%block_type = BLOCK_PROGRAM
            case (NODE_SUBROUTINE)
                result_block%block_type = BLOCK_SUBROUTINE
            end select

            ! do stuff with instructions
            select case (node%type)
            case (NODE_PROGRAM, NODE_SUBROUTINE)
                if (allocated(node%subnodes2%array)) then
                    allocate(result_block%instruction)
                    current_instruction => result_block%instruction
                    current_instruction%instruction = OP_NOP
                    nullify(current_instruction%next)
                    do i = 1, node%subnodes2%size - 1
                        call internal_gen_ir(tree, node%subnodes2%array(i), currnum, symbols, symbolidx, result_block, &
                                            current_instruction, varsizes)
                    end do
                end if
            case (NODE_TYPE)
                ! check for pre existing
                i = 0
                if (allocated(result_block%variables)) then
                    do i = 1, size(result_block%variables)
                        if (result_block%variables(i)%var%name == node%value) then
                            if (result_block%variables(i)%var%vartype%type /= TYPE_NONE) then
                                call throw('duplicate variable definition', node%fname, node%startlnum, node%startchar)
                            end if
                            exit ! occurs if variable is function argument
                        end if
                    end do
                end if
                if (i == 0) then
                    allocate(result_block%variables(1))
                    i = 1
                    result_block%variables(i)%var%vartype = eval_type(tree, currnode, symbols(symbolidx))
                else if (i==size(result_block%variables)+1) then
                    block
                        type(ssa_var), allocatable :: tmp(:)
                        call move_alloc(result_block%variables, tmp)
                        allocate(result_block%variables(size(tmp) + 1))
                        result_block%variables(:size(tmp)) = tmp
                    end block
                    result_block%variables(i)%var%vartype = eval_type(tree, currnode, symbols(symbolidx))
                else
                    result_block%variables(i)%var%vartype = eval_type(tree, currnode, symbols(symbolidx))
                    associate (prop => result_block%variables(i)%var%vartype%properties)
                        result_block%variables(i)%var%vartype%properties = ior(prop, int(PROP_INDIRECT, SMALL))
                    end associate
                end if
                result_block%variables(i)%var%name = trim(node%value)
                
            case (NODE_ASSIGNMENT)
                block
                    integer :: lresultvar, rresultvar
                    type(type) :: lresulttype, rresulttype
                    if (.not.allocated(node%subnodes2%array)) then
                        call throw('expected expression in assignment', 'unknown', 0_SMALL, 0_SMALL)
                    end if
                    call internal_gen_rval_ir(tree, node%subnodes2%array(1), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, rresultvar, rresulttype, varsizes)
                    call internal_gen_lval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, result_block, &
                                                current_instruction,lresultvar,lresulttype,varsizes)
                    call gen_ir_cast_to(lresulttype%kind, rresultvar, rresulttype%kind, &
                                        varsizes, current_instruction, currnum)
                    call insert_inst3(current_instruction, OP_STR, &
                                        V_NONE, 0, 0_SMALL, &
                                        V_VAR, lresultvar, varsizes%array(lresultvar), &
                                        V_VAR, rresultvar, rresulttype%kind)
                    nullify(current_instruction%next)
                end block
            case (NODE_CALL)
                block
                    integer, allocatable :: argsloc(:)
                    type(sem_proc), pointer :: subroutine_ptr
                    logical :: rval
                    type(type) :: resulttype
                    integer(SMALL) :: popcnt
                    popcnt = 0
                    ! find subroutine
                    nullify(subroutine_ptr)

                    outer: &
                    do i = 1, size(symbols(symbolidx)%functbl)
                        associate (inter => symbols(symbolidx)%functbl(i))
                            if (allocated(inter%name)) then
                                if (inter%name /= node%value) cycle
                            end if
                            do j = 1, size(inter%functions)
                                if (.not.allocated(inter%name) .and. inter%functions(j)%name /= node%value) cycle
                                if (.not.inter%functions(j)%subrout) then
                                    call throw('subroutine expected in call statement', node%fname, node%startlnum, node%startchar)
                                end if
                                subroutine_ptr => symbols(symbolidx)%functbl(i)%functions(j)
                                exit outer
                            end do
                        end associate
                    end do outer
                    if (associated(subroutine_ptr)) then
                        ! todo: optionals
                        if (node%subnodes%size /= 0) then
                            if (.not.allocated(subroutine_ptr%arguments)) then
                                call throw('invalid amount of arguments in subroutine call (expected 0, passed '//&
                                            itoa(node%subnodes%size - 1)//')', node%fname, node%startlnum, node%startchar)
                            end if
                            if (size(subroutine_ptr%arguments) /= node%subnodes%size - 1) then
                                call throw('invalid amount of arguments in subroutine call (expected '//&
                                            itoa(size(subroutine_ptr%arguments))//', passed '//&
                                            itoa(node%subnodes%size - 1)//')', node%fname, node%startlnum, node%startchar)
                            end if
                        end if
                        if (allocated(subroutine_ptr%arguments)) then
                            allocate(argsloc(size(subroutine_ptr%arguments)))
                        else
                            allocate(argsloc(0))
                        end if
                    else
                        allocate(argsloc(node%subnodes%size - 1))
                    end if
                    do i = node%subnodes%size - 1, 1, -1
                        call internal_gen_lval_ir(tree, node%subnodes%array(i), currnum, symbols, symbolidx, result_block, &
                                                    current_instruction, argsloc(i), resulttype, varsizes, rval)
                        if (rval) then
                            call internal_gen_rval_ir(tree, node%subnodes%array(i), currnum,symbols, symbolidx, result_block, &
                                                        current_instruction, argsloc(i), resulttype, varsizes)
                            call insert_inst2(current_instruction, OP_PSH, &
                                                V_NONE, 0, 0_SMALL, &
                                                V_VAR, argsloc(i), resulttype%kind)
                            popcnt = popcnt + 1_SMALL
                            
                            call insert_inst2(current_instruction, OP_MOV, &
                                                V_VAR, currnum, -1_SMALL, &
                                                V_SP, 0, -1_SMALL)
                            nullify(current_instruction%next)
                            argsloc(i) = currnum
                            currnum = currnum + 1
                            call varsizes%append(-1_SMALL)
                        end if
                    end do
                    allocate(current_instruction%next)
                    current_instruction => current_instruction%next
                    current_instruction%instruction = OP_CALL
                    allocate(current_instruction%operands(2 + size(argsloc)))
                    current_instruction%operands(1)%type = V_NONE
                    current_instruction%operands(2)%type = V_SYMB
                    if (associated(subroutine_ptr)) then
                        current_instruction%operands(2)%value = 's_'//subroutine_ptr%srcmod//'_'//tolower(subroutine_ptr%name)
                    else
                        current_instruction%operands(2)%value = 's__'//tolower(node%value)
                    end if
                    do i = 3, size(current_instruction%operands)
                        current_instruction%operands(i)%type = V_VAR
                        current_instruction%operands(i)%value = argsloc(i - 2)
                        current_instruction%operands(i)%kind = -1_SMALL
                    end do
                    nullify(current_instruction%next)

                    if (popcnt /= 0_SMALL) then
                        call insert_inst3(current_instruction, OP_ADD, &
                                            V_SP, 0, -1_SMALL, &
                                            V_SP, 0, -1_SMALL, &
                                            V_IMM, int(popcnt), 0_SMALL)
                    end if
                end block
                nullify(current_instruction%next)
            case (NODE_USE)
                nullify(current_instruction%next)
            end select
        end associate
    end subroutine

    recursive subroutine internal_gen_lval_ir(tree, currnode, currnum, symbols, symbolidx, result_block, current_instruction, &
                                                result, resulttype, varsizes, error)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        type(sem_module), intent(in) :: symbols(:)
        integer, intent(inout) :: symbolidx
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer, intent(out) :: result
        type(type), intent(out) :: resulttype
        type(siarr), intent(inout) :: varsizes
        logical, optional, intent(out) :: error

        integer :: i
        
        if (present(error)) error = .false.
        associate (node => tree%nodes(currnode))
            select case (node%type)
            case (NODE_STRING)
                ! search for variable
                do i = 1, size(result_block%variables)
                    if (result_block%variables(i)%var%name /= node%value) cycle


                    resulttype = result_block%variables(i)%var%vartype

                    if (resulttype%type == TYPE_REAL) resulttype%kind = resulttype%kind + 20_SMALL
                    if (resulttype%type == TYPE_LOGICAL) resulttype%kind = resulttype%kind + 40_SMALL

                    call insert_inst2(current_instruction, OP_ADRLV, &
                                        V_VAR, currnum, -1_SMALL, &
                                        V_IMM, i, 0_SMALL)
                    nullify(current_instruction%next)
                    result = currnum
                    currnum = currnum + 1
                    call varsizes%append(-1_SMALL)
                    if (iand(result_block%variables(i)%var%vartype%properties, int(PROP_INDIRECT, SMALL)) == 0) return
                    
                    call insert_inst2(current_instruction, OP_LOD, &
                                        V_VAR, currnum, -1_SMALL, &
                                        V_VAR, currnum - 1, -1_SMALL)
                    nullify(current_instruction%next)
                    result = currnum
                    currnum = currnum + 1
                    call varsizes%append(-1_SMALL)
                    return
                end do
                do i = 1, size(symbols(symbolidx)%vartbl)
                    if (symbols(symbolidx)%vartbl(i)%name == node%value) then
                        call insert_inst2(current_instruction, OP_ADRGV, &
                                            V_VAR, currnum, 0_SMALL, &
                                            V_IMM, i, 0_SMALL)
                        nullify(current_instruction%next)
                        result = currnum
                        currnum = currnum + 1
                        resulttype = symbols(symbolidx)%vartbl(i)%vartype
                        call varsizes%append(-1_SMALL)
                        return
                    end if
                end do
                if (i == size(symbols(symbolidx)%vartbl) + 1) then
                    call throw('unknown variable name: '//trim(node%value), node%fname, node%startlnum, node%startchar)
                end if
            case default
                if (present(error)) then
                    error = .true.
                    return
                else
                    call throw('value must be a variable', node%fname, node%startlnum, node%startchar)
                end if
            end select
        end associate
    end subroutine

    recursive subroutine internal_gen_rval_ir(tree, currnode, currnum, symbols, symbolidx, result_block, current_instruction, &
                                                result, resulttype, varsizes)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        type(sem_module), intent(in) :: symbols(:)
        integer, intent(inout) :: symbolidx
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer, intent(out) :: result
        type(type), intent(out) :: resulttype
        type(siarr), intent(inout) :: varsizes

        integer :: i, j, k
        integer, allocatable :: ops(:)
        integer :: result1, result2
        type(type) :: resulttype1, resulttype2
        associate (node => tree%nodes(currnode))
            select case (node%type)
            case (NODE_ADD, NODE_SUB, NODE_MLT, NODE_DIV, NODE_EQ, NODE_NE, NODE_LT, NODE_LE, NODE_GT, NODE_GE, NODE_AND, NODE_OR)
                call internal_gen_rval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, result_block, &
                                            current_instruction, result1, resulttype1, varsizes)
                call internal_gen_rval_ir(tree, node%subnodes2%array(1), currnum, symbols, symbolidx, result_block, &
                                            current_instruction, result2, resulttype2, varsizes)
                call gen_ir_insert_cast(result1, resulttype1%kind, result2, resulttype2%kind, &
                                        varsizes, current_instruction, currnum)
                allocate(current_instruction%next)
                current_instruction => current_instruction%next
                select case (node%type)
                case (NODE_ADD)
                    current_instruction%instruction = OP_ADD
                case (NODE_SUB)
                    current_instruction%instruction = OP_SUB
                case (NODE_MLT)
                    current_instruction%instruction = OP_MLT
                case (NODE_DIV)
                    current_instruction%instruction = OP_DIV
                case (NODE_EQ)
                    current_instruction%instruction = OP_EQ
                case (NODE_NE)
                    current_instruction%instruction = OP_NE
                case (NODE_LT)
                    current_instruction%instruction = OP_LT
                case (NODE_LE)
                    current_instruction%instruction = OP_LE
                case (NODE_GT)
                    current_instruction%instruction = OP_GT
                case (NODE_GE)
                    current_instruction%instruction = OP_GE
                case (NODE_AND)
                    current_instruction%instruction = OP_AND
                case (NODE_OR)
                    current_instruction%instruction = OP_OR
                end select
                allocate(current_instruction%operands(3))
                current_instruction%operands(2)%type = V_VAR
                current_instruction%operands(2)%value = result1
                current_instruction%operands(2)%kind = resulttype1%kind
                current_instruction%operands(3)%type = V_VAR
                current_instruction%operands(3)%value = result2
                current_instruction%operands(3)%kind = resulttype2%kind
                current_instruction%operands(1)%type = V_VAR
                current_instruction%operands(1)%value = currnum
                current_instruction%operands(1)%kind = resulttype1%kind
                result = currnum
                currnum = currnum + 1
                resulttype = resulttype1
                call varsizes%append(resulttype%kind)
            case (NODE_NOT)
                call internal_gen_rval_ir(tree, node%subnodes2%array(1), currnum, symbols, symbolidx, result_block, &
                                            current_instruction, result2, resulttype2, varsizes)
                call insert_inst2(current_instruction, OP_NOT, &
                                    V_VAR, currnum, resulttype2%kind, &
                                    V_VAR, result2, resulttype2%kind)
                result = currnum
                currnum = currnum + 1
                resulttype = resulttype2
                call varsizes%append(resulttype%kind)
            case (NODE_INT_VAL, NODE_REAL_VAL, NODE_LOGICAL_VAL)
                allocate(current_instruction%next)
                current_instruction => current_instruction%next
                current_instruction%instruction = OP_MOV
                allocate(current_instruction%operands(2))
                current_instruction%operands(1)%type = V_VAR
                current_instruction%operands(1)%value = currnum
                result = currnum
                currnum = currnum + 1
                current_instruction%operands(2)%type = V_IMM
                if (index(node%value, '_') == 0) then
                    if (node%type == NODE_INT_VAL) then
                        current_instruction%operands(2)%value = atoi(node%value)
                    else if (node%type == NODE_REAL_VAL) then
                        current_instruction%operands(2)%value = ator(node%value)
                    else if (node%type == NODE_LOGICAL_VAL) then
                        current_instruction%operands(2)%value = atol(node%value)
                    end if

                    current_instruction%operands(2)%kind = 0_SMALL
                    resulttype%kind = 4_SMALL
                else
                    big: &
                    block
                        integer :: idx
                        character(:), allocatable :: name
                        idx = index(node%value, '_')
                        current_instruction%operands(2)%value = atoi(node%value(:idx - 1))
                        if (node%value(idx + 1:idx + 1) >= '0' .and. node%value(idx + 1:idx + 1) <= '9') then
                            current_instruction%operands(2)%kind = int(atoi(node%value(idx + 1:)), SMALL)
                        else
                            name = trim(node%value(idx + 1:))
                            do i = 1, size(symbols(symbolidx)%vartbl) ! TODO: local constants
                                associate (var => symbols(symbolidx)%vartbl(i))
                                    if (var%name == name) then
                                        if (iand(var%vartype%properties, int(PROP_PARAMETER, SMALL)) /= 0) then
                                            select type (typ => var%value)
                                            type is (integer)
                                                current_instruction%operands(2)%kind = int(typ, SMALL)
                                            class default
                                                call throw('kind value must be integer', node%fname, node%startlnum, node%startchar)
                                            end select
                                        else
                                            call throw('kind value must be parameter', node%fname, node%startlnum, node%startchar)
                                        end if
                                        exit big
                                    end if
                                end associate
                            end do
                            call throw('kind value ('//trim(node%value(idx + 1:))//') must be integer parameter variable '//&
                                        'or integer constant', node%fname, node%startlnum, node%startchar)
                        end if
                    end block big
                    resulttype%kind = current_instruction%operands(2)%kind
                end if

                if (node%type == NODE_REAL_VAL) then
                    resulttype%kind = resulttype%kind + 20_SMALL
                    current_instruction%operands(2)%kind = current_instruction%operands(2)%kind + 20_SMALL
                else if (node%type == NODE_LOGICAL_VAL) then
                    resulttype%kind = resulttype%kind + 40_SMALL
                    current_instruction%operands(2)%kind = current_instruction%operands(2)%kind + 40_SMALL
                end if
                
                current_instruction%operands(1)%kind = resulttype%kind
                call varsizes%append(resulttype%kind)

                if (node%type == NODE_INT_VAL) then
                    resulttype%type = TYPE_INTEGER
                else
                    resulttype%type = TYPE_REAL
                end if
                
                nullify(current_instruction%next)
            case (NODE_CHAR_VAL)
                call insert_inst3(current_instruction, OP_ASOCMEM, &
                                    V_NONE, 0, 0_SMALL, &
                                    V_VAR, currnum, -2_SMALL, &
                                    V_STR, 0, 0_SMALL)
                select type (val => node%value2)
                type is (character(*))
                    current_instruction%operands(3)%value = val
                    current_instruction%operands(3)%kind = 60_SMALL
                end select
                nullify(current_instruction%next)

                call varsizes%append(-2_SMALL)
                resulttype%type = TYPE_CHARACTER
                resulttype%kind = -2_SMALL
                resulttype%properties = 0_SMALL
                result = currnum
                currnum = currnum + 1
            case (NODE_STRING)
                ! search for variable
                do i = 1, size(result_block%variables)
                    if (result_block%variables(i)%var%name == node%value) then
                        resulttype = result_block%variables(i)%var%vartype
                        
                        call insert_inst2(current_instruction, OP_LODLV, &
                                            V_VAR, currnum, -1_SMALL, &
                                            V_IMM, i, 0_SMALL)

                        select case (resulttype%type)
                        case (TYPE_REAL)
                            resulttype%kind = resulttype%kind + 20_SMALL
                        case (TYPE_LOGICAL)
                            resulttype%kind = resulttype%kind + 40_SMALL
                        case (TYPE_CHARACTER)
                            resulttype%kind = resulttype%kind + 60_SMALL
                        end select

                        if (iand(result_block%variables(i)%var%vartype%properties, int(PROP_INDIRECT, SMALL)) /= 0) then
                            call varsizes%append(-1_SMALL)

                            allocate(current_instruction%next)
                            current_instruction => current_instruction%next
                            current_instruction%instruction = OP_LOD
                            allocate(current_instruction%operands(2))
                            current_instruction%operands(2)%type = V_VAR
                            current_instruction%operands(2)%value = currnum
                            current_instruction%operands(2)%kind = -1_SMALL
                            currnum = currnum + 1

                            call varsizes%append(-1_SMALL)

                            current_instruction%operands(1)%type = V_VAR
                            current_instruction%operands(1)%value = currnum
                            current_instruction%operands(1)%kind = resulttype%kind
                        else
                            current_instruction%operands(1)%kind = resulttype%kind
                            call varsizes%append(resulttype%kind)
                        end if

                        nullify(current_instruction%next)
                        result = currnum
                        currnum = currnum + 1
                        return
                    end if
                end do

                do i = 1, size(symbols(symbolidx)%vartbl)
                    if (symbols(symbolidx)%vartbl(i)%name == node%value) then
                        if (iand(symbols(symbolidx)%vartbl(i)%vartype%properties, int(PROP_PARAMETER, SMALL)) /= 0) then
                            allocate(current_instruction%next)
                            current_instruction => current_instruction%next
                            current_instruction%instruction = OP_MOV
                            allocate(current_instruction%operands(2))
                            resulttype = symbols(symbolidx)%vartbl(i)%vartype
                            current_instruction%operands(2)%type = V_IMM
                            call poly_assign_poly(current_instruction%operands(2)%value, symbols(symbolidx)%vartbl(i)%value)
                            current_instruction%operands(2)%kind = resulttype%kind
                            current_instruction%operands(1)%type = V_VAR
                            current_instruction%operands(1)%value = currnum
                            current_instruction%operands(1)%kind = resulttype%kind
                            result = currnum
                            currnum = currnum + 1
                            call varsizes%append(resulttype%kind)
                            return
                        end if
                        resulttype = symbols(symbolidx)%vartbl(i)%vartype

                        call insert_inst2(current_instruction, OP_LODGV, &
                                            V_VAR, currnum, resulttype%kind, &
                                            V_IMM, i, 0_SMALL)

                        result = currnum
                        currnum = currnum + 1
                        call varsizes%append(resulttype%kind)
                        return
                    end if
                end do
                if (i == size(symbols(symbolidx)%vartbl) + 1) then
                    call throw('unknown variable name: '//trim(node%value), 'unknown', 0_SMALL, 0_SMALL)
                end if
            case (NODE_FNC_ARR)
                ! TODO: arrays
                do i = 1, size(symbols(symbolidx)%functbl)
                    associate(inter => symbols(symbolidx)%functbl(i))
                        if (inter%name == node%value) then
                            do j = 1, size(inter%functions)
                                associate(func => inter%functions(j))
                                    if (.not.func%subrout .and. func%name == node%value) then
                                        ! func found
                                        allocate(ops(node%subnodes%size-1))
                                        do k = 1, node%subnodes%size - 1
                                            call internal_gen_rval_ir(tree, node%subnodes%array(k), currnum, symbols, symbolidx, &
                                                                        result_block, current_instruction, result1, resulttype1, &
                                                                        varsizes)
                                            ops(k) = result1
                                        end do
                                        allocate(current_instruction%next)
                                        current_instruction => current_instruction%next
                                        current_instruction%instruction = OP_CALL
                                        allocate(current_instruction%operands(node%subnodes%size + 1))
                                        resulttype = func%return%vartype
                                        current_instruction%operands(1)%type = V_VAR
                                        current_instruction%operands(1)%value = currnum
                                        current_instruction%operands(1)%kind = resulttype%kind
                                        result = currnum
                                        current_instruction%operands(2)%type = V_SYMB
                                        current_instruction%operands(2)%value = 'f_'//func%name
                                        do k = 1, size(ops)
                                            current_instruction%operands(k + 2)%type = V_VAR
                                            current_instruction%operands(k + 2)%value = ops(k)
                                            current_instruction%operands(k + 2)%kind = varsizes%array(ops(k))
                                        end do
                                    end if
                                end associate
                            end do
                        end if
                    end associate
                end do
                select case (trim(node%value))
                case ('NINT')
                    call internal_gen_rval_ir(tree, node%subnodes%array(1), currnum, symbols, symbolidx, result_block, &
                                                current_instruction, result1, resulttype1, varsizes)
                    call insert_inst3(current_instruction, OP_CALL, &
                                        V_VAR, currnum, 4_SMALL, &
                                        V_SYMB, 0, -1_SMALL, &
                                        V_VAR, result1, 4_SMALL)
                    current_instruction%operands(2)%value = '_flib_nint'
                    result = currnum
                    currnum = currnum + 1
                    resulttype%type = TYPE_INTEGER
                    resulttype%kind = 4_SMALL
                    call varsizes%append(4_SMALL)
                    return
                end select
                call throw('unknown function/array name: '//trim(node%value), node%fname, node%startlnum, node%startchar)
            case default
                if (allocated(node%fname)) then
                    call throw('unexpected node type in ir generation', node%fname, node%startlnum, node%startchar)
                else
                    call throw('unexpected node type in ir generation', 'unknown', 0_SMALL, 0_SMALL)
                end if
            end select
        end associate
    end subroutine

    recursive subroutine ir_finalize(input)
        type(ir), pointer, intent(inout) :: input

        integer :: i

        if (allocated(input%children)) then
            do i = 1, size(input%children)
                if (associated(input%children(i)%ptr)) then
                    if (input%children_dup(i)) cycle
                    call ir_finalize(input%children(i)%ptr)
                    nullify(input%children(i)%ptr)
                end if
            end do
        end if
        if (allocated(input%functions)) then
            do i = 1, size(input%functions)
                if (associated(input%functions(i)%ptr)) then
                    call ir_finalize(input%functions(i)%ptr)
                    nullify(input%functions(i)%ptr)
                end if
            end do
        end if
        
        call ssa_finalize(input%instruction)
        deallocate(input)
    end subroutine

    subroutine ssa_finalize(input)
        type(ir_instruction), pointer :: input

        type(ir_instruction), pointer :: current, tmp
        current => input
        do while (associated(current))
            tmp => current
            current => current%next
            deallocate(tmp)
        end do
    end subroutine
end module