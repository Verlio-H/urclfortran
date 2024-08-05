module irgen
    use semantic
    implicit none

    integer(SMALL), parameter :: OP_NOP = 0
    integer(SMALL), parameter :: OP_MOV = 1
    integer(SMALL), parameter :: OP_ADD = 2
    integer(SMALL), parameter :: OP_PSH = 2000
    integer(SMALL), parameter :: OP_CALL = 2001
    integer(SMALL), parameter :: OP_END = 3000

    integer(SMALL), parameter :: V_NONE = 0
    integer(SMALL), parameter :: V_IMM = 1
    integer(SMALL), parameter :: V_VAR = 2
    integer(SMALL), parameter :: V_SYMB = 3

    integer(SMALL), parameter :: BLOCK_ROOT = 0
    integer(SMALL), parameter :: BLOCK_PROGRAM = 1
    integer(SMALL), parameter :: BLOCK_SUBROUTINE = 2


    type operand
        integer(SMALL) :: type
        class(*), allocatable :: value
    end type

    type ir_instruction
        integer(SMALL) :: instruction = 0
        type(operand), allocatable :: operands(:)
        type(ir_instruction), pointer :: next
    end type

    type ir ! reference counted
        integer(SMALL) :: counter = 1
        integer(SMALL) :: block_type = 0
        character(len=:), allocatable :: name
        type(ir_instruction), pointer :: instruction
        type(ir_ptr), allocatable :: children(:)
        type(ir_ptr), allocatable :: parents(:)
        type(ir_ptr), allocatable :: functions(:)
        type(ssa_var), allocatable :: variables(:)
    end type

    type ir_ptr
        type(ir), pointer :: ptr
    end type


    type ssa_var
        type(sem_variable) :: var
        integer :: number
        logical :: global
    end type
contains
    subroutine gen_ir(tree,symbols,result)
        type(ast), intent(in) :: tree
        type(sem_module), intent(in) :: symbols(:)
        type(ir_ptr), allocatable, intent(inout) :: result(:)
        
        integer :: currnum
        integer :: i
        type(ir_instruction), pointer :: current_instruction
        currnum = 0
        ! assumes root is at index 1
        
        nullify(current_instruction)
        allocate(result(tree%nodes(1)%subnodes%size-1))
        do i=1,tree%nodes(1)%subnodes%size-1
            allocate(result(i)%ptr)
            call internal_gen_ir(tree,tree%nodes(1)%subnodes%array(i),currnum,symbols,result(i)%ptr,current_instruction)
        end do

    end subroutine

    recursive subroutine internal_gen_test_ir(currnum,result)
        integer, intent(inout) :: currnum
        type(ir), pointer, intent(inout) :: result

        type(ir_instruction), pointer :: current

        allocate(result%instruction)
        current => result%instruction

        result%block_type = BLOCK_PROGRAM
        current%instruction = OP_MOV
        result%name = 'e'

        allocate(current%operands(2))

        current%operands(1)%type = V_VAR
        current%operands(1)%value = currnum
        currnum = currnum + 1
        current%operands(2)%type = V_IMM
        current%operands(2)%value = 4
        nullify(current%next)
        allocate(result%children(1))
        allocate(result%children(1)%ptr,source=ir(instruction=NULL()))
        allocate(result%children(1)%ptr%instruction)
        current => result%children(1)%ptr%instruction
        result%children(1)%ptr%counter = 1
        result%children(1)%ptr%name = 'f'
        result%children(1)%ptr%block_type = BLOCK_SUBROUTINE
        current%instruction = OP_ADD
        !if (allocated(current%operands)) deallocate(current%operands) !gfortran compiler bug
        allocate(current%operands(3))
        !allocate(current%operands(3))
        current%operands(1)%type = V_VAR
        current%operands(1)%value = currnum
        currnum = currnum + 1
        current%operands(2)%type = V_VAR
        current%operands(2)%value = currnum - 2
        current%operands(3)%type = V_IMM
        current%operands(3)%value = 3
        nullify(current%next)
    end subroutine

    recursive subroutine internal_gen_ir(tree,currnode,currnum,symbols,result_block,current_instruction)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        type(sem_module), intent(in) :: symbols(:)
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction

        integer :: i

        associate (node=>tree%nodes(currnode))
            select case (node%type)

            ! do stuff with node
            case (NODE_PROGRAM,NODE_MODULE)
                result_block%name = trim(node%value)
                if (allocated(node%subnodes%array)) then
                    allocate(result_block%functions(node%subnodes%size-1))
                end if
                do i=1,node%subnodes%size-1
                    allocate(result_block%functions(i)%ptr)
                    associate(new_node=>node%subnodes%array(i))
                        call internal_gen_ir(tree,new_node,currnum,symbols,result_block%functions(i)%ptr,current_instruction)
                    end associate
                end do
                nullify(result_block%instruction)
            case (NODE_SUBROUTINE)
                result_block%name = trim(node%value)
                if (node%subnodes%size/=0) then
                    allocate(result_block%variables(node%subnodes%size-1))
                end if
                do i=1,node%subnodes%size-1
                    ! TODO: implicit type
                    result_block%variables(i)%var%name = trim(tree%nodes(node%subnodes%array(i))%value)
                    result_block%variables(i)%var%vartype%type = TYPE_NONE
                    result_block%variables(i)%number = currnum
                    currnum = currnum + 1
                end do
                nullify(result_block%instruction)
            case (NODE_TYPE)
            case default
                print*,node%type
            end select

            ! do stuff with instructions
            select case (node%type)
            case (NODE_PROGRAM,NODE_SUBROUTINE)
                if (allocated(node%subnodes2%array)) then
                    allocate(result_block%instruction)
                    current_instruction => result_block%instruction
                    do i=1,node%subnodes2%size-1
                        call internal_gen_ir(tree,node%subnodes2%array(i),currnum,symbols,result_block,current_instruction)
                    end do
                end if
            case (NODE_TYPE)
                ! check for pre existing
                if (allocated(result_block%variables)) then
                    do i=1,size(result_block%variables)
                    end do
                end if
            case (NODE_ASSIGNMENT,NODE_USE,NODE_CALL)
                nullify(current_instruction%next)
            end select
        end associate
    end subroutine

    recursive subroutine ir_print(input,printed)
        type(ir), pointer :: input
        type(carr), pointer, optional :: printed

        type(carr), pointer :: actual_printed
        integer :: i

        write(*,'(A)') input%name//':'
        if (allocated(input%children)) then
            write(*,'(A)') '  children:'
            do i=1,size(input%children)
                write(*,'(A)') '    '//input%children(i)%ptr%name
            end do
        end if
        if (allocated(input%parents)) then
            write(*,'(A)') '  parents:'
            do i=1,size(input%parents)
                write(*,'(A)') '    '//input%parents(i)%ptr%name
            end do
        end if
        if (allocated(input%functions)) then
            write(*,'(A)') '  functions:'
            do i=1,size(input%functions)
                write(*,'(A)') '    '//input%functions(i)%ptr%name
            end do
        end if
        if (allocated(input%variables)) then
            write(*,'(A)') '  variables:'
            do i=1,size(input%variables)
                write(*,'(A)') '    '//input%variables(i)%var%name//': '//itoa(input%variables(i)%number)
            end do
        end if
        if (present(printed)) then
            call printed%append(input%name)
            actual_printed => printed
        else
            allocate(actual_printed)
            call actual_printed%append(input%name)
        end if
        if (associated(input%instruction)) then
            write(*,'(A)') '  instructions:'
            call ssa_print(input%instruction)
        end if
        if (allocated(input%children)) then
            call ir_print_children(input%functions,actual_printed)
        end if

        if (allocated(input%functions)) then
            call ir_print_children(input%functions,actual_printed)
        end if



    end subroutine

    recursive subroutine ir_print_children(children,printed)
        type(ir_ptr), intent(in) :: children(:)
        type(carr), pointer :: printed

        integer :: i,j
        do i=1,size(children)
            do j=1,printed%size-1
                if (children(i)%ptr%name/=printed%array(j)%value) then
                    call ir_print(children(i)%ptr,printed)
                end if
            end do
        end do
    end subroutine

    recursive subroutine ssa_print(input)
        type(ir_instruction), pointer :: input

        integer(SMALL) :: i
        select case (input%instruction)
        case (OP_NOP)
            write(*,'(A)',advance='no') '    nop'
        case (OP_MOV)
            write(*,'(A)',advance='no') '    mov'
        case (OP_ADD)
            write(*,'(A)',advance='no') '    add'
        case (OP_PSH)
            write(*,'(A)',advance='no') '    psh'
        case (OP_CALL)
            write(*,'(A)',advance='no') '    call'
        case (OP_END)
            write(*,'(A)',advance='no') '    end'
        end select
        if (allocated(input%operands)) then
            do i=1,int(size(input%operands),SMALL)
                select case (input%operands(i)%type)
                case (V_NONE)
                    write(*,'(A)',advance='no') ' none'
                case (V_IMM)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir','unknown',0_2,0_2)
                    end if
                    select type (val=>input%operands(i)%value)
                    type is (integer)
                        write(*,'(A)',advance='no') ' i'//itoa(val)
                    type is (real)
                        write(*,'(A)',advance='no') ' r'//rtoa(val)
                    class default
                        call throw('error in ir','unknown',0_2,0_2)
                    end select
                case (V_VAR)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir','unknown',0_2,0_2)
                    end if
                    select type (val=>input%operands(i)%value)
                    type is (integer)
                        write(*,'(A)',advance='no') ' %'//itoa(val)
                    class default
                        call throw('error in ir','unknown',0_2,0_2)
                    end select
                case (V_SYMB)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir','unknown',0_2,0_2)
                    end if
                    select type (val=>input%operands(i)%value)
                    type is (character(len=*))
                        write(*,'(A)',advance='no') ' '//val
                    class default
                        call throw('error in ir','unknown',0_2,0_2)
                    end select
                end select
            end do
        end if
        write(*,'(A)') ''
        if (associated(input%next)) call ssa_print(input%next)
    end subroutine

    recursive subroutine ir_finalize(input)
        type(ir), pointer, intent(inout) :: input
        integer :: i
        if (allocated(input%children)) then
            do i=1,size(input%children)
                if (associated(input%children(i)%ptr)) then
                    call ir_finalize(input%children(i)%ptr)
                    nullify(input%children(i)%ptr)
                end if
            end do
        end if
        input%counter = input%counter - 1_2
        if (input%counter==0_2) then
            call ssa_finalize(input%instruction)
            deallocate(input)
        end if
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