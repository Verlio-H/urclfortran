module irgen
    use semantic
    implicit none

    integer(SMALL), parameter :: OP_NOP = 0
    integer(SMALL), parameter :: OP_MOV = 1
    integer(SMALL), parameter :: OP_ADD = 2
    integer(SMALL), parameter :: OP_PSH = 2000
    integer(SMALL), parameter :: OP_STR = 2001
    integer(SMALL), parameter :: OP_LOD = 2002
    integer(SMALL), parameter :: OP_CALL = 2003
    integer(SMALL), parameter :: OP_END = 3000

    integer(SMALL), parameter :: V_NONE = 0
    integer(SMALL), parameter :: V_IMM = 1
    integer(SMALL), parameter :: V_VAR = 2
    integer(SMALL), parameter :: V_SYMB = 3
    integer(SMALL), parameter :: V_BP = 4

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
        logical :: global
    end type
contains
    subroutine gen_ir(tree,symbols,result,wordsize,ptrsize)
        type(ast), intent(in) :: tree
        type(sem_module), intent(in) :: symbols(:)
        type(ir_ptr), allocatable, intent(inout) :: result(:)
        integer(SMALL), intent(in) :: wordsize
        integer(SMALL), intent(in) :: ptrsize
        
        integer :: currnum
        integer :: curroffset
        integer :: i
        integer :: symbolidx
        type(ir_instruction), pointer :: current_instruction
        currnum = 1
        curroffset = 0
        ! assumes root is at index 1
        
        ! allocate(result(1))
        ! allocate(result(1)%ptr)
        ! call internal_gen_test_ir(currnum,result(1)%ptr)
        ! return

        symbolidx = 0

        nullify(current_instruction)
        allocate(result(tree%nodes(1)%subnodes%size-1))
        do i=1,tree%nodes(1)%subnodes%size-1
            allocate(result(i)%ptr)
            call internal_gen_ir(tree,tree%nodes(1)%subnodes%array(i),currnum,curroffset,symbols,symbolidx,result(i)%ptr, &
                current_instruction,wordsize,ptrsize)
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

    recursive subroutine internal_gen_ir(tree,currnode,currnum,curroffset,symbols,symbolidx,result_block,current_instruction,&
     wordsize,ptrsize)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        integer, intent(inout) :: curroffset
        type(sem_module), intent(in) :: symbols(:)
        integer, intent(inout) :: symbolidx
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer(SMALL), intent(in) :: wordsize
        integer(SMALL), intent(in) :: ptrsize

        integer :: i
        type(type) :: tmptype

        associate (node=>tree%nodes(currnode))
            select case (node%type)

            ! do stuff with node
            case (NODE_PROGRAM,NODE_MODULE)
                result_block%name = trim(node%value)
                do symbolidx=1,size(symbols)
                    if (symbols(symbolidx)%name==result_block%name) exit
                end do
                if (symbolidx>size(symbols)) then
                    call throw("symbol table does not exist for module "//result_block%name, "unknown", 0_2, 0_2, .true.)
                end if

                if (allocated(node%subnodes%array)) then
                    allocate(result_block%functions(node%subnodes%size-1))
                end if
                do i=1,node%subnodes%size-1
                    allocate(result_block%functions(i)%ptr)
                    associate(new_node=>node%subnodes%array(i))
                        call internal_gen_ir(tree,new_node,currnum,curroffset,symbols,symbolidx,result_block%functions(i)%ptr, &
                            current_instruction,wordsize,ptrsize)
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
                    result_block%variables(i)%var%offset = 1+(i-1)*ptrsize
                end do
                nullify(result_block%instruction)
            case (NODE_TYPE,NODE_USE,NODE_ASSIGNMENT)
            case default
                print*,node%type
            end select

            ! do stuff with instructions
            select case (node%type)
            case (NODE_PROGRAM,NODE_SUBROUTINE)
                if (allocated(node%subnodes2%array)) then
                    allocate(result_block%instruction)
                    current_instruction => result_block%instruction
                    curroffset = 0
                    do i=1,node%subnodes2%size-1
                        call internal_gen_ir(tree,node%subnodes2%array(i),currnum,curroffset,symbols,symbolidx,result_block, &
                            current_instruction,wordsize,ptrsize)
                    end do
                end if
            case (NODE_TYPE)
                ! check for pre existing
                i = 0
                if (allocated(result_block%variables)) then
                    do i=1,size(result_block%variables)
                        if (result_block%variables(i)%var%name==node%value) then
                            if (result_block%variables(i)%var%vartype%type/=TYPE_NONE) then
                                call throw('duplicate variable definition',node%fname,node%startlnum,node%startchar)
                            end if
                            exit ! occurs if variable is function argument
                        end if
                    end do
                end if
                if (i==0) then
                    allocate(result_block%variables(1))
                    i = 1
                    result_block%variables(i)%var%offset = 0
                else if (i==size(result_block%variables)+1) then
                    block
                        type(ssa_var), allocatable :: tmp(:)
                        call move_alloc(result_block%variables,tmp)
                        allocate(result_block%variables(size(tmp)+1))
                        result_block%variables(:size(tmp)) = tmp
                    end block
                    result_block%variables(i)%var%offset = 0
                end if
                result_block%variables(i)%var%name = trim(node%value)
                result_block%variables(i)%var%vartype = eval_type(tree,currnode,symbols(symbolidx))
                if (result_block%variables(i)%var%offset == 0) then
                    result_block%variables(i)%var%offset = -1-curroffset
                    if (wordsize == 16) then
                        curroffset = curroffset + result_block%variables(i)%var%vartype%kind/2
                    else if (wordsize == 8) then
                        curroffset = curroffset + result_block%variables(i)%var%vartype%kind
                    end if
                end if
            case (NODE_ASSIGNMENT)
                block
                    integer :: lresultvar,rresultvar
                    type(type) :: lresulttype,rresulttype
                    call internal_gen_rval_ir(tree,node%subnodes2%array(1),currnum,curroffset,symbols,symbolidx,result_block, &
                        current_instruction, rresultvar, rresulttype)
                    call internal_gen_lval_ir(tree,node%subnodes%array(1),currnum,curroffset,symbols,symbolidx,result_block, &
                        current_instruction, lresultvar,lresulttype)
                    allocate(current_instruction%next)
                    current_instruction => current_instruction%next
                    current_instruction%instruction = OP_STR
                    allocate(current_instruction%operands(2))
                    current_instruction%operands(1)%type = V_VAR
                    current_instruction%operands(1)%value = lresultvar
                    current_instruction%operands(2)%type = V_VAR
                    current_instruction%operands(2)%value = rresultvar
                    nullify(current_instruction%next)
                end block
            case (NODE_USE,NODE_CALL)
                nullify(current_instruction%next)
            end select
        end associate
    end subroutine

    recursive subroutine internal_gen_lval_ir(tree,currnode,currnum,curroffset,symbols,symbolidx,result_block,current_instruction,&
     result,resulttype)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        integer, intent(inout) :: curroffset
        type(sem_module), intent(in) :: symbols(:)
        integer, intent(inout) :: symbolidx
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer, intent(out) :: result
        type(type), intent(out) :: resulttype

        integer :: i
        
        associate (node=>tree%nodes(currnode))
            select case (node%type)
            case (NODE_STRING)
                ! search for variable
                do i=1,size(result_block%variables)
                    if (result_block%variables(i)%var%name == node%value) then
                        result = result_block%variables(i)%var%offset
                        allocate(current_instruction%next)
                        current_instruction => current_instruction%next
                        current_instruction%instruction = OP_ADD
                        allocate(current_instruction%operands(3))
                        current_instruction%operands(2)%type = V_IMM
                        current_instruction%operands(2)%value = result
                        current_instruction%operands(1)%type = V_VAR
                        current_instruction%operands(1)%value = currnum
                        current_instruction%operands(3)%type = V_BP
                        result = currnum
                        currnum = currnum + 1
                        resulttype = result_block%variables(i)%var%vartype
                        return
                    end if
                end do
                do i=1,size(symbols(symbolidx)%vartbl)
                    if (symbols(symbolidx)%vartbl(i)%name == node%value) then
                        result = symbols(symbolidx)%vartbl(i)%offset ! TODO: globals
                        allocate(current_instruction%next)
                        current_instruction => current_instruction%next
                        current_instruction%instruction = OP_ADD
                        allocate(current_instruction%operands(3))
                        current_instruction%operands(2)%type = V_IMM
                        current_instruction%operands(2)%value = result
                        current_instruction%operands(1)%type = V_VAR
                        current_instruction%operands(1)%value = currnum
                        current_instruction%operands(3)%type = V_VAR
                        current_instruction%operands(3)%value = 0
                        result = currnum
                        currnum = currnum + 1
                        resulttype = symbols(symbolidx)%vartbl(i)%vartype
                        return
                    end if
                end do
                if (i == size(symbols(symbolidx)%vartbl) + 1) then
                    call throw('unknown variable name: '//trim(node%value),'unknown',0_SMALL,0_SMALL,.true.)
                end if
            case default
                print*,node%type
            end select
        end associate
    end subroutine

    recursive subroutine internal_gen_rval_ir(tree,currnode,currnum,curroffset,symbols,symbolidx,result_block,current_instruction,&
     result,resulttype)
        type(ast), intent(in) :: tree
        integer, intent(in) :: currnode
        integer, intent(inout) :: currnum
        integer, intent(inout) :: curroffset
        type(sem_module), intent(in) :: symbols(:)
        integer, intent(inout) :: symbolidx
        type(ir), pointer, intent(inout) :: result_block
        type(ir_instruction), pointer, intent(inout) :: current_instruction
        integer, intent(out) :: result
        type(type), intent(out) :: resulttype

        integer :: i, j, k
        integer, allocatable :: ops(:)
        integer :: result1, result2
        type(type) :: resulttype1, resulttype2
        associate (node=>tree%nodes(currnode))
            select case (node%type)
            case (NODE_ADD)
                call internal_gen_rval_ir(tree,node%subnodes%array(1),currnum,curroffset,symbols,symbolidx,result_block, &
                    current_instruction,result1,resulttype1)
                call internal_gen_rval_ir(tree,node%subnodes2%array(1),currnum,curroffset,symbols,symbolidx,result_block, &
                    current_instruction,result2,resulttype2)
                allocate(current_instruction%next)
                current_instruction => current_instruction%next
                current_instruction%instruction = OP_ADD
                allocate(current_instruction%operands(3))
                current_instruction%operands(2)%type = V_VAR
                current_instruction%operands(2)%value = result1
                current_instruction%operands(3)%type = V_VAR
                current_instruction%operands(3)%value = result2
                current_instruction%operands(1)%type = V_VAR
                current_instruction%operands(1)%value = currnum
                result = currnum
                currnum = currnum + 1
                resulttype = resulttype1
            case (NODE_INT_VAL)
                allocate(current_instruction%next)
                current_instruction => current_instruction%next
                current_instruction%instruction = OP_MOV
                allocate(current_instruction%operands(2))
                current_instruction%operands(1)%type = V_VAR
                current_instruction%operands(1)%value = currnum
                result = currnum
                currnum = currnum + 1
                current_instruction%operands(2)%type = V_IMM
                current_instruction%operands(2)%value = atoi(node%value)
                resulttype%kind = 4 !TODO: Fix integer value kinds
                resulttype%type = TYPE_INTEGER
                nullify(current_instruction%next)
            case (NODE_STRING)
                ! search for variable
                do i=1,size(result_block%variables)
                    if (result_block%variables(i)%var%name == node%value) then
                        result = result_block%variables(i)%var%offset
                        allocate(current_instruction%next)
                        current_instruction => current_instruction%next
                        current_instruction%instruction = OP_ADD
                        allocate(current_instruction%operands(3))
                        current_instruction%operands(2)%type = V_IMM
                        current_instruction%operands(2)%value = result
                        current_instruction%operands(1)%type = V_VAR
                        current_instruction%operands(1)%value = currnum
                        current_instruction%operands(3)%type = V_BP

                        allocate(current_instruction%next)
                        current_instruction => current_instruction%next
                        current_instruction%instruction = OP_LOD
                        allocate(current_instruction%operands(2))
                        current_instruction%operands(2)%type = V_VAR
                        current_instruction%operands(2)%value = currnum
                        currnum = currnum + 1
                        current_instruction%operands(1)%type = V_VAR
                        current_instruction%operands(1)%value = currnum

                        result = currnum
                        currnum = currnum + 1
                        resulttype = result_block%variables(i)%var%vartype
                        return
                    end if
                end do
                do i=1,size(symbols(symbolidx)%vartbl)
                    if (symbols(symbolidx)%vartbl(i)%name == node%value) then
                        if (iand(symbols(symbolidx)%vartbl(i)%vartype%properties,int(PROP_PARAMETER,SMALL)) /= 0) then
                            allocate(current_instruction%next)
                            current_instruction => current_instruction%next
                            current_instruction%instruction = OP_MOV
                            allocate(current_instruction%operands(2))
                            current_instruction%operands(2)%type = V_IMM
                            current_instruction%operands(2)%value = symbols(symbolidx)%vartbl(i)%value
                            current_instruction%operands(1)%type = V_VAR
                            current_instruction%operands(1)%value = currnum
                            result = currnum
                            currnum = currnum + 1
                            resulttype = symbols(symbolidx)%vartbl(i)%vartype
                            return
                        end if
                        allocate(current_instruction%next)
                        current_instruction => current_instruction%next
                        current_instruction%instruction = OP_ADD
                        allocate(current_instruction%operands(3))
                        current_instruction%operands(2)%type = V_IMM
                        current_instruction%operands(2)%value = symbols(symbolidx)%vartbl(i)%offset
                        current_instruction%operands(1)%type = V_VAR
                        current_instruction%operands(1)%value = currnum
                        current_instruction%operands(3)%type = V_VAR
                        current_instruction%operands(3)%value = 0
                        
                        allocate(current_instruction%next)
                        current_instruction => current_instruction%next
                        current_instruction%instruction = OP_LOD
                        allocate(current_instruction%operands(2))
                        current_instruction%operands(2)%type = V_VAR
                        current_instruction%operands(2)%value = currnum
                        currnum = currnum + 1
                        current_instruction%operands(1)%type = V_VAR
                        current_instruction%operands(1)%value = currnum
                        result = currnum
                        currnum = currnum + 1
                        resulttype = symbols(symbolidx)%vartbl(i)%vartype
                        return
                    end if
                end do
                if (i == size(symbols(symbolidx)%vartbl) + 1) then
                    call throw('unknown variable name: '//trim(node%value),'unknown',0_SMALL,0_SMALL,.true.)
                end if
            case (NODE_FNC_ARR)
                ! TODO: arrays
                do i=1,size(symbols(symbolidx)%functbl)
                    associate(inter => symbols(symbolidx)%functbl(i))
                        if (inter%name == node%value) then
                            do j=1,size(inter%functions)
                                associate(func => inter%functions(j))
                                    if (.not.func%subrout .and. func%name == node%value) then
                                        ! func found
                                        allocate(ops(node%subnodes%size-1))
                                        do k=1,node%subnodes%size-1
                                            call internal_gen_rval_ir(tree,node%subnodes%array(k),currnum,curroffset,symbols, &
                                                symbolidx,result_block,current_instruction,result1,resulttype1)
                                            ops(k) = result1
                                        end do
                                        allocate(current_instruction%next)
                                        current_instruction => current_instruction%next
                                        current_instruction%instruction = OP_CALL
                                        allocate(current_instruction%operands(node%subnodes%size+1))
                                        current_instruction%operands(1)%type = V_VAR
                                        current_instruction%operands(1)%value = currnum
                                        result = currnum
                                        resulttype = func%return%vartype
                                        current_instruction%operands(2)%type = V_SYMB
                                        current_instruction%operands(2)%value = '_'//func%name
                                        do k=1,size(ops)
                                            current_instruction%operands(k+2)%type = V_VAR
                                            current_instruction%operands(k+2)%value = ops(k)
                                        end do
                                    end if
                                end associate
                            end do
                        end if
                    end associate
                end do
                select case (trim(node%value))
                case ('NINT')
                    call internal_gen_rval_ir(tree,node%subnodes%array(1),currnum,curroffset,symbols,symbolidx,result_block, &
                        current_instruction,result1,resulttype1)
                    allocate(current_instruction%next)
                    current_instruction => current_instruction%next
                    current_instruction%instruction = OP_CALL
                    allocate(current_instruction%operands(3))
                    current_instruction%operands(1)%type = V_VAR
                    current_instruction%operands(1)%value = currnum
                    result = currnum
                    currnum = currnum + 1
                    resulttype%type = TYPE_INTEGER
                    resulttype%kind = 4
                    current_instruction%operands(2)%type = V_SYMB
                    current_instruction%operands(2)%value = '_flib_nint'
                    current_instruction%operands(3)%type = V_VAR
                    current_instruction%operands(3)%value = result1
                    return
                end select
                call throw('unknown function/array name: '//trim(node%value),'unknown',0_SMALL,0_SMALL,.true.)
            case default
                print*,node%type
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
                write(*,'(A)') '    '//input%variables(i)%var%name//': '//itoa(input%variables(i)%var%offset)
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
        case (OP_STR)
            write(*,'(A)',advance='no') '    str'
        case (OP_LOD)
            write(*,'(A)',advance='no') '    lod'
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
                case (V_BP)
                    write(*,'(A)',advance='no') ' bp'
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