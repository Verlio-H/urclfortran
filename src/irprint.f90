submodule (irgen) irprint
    use include, only: rtoa, ltoa, itoa2
    implicit none

contains
    module recursive subroutine ir_print(input, printed)
        type(ir), pointer, intent(in) :: input
        type(carr), pointer, optional, intent(in) :: printed

        type(carr), pointer :: actual_printed
        integer :: i

        write(*, '(A)') input%name//':'
        if (allocated(input%children)) then
            write(*, '(A)') '  children:'
            do i = 1, size(input%children)
                write(*, '(A)', advance='no') '    '//input%children(i)%ptr%name
                if (input%children_dup(i)) then
                    write(*, '(A)') ' (duplicated)'
                else
                    write(*, '(A)') ''
                end if
            end do
        end if
        if (allocated(input%parents)) then
            write(*, '(A)') '  parents:'
            do i = 1, size(input%parents)
                write(*, '(A)') '    '//input%parents(i)%ptr%name
            end do
        end if
        if (allocated(input%functions)) then
            write(*, '(A)') '  functions:'
            do i = 1, size(input%functions)
                write(*, '(A)') '    '//input%functions(i)%ptr%name
            end do
        end if
        if (allocated(input%variables)) then
            write(*, '(A)') '  variables:'
            do i = 1, size(input%variables)
                write(*, '(A)') '    '//input%variables(i)%var%name//': '//itoa2(input%variables(i)%var%vartype%kind)
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
            write(*, '(A)') '  instructions:'
            call ssa_print(input%instruction)
        end if
        if (allocated(input%children)) then
            call ir_print_children(input%children, actual_printed)
        end if

        if (allocated(input%functions)) then
            call ir_print_children(input%functions, actual_printed)
        end if
    end subroutine

    recursive subroutine ir_print_children(children, printed)
        type(ir_ptr), intent(in) :: children(:)
        type(carr), pointer :: printed

        integer :: i, j
        logical :: printir
        do i = 1, size(children)
            printir = .true.
            do j = 1, printed%size - 1
                if (children(i)%ptr%name == printed%array(j)%value) then
                    printir = .false.
                end if
            end do
            if (printir) call ir_print(children(i)%ptr, printed)
        end do
    end subroutine

    recursive subroutine ssa_print(input)
        type(ir_instruction), pointer :: input

        integer(SMALL) :: i

        select case (input%instruction)
        case (OP_NOP)
            write(*, '(A)', advance='no') '    nop'
        case (OP_MOV)
            write(*, '(A)', advance='no') '    mov'
        case (OP_ADD)
            write(*, '(A)', advance='no') '    add'
        case (OP_SUB)
            write(*, '(A)', advance='no') '    sub'
        case (OP_MLT)
            write(*, '(A)', advance='no') '    mlt'
        case (OP_UMLT)
            write(*, '(A)', advance='no') '    umlt'
        case (OP_DIV)
            write(*, '(A)', advance='no') '    div'
        case (OP_CAST)
            write(*, '(A)', advance='no') '    cast'
        case (OP_SETL)
            write(*, '(A)', advance='no') '    setl'
        case (OP_SSETL)
            write(*, '(A)', advance='no') '    ssetl'
        case (OP_EQ)
            write(*, '(A)', advance='no') '    eq'
        case (OP_NE)
            write(*, '(A)', advance='no') '    ne'
        case (OP_LT)
            write(*, '(A)', advance='no') '    lt'
        case (OP_LE)
            write(*, '(A)', advance='no') '    le'
        case (OP_GT)
            write(*, '(A)', advance='no') '    gt'
        case (OP_GE)
            write(*, '(A)', advance='no') '    ge'
        case (OP_NOT)
            write(*, '(A)', advance='no') '    not'
        case (OP_AND)
            write(*, '(A)', advance='no') '    and'
        case (OP_OR)
            write(*, '(A)', advance='no') '    or'
        case (OP_PSH)
            write(*, '(A)', advance='no') '    psh'
        case (OP_STR)
            write(*, '(A)', advance='no') '    str'
        case (OP_LOD)
            write(*, '(A)', advance='no') '    lod'
        case (OP_CALL)
            write(*, '(A)', advance='no') '    call'
        case (OP_END)
            write(*, '(A)', advance='no') '    end'
        case (OP_ADRLV)
            write(*, '(A)', advance='no') '    adrlv'
        case (OP_ADRGV)
            write(*, '(A)', advance='no') '    adrgv'
        case (OP_STRLV)
            write(*, '(A)', advance='no') '    strlv'
        case (OP_STRGV)
            write(*, '(A)', advance='no') '    strgv'
        case (OP_LODLV)
            write(*, '(A)', advance='no') '    lodlv'
        case (OP_LODGV)
            write(*, '(A)', advance='no') '    lodgv'
        case (OP_ASOCMEM)
            write(*, '(A)', advance='no') '    d'
        case (OP_BR)
            write(*, '(A)', advance='no') '    br'
        end select
        if (allocated(input%operands)) then
            do i = 1, int(size(input%operands), SMALL)
                select case (input%operands(i)%type)
                case (V_NONE)
                    write(*, '(A)', advance='no') ' none'
                case (V_BP)
                    write(*, '(A)', advance='no') ' bp'
                case (V_SP)
                    write(*, '(A)', advance='no') ' sp'
                case (V_IMM)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end if
                    select type (val => input%operands(i)%value)
                    type is (integer)
                        write(*, '(A)', advance='no') ' i'//itoa(val)//'_'//itoa2(input%operands(i)%kind)
                    type is (real)
                        write(*, '(A)', advance='no') ' r'//rtoa(val)//'_'//itoa2(input%operands(i)%kind)
                    type is (logical)
                        write(*, '(A)', advance='no') ' l'//ltoa(val)//'_'//itoa2(input%operands(i)%kind)
                    class default
                        call throw('error in ir', 'unknown', 0_SMALL,0_SMALL)
                    end select
                case (V_VAR)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end if
                    select type (val => input%operands(i)%value)
                    type is (integer)
                        write(*, '(A)', advance='no') ' %'//itoa(val)//'_'//itoa2(input%operands(i)%kind)
                    class default
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end select
                case (V_SYMB)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end if
                    select type (val => input%operands(i)%value)
                    type is (character(*))
                        write(*, '(A)', advance='no') ' '//val
                    class default
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end select
                case (V_STR)
                    if (.not.allocated(input%operands(i)%value)) then
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end if
                    select type (val => input%operands(i)%value)
                    type is (character(*))
                        write(*, '(A)', advance='no') ' s"'//val//'"'
                    class default
                        call throw('error in ir', 'unknown', 0_SMALL, 0_SMALL)
                    end select
                end select
            end do
        end if
        write(*, '(A)') ''
        if (associated(input%next)) call ssa_print(input%next)
    end subroutine
end submodule
