module inst_select
   use include, only: SMALL, BIG, string, sitoa
   use ir_instructions, only: ir_instruction, INST_STATIC, ir_op_container
   use ir, only: full_ir, ir_procedure, ir_block, operand_ssa_var, operand_asm_reg
   implicit none (type, external)

   type :: backend_info
      integer :: reg_count = 26
      integer :: arg_count = 8
      integer :: caller_count = 10
   end type

contains
   function register_name(index) result(reg)
      integer(SMALL), intent(in) :: index
      character(:), allocatable :: reg

      select case (index)
      case (0)
         result = 'SP'
      case default
         result = 'R'//sitoa(index - 1)
      end select
   end function

   subroutine instruction_select(info, input)
      type(backend_info), intent(inout) :: info
      type(full_ir), target, intent(inout) :: input
      
      integer(BIG) :: i

      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'malformed input to instruction selection'
         type is (ir_procedure)
            call instruction_select_proc(input, proc)
         end select
      end do
   end subroutine

   subroutine instruction_select_proc(info, input, proc)
      type(backend_info), intent(inout) :: info
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc

      integer(BIG) :: i
      type(ir_block), pointer :: blk
      class(*), allocatable :: temp_inst
      type(operand_ssa_var) :: ssa_op
      type(operand_asm_reg) :: reg_op
      integer(BIG) :: value_size

      do i = 1, proc%blocks%size
         blk => proc%get_block(input, i)
         if (i == 1) then
            ! insert preamble for calling convention
            do j = 1, size(proc%arguments)
               ssa_op%idx = j
               reg_op%index = j
               temp_inst = ir_instruction(INST_STATIC, [ir_op_container(ssa_op)], [ir_op_container(reg_op)], proc%loc)
               call blk%content%move_insert(1_BIG, temp_inst)
            end do
            call instruction_select_block(blk, proc)
         end if
      end do
   end subroutine

   subroutine instruction_select_block(info, input, proc, blk)
      type(backend_info), intent(inout) :: info
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      type(ir_block), pointer, intent(inout) :: blk

      integer(BIG) :: i

      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'malformed block'
         type is (ir_instruction)
            select case (inst%inst_type)
            case (INST_PHI)
               ! insert phis into parent nodes
            case (INST_ASSIGN)
            case (INST_CALL)
               ! handle calling convention
            case (INST_CAST)
            case (INST_GET)
               ! load
            case (INST_SET)
               ! store
            end select
         end select
      end do
   end subroutine
end module
