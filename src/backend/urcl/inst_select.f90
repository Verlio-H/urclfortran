module urcl_inst_select
   use include, only: SMALL, BIG, string, sitoa
   use ir_instructions, only: ir_instruction, ir_op_container, INST_PHI, INST_ASSIGN, INST_CALL, INST_CAST, INST_GET, INST_SET
   use ir, only: full_ir, ir_procedure, ir_block, operand_ssa_var, operand_asm_reg
   use data_mod, only: list
   implicit none (type, external)

contains
   function register_name(index) result(reg)
      integer(SMALL), intent(in) :: index
      character(:), allocatable :: reg

      select case (index)
      case (0)
         reg = 'SP'
      case default
         reg = 'R'//sitoa(index - 1_SMALL)
      end select
   end function

   subroutine instruction_select(bits, args, caller, callee, input, reg_associations)
      integer(SMALL), intent(in) :: bits, args, caller, callee
      type(full_ir), target, intent(inout) :: input
      type(list), intent(inout) :: reg_associations(:)
      
      integer(BIG) :: i

      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'malformed input to instruction selection'
         type is (ir_procedure)
            call instruction_select_proc(bits, args, caller, callee, input, proc, reg_associations(i))
         end select
      end do
   end subroutine

   subroutine instruction_select_proc(bits, args, caller, callee, input, proc, reg_associations)
      integer(SMALL), intent(in) :: bits, args, caller, callee
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      type(list), intent(inout) :: reg_associations

      integer(BIG) :: i
      integer(SMALL) :: j
      type(ir_block), pointer :: blk
      class(*), allocatable :: temp_inst
      type(operand_ssa_var) :: ssa_op
      type(operand_asm_reg) :: reg_op
      integer(BIG) :: value_size


      do i = 1, proc%blocks%size
         blk => proc%get_block(input, i)
         if (i == 1) then
            ! insert preamble for calling convention
            ! assume no extra stack allocations for now
            do j = 1, size(proc%arguments)
               call reg_associations%set(int(proc%ssa_arguments(j), BIG), j)
            end do
         end if
         call instruction_select_block(input, proc, blk)
      end do
   end subroutine

   subroutine instruction_select_block(input, proc, blk)
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      type(ir_block), pointer, intent(inout) :: blk

      type(list) :: new_content
      integer(BIG) :: i
      class(*), allocatable :: tmp_allocatable

      new_content = list(ir_instruction())
      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'malformed block'
         type is (ir_instruction)
            select case (inst%inst_type)
            !case (INST_ASSIGN)
            !case (INST_CALL)
               ! handle calling convention
            !case (INST_CAST)
            !case (INST_GET)
               ! load
            !case (INST_SET)
               ! store
            case default
               ! copy over
               tmp_allocatable = blk%content%move_get(i)
               call new_content%move_push(tmp_allocatable)
            end select
         end select
      end do

      call new_content%move(blk%content)
   end subroutine
end module
