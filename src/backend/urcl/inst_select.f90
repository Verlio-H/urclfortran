module urcl_inst_select
   use include, only: SMALL, BIG, string, sitoa, throw, bitoa
   use ir_instructions, only: ir_instruction, ir_op_container, INST_PHI, INST_ASSIGN, INST_CALL, INST_GET, INST_SET, &
      ir_operand
   use ir, only: full_ir, ir_procedure, ir_block, operand_ssa_var, operand_asm_reg, full_ir_type, operand_asm_instruction, &
      operand_comptime, operand_ir_var, comptime_addr, comptime_int
   use data_mod, only: list
   use urcl_init_ir, only: ASM_MOV, ASM_BSR, ASM_AND, ASM_BSL, ASM_OR, ASM_STR, ASM_LSTR, ASM_LOD, ASM_LLOD
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

   subroutine instruction_select(bits, args, caller, callee, input, reg_associations, associations)
      integer(SMALL), intent(in) :: bits, args, caller, callee
      type(full_ir), target, intent(inout) :: input
      type(list), intent(inout) :: reg_associations(:)
      type(list), intent(inout) :: associations(:)
      
      integer(BIG) :: i

      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'malformed input to instruction selection'
         type is (ir_procedure)
            call instruction_select_proc(bits, args, caller, callee, input, proc, reg_associations(i), associations(i))
         end select
      end do
   end subroutine

   subroutine instruction_select_proc(bits, args, caller, callee, input, proc, reg_assoc, associations)
      integer(SMALL), intent(in) :: bits, args, caller, callee
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      type(list), intent(inout) :: reg_assoc
      type(list), intent(inout) :: associations

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
               call reg_assoc%set(int(proc%ssa_arguments(j), BIG), j)
            end do
         end if
         call instruction_select_block(input, proc, blk, associations, bits)
      end do
   end subroutine

   subroutine insert_aggregate_inst(output, proc, l_op, r_op, l_bits, r_bits, assoc, r_shift, l_shift, force_temp)
      type(list), intent(inout) :: output
      type(ir_procedure), intent(inout) :: proc
      class(ir_operand), intent(in) :: l_op, r_op
      integer(BIG), intent(in) :: l_bits, r_bits
      type(list), intent(inout) :: assoc
      integer(BIG), intent(in) :: r_shift, l_shift
      logical, intent(in) :: force_temp

      class(*), allocatable :: instruction
      integer(BIG) :: ssa_idx, ssa_idx2
      logical :: insert_shift, insert_and

      insert_shift = r_shift /= 0
      insert_and = r_bits - abs(r_shift) > l_bits

      ! shift
      if (insert_shift) then
         if (insert_and .or. force_temp) then
            ! need to make an intermediate var
            ! TODO: use actual type
            call assoc%push(full_ir_type(type=1))
            ssa_idx = proc%ssa_counter
            proc%ssa_counter = proc%ssa_counter + 1
         end if
         ! bsr
         instruction = ir_instruction(inst_type=INST_CALL)
         select type (instruction)
         type is (ir_instruction)
            allocate(instruction%op1(1), instruction%op2(3))
            if (insert_and .or. force_temp) then
               instruction%op1(1)%val = operand_ssa_var(idx=ssa_idx)
            else
               instruction%op1(1)%val = l_op
            end if
            if (r_shift < 0) then
               instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_BSL))
            else
               instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_BSR))
            end if
            instruction%op2(2)%val = r_op
            instruction%op2(3)%val = operand_comptime(val=comptime_int(abs(r_shift)))
         end select
         call output%move_push(instruction)
      end if

      ! and
      if (insert_and) then
         if (force_temp) then
            ! TODO: use actual type
            call assoc%push(full_ir_type(type=1))
            ssa_idx2 = proc%ssa_counter
            proc%ssa_counter = proc%ssa_counter + 1
         end if
         instruction = ir_instruction(inst_type=INST_CALL)
         select type (instruction)
         type is (ir_instruction)
            allocate(instruction%op1(1), instruction%op2(3))
            if (force_temp) then
               instruction%op1(1)%val = operand_ssa_var(idx=ssa_idx)
            else
               instruction%op1(1)%val = l_op
            end if
            instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_AND))
            if (insert_shift) then
               instruction%op2(2)%val = operand_ssa_var(idx=ssa_idx)
            else
               instruction%op2(2)%val = r_op
            end if
            ! TODO: overflow issue
            instruction%op2(3)%val = operand_comptime(val=comptime_int(shiftl(2 ** l_bits - 1, l_shift)))
         end select
         call output%move_push(instruction)
      end if

      if (.not.(insert_shift .or. insert_and .or. force_temp)) then
         instruction = ir_instruction(inst_type=INST_CALL)
         select type (instruction)
         type is (ir_instruction)
            allocate(instruction%op1(1), instruction%op2(2))
            instruction%op1(1)%val = l_op
            instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_MOV))
            instruction%op2(2)%val = r_op
         end select
         call output%move_push(instruction)
      end if
   end subroutine

   recursive subroutine ssa_aggregate(output, input, proc, operand, inst, idx, offset, assoc, shift, isa_bits)
      type(list), intent(inout) :: output
      type(full_ir), intent(in) :: input
      type(ir_procedure), intent(inout) :: proc
      class(ir_operand), intent(in) :: operand
      type(ir_instruction), target, intent(inout) :: inst
      integer(BIG), intent(inout) :: idx
      integer(SMALL), intent(inout) :: offset
      type(list), intent(inout) :: assoc
      integer(BIG), intent(in) :: shift
      integer(SMALL), intent(in) :: isa_bits

      integer(BIG) :: l_bits, op_bits, r_bits, slice_lower, ssa_idx, counter_before, before_idx
      type(full_ir_type), pointer :: l_type
      class(*), allocatable :: instruction
      type(ir_op_container), pointer :: array2(:)

      select type (operand)
      class default
         error stop 'non ssa operand in ssa_aggregate'
      type is (operand_ssa_var)
         ! get the size of the output variable
         select type (type => assoc%get(int(operand%idx, BIG)))
         class default
            error stop 'invalid associations input to ssa_aggregate'
         type is (full_ir_type)
            l_bits = type%bit_count(input) - shift
            l_type => type
         end select

         ! get the size of the input variable
         select type (right_op => inst%op2(idx)%val)
         class default
            call throw('invalid argument type in assignment at index: '//bitoa(idx), inst%loc)
         type is (operand_comptime)
            ! TODO: remove when lfortran fixes bug
            array2 => inst%op2
            op_bits = l_bits
            r_bits = l_bits
            if (shift /= 0) then
               call throw('Cannot have constants in bit field combining scenarios', inst%loc)
            end if
            slice_lower = 0
         type is (operand_ir_var)
            op_bits = l_bits
            r_bits = l_bits
            if (shift /= 0) then
               call throw('Cannot have constants in bit field combining scenarios', inst%loc)
            end if
            slice_lower = 0
         type is (operand_ssa_var)
            select type (type => assoc%get(int(right_op%idx, BIG)))
            type is (full_ir_type)
               if (right_op%slice) then
                  op_bits = right_op%uoffset - (right_op%loffset + offset) + 1
                  right_op%slice = .false.
                  slice_lower = right_op%loffset + offset - shift
                  r_bits = type%bit_count(input)
               else
                  op_bits = type%bit_count(input) - offset
                  slice_lower = offset - shift
                  r_bits = op_bits
               end if
            end select
         end select

         if (slice_lower < 0) r_bits = isa_bits

         if (op_bits == l_bits) then
            call insert_aggregate_inst(output, proc, operand, inst%op2(idx)%val, l_bits, r_bits, assoc, slice_lower, shift, .false.)
            idx = idx + 1
            offset = 0
         else if (op_bits > l_bits) then
            call insert_aggregate_inst(output, proc, operand, inst%op2(idx)%val, l_bits, r_bits, assoc, slice_lower, shift, .false.)
            offset = offset + l_bits
         else ! op_bits < bits
            ! TODO: make a more efficient tree combining structure

            ! make temp for the rest
            call assoc%push(l_type)
            ssa_idx = proc%ssa_counter
            proc%ssa_counter = proc%ssa_counter + 1
            before_idx = idx
            idx = idx + 1
            offset = 0
            call ssa_aggregate(output, input, proc, operand_ssa_var(idx=ssa_idx), inst, idx, offset, assoc, shift + op_bits, &
               isa_bits)
            counter_before = proc%ssa_counter
            call insert_aggregate_inst(output, proc, operand, inst%op2(before_idx)%val, l_bits, r_bits, assoc, slice_lower, shift, &
               .true.)
            
            ! or together the values
            instruction = ir_instruction(inst_type=INST_CALL)
            select type (instruction)
            type is (ir_instruction)
               allocate(instruction%op1(1), instruction%op2(3))
               instruction%op1(1)%val = operand
               instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_OR))
               ! maybe swap 2 and 3? idk
               instruction%op2(2)%val = operand_ssa_var(idx=ssa_idx)
               if (counter_before == proc%ssa_counter) then
                  instruction%op2(3)%val = inst%op2(before_idx)%val
               else
                  instruction%op2(3)%val = operand_ssa_var(idx=proc%ssa_counter - 1)
               end if
            end select
            call output%move_push(instruction)
         end if
      end select
   end subroutine

   subroutine instruction_select_block(input, proc, blk, associations, bits)
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      type(ir_block), pointer, intent(inout) :: blk
      type(list), intent(inout) :: associations
      integer(SMALL), intent(in) :: bits

      type(list) :: new_content
      integer(BIG) :: i, idx, j
      integer(SMALL) :: offset
      class(*), target, allocatable :: instruction
      type(ir_op_container), pointer :: array1(:), array2(:)

      new_content = list(ir_instruction())
      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'malformed block'
         type is (ir_instruction)
            select case (inst%inst_type)
            case (INST_ASSIGN)
               if (allocated(inst%op1)) then
                  idx = 1
                  offset = 0
                  ! TODO: remove when lfortran fixes bug
                  array1 => inst%op1
                  do j = 1, size(array1)
                     ! TODO: remove when lfortran fixes bug
                     array2 => inst%op2
                     if (idx > size(array2)) then
                        call throw('Insufficient values provided on the right side of assignment', inst%loc, .false.)
                     else
                        call ssa_aggregate(new_content, input, proc, inst%op1(j)%val, inst, idx, offset, associations, 0_BIG, bits)
                     end if
                  end do
                  if (idx /= size(array2) + 1) then
                     call throw('Extraneous values provided on the right side of assignment', inst%loc, .false.)
                  end if
               end if
            !case (INST_CALL)
               ! handle calling convention
            case (INST_GET)
               ! TODO: remove when lfortran fixes bug
               array2 => inst%op2
               if (size(array2) /= 1) then
                  call throw('Invalid right side of get instruction', inst%loc)
               end if
               ! load
               ! TODO: remove when lfortran fixes bug
               array1 => inst%op1
               do j = 1, size(array1)
                  instruction = ir_instruction(inst_type=INST_CALL)
                  select type (instruction)
                  type is (ir_instruction)
                     allocate(instruction%op1(1))
                     if (j == 1) then
                        allocate(instruction%op2(2))
                        instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_LOD))
                     else
                        allocate(instruction%op2(3))
                        instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_LLOD))
                        instruction%op2(3)%val = operand_comptime(val=comptime_int(j - 1))
                     end if
                     instruction%op2(2)%val = inst%op2(1)%val
                     instruction%op1(1)%val = inst%op1(j)%val
                  end select
                  call new_content%move_push(instruction)
               end do
            case (INST_SET)
               ! TODO: remove when lfortran fixes bug
               array1 => inst%op1
               if (size(array1) /= 1) then
                  call throw('Invalid left side of set instruction', inst%loc)
               end if
               ! store
               ! TODO: remove when lfortran fixes bug
               array2 => inst%op2
               do j = 1, size(array2)
                  instruction = ir_instruction(inst_type=INST_CALL)
                  select type (instruction)
                  type is (ir_instruction)
                     if (j == 1) then
                        allocate(instruction%op2(3))
                        instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_STR))
                     else
                        allocate(instruction%op2(4))
                        instruction%op2(1)%val = operand_comptime(val=comptime_addr(proc=ASM_LSTR))
                        instruction%op2(3)%val = operand_comptime(val=comptime_int(j - 1))
                     end if
                     instruction%op2(2)%val = inst%op1(1)%val
                     ! TODO: remove when lfortran fixes bug
                     array2 => instruction%op2
                     instruction%op2(size(array2))%val = inst%op2(j)%val
                  end select
                  call new_content%move_push(instruction)
               end do
            case default
               ! copy over
               instruction = blk%content%move_get(i)
               call new_content%move_push(instruction)
            end select
         end select
      end do

      call new_content%move(blk%content)
   end subroutine
end module
