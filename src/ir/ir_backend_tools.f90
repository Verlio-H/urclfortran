module ir_backend_tools
   use include, only: BIG, throw, bitoa
   use ir_instructions, only: ir_instruction, INST_PHI, INST_CALL, INST_JMP, INST_BNZ, ir_op_container, ir_operand
   use ir, only: full_ir, ir_procedure, ir_block, operand_ir_block, operand_comptime, comptime_addr, ir_block_empty, &
      operand_empty, operand_ssa_var, operand_ir_var
   use ir_graph, only: proc_stats
   use data_mod, only: list
   implicit none (type, external)
contains
   subroutine split_edge(input, proc, child_blk_idx, parent_idx, jump_func, block_name)
      type(full_ir), intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      integer(BIG), value, intent(in) :: child_blk_idx, parent_idx, jump_func
      character(*), intent(in) :: block_name

      type(ir_block), pointer :: parent_blk, child_blk
      integer(BIG) :: child_idx, parent_block_idx, i
      class(*), allocatable :: new_block, new_inst
      type(ir_op_container), pointer :: ops(:)

      child_blk => proc%get_block(input, child_blk_idx)
      select type (idx => child_blk%parent_blocks%get(parent_idx))
      type is (integer(BIG))
         parent_block_idx = idx
      end select
      parent_blk => proc%get_block(input, parent_block_idx)

      child_idx = 0
      ! find corresponding child idx
      do i = 1, size(parent_blk%child_blocks)
         if (parent_blk%child_blocks(i) == child_blk_idx) then
            child_idx = i
            exit
         end if
      end do
      if (child_idx == 0) then
         call throw('Malformed cfg', proc%loc)
      end if
      ! actually do the split
      new_block = ir_block_empty()
      select type (new_block)
      type is (ir_block)
         new_block%name = block_name
         new_block%child_blocks = [child_blk_idx]
         call new_block%parent_blocks%push(parent_block_idx)
         new_inst = ir_instruction(inst_type = INST_CALL)
         select type (new_inst)
         type is (ir_instruction)
            allocate(new_inst%op1(1), new_inst%op2(1))
            new_inst%op1(1)%val = operand_ir_block(block_index = child_blk_idx)
            new_inst%op2(1)%val = operand_comptime(val=comptime_addr(proc=jump_func))
         end select
         call new_block%content%move_push(new_inst)
      end select
      call input%blocks%move_push(new_block)
      call proc%blocks%push(input%blocks%size)
      call child_blk%parent_blocks%set(parent_idx, proc%blocks%size)
      parent_blk%child_blocks(child_idx) = proc%blocks%size

      ! replace references in parent
      select type (inst => parent_blk%content%get(parent_blk%content%size))
      type is (ir_instruction)
         select case (inst%inst_type)
         case (INST_CALL, INST_JMP)
            ops => inst%op1
         case (INST_BNZ)
            ops => inst%op2
         case default
            call throw('Invalid instruction at end of block', inst%loc, .false.)
         end select
         if (.not.associated(ops)) return
         do i = 1, size(ops)
            select type (op => ops(i)%val)
            class default
               call throw('Cannot split edges where jump destinations are not static', inst%loc)
            type is (operand_ir_block)
               if (op%block_index == child_blk_idx) then
                  op%block_index = proc%blocks%size
               end if
            end select
         end do
      end select
   end subroutine

   subroutine phi_removal(input, associations, stats, jump_func, move_func)
      type(full_ir), intent(inout) :: input
      type(list), intent(in) :: associations(:)
      type(proc_stats), intent(inout):: stats(:) ! stats are invalidated
      integer(BIG), value, intent(in) :: jump_func, move_func

      integer(BIG) :: i, j, k, m, tmp_counter
      type(ir_block), pointer :: blk, parent_blk
      class(ir_operand), pointer :: reference_operand
      class(*), allocatable :: new_inst
      logical :: equal

      tmp_counter = 0
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         type is (ir_procedure)
            blk_loop: &
            do j = 1, proc%blocks%size
               blk => proc%get_block(input, j)
               k = 0
               do while (k <= blk%content%size)
                  k = k + 1
                  select type (inst => blk%content%get(k))
                  type is (ir_instruction)
                     if (inst%inst_type /= INST_PHI) cycle blk_loop
                     if (size(inst%op2) /= blk%parent_blocks%size) then
                        call throw('Malformed phi instruction', inst%loc)
                     end if
                     ! check to see if all arguments are the same
                     reference_operand => inst%op2(1)%val
                     equal = .true.
                     do m = 2, size(inst%op2)
                        select type (op => inst%op2(m)%val)
                        class default
                           call throw('Invalid argument in phi instruction', inst%loc)
                        type is (operand_empty)
                           equal = .false.
                           exit
                        type is (operand_ssa_var)
                           select type (reference_operand)
                           class default
                              equal = .false.
                              exit
                           type is (operand_ssa_var)
                              if (reference_operand%slice .or. op%slice) then
                                 call throw('Slices not allowed in phi instruction', inst%loc)
                              end if
                              if (reference_operand%idx /= op%idx) then
                                 equal = .false.
                                 exit
                              end if
                           end select
                        end select
                     end do
                     if (equal) then
                        new_inst = ir_instruction(inst_type = INST_CALL, loc = inst%loc)
                        select type (new_inst)
                        type is (ir_instruction)
                           allocate(new_inst%op1(1), new_inst%op2(2))
                           call move_alloc(inst%op1(1)%val, new_inst%op1(1)%val)
                           new_inst%op2(1)%val = operand_comptime(val=comptime_addr(proc=move_func))
                           call move_alloc(inst%op2(1)%val, new_inst%op2(2)%val)
                        end select
                        call blk%content%set(k, new_inst)
                        cycle
                     end if
                     do m = 1, blk%parent_blocks%size
                        select type (blk_idx => blk%parent_blocks%get(m))
                        class default
                           error stop 'malformed block'
                        type is (integer(BIG))
                           parent_blk => proc%get_block(input, blk_idx)
                           if (.not.same_type_as(inst%op2(m)%val, operand_empty()) .and. &
                                 allocated(parent_blk%child_blocks) .and. size(parent_blk%child_blocks) > 1) then
                              ! TODO: skip splitting if it is an indirect jump
                              call split_edge(input, proc, j, m, jump_func, '\critical'//bitoa(tmp_counter))
                              tmp_counter = tmp_counter + 1
                           end if
                           new_inst = ir_instruction(inst_type = INST_CALL, loc = inst%loc)
                           select type (new_inst)
                           type is (ir_instruction)
                              allocate(new_inst%op1(1), new_inst%op2(2))
                              new_inst%op1(1)%val = inst%op1(1)%val
                              new_inst%op2(1)%val = operand_comptime(val=comptime_addr(proc=move_func))
                              call move_alloc(inst%op2(m)%val, new_inst%op2(2)%val)
                           end select
                           call parent_blk%content%move_insert(parent_blk%content%size, new_inst)
                        end select
                     end do
                     ! TODO: make less bad
                     call blk%content%remove(k)
                     k = k - 1
                  end select
               end do
            end do blk_loop
         end select
      end do
   end subroutine
end module
