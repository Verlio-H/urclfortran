module ir_resolve_references
   use include, only: BIG, throw, location
   use ir_instructions, only: ir_instruction, INST_RET, INST_JMP, INST_BNZ
   use ir, only: full_ir, ir_var, ir_procedure, ir_block, base_comptime_val, comptime_addr, comptime_int, operand_comptime, &
      operand_ir_block
   use data_mod, only: list

   implicit none (type, external)
contains
   subroutine resolve_references(curr_ir)
      type(full_ir), intent(inout) :: curr_ir

      call resolve_var_references(curr_ir)
      call resolve_proc_references(curr_ir)
   end subroutine

   subroutine resolve_var_references(curr_ir)
      type(full_ir), intent(inout) :: curr_ir

      integer(BIG) :: i
      integer :: j

      do i = 1, curr_ir%vars%size
         select type (var => curr_ir%vars%get(i))
         class default
            error stop 'invalid vars argument to resolve_references'
         type is (ir_var)
            if (.not.allocated(var%contents)) cycle

            do j = 1, size(var%contents)
               call comptime_resolve_reference(curr_ir, var%contents(j)%val, var%loc)
            end do
         end select
      end do
   end subroutine

   subroutine resolve_proc_references(curr_ir)
      type(full_ir), intent(in) :: curr_ir

      integer(BIG) :: i, j

      do i = 1, curr_ir%procedures%size
         select type (proc => curr_ir%procedures%get(i))
         class default
            error stop 'invalid vars argument to resolve_references'
         type is (ir_procedure)
            do j = 1, proc%blocks%size
               select type (idx => proc%blocks%get(j))
               class default
                  error stop 'invalid proc argument to resolve_references'
               type is (integer(BIG))
                  select type (block => curr_ir%blocks%get(idx))
                  class default
                     error stop 'invalid blocks argument to resolve_references'
                  type is (ir_block)
                     call block_resolve_references(curr_ir, proc, block, idx)
                  end select
               end select
            end do
         end select
      end do
   end subroutine

   subroutine block_resolve_references(curr_ir, proc, bblock, idx)
      type(full_ir), intent(in) :: curr_ir
      type(ir_procedure), intent(in) :: proc
      type(ir_block), intent(inout) :: bblock
      integer(BIG), intent(in) :: idx

      integer(BIG) :: i
      integer :: j, block_count

      do i = 1, bblock%content%size
         select type (inst => bblock%content%get(i))
         class default
            error stop 'invalid block argument to resolve_references'
         type is (ir_instruction)
            if (i == bblock%content%size .and. &
               inst%inst_type /= INST_RET .and. &
               inst%inst_type /= INST_JMP .and. &
               inst%inst_type /= INST_BNZ) then
               call throw('Expecting jumping instruction at end of basic block', inst%loc, .false.)
            end if
            block_count = 0
            if (allocated(inst%op1)) then
               do j = 1, size(inst%op1)
                  select type (op => inst%op1(j)%val)
                  type is (operand_comptime)
                     call comptime_resolve_reference(curr_ir, op%val, inst%loc)
                  type is (operand_ir_block)
                     if (allocated(bblock%child_blocks)) then
                        call throw('Block has more than one branch instruction', inst%loc)
                     end if
                     call resolve_block_references(curr_ir, proc, op, inst%loc)
                     block_count = block_count + 1
                  end select
               end do 
            end if

            if (block_count /= 0) then
               allocate(bblock%child_blocks(block_count))
               block_count = 1
               do j = 1, size(inst%op1)
                  select type (op => inst%op1(j)%val)
                  type is (operand_ir_block)
                     bblock%child_blocks(block_count) = op%block_index
                     block_count = block_count + 1
                  end select
               end do
            end if

            if (.not.allocated(inst%op2)) cycle

            block_count = 0
            do j = 1, size(inst%op2)
               select type (op => inst%op2(j)%val)
               type is (operand_comptime)
                  call comptime_resolve_reference(curr_ir, op%val, inst%loc)
               type is (operand_ir_block)
                  if (allocated(bblock%child_blocks)) then
                     call throw('Block has more than one branch instruction', inst%loc)
                  end if
                  call resolve_block_references(curr_ir, proc, op, inst%loc)
                  block_count = block_count + 1
               end select
            end do

            if (block_count /= 0) then
               allocate(bblock%child_blocks(block_count))
               block_count = 1
               do j = 1, size(inst%op2)
                  select type (op => inst%op2(j)%val)
                  type is (operand_ir_block)
                     bblock%child_blocks(block_count) = op%block_index
                     block_count = block_count + 1
                  end select
               end do
            end if

            if (allocated(bblock%child_blocks)) then
               do j = 1, size(bblock%child_blocks)
                  select type (pblock => curr_ir%blocks%get(bblock%child_blocks(j)))
                  class default
                     error stop 'invalid curr_ir argument to resolve references'
                  type is (ir_block)
                     call pblock%parent_blocks%push(idx)
                  end select
               end do
            end if
         end select
      end do
   end subroutine

   subroutine resolve_block_references(curr_ir, proc, op, loc)
      type(full_ir), intent(in) :: curr_ir
      type(ir_procedure), intent(in) :: proc
      type(operand_ir_block), intent(inout) :: op
      type(location), intent(in) :: loc

      integer(BIG) :: i, found

      ! TODO: hash map
      found = 0
      do i = 1, proc%blocks%size
         select type (idx => proc%blocks%get(i))
         class default
            error stop 'invalid proc argument to resolve_block_references'
         type is (integer(BIG))
            select type (bblock => curr_ir%blocks%get(idx))
            class default
               error stop 'invalid curr_ir argument to resolve_block_references'
            type is (ir_block)
               if (bblock%name == op%name) then
                  found = idx
                  exit
               end if
            end select
         end select
      end do

      op%block_index = found
      if (found == 0) then
         call throw('Unknown block name: '//op%name, loc, .false.)
         return
      end if
   end subroutine

   subroutine comptime_resolve_reference(curr_ir, val, loc)
      type(full_ir), intent(in) :: curr_ir
      class(base_comptime_val), allocatable, intent(inout) :: val
      type(location), intent(in) :: loc

      integer(BIG) :: i
      integer(BIG) :: found

      select type (val)
      class default
         return
      type is (comptime_addr)
         if (.not.allocated(val%name)) return
         
         ! TODO: hash map
         found = 0
         do i = 1, curr_ir%procedures%size
            select type (match => curr_ir%procedures%get(i))
            class default
               error stop 'invalid procs argument to parse_comptime_val'
            type is (ir_procedure)
               if (val%name == match%name) then
                  found = i
                  exit
               end if
            end select
         end do

         if (found /= 0) then
            val%proc = found 
            deallocate(val%name)
            return
         end if

         found = 0.
         do i = 1, curr_ir%global_vars%size
            select type (idx => curr_ir%global_vars%get(i))
            class default
               error stop 'invalid globals argument to parse_comptime_val'
            type is (integer(BIG))
               select type (match => curr_ir%vars%get(idx))
               class default
                  error stop 'invalid vars argument to parse_comptime_val'
               type is (ir_var)
                  if (val%name == match%name) then
                     found = i
                     if (.not.match%static) then
                        call throw('Referenced variable must be static', loc, .false.)
                        found = 0
                     end if
                     exit
                  endif
               end select
            end select
         end do

         if (found /= 0) then
            val%var = found
            deallocate(val%name)
            return
         end if

         call throw('Invalid address: '//val%name, loc, .false.)
         deallocate(val%name)
      end select 
      val = comptime_int(0)
   end subroutine
end module
