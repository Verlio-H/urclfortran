module ir_ssa
   use include, only: SMALL, BIG, throw, sitoa
   use data_mod, only: list
   use ir_instructions, only: ir_instruction, INST_PHI, INST_ASSIGN, INST_CALL, INST_JMP, INST_BNZ, INST_RET, &
      INST_GET, INST_SET, ir_op_container
   use ir, only: full_ir, ir_procedure, ir_block, operand_ir_var, operand_ssa_var, operand_comptime, comptime_addr, ir_var, &
      full_ir_type, operand_empty
   use ir_defs, only: proc_stats, def_info, def_info_container, get_proc_def_info, ir_could_interfere
   use ir_write, only: op_string
   implicit none (type, external)
contains
   subroutine insert_block_phi(associations, input, proc, blk, def)
      type(list), intent(inout) :: associations
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      type(ir_block), intent(inout) :: blk
      type(operand_ir_var), intent(in) :: def

      integer(BIG) :: i 

      class(*), allocatable :: new

      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'invalid blk argument to insert_block_phi'
         type is (ir_instruction)
            if (inst%inst_type /= INST_PHI) exit
            if (.not.allocated(inst%op1)) cycle
            if (size(inst%op1) < 2) cycle
            select type (op2 => inst%op1(2)%val)
            class default
               error stop 'invalid operand in phi instruction'
            type is (operand_ir_var)
               if (def%equals(op2)) return
            end select
         end select
      end do

      allocate(ir_instruction :: new)
      select type (new)
      type is (ir_instruction)
         new%inst_type = INST_PHI
         allocate(new%op1(2))
         new%op1(1)%val = operand_ssa_var(0, proc%ssa_counter, def%slice, def%lindex, def%loffset, def%uindex, def%uoffset)
         new%op1(2)%val = def

         select type (inst => blk%content%get(i))
         type is (ir_instruction)
            new%loc = inst%loc
         end select
      end select

      call associations%push(def%get_type(input))
      proc%ssa_counter = proc%ssa_counter + 1

      call blk%content%move_insert(i, new)
      
   end subroutine

   subroutine ssaify(associations, input, stats)
      type(list), allocatable, intent(out) :: associations(:)
      type(full_ir), target, intent(inout) :: input
      type(proc_stats), allocatable, intent(in) :: stats(:)

      integer(BIG) :: i
      type(def_info_container), allocatable :: blkdefs(:)
      type(list) :: impure_usage_list

      impure_usage_list = list(operand_ir_var())

      allocate(associations(input%procedures%size))
      allocate(blkdefs(input%procedures%size))
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'invalid input argument to insert_phi'
         type is (ir_procedure)
            ! find all variables referenced in impure procedure calls
            call enumerate_impure_usages(impure_usage_list, input, proc)
            allocate(blkdefs(i)%info(proc%blocks%size))
            call ssaify_proc(associations(i), input, stats(i), proc, blkdefs(i)%info, impure_usage_list)
         end select
      end do
   end subroutine


   subroutine enumerate_impure_usages(usage_list, input, proc)
      type(list), intent(out) :: usage_list
      type(full_ir), intent(in) :: input
      type(ir_procedure), intent(in) :: proc

      integer(BIG) :: i, j, k
      type(ir_block), pointer :: blk

      usage_list = list(operand_ir_var())
      do i = 1, proc%blocks%size
         blk => proc%get_block(input, i)
         do j = 1, blk%content%size
            select type (inst => blk%content%get(j))
            class default
               error stop 'invalid proc input to enumerate_impure_usages'
            type is (ir_instruction)
               if (inst%inst_type /= INST_CALL) cycle
               if (.not.allocated(inst%op2)) cycle
               select type (proc_op => inst%op2(1)%val)
               type is (operand_comptime)
                  select type (proc_op_val => proc_op%val)
                  type is (comptime_addr)
                     select type (proc => input%procedures%get(proc_op_val%proc))
                     type is (ir_procedure)
                        if (proc%pure) cycle
                     end select
                  end select
               end select


               do k = 2, size(inst%op2)
                  select type (op => inst%op2(k)%val)
                  type is (operand_ir_var)
                     ! TODO: hashset
                     call usage_list%push(op)
                  end select
               end do
            end select
         end do
      end do
   end subroutine

   subroutine ssaify_proc(associations, input, stats, proc, blkdefs, impure_usage)
      type(list), intent(out) :: associations
      type(full_ir), target, intent(inout) :: input
      type(proc_stats), intent(in) :: stats
      type(ir_procedure), target, intent(inout) :: proc
      type(def_info), intent(out) :: blkdefs(:)
      type(list), intent(in) :: impure_usage !operand_ir_var

      integer(BIG) :: i, j, k, l, precount
      type(ir_block), pointer :: blk, newblk
      type(def_info) :: base_defs
      ! TODO: ring buffer
      type(list) :: queue
      logical :: break, action
      type(def_info), allocatable :: defs(:)

      queue = list(0_BIG)
      associations = list(full_ir_type())

      if (proc%blocks%size < 1) return

      ! initialize defs for arguments
      base_defs%out_defs = list(operand_ir_var())
      base_defs%out_def_numbers = list(0)
      if (allocated(proc%arguments)) allocate(proc%ssa_arguments(size(proc%arguments)))
      do i = 1, size(proc%arguments)
         call base_defs%out_defs%push(operand_ir_var(var=proc%arguments(i)))
         call associations%push(input%get_var_type(proc%arguments(i)))
         call base_defs%out_def_numbers%push(proc%ssa_counter)
         proc%ssa_arguments(i) = proc%ssa_counter
         proc%ssa_counter = proc%ssa_counter + 1
      end do

      ! insert phi nodes
      break = .false.
      do while (.not.break)
         precount = associations%size

         if (allocated(defs)) deallocate(defs)

         allocate(defs(proc%blocks%size))

         action = get_proc_def_info(defs, input, proc, impure_usage, stats)

         do i = 1, proc%blocks%size
            blk => proc%get_block(input, i)
            do j = 1, defs(i)%out_defs%size
               select type (def => defs(i)%out_defs%get(j))
               class default
                  error stop 'invalid defs argument to ssaify_proc'
               type is (operand_ir_var)
                  do k = 1, stats%frontier(i)%size
                     select type (idx => stats%frontier(i)%get(k))
                     class default
                        error stop 'invalid frontier argument to ssaify_proc'
                     type is (integer(BIG))
                        newblk => proc%get_block(input, idx)
                        call insert_block_phi(associations, input, proc, newblk, def)
                     end select
                  end do
               end select
            end do
         end do
         if (precount == associations%size .and. .not.action) break = .true.
      end do

      ! return here if don't want to ssaify fully
      ! return

      call queue%push(1_BIG)
      
      do while (queue%size /= 0)
         select type (idx => queue%get(1_BIG))
         class default
            error stop 'wrong value pushed to queue in ssaify'
         type is (integer(BIG))
            i = idx
         end select

         call queue%remove(1_BIG)

         blk => proc%get_block(input, i)
         if (stats%rtree(i) == 0) then
            blkdefs(i) = base_defs
         else
            blkdefs(i) = blkdefs(stats%rtree(i))
         end if
         call ssaify_block(associations, input, blkdefs(i), proc, blk)

         do j = 1, stats%tree(i)%size
            select type (next => stats%tree(i)%get(j))
            class default
               error stop 'invalid stats argument to ssaify_proc'
            type is (integer(BIG))
               call queue%push(next)
            end select
         end do
      end do

      ! populate phis
      do i = 1, proc%blocks%size
         blk => proc%get_block(input, i)
         do j = 1, blk%content%size
            select type (inst => blk%content%get(j))
            class default
               error stop 'invalid blk'
            type is (ir_instruction)
               if (inst%inst_type /= INST_PHI) exit
               allocate(inst%op2(blk%parent_blocks%size))
               select type (search_var => inst%op1(2)%val)
               type is (operand_ir_var)
                  parentdo: &
                  do k = 1, blk%parent_blocks%size
                     select type (parent => blk%parent_blocks%get(k))
                     class default
                        error stop 'invalid parent list in block'
                     type is (integer(BIG))
                        do l = 1, blkdefs(parent)%out_defs%size
                           select type (var => blkdefs(parent)%out_defs%get(l))
                           type is (operand_ir_var)
                              if (var%equals(search_var)) then
                                 select type (idx => blkdefs(parent)%out_def_numbers%get(l))
                                 type is (integer)
                                    inst%op2(k)%val = operand_ssa_var(idx=idx)
                                 end select
                                 cycle parentdo
                              end if
                           end select
                        end do
                        inst%op2(k)%val = operand_empty()
                     end select
                  end do parentdo
               end select
            end select
         end do
      end do
   end subroutine

   subroutine ssaify_block(associations, input, defs, proc, blk)
      type(list), intent(inout) :: associations
      type(full_ir), target, intent(inout) :: input
      type(def_info), intent(inout) :: defs
      type(ir_procedure), target, intent(inout) :: proc
      type(ir_block), target, intent(inout) :: blk

      integer(BIG) :: i, j
      integer :: idx
      class(*), allocatable :: temp_inst

      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'invalid blk argument to ssaify_block'
         type is (ir_instruction)
            ! convert ir vars to ssa
            select case (inst%inst_type)
            case default
               call throw('Unknown instruction type in ssaify_block: '//sitoa(inst%inst_type), inst%loc, .false.)
            case (INST_PHI)
               if (.not.allocated(inst%op1)) then
                  call throw('Malformed phi instruction', inst%loc, .false.)
                  cycle
               end if
               if (size(inst%op1) /= 2) then
                  call throw('Malformed phi instruction', inst%loc, .false.)
                  cycle
               end if

               select type (op1 => inst%op1(1)%val)
               class default
                  call throw('Malformed phi instruction', inst%loc, .false.)
                  cycle
               type is (operand_ssa_var)
                  idx = op1%idx
               end select

               select type (var => inst%op1(2)%val)
               class default
                  call throw('Malformed phi instruction', inst%loc, .false.)
               type is (operand_ir_var)
                  call set_ssa_binding(defs, var, idx)
               end select
            case (INST_ASSIGN, INST_CALL, INST_GET)
               if (allocated(inst%op2)) then
                  call replace_ir_vars(inst%op2, defs)
               end if

               slice: &
               if (inst%inst_type == INST_GET) then
                  if (size(inst%op1) /= 1) then
                     call throw('Get must have exactly 1 left argument', inst%loc, .false.)
                     exit slice
                  end if
                  if (size(inst%op2) /= 1) then
                     call throw('Get must have exactly 1 right argument', inst%loc, .false.)
                     exit slice
                  end if
                  select type (op1 => inst%op1(1)%val)
                  class default
                     call throw('First argument to get should be an ir var', inst%loc, .false.)
                  type is (operand_ir_var)
                     select type (op2 => inst%op2(1)%val)
                     type is (operand_ssa_var)
                        op2%slice = op1%slice
                        op2%lindex = op1%lindex
                        op2%loffset = op1%loffset
                        op2%uindex = op1%uindex
                        op2%uoffset = op1%uoffset
                     type is (operand_ir_var)
                        op2%slice = op1%slice
                        op2%lindex = op1%lindex
                        op2%loffset = op1%loffset
                        op2%uindex = op1%uindex
                        op2%uoffset = op1%uoffset
                     end select
                  end select
               end if slice

               if (.not.allocated(inst%op1)) cycle
               do j = 1, size(inst%op1)
                  select type (op => inst%op1(j)%val)
                  type is (operand_ir_var)
                     call set_ssa_binding(defs, op, proc%ssa_counter)
                     call associations%push(op%get_type(input))
                     inst%op1(j)%val = operand_ssa_var(idx=proc%ssa_counter)
                     proc%ssa_counter = proc%ssa_counter + 1
                  end select
               end do
            case (INST_JMP, INST_BNZ, INST_RET)
               if (allocated(inst%op1)) then
                  call replace_ir_vars(inst%op1, defs)
               end if
            case (INST_SET)
               if (allocated(inst%op1)) then
                  call replace_ir_vars(inst%op1, defs)
               end if
               if (allocated(inst%op2)) then
                  call replace_ir_vars(inst%op2, defs)
               end if
            end select
            ! remove invalidated vars
            do j = 1, inst%invalidate%size
               select type (var => inst%invalidate%get(j))
               class default
                  error stop 'malformed invalidated list in instruction'
               type is (operand_ir_var)
                  call remove_ssa_binding(defs, var)
               end select
            end do
         end select
      end do
   end subroutine

   subroutine replace_ir_vars(ops, defs)
      type(ir_op_container), target, intent(inout) :: ops(:)
      type(def_info), intent(inout) :: defs

      integer(BIG) :: j
      integer :: idx

      do j = 1, size(ops)
         select type (op => ops(j)%val)
         type is (operand_ir_var)
            idx = get_ssa_binding(defs, op)
            ! fetch var
            if (idx == -1) cycle
            ops(j)%val = operand_ssa_var(idx=idx, slice=op%slice, &
               lindex=op%lindex, loffset=op%loffset, &
               uindex=op%uindex, uoffset=op%uoffset)
         end select
      end do
   end subroutine

   subroutine set_ssa_binding(defs, var, idx)
      type(def_info), intent(inout) :: defs
      type(operand_ir_var), intent(in) :: var
      integer, intent(in) :: idx

      integer(BIG) :: i

      i = find_ssa_binding(defs%out_defs, var)
      if (i == 0) then
         call defs%out_defs%push(var)
         call defs%out_def_numbers%push(idx)
      else
         call defs%out_def_numbers%set(i, idx)
      end if
   end subroutine

   function get_ssa_binding(defs, var) result(idx)
      type(def_info), intent(in) :: defs
      type(operand_ir_var), intent(in) :: var
      integer :: idx

      integer(BIG) :: i

      idx = -1
      i = find_ssa_binding(defs%out_defs, var)
      if (i /= 0) then
         select type (num => defs%out_def_numbers%get(i))
         class default
            error stop 'invalid defs argument to get_ssa_binding'
         type is (integer)
            idx = num
         end select
      end if
   end function

   subroutine remove_ssa_binding(defs, var)
      type(def_info), intent(inout) :: defs
      type(operand_ir_var), intent(in) :: var

      integer(BIG) :: i

      i = find_ssa_binding(defs%out_defs, var)
      if (i /= 0) then
         call defs%out_defs%fast_remove(i)
      end if
   end subroutine

   function find_ssa_binding(defs, var) result(idx)
      type(list), intent(in) :: defs
      type(operand_ir_var), intent(in) :: var
      integer(BIG) :: idx

      do idx = 1, defs%size
         select type (val => defs%get(idx))
         class default
            error stop 'invalid def argument to find_ssa_binding'
         type is (operand_ir_var)
            if (val%equals(var)) return
         end select
      end do
      idx = 0
   end function
end module
