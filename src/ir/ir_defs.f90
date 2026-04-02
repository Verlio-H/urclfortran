module ir_defs
   use include, only: SMALL, BIG, throw, location
   use ir_instructions, only: INST_ASSIGN, INST_CALL, INST_CAST, INST_PHI, INST_RET, INST_GET, INST_JMP, INST_BNZ, INST_SET, &
      ir_instruction, ir_op_container
   use ir, only: full_ir, operand_comptime, operand_ir_var, ir_var, ir_procedure, ir_block, comptime_addr, operand_empty
   use ir_graph, only: proc_stats
   use ir_write, only: op_string
   use data_mod, only: list

   implicit none (type, external)

   type :: def_info
      type(list) :: out_defs ! operand_ir_var
      type(list) :: out_def_numbers ! integer
      ! TODO: set
      type(list) :: non_writeback ! operand_ir_var
   end type

   type :: def_info_container
      type(def_info), allocatable :: info(:)
   end type
contains
   function ir_could_interfere(input, src, dest) result(res)
      type(full_ir), intent(in) :: input
      type(operand_ir_var), intent(in) :: src, dest
      logical :: res

      type(ir_var), pointer :: srcv, destv

      select type (v => input%vars%get(src%var))
      class default
         error stop 'invalid input argument to ir_could_interfere'
      type is (ir_var)
         srcv => v
      end select

      select type (v => input%vars%get(dest%var))
      class default
         error stop 'invalid input argument to ir_could_interfere'
      type is (ir_var)
         destv => v
      end select

      if (src%var == dest%var) then
         ! src could possibly be a pointer to dest
         res = src%dereference_count < dest%dereference_count
      else if (dest%dereference_count == 0) then
         ! nothing can point to dest
         res = .false.
      else if (destv%const .or. destv%type%const_mask(dest%dereference_count)) then
         ! (dest%dereference_count is known to be non zero)
         ! nothing can overwrite const
         res = .false.
      else
         ! if src is restrict and its dereference count is one less than dest, they cannot alias
         if (src%dereference_count < srcv%type%indirection_count .and. &
               src%dereference_count + 1_SMALL == dest%dereference_count) then
            if (srcv%type%restrict_mask(src%dereference_count + 1)) then
               res = .false.
               return
            end if
         end if
         
         ! if src or dest is the direct result of a restrict pointer dereference, they cannot alias
         if (src%dereference_count > 0) then
            if (srcv%type%restrict_mask(src%dereference_count)) then
               res = .false.
               return
            end if
         end if

         ! (dest%deference_count is known to be non zero)
         if (destv%type%restrict_mask(dest%dereference_count)) then
            res = .false.
            return
         end if

         ! if src is restrictish and its deference count is one less than dest, and dest is the result of a
         !   restrictish pointer dereference, they cannot alias
         if (src%dereference_count < srcv%type%indirection_count .and. &
               src%dereference_count + 1_SMALL == dest%dereference_count) then
            if (srcv%type%restrictish_mask(src%dereference_count + 1) .and. &
                  destv%type%restrictish_mask(dest%dereference_count)) then
               res = .false.
               return
            end if
         end if

         ! if src and dest are both direct results of a restrictish pointer dereference, they cannot alias
         if (src%dereference_count > 0) then
            if (srcv%type%restrictish_mask(src%dereference_count) .and. &
                  destv%type%restrictish_mask(dest%dereference_count)) then
               res = .false.
               return
            end if
         end if

         ! otherwise, for now, assume that they could interfere
         res = .true.
         ! if src (and dest) have a non zero dereference count, and are both of the same type, or their
         !   slices consist of components of the same type, then they could alias
         !
         ! it is possible that src contains a pointer to some component of dest
         ! it is possible that dest contains a pointer to some component of src
         ! so we need to enumerate all subtypes of the two, and enumerate all of the pointers
         ! if they contain any pointers to some component of the other, then yes, otherwise no
         ! alternative faster check: if either contain pointers, then yes, otherwise no
      end if
   end function

   subroutine get_proc_def_info(output, input, proc, impure_usage, stats)
      type(def_info), intent(out) :: output(:)
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), intent(in) :: proc
      type(list), intent(in) :: impure_usage
      type(proc_stats), intent(in) :: stats

      integer(BIG) :: i
      type(ir_block), pointer :: blk
      type(def_info) :: all_defs(proc%blocks%size), base_defs
      ! TODO: ring buffer
      type(list) :: queue

      if (proc%blocks%size == 0) return

      base_defs%out_defs = list(operand_ir_var())
      do i = 1, size(proc%arguments)
         call base_defs%out_defs%push(operand_ir_var(var=proc%arguments(i)))
      end do

      queue = list(0_BIG)
      call queue%push(1_BIG)

      do while (queue%size /= 0)
         select type (idx => queue%get(1_BIG))
         class default
            error stop 'wrong value pushed to queue in ssaify'
         type is (integer(BIG))
            blk => proc%get_block(input, idx)
            if (stats%rtree(idx) == 0) then
               all_defs(idx)%out_defs = base_defs%out_defs
               all_defs(idx)%non_writeback = list(operand_ir_var())
            else
               all_defs(idx)%out_defs = all_defs(stats%rtree(idx))%out_defs
               all_defs(idx)%non_writeback = all_defs(stats%rtree(idx))%non_writeback
            end if

            call get_block_def_info(output(idx), input, blk, impure_usage, proc, all_defs(idx))

            do i = 1, stats%tree(idx)%size
               select type (next => stats%tree(idx)%get(i))
               class default
                  error stop 'invalid stats argument to ssaify_proc'
               type is (integer(BIG))
                  call queue%push(next)
               end select
            end do
         end select
         call queue%remove(1_BIG)
      end do
   end subroutine

   subroutine get_block_def_info(output, input, blk, impure_usage, proc, all_defs)
      type(def_info), intent(out) :: output
      type(full_ir), target, intent(inout) :: input
      type(ir_block), target, intent(inout) :: blk
      type(list), intent(in) :: impure_usage
      type(ir_procedure), intent(in) :: proc
      type(def_info), intent(inout) :: all_defs

      integer(BIG) :: i, j, k
      type(ir_procedure), pointer :: curr_proc
      class(*), allocatable :: temp_inst
      type(operand_ir_var) :: addr
      type(list) :: writeback

      ! TODO: hashmap
      ! TODO: insert deep writebacks preemptively to decrease loads
      ! TODO: insert deep fetches preemptively to decrease loads
      output%out_defs = list(operand_ir_var())
      
      i = 0
      do while (i < blk%content%size)
         i = i + 1
         select type (inst => blk%content%get(i))
         class default
            error stop 'invalid blk argument to get_block_def_info'
         type is (ir_instruction)
            ! insert fetches
            select case (inst%inst_type)
            case (INST_RET, INST_JMP, INST_BNZ, INST_SET) ! check op1
               if (insert_fetch(input, all_defs, blk, i, inst%op1, inst%loc)) then
                  i = i - 1
                  cycle
               end if
            case (INST_ASSIGN, INST_CAST, INST_GET, INST_CALL) ! check op2
               if (insert_fetch(input, all_defs, blk, i, inst%op2, inst%loc)) then
                  i = i - 1
                  cycle
               end if
            end select

            ! writebacks and new vars
            writeback = list(operand_ir_var())
            inst%invalidate = list(operand_ir_var())
            bigcase: &
            select case (inst%inst_type)
            case (INST_ASSIGN, INST_CALL, INST_CAST, INST_PHI, INST_GET)

               ! handle new vars and assignment writebacks
               if (allocated(inst%op1)) then
                  do j = 1, size(inst%op1)
                     select type (op => inst%op1(j)%val)
                     class default
                        cycle
                     type is (operand_ir_var)
                        if (inst%inst_type /= INST_GET) then
                           call insert_argument_writeback(output, input, op, inst, writeback)
                           call insert_argument_writeback(all_defs, input, op, inst, writeback)
                           call output%out_defs%push(op)
                        else
                           call all_defs%non_writeback%push(op)
                        end if
                        call all_defs%out_defs%push(op)
                     end select
                  end do
               end if

               if (inst%inst_type /= INST_CALL) exit bigcase
               
               if (.not.allocated(inst%op2)) then
                  call throw('Malformed call instruction', inst%loc, .false.)
                  cycle
               end if

               select type (proc_arg => inst%op2(1)%val)
               class default
                  call throw('Call instruction doesn''t have procedure', inst%loc, .false.)
               type is (operand_comptime)
                  select type (proc_arg_val => proc_arg%val)
                  class default
                     call throw('Call instruction has invalid comptime value', inst%loc, .false.) 
                  type is (comptime_addr)
                     select type (proc => input%procedures%get(proc_arg_val%proc))
                     class default
                        error stop 'invalid call inst in ssaify_block (wrong type of comptime value)'
                     type is (ir_procedure)
                        curr_proc => proc
                     end select
                  end select
               end select

               do j = 2, size(inst%op2)
                  if (j - 1 > size(curr_proc%arguments)) then
                     if (.not.curr_proc%variadic) then
                        call throw('Too many arguments to procedure '//curr_proc%name, inst%loc, .false.)
                        exit
                     end if
                  else
                     select type (var => input%vars%get(curr_proc%arguments(j - 1)))
                     class default
                        error stop 'invalid input argument to get def info'
                     type is (ir_var)
                        if (var%noderef) cycle
                     end select
                  end if

                  select type (arg => inst%op2(j)%val)
                  type is (operand_ir_var)
                     call insert_function_writeback(output, input, arg, proc, inst, writeback)
                     call insert_function_writeback(all_defs, input, arg, proc, inst, writeback)
                  end select
               end do

               if (.not.curr_proc%simple) then
                  call insert_impure_writeback(output, input, inst, impure_usage, proc, writeback)
                  call insert_impure_writeback(all_defs, input, inst, impure_usage, proc, writeback)
               end if
            case (INST_RET)
               do j = output%out_defs%size, 1, -1
                  select type (var => output%out_defs%get(j))
                  type is (operand_ir_var)
                  end select
               end do
               ! if externally accessible, writeback
               call insert_impure_writeback(output, input, inst, impure_usage, proc, writeback)
               call insert_impure_writeback(all_defs, input, inst, impure_usage, proc, writeback)
            case default
               cycle
            end select bigcase
            
            if (.not.inst%writtenback) then
               outer: &
               do j = 1, writeback%size
                  select type (var => writeback%get(j))
                  type is (operand_ir_var)
                     do k = 1, j - 1
                        select type (var2 => writeback%get(k))
                        type is (operand_ir_var)
                           if (var%equals(var2)) cycle outer
                        end select
                     end do
                     do k = 1, all_defs%non_writeback%size
                        select type (var2 => all_defs%non_writeback%get(k))
                        type is (operand_ir_var)
                           if (var%equals(var2)) cycle outer
                        end select
                     end do
                     addr = var
                     addr%dereference_count = addr%dereference_count - 1
                     temp_inst = ir_instruction(INST_SET, [ir_op_container(addr)], [ir_op_container(var)], inst%loc)
                     call blk%content%move_insert(i, temp_inst)
                  end select
               end do outer

               do j = all_defs%non_writeback%size, 1, -1
                  select type (var => all_defs%non_writeback%get(j))
                  type is (operand_ir_var)
                     do k = 1, writeback%size
                        select type (var2 => writeback%get(k))
                        type is (operand_ir_var)
                           if (var%equals(var2)) call all_defs%non_writeback%fast_remove(j)
                        end select
                     end do
                  end select
               end do

               if (writeback%size /= 0) then
                  i = i - 1
               end if
            end if
            inst%writtenback = .true.
         end select
      end do
   end subroutine

   subroutine insert_argument_writeback(output, input, op, inst, writeback)
      type(def_info), intent(inout) :: output
      type(full_ir), target, intent(inout) :: input
      type(operand_ir_var), intent(in) :: op
      type(ir_instruction), target, intent(inout) :: inst
      type(list), intent(inout) :: writeback

      integer(BIG) :: k

      do k = output%out_defs%size, 1, -1
         select type (dest => output%out_defs%get(k))
         class default
            error stop 'something terrible has happened'
         type is (operand_ir_var)
            if (inst%inst_type /= INST_PHI .and. ir_could_interfere(input, op, dest)) then
               !call throw('Assignment Writeback: '//op_string(dest, input), inst%loc, .false.)
               call writeback%push(dest)
               call inst%invalidate%push(dest)
               call output%out_defs%fast_remove(k)
            end if
         end select
      end do
   end subroutine

   subroutine insert_function_writeback(output, input, arg, proc, inst, writeback)
      type(def_info), intent(inout) :: output
      type(full_ir), target, intent(inout) :: input
      type(operand_ir_var), intent(in) :: arg
      type(ir_procedure), intent(in) :: proc
      type(ir_instruction), target, intent(inout) :: inst
      type(list), intent(inout) :: writeback

      integer(BIG) :: k

      do k = output%out_defs%size, 1, -1
         select type (orig => output%out_defs%get(k))
         type is (operand_ir_var)
            if (ir_could_interfere(input, arg, orig)) then
               !call throw('Func Writeback: '//op_string(orig, input), inst%loc, .false.)
               call writeback%push(orig)
               ! only invalidates if impure
               if (proc%pure) cycle
               call inst%invalidate%push(orig)
               call output%out_defs%fast_remove(k)
            end if
         end select
      end do
   end subroutine

   subroutine insert_impure_writeback(output, input, inst, impure_usage, proc, writeback)
      type(def_info), intent(inout) :: output
      type(full_ir), intent(in) :: input
      type(ir_instruction), intent(inout) :: inst
      type(list), intent(in) :: impure_usage
      type(ir_procedure), intent(in) :: proc
      type(list), intent(inout) :: writeback

      integer(BIG) :: j, k

      outer: &
      do j = output%out_defs%size, 1, -1
         select type (var => output%out_defs%get(j))
         class default
            error stop 'malformed out defs'
         type is (operand_ir_var)
            ! writeback static vars (aiming at globals)
            select type (true_var => input%vars%get(var%var))
            type is (ir_var)
               if (true_var%static) then
                  !call throw('Static Writeback: '//op_string(var, input), inst%loc, .false.)
                  call writeback%push(var)
                  call inst%invalidate%push(var)
                  call output%out_defs%fast_remove(j)
                  cycle
               end if
            end select

            ! writeback vars that we pass a reference to impure procedures
            do k = 1, impure_usage%size
               select type (impure_var => impure_usage%get(k))
               type is (operand_ir_var)
                  if (.not.ir_could_interfere(input, impure_var, var)) cycle
                  !call throw('Global alias pre writeback: '//op_string(var, input), inst%loc, .false.)
                  ! invalidates
                  call writeback%push(var)
                  call inst%invalidate%push(var)
                  call output%out_defs%fast_remove(j)
                  cycle outer
               end select
            end do

            ! writeback vars that are argumetns to the current procedure and thus could be used elsewhere
            do k = 1, size(proc%arguments)
               if (.not.ir_could_interfere(input, operand_ir_var(var=proc%arguments(k)), var)) cycle
               !call throw('Impure Arg Writeback: '//op_string(var, input), inst%loc, .false.)
               ! invalidates
               call writeback%push(var)
               call inst%invalidate%push(var)
               call output%out_defs%fast_remove(j)
               cycle outer
            end do
         end select
      end do outer
   end subroutine

   function insert_fetch(input, defs, blk, i, ops, loc) result(did_fetch)
      type(full_ir), target, intent(inout) :: input
      type(def_info), intent(inout) :: defs
      type(ir_block), target, intent(inout) :: blk
      integer(BIG), intent(in) :: i
      type(ir_op_container), intent(inout) :: ops(:)
      type(location), intent(in) :: loc
      logical :: did_fetch

      class(*), allocatable :: temp_inst
      type(operand_ir_var) :: addr
      integer(BIG) :: j, k

      outer: &
      do j = 1, size(ops)
         select type (op => ops(j)%val)
         type is (operand_ir_var)
            do k = 1, defs%out_defs%size
               select type (var => defs%out_defs%get(k))
               class default
                  error stop 'malformed defs argument to insert_fetch'
               type is (operand_ir_var)
                  if (var%equals(op)) cycle outer
               end select
            end do
            if (op%slice) then
               addr = operand_ir_var(var=op%var, dereference_count=op%dereference_count)
            else if (op%dereference_count == 0) then
               select type (var => input%vars%get(op%var))
               type is (ir_var)
                  if (.not.var%const) then
                     call throw('Warning: Variable '//op_string(op, input)//' potentially used before definition', loc, .false.)
                     ops(j)%val = operand_empty()
                  end if
               end select
               cycle
            else
               addr = operand_ir_var(var=op%var, dereference_count=op%dereference_count - 1)
            end if

            temp_inst = ir_instruction(INST_GET, [ir_op_container(op)], [ir_op_container(addr)], loc)
            call blk%content%move_insert(i, temp_inst)
            did_fetch = .true.
            return
         end select
      end do outer
      did_fetch = .false.
   end function

   subroutine print_def_info(curr_ir, proc, defs)
      type(full_ir), intent(in) :: curr_ir
      type(ir_procedure), intent(in) :: proc
      type(def_info), intent(in) :: defs(:)

      integer(BIG) :: i, j
      type(ir_block), pointer :: blk
      character(:), allocatable :: line

      do i = 1, size(defs)
         blk => proc%get_block(curr_ir, i)
         line = blk%name//': '
         do j = 1, defs(i)%out_defs%size
            select type (op => defs(i)%out_defs%get(j))
            type is (operand_ir_var)
               if (j /= 1) line = line//', '
               line = line//op_string(op, curr_ir)
            end select
         end do
         write (*, '(A)') line
      end do
   end subroutine
end module
