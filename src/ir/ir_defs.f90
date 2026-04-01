module ir_defs
   use include, only: SMALL, BIG, throw
   use ir_instructions, only: INST_ASSIGN, INST_CALL, INST_CAST, INST_PHI, INST_RET, ir_instruction
   use ir, only: full_ir, operand_comptime, operand_ir_var, operand_ssa_var, ir_var, ir_procedure, ir_block, comptime_addr
   use ir_graph, only: proc_stats
   use ir_write, only: op_string
   use data_mod, only: list

   implicit none (type, external)

   type :: def_info
      type(list) :: out_defs ! operand_ir_var
      type(list) :: out_def_numbers ! integer
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
      type(def_info) :: all_defs(proc%blocks%size)
      ! TODO: ring buffer
      type(list) :: queue

      if (proc%blocks%size == 0) return

      queue = list(0_BIG)
      call queue%push(1_BIG)

      do while (queue%size /= 0)
         select type (idx => queue%get(1_BIG))
         class default
            error stop 'wrong value pushed to queue in ssaify'
         type is (integer(BIG))
            blk => proc%get_block(input, idx)
            if (stats%rtree(idx) == 0) then
               all_defs(idx)%out_defs = list(operand_ir_var())
            else
               all_defs(idx)%out_defs = all_defs(stats%rtree(idx))%out_defs
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

      integer(BIG) :: i, j
      type(ir_procedure), pointer :: curr_proc

      ! TODO: hashmap
      ! TODO: insert deep writebacks preemptively to decrease loads
      ! TODO: insert deep fetches preemptively to decrease loads
      output%out_defs = list(operand_ir_var())
      
      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'invalid blk argument to get_block_def_info'
         type is (ir_instruction)
            inst%writeback = list(operand_ir_var())
            inst%invalidate = list(operand_ir_var())
            select case (inst%inst_type)
            case (INST_ASSIGN, INST_CALL, INST_CAST, INST_PHI)
            case (INST_RET)
               do j = output%out_defs%size, 1, -1
                  select type (var => output%out_defs%get(j))
                  type is (operand_ir_var)
                  end select
               end do
               ! if externally accessible, writeback
               call insert_impure_writeback(output, input, inst, impure_usage, proc)
               call insert_impure_writeback(all_defs, input, inst, impure_usage, proc)
               cycle
            case default
               cycle
            end select

            ! handle new vars and assignment writebacks
            if (allocated(inst%op1)) then
               do j = 1, size(inst%op1)
                  select type (op => inst%op1(j)%val)
                  class default
                     cycle
                  type is (operand_ir_var)
                     call insert_argument_writeback(output, input, op, inst)
                     call insert_argument_writeback(all_defs, input, op, inst)
                     call output%out_defs%push(op)
                     call all_defs%out_defs%push(op)
                  end select
               end do
            end if

            if (inst%inst_type /= INST_CALL) cycle
            
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
                  call insert_function_writeback(output, input, arg, proc, inst)
                  call insert_function_writeback(all_defs, input, arg, proc, inst)
               end select
            end do

            if (.not.curr_proc%simple) then
               call insert_impure_writeback(output, input, inst, impure_usage, proc)
               call insert_impure_writeback(all_defs, input, inst, impure_usage, proc)
            end if
         end select
      end do
   end subroutine

   subroutine insert_argument_writeback(output, input, op, inst)
      type(def_info), intent(inout) :: output
      type(full_ir), target, intent(inout) :: input
      type(operand_ir_var), intent(in) :: op
      type(ir_instruction), target, intent(inout) :: inst

      integer(BIG) :: k

      do k = output%out_defs%size, 1, -1
         select type (dest => output%out_defs%get(k))
         class default
            error stop 'something terrible has happened'
         type is (operand_ir_var)
            if (inst%inst_type /= INST_PHI .and. ir_could_interfere(input, op, dest)) then
               !call throw('Assignment Writeback: '//op_string(dest, input), inst%loc, .false.)
               call inst%writeback%push(dest)
               call inst%invalidate%push(dest)
               call output%out_defs%fast_remove(k)
            end if
         end select
      end do
   end subroutine

   subroutine insert_function_writeback(output, input, arg, proc, inst)
      type(def_info), intent(inout) :: output
      type(full_ir), target, intent(inout) :: input
      type(operand_ir_var), intent(in) :: arg
      type(ir_procedure), intent(in) :: proc
      type(ir_instruction), target, intent(inout) :: inst

      integer(BIG) :: k

      do k = output%out_defs%size, 1, -1
         select type (orig => output%out_defs%get(k))
         type is (operand_ir_var)
            if (ir_could_interfere(input, arg, orig)) then
               !call throw('Func Writeback: '//op_string(orig, input), inst%loc, .false.)
               call inst%writeback%push(orig)
               ! only invalidates if impure
               if (proc%pure) cycle
               call inst%invalidate%push(orig)
               call output%out_defs%fast_remove(k)
            end if
         end select
      end do
   end subroutine

   subroutine insert_impure_writeback(output, input, inst, impure_usage, proc)
      type(def_info), intent(inout) :: output
      type(full_ir), intent(in) :: input
      type(ir_instruction), intent(inout) :: inst
      type(list), intent(in) :: impure_usage
      type(ir_procedure), intent(in) :: proc

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
                  call inst%writeback%push(var)
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
                  call inst%writeback%push(var)
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
               call inst%writeback%push(var)
               call inst%invalidate%push(var)
               call output%out_defs%fast_remove(j)
               cycle outer
            end do
         end select
      end do outer
   end subroutine

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
