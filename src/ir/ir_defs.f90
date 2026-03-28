module ir_defs
   use include, only: SMALL, BIG
   use ir_instructions, only: INST_ASSIGN, INST_CALL, INST_CAST, ir_instruction
   use ir, only: full_ir, operand_ir_var, operand_ssa_var, ir_var, ir_procedure, ir_block
   use ir_graph, only: proc_stats
   use ir_write, only: op_string
   use data_mod, only: list

   implicit none (type, external)

   type :: block_def_info
      type(list) :: out_defs ! operand_ir_var
      type(list) :: out_def_numbers ! integer
   end type

   type :: def_info
      type(block_def_info), allocatable :: info(:)
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
         res = src%dereference_count <= dest%dereference_count
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

   subroutine get_def_info(output, input)
      type(def_info), allocatable, intent(out) :: output(:)
      type(full_ir), intent(in) :: input

      integer(BIG) :: i

      allocate(output(input%procedures%size))
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'invalid input argument to get_def_info'
         type is (ir_procedure)
            call get_proc_def_info(output(i), input, proc)
         end select
      end do
   end subroutine

   subroutine get_proc_def_info(output, input, proc)
      type(def_info), intent(out) :: output
      type(full_ir), intent(in) :: input
      type(ir_procedure), intent(in) :: proc

      integer(BIG) :: i
      type(ir_block), pointer :: blk

      allocate(output%info(proc%blocks%size))
      do i = 1, proc%blocks%size
         blk => proc%get_block(input, i)
         call get_block_def_info(output%info(i), input, blk)
      end do
   end subroutine

   subroutine get_block_def_info(output, input, blk)
      type(block_def_info), intent(out) :: output
      type(full_ir), intent(in) :: input
      type(ir_block), intent(in) :: blk

      integer(BIG) :: i, j, k

      ! TODO: hashmap
      output%out_defs = list(operand_ir_var())
      
      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'invalid blk argument to get_block_def_info'
         type is (ir_instruction)
            select case (inst%inst_type)
            case (INST_ASSIGN, INST_CALL, INST_CAST)
            case default
               cycle
            end select
            if (.not.allocated(inst%op1)) cycle

            do j = 1, size(inst%op1)
               select type (op => inst%op1(j)%val)
               class default
                  cycle
               type is (operand_ir_var)
                  do k = output%out_defs%size, 1, -1
                     select type (dest => output%out_defs%get(k))
                     class default
                        error stop 'something terrible has happened'
                     type is (operand_ir_var)
                        if (ir_could_interfere(input, op, dest)) then
                           call output%out_defs%fast_remove(k)
                        end if
                     end select
                  end do
                  call output%out_defs%push(op)
               end select
            end do
         end select
      end do
   end subroutine

   subroutine print_def_info(curr_ir, proc, defs)
      type(full_ir), intent(in) :: curr_ir
      type(ir_procedure), intent(in) :: proc
      type(block_def_info), intent(in) :: defs(:)

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
