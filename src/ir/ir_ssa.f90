module ir_ssa
   use include, only: BIG
   use data_mod, only: list
   use ir_instructions, only: ir_instruction, INST_PHI
   use ir, only: full_ir, ir_procedure, ir_block, operand_ir_var, operand_ssa_var
   use ir_defs, only: proc_stats, def_info, block_def_info
   implicit none (type, external)
contains
   subroutine insert_phi(input, stats, defs)
      type(full_ir), intent(inout) :: input
      type(proc_stats), allocatable, intent(in) :: stats(:)
      type(def_info), allocatable, intent(in) :: defs(:)

      integer(BIG) :: i

      do i = 1, size(stats)
         select type (proc => input%procedures%get(i))
         class default
            error stop 'invalid input argument to insert_phi'
         type is (ir_procedure)
            call insert_proc_phi(input, stats(i)%frontier, defs(i)%info, proc)
         end select
      end do
   end subroutine

   subroutine insert_proc_phi(input, frontier, defs, proc)
      type(full_ir), intent(inout) :: input
      type(list), allocatable, intent(in) :: frontier(:)
      type(block_def_info), allocatable, intent(in) :: defs(:)
      type(ir_procedure), intent(inout) :: proc

      integer(BIG) :: i, j, k
      type(ir_block), pointer :: blk, newblk
      
      do i = 1, proc%blocks%size
         blk => proc%get_block(input, i)
         do j = 1, defs(i)%out_defs%size
            select type (def => defs(i)%out_defs%get(j))
            class default
               error stop 'invalid defs argument to insert_proc_phi'
            type is (operand_ir_var)
               do k = 1, frontier(i)%size
                  select type (idx => frontier(i)%get(k))
                  class default
                     error stop 'invalid frontier argument to insert_proc_phi'
                  type is (integer(BIG))
                     newblk => proc%get_block(input, idx)
                     call insert_block_phi(input, proc, newblk, def)
                  end select
               end do
            end select
         end do
      end do
   end subroutine

   subroutine insert_block_phi(input, proc, blk, def)
      type(full_ir), intent(inout) :: input
      type(ir_procedure), intent(inout) :: proc
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
               if (def%var /= op2%var) cycle
               if (def%dereference_count /= op2%dereference_count) cycle
               if (def%slice .neqv. op2%slice) cycle
               if (def%slice) then
                  if (def%lindex /= op2%lindex) cycle
                  if (def%loffset /= op2%loffset) cycle
                  if (def%uindex /= op2%uindex) cycle
                  if (def%uoffset /= op2%uoffset) cycle
               end if
               return
            end select
         end select
      end do

      allocate(ir_instruction :: new)
      select type (new)
      type is (ir_instruction)
         new%inst_type = INST_PHI
         allocate(new%op1(2))
         new%op1(1)%val = operand_ssa_var(0, proc%ssa_counter, .false., def%slice, def%lindex, def%loffset, def%uindex, def%uoffset)
         new%op1(2)%val = def
      end select

      proc%ssa_counter = proc%ssa_counter + 1

      call blk%content%move_insert(i, new)
      
   end subroutine
end module
