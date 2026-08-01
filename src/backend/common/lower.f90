module backend_lower
   use include, only: SMALL, BIG, bitoa, throw, location
   use ir_instructions, only: ir_instruction, INST_PHI, ir_op_container
   use ir, only: full_ir, ir_procedure, ir_type, ir_var, ir_subtype, ir_block, HINT_INVALID, operand_ir_var, operand_ssa_var, &
      full_ir_type, comptime_int, operand_comptime
   use data_mod, only: list
   implicit none (type, external)
contains
   recursive subroutine lower_ir_types(input, associations)
      type(full_ir), target, intent(inout) :: input
      type(list), intent(inout) :: associations(:)

      integer(BIG) :: i, j, k, offset
      integer(BIG), allocatable :: var_map(:, :), counts(:)
      integer(BIG) :: max_comps
      integer(BIG) :: inputvarssize
      class(*), allocatable :: temp_var

      logical :: change

      ! find max components
      max_comps = 0
      do i = 1, input%types%size
         select type (type => input%types%get(i))
         class default
            error stop 'invalid input%types argument to lower_ir_types'
         type is (ir_type)
            max_comps = max(max_comps, type%comp_count())
         end select
      end do

      ! build new var map
      allocate(var_map(max_comps, input%vars%size))
      allocate(counts(input%vars%size))

      change = .false.
      inputvarssize = input%vars%size
      do i = 1, inputvarssize
         select type (var => input%vars%get(i))
         class default
            error stop 'invalid input%vars argument to lower_ir_types'
         type is (ir_var)
            if (.not.var%active) cycle
            if (var%type%indirection_count /= 0) then
               var_map(1, i) = i
               counts(i) = 1
               cycle
            end if

            select type (type => input%types%get(var%type%type))
            type is (ir_type)
               if (type%subtypes%size < 1) then
                  call throw('Type must have at least one component', var%loc, .false.)
                  var_map(1, i) = i
                  counts(i) = 1
                  cycle
               end if

               if (type%comp_count() == 1) then
                  select type (subtype => type%subtypes%get(1_BIG))
                  class default
                     error stop 'invalid type in lower_ir_types'
                  type is (ir_subtype)
                     if (subtype%count == 1 .and. subtype%hint /= HINT_INVALID) then
                        var_map(1, i) = i
                        counts(i) = 1
                        cycle
                     end if
                  end select
               end if

               ! iterate over subtypes
               var%active = .false.
               counts(i) = type%comp_count()
               offset = 1
               change = .true.
               do j = 1, type%subtypes%size
                  select type (subtype => type%subtypes%get(j))
                  class default
                     error stop 'invalid type in lower_ir_types'
                  type is (ir_subtype)
                     do k = 1, subtype%count
                        temp_var = var
                        select type (temp_var)
                        type is (ir_var)
                           temp_var%active = .true.
                           temp_var%name = var%name//'.'//bitoa(offset)
                           if (subtype%hint /= HINT_INVALID) then
                              call throw('Compound type must be of non fundamental types', var%loc, .false.)
                           else
                              temp_var%type = subtype%type
                           end if
                        end select
                        call input%vars%move_push(temp_var)
                        var_map(offset, i) = input%vars%size
                        offset = offset + 1
                     end do
                  end select
               end do
            end select
         end select
      end do

      ! no need to update global vars since they should all be pointers

      ! update procedures
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'invalid input argument to lower_ir_types'
         type is (ir_procedure)
            if (lower_proc_types(input, proc, var_map, counts, associations(i))) change = .true.
         end select
      end do

      if (change) call lower_ir_types(input, associations)
   end subroutine

   function lower_proc_types(input, proc, var_map, counts, associations) result(change)
      type(full_ir), target, intent(inout) :: input
      type(ir_procedure), target, intent(inout) :: proc
      integer(BIG), intent(in) :: var_map(:, :), counts(:)
      type(list), intent(inout) :: associations
      logical :: change

      integer(BIG) :: i, arg_count, j, subtype_count, procvarssize
      class(*), allocatable :: temp_type, temp_value
      integer(BIG), allocatable :: new_args(:)
      integer, allocatable :: new_ssa_args(:)
      integer(BIG), allocatable :: ssa_map(:, :), ssa_counts(:)
      type(ir_subtype), pointer :: curr_subtype
      type(ir_block), pointer :: blk_ptr

      type(list) :: new_assoc

      ! lower vars
      change = .false.
      i = 1_BIG
      do while (i <= proc%vars%size)
         select type (idx => proc%vars%get(i))
         class default
            error stop 'invalid proc argument to lower_proc_types'
         type is (integer(BIG))
            ! add all count vars
            if (counts(idx) <= 0) then
               error stop 'non positive count'
            end if

            if (var_map(1, idx) /= idx) change = .true.

            do j = 1, counts(idx)
               call proc%vars%insert(i, var_map(j, idx))
               i = i + 1
            end do
            
            ! remove original
            call proc%vars%remove(i)
         end select
      end do

      ! create ssa var mappings
      allocate(ssa_map(size(var_map, 1), proc%ssa_counter - 1))
      allocate(ssa_counts(proc%ssa_counter - 1))

      new_assoc = list(full_ir_type())
      proc%ssa_counter = 1
      do i = 1, associations%size
         select type (assoc => associations%get(i))
         class default
            error stop 'malformed associations argument to lower_proc_types'
         type is (full_ir_type)
            if (assoc%indirection_count /= 0) then
               ssa_map(1, i) = proc%ssa_counter
               ssa_counts(i) = 1
               temp_value = associations%move_get(i)
               call new_assoc%move_push(temp_value)
               proc%ssa_counter = proc%ssa_counter + 1
               cycle
            end if

            select type (var_type => input%types%get(assoc%type))
            class default
               error stop 'malformed input argument to lower_proc_types'
            type is (ir_type)
               ssa_counts(i) = var_type%comp_count()
               select type (subtype => var_type%subtypes%get(1_BIG))
               type is (ir_subtype)
                  if (ssa_counts(i) == 1 .and. subtype%hint /= HINT_INVALID) then
                     ssa_map(1, i) = proc%ssa_counter
                     ssa_counts(i) = 1
                     temp_value = associations%move_get(i)
                     call new_assoc%move_push(temp_value)
                     proc%ssa_counter = proc%ssa_counter + 1
                     cycle
                  end if
               end select
               change = .true.
               subtype_count = 0
               do j = 1, ssa_counts(i)
                  ssa_map(j, i) = proc%ssa_counter
                  if (subtype_count == 0) then

                     select type (subtype => var_type%subtypes%get(j))
                     class default
                        error stop 'malformed type in lower proc type'
                     type is (ir_subtype)
                        if (subtype%hint /= HINT_INVALID) then
                           error stop 'compound type must be of non fundamental types'
                        end if
                        curr_subtype => subtype
                        subtype_count = subtype%count
                     end select
                  end if
                  temp_type = curr_subtype%type
                  subtype_count = subtype_count - 1_BIG
                  call new_assoc%move_push(temp_type)
                  proc%ssa_counter = proc%ssa_counter + 1
               end do
            end select
         end select
      end do

      ! lower arguments
      if (allocated(proc%arguments)) then
         arg_count = 0
         do i = 1, size(proc%arguments)
            arg_count = arg_count + counts(proc%arguments(i))
         end do
         allocate(new_args(arg_count))
         if (proc%blocks%size >= 1) allocate(new_ssa_args(arg_count))
         arg_count = 1
         do i = 1, size(proc%arguments)
            do j = 1, counts(proc%arguments(i))
               new_args(arg_count) = var_map(j, proc%arguments(i))
               if (proc%blocks%size >= 1) then
                  new_ssa_args(arg_count) = ssa_map(j, proc%ssa_arguments(i))
               end if
               arg_count = arg_count + 1
            end do
         end do
         call move_alloc(new_args, proc%arguments)
         call move_alloc(new_ssa_args, proc%ssa_arguments)
      end if

      call new_assoc%move(associations)

      do i = 1, proc%blocks%size
         ! TODO: remove when lfortran fixes bug
         blk_ptr => proc%get_block(input, i)
         call lower_block_types(input, associations, proc, blk_ptr, ssa_map, ssa_counts, change)
      end do
   end function

   subroutine lower_block_types(input, associations, proc, blk, ssa_map, ssa_counts, change)
      type(full_ir), target, intent(inout) :: input
      type(list), intent(in) :: associations
      type(ir_procedure), target, intent(inout) :: proc
      type(ir_block), target, intent(inout) :: blk
      integer(BIG), intent(in) :: ssa_map(:, :), ssa_counts(:)
      logical, intent(inout) :: change

      integer(BIG) :: i

      ! TODO: resolve phis
      do i = 1, blk%content%size
         select type (inst => blk%content%get(i))
         class default
            error stop 'invalid blk argument to lower_block_types'
         type is (ir_instruction)
            if (allocated(inst%op1)) call lower_op_types(inst%op1, ssa_map, ssa_counts, associations, input, inst%loc, change)
            if (allocated(inst%op2)) call lower_op_types(inst%op2, ssa_map, ssa_counts, associations, input, inst%loc, change)
         end select
      end do
   end subroutine

   subroutine lower_op_types(ops, ssa_map, ssa_counts, associations, input, loc, change)
      type(ir_op_container), allocatable, intent(inout) :: ops(:)
      integer(BIG), intent(in) :: ssa_map(:, :), ssa_counts(:)
      type(list), intent(in) :: associations
      type(full_ir), intent(in) :: input
      type(location), intent(in) :: loc
      logical, intent(inout) :: change

      integer(BIG) :: new_size, i, idx, j, bit_count
      type(ir_op_container), allocatable :: new_ops(:)
      integer(BIG) :: lindex, loffset, uindex, uoffset, max_comp, max_bit
      ! TODO: arbitrary size
      integer(BIG) :: value
      type(ir_subtype), pointer :: subtype

      new_size = 0
      do i = 1, size(ops)
         select type (op => ops(i)%val)
         class default
            new_size = new_size + 1
         type is (operand_ssa_var)
            if (op%slice) then
               new_size = new_size + op%uindex - op%lindex + 1
            else
               new_size = new_size + ssa_counts(op%idx)
            end if
         type is (operand_comptime)
            select type (val => op%val)
            class default
               new_size = new_size + 1
            type is (comptime_int)
               if (val%type == 0) then
                  new_size = new_size + 1
               else
                  select type (type => input%types%get(val%type))
                  class default
                     error stop 'malformed input ir'
                  type is (ir_type)
                     new_size = new_size + type%comp_count()
                  end select
               end if
            end select
         end select
      end do

      allocate(new_ops(new_size))

      idx = 1
      do i = 1, size(ops)
         select type (op => ops(i)%val)
         class default
            call move_alloc(ops(i)%val, new_ops(idx)%val)
            idx = idx + 1
         type is (operand_comptime)
            val_type: &
            select type (val => op%val)
            class default
               call move_alloc(ops(i)%val, new_ops(idx)%val)
               idx = idx + 1
            type is (comptime_int)
               if (val%type == 0) then
                  call move_alloc(ops(i)%val, new_ops(idx)%val)
                  idx = idx + 1
                  exit val_type
               end if
               value = val%val
               select type (type => input%types%get(val%type))
               type is (ir_type)
                  do j = 1, type%comp_count()
                     subtype => type%comp(j)
                     if (subtype%hint == HINT_INVALID) then
                        bit_count = subtype%type%bit_count(input)
                     else
                        new_ops(idx)%val = operand_comptime(val=comptime_int(val=value))
                        idx = idx + 1
                        exit
                     end if
                     if (subtype%type%indirection_count /= 0) then
                        call throw('Current cannot have int literal for pointer value', loc)
                     end if
                     new_ops(idx)%val = operand_comptime(val=comptime_int( &
                        val=iand(value, 2_BIG ** bit_count - 1), &
                        type=subtype%type%type &
                     ))
                     idx = idx + 1
                     change = .true.
                     value = shiftr(value, bit_count)
                  end do
               end select
            end select val_type
         type is (operand_ssa_var)
            if (op%slice) then
               if (op%lindex < 1 .or. op%uindex > ssa_counts(op%idx)) then
                  call throw('Slice index out of range', loc)
               end if
               call find_comp(lindex, loffset, op%loffset, ssa_map(op%lindex, op%idx), associations, input, loc, max_comp, max_bit)
               call find_comp(uindex, uoffset, op%uoffset, ssa_map(op%uindex, op%idx), associations, input, loc)
               if (op%lindex == op%uindex) then
                  new_ops(idx)%val = operand_ssa_var(idx=ssa_map(op%lindex, op%idx), slice=.true., &
                     lindex=lindex, loffset=loffset, &
                     uindex=uindex, uoffset=uoffset)
                  idx = idx + 1
               else
                  new_ops(idx)%val = operand_ssa_var(idx=ssa_map(op%lindex, op%idx), slice=.true., &
                     lindex=lindex, loffset=loffset, &
                     uindex=max_comp, uoffset=max_bit)
                  idx = idx + 1
                  do j = op%lindex + 1, op%uindex - 1
                     new_ops(idx)%val = operand_ssa_var(idx=ssa_map(j, op%idx))
                     idx = idx + 1
                  end do
                  new_ops(idx)%val = operand_ssa_var(idx=ssa_map(op%uindex, op%idx), slice=.true., &
                     lindex=1, loffset=0, &
                     uindex=uindex, uoffset=uoffset)
                  idx = idx + 1
               end if
            else
               do j = 1, ssa_counts(op%idx)
                  new_ops(idx)%val = operand_ssa_var(idx=ssa_map(j, op%idx))
                  idx = idx + 1
               end do
            end if
         end select
      end do

      call move_alloc(new_ops, ops)
   end subroutine

   subroutine find_comp(out_index, out_offset, in_offset, ssa_idx, associations, input, loc, max_comp, max_bit)
      integer(BIG), intent(out) :: out_index
      integer(BIG), intent(out) :: out_offset
      integer(BIG), intent(in) :: in_offset
      integer(BIG), intent(in) :: ssa_idx
      type(list), intent(in) :: associations ! integer(BIG)
      type(full_ir), intent(in) :: input
      type(location), intent(in) :: loc
      integer(BIG), intent(out), optional :: max_comp
      integer(BIG), intent(out), optional :: max_bit

      select type (type => associations%get(ssa_idx))
      class default
         error stop 'malformed associations list in find_comp'
      type is (full_ir_type)
         call find_comp_type(out_index, out_offset, in_offset, type, input, loc, max_comp, max_bit)
      end select
   end subroutine

   subroutine find_comp_type(out_index, out_offset, in_offset, type, input, loc, max_comp, max_bit)
      integer(BIG), intent(out) :: out_index
      integer(BIG), intent(out) :: out_offset
      integer(BIG), intent(in) ::  in_offset
      type(full_ir_type), intent(in) :: type
      type(full_ir), intent(in) :: input
      type(location), intent(in) :: loc
      integer(BIG), intent(out), optional :: max_comp
      integer(BIG), intent(out), optional :: max_bit

      integer(BIG) :: offset, index, last_size, count

      if (type%indirection_count /= 0) then
         if (present(max_comp)) then
            max_comp = 1
         end if
         if (present(max_bit)) then
            max_bit = 1
         end if
         out_index = 1
         out_offset = 0
         return
      end if

      offset = in_offset
      select type (true_type => input%types%get(type%type))
      class default
         error stop 'malformed input argument to find_comp_type'
      type is (ir_type)
         if (present(max_comp)) then
            max_comp = true_type%comp_count()
         end if
         if (present(max_bit)) then
            select type (subtype => true_type%subtypes%get(true_type%subtypes%size))
            class default
               error stop 'malformed subtype in find_comp_type'
            type is (ir_subtype)
               if (subtype%hint == HINT_INVALID) then
                  max_bit = subtype%type%bit_count(input) - 1
               else
                  max_bit = subtype%size - 1
               end if
            end select
         end if
         index = 0
         count = 0
         out_index = 0
         do while (offset >= 0)
            if (count == 0) then
               index = index + 1
               if (index > true_type%comp_count()) then
                  call throw('Offset '//bitoa(in_offset)//' exceeds type size', loc, .false.)
                  out_offset = 0
                  return
               end if
               select type (subtype => true_type%subtypes%get(index))
               class default
                  error stop 'malformed type argument to find_comp_type'
               type is (ir_subtype)
                  if (subtype%hint == HINT_INVALID) then
                     last_size = subtype%type%bit_count(input)
                  else
                     last_size = subtype%size
                  end if
                  count = subtype%count
               end select
            end if
            offset = offset - last_size
            count = count - 1
            out_index = out_index + 1
         end do
         if (index < 1) then
            call throw('Offset cannot be negative', loc, .false.)
            out_offset = 0
            out_index = 1
            return
         end if
         out_offset = offset + last_size
      end select
   end subroutine

   subroutine lower_ir_bits(input, bits)
      type(full_ir), target, intent(inout) :: input
      integer(SMALL), intent(in) :: bits

      integer(BIG) :: i

      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'invalid input argument to lower_ir_bits'
         type is (ir_procedure)
            call lower_proc_bits(input, bits, proc)
         end select
      end do
   end subroutine

   subroutine lower_proc_bits(input, bits, proc)
      type(full_ir), target, intent(inout) :: input
      integer(SMALL), intent(in) :: bits
      type(ir_procedure), target, intent(inout) :: proc
   end subroutine
end module
