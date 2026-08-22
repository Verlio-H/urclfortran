module backend_calling_convention
   use include, only: SMALL, BIG, throw
   use ir_instructions, only: ir_instruction, INST_ASSIGN, INST_RET, INST_CALL, ir_op_container
   use ir, only: full_ir, ir_procedure, full_ir_type, ir_type, ir_subtype, ir_var, ir_block, operand_ir_var, HINT_INVALID, &
      operand_comptime, comptime_int, comptime_addr
   use data_mod, only: list
   implicit none (type, external)

   abstract interface
      function cutoff_func_interface(userptr, intermediate, proc, type, isret) result(result)
         import
         class(*), intent(in) :: userptr
         type(full_ir), intent(in) :: intermediate
         type(ir_procedure), intent(in) :: proc
         type(ir_type), intent(in) :: type
         logical, value, intent(in) :: isret
         logical :: result
      end function

      function mapping_func_interface(subtype, userptr) result(result)
         import
         type(ir_subtype), intent(in) :: subtype
         class(*), intent(in) :: userptr
         integer(BIG) :: result
      end function
   end interface
contains
   recursive function type_corrected_bit_count(type, mapping_func, ptrsize, intermediate, userptr) result(result)
      type(ir_type), intent(in) :: type
      procedure(mapping_func_interface) :: mapping_func
      integer(SMALL), value, intent(in) :: ptrsize
      type(full_ir), intent(in) :: intermediate
      class(*), intent(in) :: userptr
      integer(BIG) :: result

      integer(BIG) :: i, subtype_size

      result = 0
      do i = 1, type%subtypes%size
         select type (subtype => type%subtypes%get(i))
         type is (ir_subtype)
            if (subtype%hint == HINT_INVALID) then
               if (subtype%type%indirection_count /= 0) then
                  subtype_size = ptrsize
               else
                  select type (child_type => intermediate%types%get(subtype%type%type))
                  type is (ir_type)
                     subtype_size = type_corrected_bit_count(child_type, mapping_func, ptrsize, intermediate, userptr)
                  end select
               end if
            else
               subtype_size = mapping_func(subtype, userptr)
            end if
            result = subtype%count * subtype_size
         end select
      end do
   end function

   subroutine fix_large_values(input, cutoff_func, userptr)
      type(full_ir), target, intent(inout) :: input
      procedure(cutoff_func_interface), pointer, intent(in) :: cutoff_func
      class(*), intent(in) :: userptr

      integer(BIG) :: i, j, k, m, var_idx, type_idx
      class(*), allocatable :: new_arg, new_inst, new_var
      type(ir_block), pointer :: blk
      
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         type is (ir_procedure)
            do j = 1, size(proc%arguments)
               var_idx = proc%arguments(j)
               select type (var => input%vars%get(var_idx))
               type is (ir_var)
                  if (var%type%indirection_count /= 0) then
                     cycle
                  end if
                  select type (type => input%types%get(var%type%type))
                  type is (ir_type)
                     if (.not.cutoff_func(userptr, input, proc, type, .false.)) cycle
                  end select
                  new_arg = var
               end select

               select type (new_arg)
               type is (ir_var)
                  new_arg%name = '\p\'//new_arg%name
                  if (new_arg%type%indirection_count /= 0) then
                     call throw('Currently cannot pointerify pointers in fix_large_values', proc%loc)
                  end if
                  new_arg%type%indirection_count = new_arg%type%indirection_count + 1_SMALL
                  new_arg%type%restrict_mask = [.false.]
                  new_arg%type%restrictish_mask = [.false.]
                  if (.not.allocated(new_arg%type%const_mask)) then
                     call throw('Const mask not allocated', proc%loc)
                  end if
                  if (size(new_arg%type%const_mask) /= 1) then
                     call throw('Const mask is incorrect size', proc%loc)
                  end if
                  new_arg%type%const_mask = [.true.]
                  allocate(new_arg%type%array_sizes(0:1))
                  new_arg%type%array_sizes(0:) = [1, 1]
               end select
               call input%vars%move_push(new_arg)
               proc%arguments(j) = input%vars%size

               if (proc%blocks%size == 0) cycle
               
               ! add back variable
               call proc%vars%push(var_idx)
               ! get first block
               blk => proc%get_block(input, 1)
               ! add instruction to the start
               new_inst = ir_instruction(inst_type = INST_ASSIGN, loc = proc%loc)
               select type (new_inst)
               type is (ir_instruction)
                  allocate(new_inst%op1(1), new_inst%op2(1))
                  new_inst%op1(1)%val = operand_ir_var(var=var_idx)
                  new_inst%op2(1)%val = operand_ir_var(var=input%vars%size, dereference_count = 1)
               end select
               call blk%content%insert(1, new_inst)
            end do

            if (allocated(proc%return_type)) then
               if (proc%return_type%indirection_count /= 0) then
                  cycle
               else
                  select type (type => input%types%get(proc%return_type%type))
                  type is (ir_type)
                     if (.not.cutoff_func(userptr, input, proc, type, .true.)) cycle
                  end select
               end if
               
               ! add arg
               new_arg = ir_var(name = '\ret', loc = proc%loc)
               select type (new_arg)
               type is (ir_var)
                  new_arg%type%indirection_count = 1
                  new_arg%type%type = proc%return_type%type
                  new_arg%type%const_mask = [.false.]
                  new_arg%type%restrict_mask = [.false.]
                  new_arg%type%restrictish_mask = [.false.]
                  allocate(new_arg%type%array_sizes(0:1))
                  new_arg%type%array_sizes(0:) = [1, 1]
               end select
               call input%vars%move_push(new_arg)
               proc%arguments = [input%vars%size, proc%arguments]
               call proc%vars%insert(1, input%vars%size)
               deallocate(proc%return_type)
               ! replace all return instructions
               do j = 1, proc%blocks%size
                  blk => proc%get_block(input, j)
                  select type (inst => blk%content%get(blk%content%size))
                  type is (ir_instruction)
                     if (inst%inst_type == INST_RET) then
                        inst%inst_type = INST_ASSIGN
                        inst%op2 = inst%op1
                        deallocate(inst%op1)
                        allocate(inst%op1(1))
                        inst%op1(1)%val = operand_ir_var(var = input%vars%size, dereference_count = 1)
                        new_inst = ir_instruction(inst_type = INST_RET, loc = inst%loc)
                        select type (new_inst)
                        type is (ir_instruction)
                           allocate(new_inst%op1(0))
                        end select
                        call blk%content%move_push(new_inst)
                     end if
                  end select
               end do
            end if
         end select
      end do

      ! update call points
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         type is (ir_procedure)
            do j = 1, proc%blocks%size
               blk => proc%get_block(input, j)
               do k = blk%content%size, 1, -1
                  select type (inst => blk%content%get(k))
                  type is (ir_instruction)
                     if (inst%inst_type /= INST_CALL) cycle
                     ret: &
                     block
                        if (.not.allocated(inst%op1)) exit ret
                        if (size(inst%op1) > 1) then
                           call throw('Must have no more than 1 argument on left side of call', inst%loc, .false.)
                        end if
                        if (size(inst%op1) /= 1) exit ret
                        select type (ret_var => inst%op1(1)%val)
                        class default
                           call throw('Invalid operand type on left side of call', inst%loc, .false.)
                        type is (operand_ir_var)
                           select type (var => input%vars%get(ret_var%var))
                           type is (ir_var)
                              if (var%type%indirection_count /= ret_var%dereference_count) exit ret
                              select type (type => input%types%get(var%type%type))
                              type is (ir_type)
                                 if (.not.cutoff_func(userptr, input, proc, type, .true.)) exit ret
                              end select

                              if (ret_var%dereference_count /= 0) then
                                 inst%op2 = [ &
                                    inst%op2(1), &
                                    ir_op_container(operand_ir_var( &
                                       var = ret_var%var, dereference_count = ret_var%dereference_count - 1 &
                                    )), &
                                    inst%op2(2:) &
                                 ]
                                 deallocate(inst%op1)
                              else
                                 ! new temporary value
                                 new_var = ir_var(name ='\tmp_ret', loc = var%loc, const = .true.)
                                 select type (new_var)
                                 type is (ir_var)
                                    new_var%type%type = var%type%type
                                    new_var%type%indirection_count = 1
                                    ! TODO: reason through if this is actually correct
                                    new_var%type%restrict_mask = [.false.]
                                    new_var%type%restrictish_mask = [.true.]
                                    new_var%type%const_mask = [.true.]
                                    allocate(new_var%type%array_sizes(0:1))
                                    new_var%type%array_sizes(:) = [1, 1]
                                 end select
                                 call input%vars%move_push(new_var)
                                 call proc%vars%push(input%vars%size)
                                 inst%op2 = [inst%op2(1), ir_op_container(operand_ir_var(var = input%vars%size)), inst%op2(2:)]
                                 new_inst = ir_instruction(inst_type = INST_ASSIGN, loc = inst%loc)
                                 select type (new_inst)
                                 type is (ir_instruction)
                                    allocate(new_inst%op2(1))
                                    call move_alloc(inst%op1, new_inst%op1)
                                    new_inst%op2(1)%val = operand_ir_var(var = input%vars%size, dereference_count = 1)
                                 end select
                                 call blk%content%insert(k + 1, new_inst)
                              end if
                           end select
                        end select
                     end block ret
                     do m = 2, size(inst%op2)
                        select type (op => inst%op2(m)%val)
                        class default
                           call throw('Invalid argument type to function', inst%loc)
                        type is (operand_ir_var)
                           select type (var => input%vars%get(op%var))
                           type is (ir_var)
                              if (op%dereference_count /= var%type%indirection_count) cycle
                              select type (type => input%types%get(var%type%type))
                              type is (ir_type)
                                 if (.not.cutoff_func(userptr, input, proc, type, .true.)) cycle
                              end select
                              if (op%dereference_count /= 0) then
                                 op%dereference_count = op%dereference_count - 1
                                 cycle
                              end if
                              type_idx = var%type%type
                           end select
                        type is (operand_comptime)
                           select type (true_op => op%val)
                           class default
                              call throw('Invalid argument type to function', inst%loc)
                           type is (comptime_int)
                              if (true_op%type == 0) then
                                 call throw('Integer used as function argument must have type', inst%loc)
                              end if
                              select type (type => input%types%get(true_op%type))
                              type is (ir_type)
                                 if (.not.cutoff_func(userptr, input, proc, type, .true.)) cycle
                              end select
                              type_idx = true_op%type
                           type is (comptime_addr)
                              ! do nothing
                              cycle
                           end select
                        end select
                        ! insert tmp copy
                        new_var = ir_var(name ='\tmp_arg', loc = inst%loc, const = .true.)
                        select type (new_var)
                        type is (ir_var)
                           new_var%type%type = type_idx
                           new_var%type%indirection_count = 1
                           ! TODO: reason through if this is actually correct
                           new_var%type%restrict_mask = [.false.]
                           new_var%type%restrictish_mask = [.true.]
                           new_var%type%const_mask = [.true.]
                           allocate(new_var%type%array_sizes(0:1))
                           new_var%type%array_sizes(:) = [1, 1]
                        end select
                        call input%vars%move_push(new_var)
                        call proc%vars%push(input%vars%size)

                        new_inst = ir_instruction(inst_type = INST_ASSIGN, loc = inst%loc)
                        select type (new_inst)
                        type is (ir_instruction)
                           allocate(new_inst%op2(1), new_inst%op1(1))
                           call move_alloc(inst%op2(m)%val, new_inst%op2(1)%val)
                           new_inst%op1(1)%val = operand_ir_var(var = input%vars%size, dereference_count = 1)
                        end select
                        inst%op2(m)%val = operand_ir_var(var = input%vars%size)
                        ! note that this is possibly an area where things could get messed up
                        ! but the list contains pointers so inst shouldn't point to a new thing
                        call blk%content%insert(k, new_inst)
                     end do
                  end select
               end do
            end do
         end select
      end do
   end subroutine
end module
