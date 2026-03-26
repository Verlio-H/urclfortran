module ir_write
   use ir_instructions, only: ir_instruction, ir_operand, INST_RET, INST_ASSIGN, INST_CALL, INST_JMP, INST_BNZ, ir_op_container
   use ir, only: full_ir, HINT_STRINGS, ir_type, ir_procedure, full_ir_type, ir_var, ir_block, ir_subtype, base_comptime_val, &
      comptime_int, comptime_addr, HINT_INVALID, operand_ir_var, operand_comptime, operand_ir_block
   use include, only: BIG, string, sitoa, SMALL, bitoa
   use data_mod, only: list
    
   implicit none (type, external)
contains
   subroutine write_type(output, input, curr_ir, indent)
      type(list), intent(inout) :: output
      type(ir_type), intent(in) :: input
      type(full_ir), target, intent(in) :: curr_ir
      integer(SMALL), intent(in) :: indent

      integer(BIG) :: j
      character(:), allocatable :: line

      line = '#'//input%name//':'
      do j = 1, input%subtypes%size
         select type (subtype => input%subtypes%get(j))
         type is (ir_subtype)
            line = line//' '
            if (subtype%size /= 0) then
               line = line//sitoa(subtype%size)//'b'
            end if
            if (subtype%hint /= HINT_INVALID) then
               line = line//'(.'//trim(HINT_STRINGS(subtype%hint))//')'
            else
               line = line//'('//type_string(curr_ir, subtype%type)//')'
            end if
            if (subtype%count /= 1) then
               line = line//bitoa(subtype%count)
            end if
         class default
            error stop 'invalid input argument to write_type'
         end select
      end do
      call output%push(string(line))
      call output%push(string(''))
   end subroutine

   subroutine write_procedure(output, input, curr_ir, indent)
      type(list), intent(inout) :: output
      type(ir_procedure), intent(in) :: input
      type(full_ir), target, intent(in) :: curr_ir
      integer(SMALL), intent(in) :: indent

      character(:), allocatable :: line

      integer :: i
      integer(BIG) :: j

      line = repeat('   ', indent)
      if (input%fundamental) then
         line = line//'^'
      end if
      if (input%pure .or. input%simple .or. input%commutative .or. input%associative .or. input%zero_identity .or. &
            input%lreduce_0_0 .or. input%rreduce_0_0 .or. input%rreduce_0_1 .or. input%rzero_illegal .or. input%reduce_add .or. &
            input%reduce_mlt .or. input%evaluatable .or. input%non_fundamental) then
         line = line//'('
         if (input%pure .and. .not.input%simple) line = line//'pure,'
         if (input%simple) line = line//'simple,'
         if (input%commutative) line = line//'commutative,'
         if (input%associative) line = line//'associative,'
         if (input%zero_identity) line = line//'zero_identity,'
         if (input%lreduce_0_0) line = line//'lreduce_0_0,'
         if (input%rreduce_0_0) line = line//'rreduce_0_0,'
         if (input%rreduce_0_1) line = line//'rreduce_0_1,'
         if (input%rzero_illegal) line = line//'rzero_illegal,'
         if (input%reduce_add) line = line//'reduce_add,'
         if (input%reduce_mlt) line = line//'reduce_mlt,'
         if (input%evaluatable) line = line//'eval,'
         if (input%non_fundamental) line = line//'non_fund,'
         line(len(line):) = ')'
      end if
      line = line//input%name//'('

      do i = 1, size(input%arguments)
         select type (var => curr_ir%vars%get(input%arguments(i)))
         type is (ir_var)
            line = line//var_string(curr_ir, var)
            if (i /= size(input%arguments)) line = line//', '
         end select
      end do
      
      if (input%variadic) then
         if (size(input%arguments) > 0) then
            line = line//', '
         end if
         line = line//'...'
      end if
      line = line//')'
      if (allocated(input%return_type)) then
         line = line//': '//type_string(curr_ir, input%return_type)
      end if

      if (input%vars%size /= 0 .or. input%blocks%size /= 0) then
         call output%push(string(line//' {'))
      else
         call output%push(string(line))
      end if

      if (input%vars%size /= 0) then
         do j = size(input%arguments) + 1, input%vars%size
            select type (idx => input%vars%get(j))
            class default
               error stop 'invalid input argument to write procedure'
            type is (integer(BIG))
               select type (var => curr_ir%vars%get(idx))
               class default
                  error stop 'invalid curr_ir argument to write procedure'
               type is (ir_var)
                  call write_var(output, var, curr_ir, indent + 1_SMALL)
               end select
            end select
         end do
      end if
         
      if (input%blocks%size /= 0) then
         do j = 1, input%blocks%size
            select type (idx => input%blocks%get(j))
            class default
               error stop 'invalid input argument to write procedure'
            type is (integer(BIG))
               select type (block => curr_ir%blocks%get(idx))
               class default
                  error stop 'invalid curr_ir argument to write procedure'
               type is (ir_block)
                  call write_block(output, block, curr_ir, indent)
               end select
            end select
         end do
      end if

      if (input%vars%size /= 0 .or. input%blocks%size /= 0) then
         call output%push(string(repeat('   ', indent)//'}'))
      end if
      call output%push(string(''))
   end subroutine

   subroutine write_block(output, input, curr_ir, indent)
      type(list), intent(inout) :: output
      type(ir_block), intent(in) :: input
      type(full_ir), target, intent(in) :: curr_ir
      integer(SMALL), intent(in) :: indent

      integer(BIG) :: i

      call output%push(string(repeat('   ', indent)//input%name//':'))

      do i = 1, input%content%size
         select type (inst => input%content%get(i))
         type is (ir_instruction)
            call write_instruction(output, inst, curr_ir, indent + 1_SMALL)
         class default
            error stop 'invalid block argument to write_block'
         end select
      end do 
   end subroutine

   subroutine write_instruction(output, input, curr_ir, indent)
      type(list), intent(inout) :: output
      type(ir_instruction), intent(in) :: input
      type(full_ir), target, intent(in) :: curr_ir
      integer(SMALL), intent(in) :: indent

      character(:), allocatable :: result

      result = repeat('   ', indent)

      select case (input%inst_type)
      case (INST_RET)
         result = result//'return'
         if (allocated(input%op1)) then
            result = result//' '//op_list_string(input%op1, curr_ir)
         end if
      case (INST_ASSIGN)
         if (allocated(input%op1)) then
            result = result//op_list_string(input%op1, curr_ir)//' '
         end if
         result = result//'<-'
         if (allocated(input%op2)) then
            result = result//' '//op_list_string(input%op2, curr_ir)
         end if
      case (INST_CALL)
         if (allocated(input%op1)) then
            result = result//op_list_string(input%op1, curr_ir)//' '
         end if
         result = result//'= '//op_string(input%op2(1)%val, curr_ir)//'('//op_list_string(input%op2(2:), curr_ir)//')'
      case (INST_JMP)
         result = result//'j '
         if (allocated(input%op1)) then
            result = result//block_list_string(input%op1, curr_ir)
         end if
      case (INST_BNZ)
         result = result//'bnz '
         if (allocated(input%op1)) then
            result = result//op_list_string(input%op1, curr_ir)//' '
         end if
         if (allocated(input%op2)) then
            result = result//' '//block_list_string(input%op2, curr_ir)
         end if
      case default
         error stop 'unknown instruction '//sitoa(input%inst_type)//' in write_instruction'
      end select

      call output%push(string(result))
   end subroutine

   function block_list_string(input, curr_ir) result(result)
      type(ir_op_container), intent(in) :: input(:)
      type(full_ir), intent(in) :: curr_ir
      character(:), allocatable :: result

      integer(BIG) :: i

      result = ''
      do i = 1, size(input)
         if (i /= 1) result = result//' '
         select type (val => input(i)%val)
         class default
            error stop 'invalid input argument to block_list_string'
         type is (operand_ir_block)
            select type (block => curr_ir%blocks%get(val%block_index))
            class default
               error stop 'invalid curr_ir argument to block_list_string'
            type is (ir_block)
               result = result//block%name
            end select
         end select
      end do
   end function

   function op_list_string(input, curr_ir) result(result)
      type(ir_op_container), intent(in) :: input(:)
      type(full_ir), intent(in) :: curr_ir
      character(:), allocatable :: result

      integer(BIG) :: i
      
      result = ''
      do i = 1, size(input)
         if (i /= 1) result = result//', '
         result = result//op_string(input(i)%val, curr_ir)
      end do
   end function

   function op_string(input, curr_ir) result(result)
      class(ir_operand), intent(in), allocatable :: input
      type(full_ir), target, intent(in) :: curr_ir
      character(:), allocatable :: result

      if (.not.allocated(input)) then
         result = 'UNALLOCATED'
         return
      end if

      select type (input)
      class default
         error stop 'unknown type in parse op'
      type is (operand_ir_var)
         result = ''
         if (input%slice) then
            result = '['
            if (input%lindex /= 0) then
               result = result//bitoa(input%lindex)//'.'
            end if
            result = result//bitoa(input%loffset)//':'
            if (input%uindex /= 0) then
               result = result//bitoa(input%uindex)//'.'
            end if
            result = result//bitoa(input%uoffset)//']'
         end if
         result = result//repeat('*', input%dereference_count)
         select type (var => curr_ir%vars%get(input%var))
         class default
            error stop 'invalid curr_ir argument to parse_or'
         type is (ir_var)
            result = result//var%name
         end select
      type is (operand_comptime)
         result = get_comptime_val(input%val, curr_ir)
      end select
   end function

   subroutine write_var(output, input, curr_ir, indent)
      type(list), intent(inout) :: output
      type(ir_var), intent(in) :: input
      type(full_ir), target, intent(in) :: curr_ir
      integer(SMALL), intent(in) :: indent

      character(:), allocatable :: line

      integer(BIG) :: i

      line = repeat('   ', indent)//var_string(curr_ir, input)
      if (allocated(input%contents)) then
         line = line//' {'
      end if
      call output%push(string(line))

      if (allocated(input%contents)) then
         do i = 1, size(input%contents)
            line = repeat('   ', indent + 1_SMALL)//get_comptime_val(input%contents(i)%val, curr_ir)
            call output%push(string(line))
         end do
         call output%push(string(repeat('   ', indent)//'}'))
      end if
   end subroutine

   function var_string(curr_ir, input) result(line)
      type(full_ir), intent(in) :: curr_ir
      type(ir_var), intent(in) :: input
      character(:), allocatable :: line

      if (input%static) then
         line = line//'*'
      end if
      if (input%extern) then
         line = line//'^'
      end if
      if (input%export) then
         line = line//'@'
      end if
      if (input%const) then
         line = line//'#'
      end if

      line = line//input%name//': '//type_string(curr_ir, input%type)
   end function

   function get_comptime_val(val, curr_ir) result(value)
      class(base_comptime_val), intent(in) :: val
      type(full_ir), target, intent(in) :: curr_ir
      character(:), allocatable :: value

      select type (val)
      type is (comptime_int)
         value = bitoa(val%val)
      type is (comptime_addr)
         if (val%proc /= 0) then
            select type (proc => curr_ir%procedures%get(val%proc))
            class default
               error stop 'invalid curr_ir argument to write_comptime_val'
            type is (ir_procedure)
               value = proc%name
            end select
         else if (val%var /= 0) then
            select type (var => curr_ir%vars%get(val%var))
            class default
               error stop 'invalid curr_ir argument to write_comptime_val'
            type is (ir_var)
               value = var%name
            end select
         else
            error stop 'invalid comptime address, neither var nor proc'
         end if
         if (val%offset /= 0) then
            value = value//'+'//bitoa(val%offset)
         end if
      class default
         error stop 'unknown comptime value type'
      end select
   end function

   function type_string(curr_ir, t) result(result)
      type(full_ir), target, intent(in) :: curr_ir
      type(full_ir_type), intent(in) :: t
      character(:), allocatable :: result
      
      type(ir_type), pointer :: type_ptr
      integer(SMALL) :: j

      
      if (.not.t%unknown) then
         select type (type => curr_ir%types%get(t%type))
         type is (ir_type)
            type_ptr => type
         class default
            error stop 'invalid curr_ir argument to type_string'
         end select
      end if
      
      result = ''
      if (t%const_mask(0)) then
         result = result//'#'
      end if
      if (t%array_sizes(0) /= 1) then
         result = result//'['//bitoa(t%array_sizes(0))//']'
      end if
      do j = 1, t%indirection_count
         result = result//'*'
         if (t%const_mask(j)) result = result//'#'
         if (t%restrict_mask(j)) then
            result = result//'%'
         else if (t%restrictish_mask(j)) then
            result = result//'~'
         end if
         if (t%array_sizes(j) /= 1) then
            result = result//'['//bitoa(t%array_sizes(j))//']'
         end if
      end do
      if (t%unknown) then
         result = result//'?'
      else
         result = result//type_ptr%name
      end if
   end function

   subroutine write_ir(output, curr_ir)
      type(list), intent(inout) :: output
      type(full_ir), target, intent(in) :: curr_ir

      integer(BIG) :: i

      do i = 1, curr_ir%types%size
         select type (type => curr_ir%types%get(i))
         type is (ir_type)
            call write_type(output, type, curr_ir, 0_SMALL)
         class default
            error stop 'invalid curr_ir argument to write_ir'
         end select
      end do

      do i = 1, curr_ir%global_vars%size
         select type (idx => curr_ir%global_vars%get(i))
         type is (integer(BIG))
            select type (var => curr_ir%vars%get(idx))
            type is (ir_var)
               call write_var(output, var, curr_ir, 0_SMALL)
            class default
               error stop 'invalid curr_ir argument to write_ir'
            end select
         class default
            error stop 'invalid curr_ir argument to write_ir'
         end select
      end do
      call output%push(string(''))

      do i = 1, curr_ir%procedures%size
         select type (proc => curr_ir%procedures%get(i))
         type is (ir_procedure)
            call write_procedure(output, proc, curr_ir, 0_SMALL)
         class default
            error stop 'invalid curr_ir argument to write_ir'
         end select
      end do 
   end subroutine
end module
