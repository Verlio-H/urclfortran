module ir_parse_code
   use include, only: SMALL, BIG, location, count_char, throw, atobi, trim_index
   use ir_instructions, only: ir_operand, ir_op_container, INST_RET, INST_ASSIGN, INST_CALL, INST_JMP, INST_BNZ
   use ir, only: full_ir, ir_block, ir_procedure, ir_instruction, ir_var, operand_ir_var, operand_comptime, base_comptime_val, &
      operand_ir_block
   use ir_parse_helper, only: parse_comptime_val
   use data_mod, only: list

   implicit none (type, external)
contains
   subroutine parse_instruction(curr_ir, str, idx, proc, loc)
      type(full_ir), intent(inout) :: curr_ir
      character(*), target, intent(in) :: str
      integer(BIG), intent(in) :: idx
      type(ir_procedure), intent(inout) :: proc
      type(location), intent(in) :: loc

      type(ir_instruction) :: result
      character(:), pointer :: current, left, right

      integer :: end_index

      result%loc = loc
      if (str(:min(len(str), len('return '))) == 'return ') then
         result%inst_type = INST_RET

         current => str(len('return ') + 1:)
         if (len(current) /= 0) then
            allocate(result%op1(count_char(current, ',') + 1))
            if (.not.parse_value_list(result%op1(:), curr_ir, current, proc, loc)) return
         end if
      else if (index(str, '<-') /= 0) then
         result%inst_type = INST_ASSIGN
         end_index = index(str, '<-')
         left => str(:end_index - 1)
         right => str(end_index + 2:)

         left => left(trim_index(left):)
         if (len(left) /= 0) then
            allocate(result%op1(count_char(left, ',') + 1))
            if (.not.parse_value_list(result%op1(:), curr_ir, left, proc, loc)) return
         end if

         right => right(trim_index(right):)
         if (len(right) /= 0) then
            allocate(result%op2(count_char(right, ',') + 1))
            if (.not.parse_value_list(result%op2(:), curr_ir, right, proc, loc)) return
         end if
      else if (index(str, '=') /= 0) then
         result%inst_type = INST_CALL
         end_index = index(str, '=')
         left => str(:end_index - 1)
         right => str(end_index + 1:)

         left => left(trim_index(left):)
         if (len(left) /= 0) then
            allocate(result%op1(count_char(left, ',') + 1))
            if (.not.parse_value_list(result%op1(:), curr_ir, left, proc, loc)) return
         end if

         right => right(trim_index(right):)
         end_index = index(right, '(')
         if (end_index == 0) then
            call throw('Call missing left paren', loc, .false.)
            return
         end if
         current => right(:end_index - 1)
         right => right(end_index + 1:)
         end_index = index(right, ')')
         if (end_index == 0) then
            call throw('Call missing right paren', loc, .false.)
            return
         end if
         right => right(:end_index - 1)
         right => right(trim_index(right):)
         if (len(right) == 0) then
            allocate(result%op2(1))
         else
            allocate(result%op2(count_char(right, ',') + 2))
            if (.not.parse_value_list(result%op2(2:), curr_ir, right, proc, loc)) return
         end if
         if (.not.parse_value(result%op2(1)%val, curr_ir, current, proc, loc)) return
      else if (str(:min(len(str), 2)) == 'j ') then
         result%inst_type = INST_JMP
         current => str(2:)
         current => current(trim_index(current):)
         if (len(current) == 0) then
            call throw('Missing block in jump statement', loc, .false.)
            return
         end if
         allocate(result%op1(1))
         allocate(operand_ir_block :: result%op1(1)%val)
         select type (val => result%op1(1)%val)
         type is (operand_ir_block)
            val%name = current
         end select
      else if (str(:min(len(str), 4)) == 'bnz ') then
         result%inst_type = INST_BNZ
         current => str(4:)
         current => current(trim_index(current):)
         if (len(current) == 0) then
            call throw('Missing value in bnz statement', loc, .false.)
            return
         end if
         left => current(:index(current, ' ') - 1)
         ! condition
         allocate(result%op1(1))
         if (.not.parse_value(result%op1(1)%val, curr_ir, left, proc, loc)) return

         ! destinations
         allocate(result%op2(2))
         allocate(operand_ir_block :: result%op2(1)%val)
         allocate(operand_ir_block :: result%op2(2)%val)
         current => current(index(current, ' ') + 1:)
         current => current(trim_index(current):)

         if (len(current) == 0) then
            call throw('Missing destination block in bnz statement', loc, .false.)
            return
         end if

         select type (val => result%op2(1)%val)
         type is (operand_ir_block)
            val%name = current(:index(current, ' ') - 1)
            current => current(index(current, ' ') + 1:)
         end select

         current => current(trim_index(current):)

         if (len(current) == 0) then
            call throw('Missing default block in bnz statement', loc, .false.)
            return
         end if

         select type (val => result%op2(2)%val)
         type is (operand_ir_block)
            val%name = current(:len_trim(current))
         end select
      else if (len_trim(str) /= 0) then
         call throw('Unknown instruction: '//str, loc, .false.)
         return
      end if

      select type (block => curr_ir%blocks%get(idx))
      class default
         error stop 'invalid curr_ir argument to parse_instruction'
      type is (ir_block)
         call block%content%push(result)
      end select
   end subroutine

   function parse_value_list(output, fullir, str, proc, loc) result(ok)
      type(ir_op_container), intent(out) :: output(:)
      type(full_ir), intent(inout) :: fullir
      character(*), target, intent(in) :: str
      type(ir_procedure), intent(inout) :: proc
      type(location), intent(in) :: loc
      logical :: ok

      character(:), pointer :: current
      integer :: i, end_index

      ok = .true.

      current => str
      do i = 1, size(output)
         if (len(current) == 0) then
            error stop 'invariant broken in parse value list'
         end if
         end_index = index(current, ',')
         if (end_index == 0) then
            end_index = len(current) + 1
         end if
         
         ok = parse_value(output(i)%val, fullir, current(:end_index - 1), proc, loc)
         if (.not.ok) return
         current => current(end_index + 1:)
      end do
      
   end function

   function parse_value(output, fullir, str, proc, loc) result(ok)
      class(ir_operand), allocatable, intent(out) :: output
      type(full_ir), intent(inout) :: fullir
      character(*), target, intent(in) :: str
      type(ir_procedure), intent(inout) :: proc
      type(location), intent(in) :: loc
      logical :: ok

      integer(BIG) :: i, match_idx
      logical :: match
      integer(SMALL) :: indirection_count
      integer(BIG) :: lindex, loffset
      integer(BIG) :: uindex, uoffset
      integer :: end_index
      logical :: slice

      character(:), pointer :: curr
      character(:), allocatable, target :: true

      true = trim(str(trim_index(str):))
      curr => true

      ok = .true.
      if (len(str) == 0) then
         return
      end if
      
      slice = .false.
      if (curr(:1) == '[') then
         slice = .true.
         lindex = 1
         uindex = 1
         curr => curr(2:)
         end_index = index(curr, ':')
         if (end_index == 0 .or. end_index >= index(curr, ']')) then
            call throw('Invalid slice', loc, .false.)
            return
         end if

         end_index = index(curr, '.')
         if (end_index /= 0 .and. end_index < index(curr, ':')) then
            lindex = atobi(curr(:end_index - 1)) + 1
            curr => curr(end_index + 1:)
         end if

         end_index = index(curr, ':')
         loffset = atobi(curr(:end_index - 1))
         curr => curr(end_index + 1:)

         end_index = index(curr, '.')
         if (end_index /= 0 .and. end_index < index(curr, ']')) then
            uindex = atobi(curr(:end_index - 1)) + 1
            curr => curr(end_index + 1:)
         end if

         end_index = index(curr, ']')
         uoffset = atobi(curr(:end_index - 1))
         curr => curr(end_index + 1:)
      end if

      indirection_count = 0_SMALL
      do while (curr(:1) == '*')
         indirection_count = indirection_count + 1_SMALL
         curr => curr(2:)
      end do

      match = .false.
      do i = 1, proc%vars%size
         select type (idx => proc%vars%get(i))
         class default
            error stop 'invalid proc argument to parse_value'
         type is (integer(BIG))
            select type (var => fullir%vars%get(idx))
            class default
               error stop 'invalid fullir argument to parse_value'
            type is (ir_var)
               if (var%name == curr) then
                  match_idx = idx
                  match = .true.
                  exit
               end if
            end select
         end select
      end do

      if (.not.match) then
         do i = 1, fullir%global_vars%size
            select type (idx => fullir%global_vars%get(i))
            class default
               error stop 'invalid fullir argument to parse_value'
            type is (integer(BIG))
               select type (var => fullir%vars%get(idx))
               class default
                  error stop 'invalid fullir argument to parse_value'
               type is (ir_var)
                  if (var%name == curr) then
                     match_idx  = idx
                     match = .true.
                     exit
                  end if
               end select
            end select
         end do
      end if

      if (match) then
         allocate(operand_ir_var :: output)
         select type (output)
         type is (operand_ir_var)
            output%var = match_idx
            output%dereference_count = indirection_count
            if (slice) then
               output%slice = slice
               output%lindex = lindex
               output%loffset = loffset
               output%uindex = uindex
               output%uoffset = uoffset
            end if
         end select
         return
      end if

      allocate(operand_comptime :: output)
      select type (output)
      type is (operand_comptime)
         call parse_comptime_val(output%val, curr, loc)
      end select
   end function
end module
