module ir_parse_procedure
   use include, only: BIG, annotated_string, trim_index, throw
   use ir, only: full_ir, ir_procedure, ir_block, ir_procedure_empty, ir_block_empty
   use ir_parse_helper, only: skip_block
   use ir_parse_procedure_helper, only: parse_procedure_properties, count_procedure_args, parse_procedure_arguments
   use ir_parse_code, only: parse_instruction
   use ir_parse_var, only: parse_var
   use data_mod, only: list

   implicit none (type, external)
contains
   subroutine parse_procedure(curr_ir, str, i)
      type(full_ir), target, intent(inout) :: curr_ir
      type(list), target, intent(in) :: str
      integer(BIG), intent(inout) :: i

      type(ir_procedure) :: result
      
      type(ir_block) :: temp_block
      logical :: block_seen

      integer :: count
      integer(BIG) :: precount

      integer :: end_index

      class(*), pointer :: line_str_poly
      class(annotated_string), pointer :: line_str
      character(:), pointer :: line
      class(*), allocatable :: temp

      result = ir_procedure_empty()

      line_str_poly => str%get(i)
      select type (line_str_poly)
      class is (annotated_string)
         line_str => line_str_poly
      class default
         error stop 'invalid str argument for parse_procedure'
      end select

      line => line_str%val(trim_index(line_str%val):)

      result%loc = line_str%loc
      result%loc%loc_chain(1)%start_column = trim_index(line_str%val)

      if (len_trim(line) == 0) then
         call throw('Invalid procedure declaration', result%loc, .false.)
         return
      end if

      if(.not.parse_procedure_properties(result, line)) then
         call throw('Invalid procedure properties', result%loc, .false.)
         return
      end if

      end_index = index(line, '(')
      if (end_index == 0) then
         call throw('Invalid procedure declaration', result%loc, .false.)
         call skip_block(str, i)
         return 
      end if

      result%name = line(:end_index - 1)
      line => line(end_index + 1:)
      line => line(trim_index(line):)

      count = count_procedure_args(line)

      allocate(result%arguments(count))

      if (.not.parse_procedure_arguments(result, curr_ir, line)) then
         call throw('Invalid procedure arguments', result%loc, .false.)
         call skip_block(str, i)
         return
      end if

      ! parse contents
      if (index(line, '{') /= 0) then
         i = i + 1
         line_str_poly => str%get(i)
         select type (line_str_poly)
         class is (annotated_string)
            line_str => line_str_poly
         class default
            error stop 'invalid str argument for parse_procedure'
         end select
         line => line_str%val(trim_index(line_str%val):)

         block_seen = .false.
         do while (index(line, '}') /= 1)
            if (len_trim(line) == 0) then
               continue
            else if (line(trim_index(line):trim_index(line)) == '!') then ! comment
               continue
            else if (line(len_trim(line):) == ':') then ! start of block
               temp_block = ir_block_empty()
               temp_block%name = line(:len_trim(line) - 1)
               allocate(temp, source=temp_block)
               call curr_ir%blocks%move_push(temp)
               call result%blocks%push(curr_ir%blocks%size)
               block_seen = .true.
            else if (.not.block_seen) then ! variable declaration
               precount = curr_ir%vars%size
               call parse_var(curr_ir, str, i)
               if (precount /= curr_ir%vars%size) then
                  call result%vars%push(curr_ir%vars%size)
               end if
            else ! instruction
               call parse_instruction(curr_ir, line, curr_ir%blocks%size, result, line_str%loc)
            end if
            i = i + 1

            line_str_poly => str%get(i)
            select type (line_str_poly)
            class is (annotated_string)
               line_str => line_str_poly
            class default
               error stop 'invalid str argument for parse_procedure'
            end select
            line => line_str%val(trim_index(line_str%val):)
         end do
         if (.not.block_seen) then
            call throw('Missing block declaration in procedure', result%loc)
         end if
      end if

      call skip_block(str, i)

      allocate(temp, source=result)
      call curr_ir%procedures%move_push(temp)

   end subroutine
end module
