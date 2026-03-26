module ir_parse_procedure_helper
   use include, only: BIG, trim_index
   use ir, only: full_ir, ir_procedure
   use ir_parse_helper, only: parse_type_string
   use ir_parse_var, only: parse_var

   implicit none (type, external)
contains
   function parse_procedure_properties(result, line) result(ok)
      type(ir_procedure), intent(inout) :: result
      character(:), pointer, intent(inout) :: line
      logical :: ok

      integer :: end_index

      if (line(:1) == '^') then
         result%fundamental = .true.
         line => line(2:)
      end if

      if (line(:1) == '(') then
         do while (line(:1) /= ')')
            line => line(2:)
            line => line(trim_index(line):)
            end_index = index(line, ',')
            if (end_index == 0 .or. index(line, ')') < end_index) then
               end_index = index(line, ')')
            end if
            if (end_index == 0) then
               ok = .false.
               return
            end if
            select case (line(:end_index - 1))
            case ('simple')
               result%simple = .true.
               result%pure = .true.
            case ('commutative')
               result%commutative = .true.
            case ('pure')
               result%pure = .true.
            case ('associative')
               result%associative = .true.
            case ('zero_identity')
               result%zero_identity = .true.
            case ('one_identity')
               result%one_identity = .true.
            case ('lreduce_0_0')
               result%lreduce_0_0 = .true.
            case ('rreduce_0_0')
               result%rreduce_0_0 = .true.
            case ('rreduce_0_1')
               result%rreduce_0_1 = .true.
            case ('rzero_illegal')
               result%rzero_illegal = .true.
            case ('reduce_add')
               result%reduce_add = .true.
            case ('reduce_mlt')
               result%reduce_mlt = .true.
            case ('eval')
               result%evaluatable = .true.
            case ('non_fund')
               if (result%non_fundamental) then
                  ok = .false.
                  return
               end if
               result%non_fundamental = .true.
            case default
               ok = .false.
               return 
            end select
            line => line(end_index:)
         end do
         line => line(2:)
      end if

      ok = .true.
   end function

   function count_procedure_args(line) result(count)
      character(:), pointer, intent(inout) :: line
      integer :: count

      integer :: loc

      count = 0
      if (line(:1) /= ')') count = 1
      loc = 1
      do while (line(loc:loc) /= ')')
         if (line(loc:loc) == ',') count = count + 1
         loc = loc + 1
      end do

      if (loc >= 4) then
         if (line(loc - 3:loc) == '...)') then
            count = count - 1
         end if
      end if
   end function

   function parse_procedure_arguments(result, curr_ir, line) result(ok)
      type(ir_procedure), intent(inout) :: result
      type(full_ir), intent(inout) :: curr_ir
      character(:), pointer, intent(inout) :: line
      logical :: ok

      integer :: loc, end_index
      integer(BIG) :: precount
      character(:), allocatable :: type_str
      
      loc = 1
      do while (line(:1) /= ')')
         if (line(:1) == ',') then
            line => line(2:)
         end if
         line => line(trim_index(line):)
         if (line(:min(4, len(line))) == '...)') then
            result%variadic = .true.
            line => line(4:)
            exit
         end if
         end_index = index(line, ',')
         if (end_index == 0) then
            end_index = index(line, ')')
         end if

         precount = curr_ir%vars%size
         call parse_var(curr_ir, start_line=line(:end_index - 1))
         if (precount /= curr_ir%vars%size) then
            call result%vars%push(curr_ir%vars%size)
            result%arguments(loc) = curr_ir%vars%size
         end if
         line => line(end_index:)
!         end_index = index(line, ':')
!         if (end_index > index(line, ',') .and. end_index > index(line, ')')) then
!            ok = .false.
!            return
!         end if
!         result%argument_names(loc)%val = line(:end_index - 1)
!         line => line(end_index + 1:)
!         line => line(trim_index(line):)
!         end_index = index(line, ',')
!         if (end_index == 0) end_index = index(line, ')')
!         if (end_index == 0) then
!            ok = .false.
!            return
!         end if
!         type_str_ptr => line(:end_index - 1)
!         if (line(end_index:end_index) == ')') then
!            line => line(end_index:)
!         else
!            line => line(end_index + 1:)
!            line => line(trim_index(line):)
!         end if
!         result%arguments(loc) = parse_type_string(curr_ir, type_str_ptr, result%loc)
         loc = loc + 1
      end do
      line => line(2:)
      end_index = index(line, ':')
      if (end_index /= 0) then
         line => line(end_index + 1:)
         line => line(trim_index(line):)
         if (index(line, '{') == 0) then
            type_str = trim(line)
         else
            type_str = trim(line(:index(line, '{') - 1))
         end if
         result%return_type = parse_type_string(curr_ir, type_str, result%loc)
      end if

      ok = .true.
   end function
end module
