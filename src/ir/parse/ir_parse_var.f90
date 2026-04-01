module ir_parse_var
   use include, only: BIG, annotated_string, trim_index, throw
   use ir, only: full_ir, ir_var
   use ir_parse_helper, only: parse_type_string, skip_block, parse_comptime_val
   use data_mod, only: list

   implicit none (type, external)
contains
   subroutine parse_var(curr_ir, str, i, start_line)
      type(full_ir), intent(inout) :: curr_ir
      type(list), intent(in), optional :: str
      integer(BIG), intent(inout), optional :: i
      character(*), target, intent(in), optional :: start_line

      type(ir_var) :: result

      integer :: end_index
      integer(BIG) :: count
      logical :: initialized

      character(:), pointer :: line

      if (present(start_line)) then
         line => start_line
      else
         select type (line_str => str%get(i))
         class default
            error stop 'invalid str argument to parse_var'
         type is (annotated_string)
            line => line_str%val(trim_index(line_str%val):)

            result%loc = line_str%loc
            result%loc%loc_chain(1)%start_column = trim_index(line_str%val)
         end select
      end if

      if (line(:1) == '*') then
         result%static = .true.
         line => line(2:)
      end if
      if (line(:1) == '/') then
         result%extern = .true.
         line => line(2:)
      end if
      if (line(:1) == '^') then
         if (result%extern) then
            call throw('Variable cannot be both extern and export', result%loc, .false.)
            return
         end if
         result%export = .true.
         line => line(2:)
      end if

      if (line(:1) == '#') then
         result%const = .true.
         line => line(2:)
      end if

      if (line(:1) == '&') then
         result%noderef = .true.
         line => line(2:)
      end if
      
      end_index = index(line, ':')
      if (end_index == 0) then
         call throw('Invalid variable declaration', result%loc, .false.)
         return
      end if
      result%name = line(:end_index - 1)
      line => line(end_index + 1:)
      line => line(trim_index(line):)

      end_index = len_trim(line)
      initialized = .false.
      if (line(end_index:end_index) == '{') then
         end_index = end_index - 1
         initialized = .true.
      end if
      result%type = parse_type_string(curr_ir, line(:end_index), result%loc)

      if (initialized) then
         if (.not.present(str) .or. .not.present(str)) then
            error stop 'str and i must be passed if initialized vars are allowed'
         end if
         ! count elements in initializer
         count = 0
         do
            count = count + 1
            select type (line_str => str%get(i + count))
            class default
               error stop 'invalid str argument to parse_var'
            type is (annotated_string)
               line => line_str%val(trim_index(line_str%val):)
            end select

            line => line(trim_index(line):)
            if (line(:1) == '}') then
               exit
            else if (line(:1) == '!') then
               cycle
            end if
         end do
         allocate(result%contents(count - 1))
         
         ! populate elements in initializer
         count = 0
         do
            count = count + 1
            i = i + 1
            select type (line_str => str%get(i))
            type is (annotated_string)
               line => line_str%val(trim_index(line_str%val):)
            end select

            line => line(trim_index(line):)
            if (line(:1) == '}') then
               exit
            else if (line(:1) == '!') then
               cycle
            end if

            call parse_comptime_val(result%contents(count)%val, trim(line), result%loc)
         end do
      end if

      if (present(str) .and. present(i)) then
         call skip_block(str, i)
      end if

      call curr_ir%vars%push(result)
   end subroutine
end module
