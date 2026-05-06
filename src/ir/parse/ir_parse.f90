module ir_parse
   use include, only: BIG, annotated_string, trim_index, throw, atosi, atobi
   use ir, only: full_ir, ir_type, ir_subtype, HINT_INT, HINT_UINT, HINT_ADDR, HINT_FLOAT, HINT_IVEC_8, HINT_IVEC_16, HINT_IVEC_32,&
      HINT_IVEC_64, HINT_FVEC_16, HINT_FVEC_32, HINT_FVEC_64, ir_type_empty
   use ir_parse_helper, only: parse_type_string
   use ir_parse_var, only: parse_var
   use ir_resolve_references, only: resolve_references
   use ir_parse_procedure, only: parse_procedure
   use data_mod, only: list

   implicit none (type, external)
contains
   subroutine parse_ir(curr_ir, str)
      type(full_ir), intent(inout) :: curr_ir
      type(list), target, intent(in) :: str

      integer(BIG) :: i
      integer(BIG) :: precount

      class(*), pointer :: line_str_poly
      class(annotated_string), pointer :: line_str
      character(:), pointer :: line

      i = 1
      do while (i <= str%size)
         line_str_poly => str%get(i)
         select type (line_str_poly)
         class is (annotated_string)
            line_str => line_str_poly
         class default
            error stop 'invalid str argument for parse_ir'
         end select

         line => line_str%val(trim_index(line_str%val):)
         if (len(line) > 0) then
            if (line(:1) == '#') then
               call parse_type(curr_ir, str, i)
            else if (line(:1) == '*') then
               precount = curr_ir%vars%size
               call parse_var(curr_ir, str, i)
               if (precount /= curr_ir%vars%size) then
                  call curr_ir%global_vars%push(curr_ir%vars%size)
               end if
            else if (line(:1) /= '!') then
               call parse_procedure(curr_ir, str, i)
            end if
         end if
         i = i + 1
      end do

      call resolve_references(curr_ir)
   end subroutine

   subroutine parse_type(curr_ir, str, i)
      type(full_ir), intent(inout) :: curr_ir
      type(list), target, intent(in) :: str
      integer(BIG), intent(inout) :: i

      type(ir_type) :: result
      type(ir_subtype) :: subtype

      integer :: colon_index, adjust_index, end_index

      class(*), pointer :: line_str_poly
      class(annotated_string), pointer :: line_str
      character(:), pointer :: line

      result = ir_type_empty()

      line_str_poly => str%get(i)
      select type (line_str_poly)
      class is (annotated_string)
         line_str => line_str_poly
      class default
         error stop 'invalid str argument for parse_type'
      end select

      line => line_str%val(trim_index(line_str%val):)

      result%loc = line_str%loc
      result%loc%loc_chain(1)%start_column = trim_index(line_str%val)

      line => line(2:)

      colon_index = index(line, ':')
      if (colon_index == 0) then
         call throw('Missing colon in type declaration', result%loc, .false.)
         return
      end if

      result%name = line(:colon_index - 1)
      line => line(colon_index + 1:)
      
      do while (len_trim(line) /= 0)
         adjust_index = trim_index(line) 
         line => line(adjust_index:)
         subtype%count = 1
         end_index = index(line, 'b')            
         if (end_index /= 0 .and. end_index < index(line, '(')) then
            subtype%size = atosi(line(:end_index - 1))
            line => line(end_index + 1:)
         end if
         end_index = index(line, ')')
         if (len(line) == 0) then
            call throw('Invalid size hint in type declaration, missing hint', result%loc, .false.)
            return
         end if
         if (end_index == 0 .or. line(:1) /= '(') then
            error stop line
            call throw('Invalid size hint in type declaration, missing parenthesis', result%loc, .false.)
            return
         end if
         if (line(2:2) == '.') then
            select case (line(3:end_index - 1))
            case ('int')
               subtype%hint = HINT_INT
            case ('uint')
               subtype%hint = HINT_UINT
            case ('addr')
               subtype%hint = HINT_ADDR
            case ('float')
               subtype%hint = HINT_FLOAT
            case ('ivec8')
               subtype%hint = HINT_IVEC_8
            case ('ivec16')
               subtype%hint = HINT_IVEC_16
            case ('ivec32')
               subtype%hint = HINT_IVEC_32
            case ('ivec64')
               subtype%hint = HINT_IVEC_64
            case ('fvec16')
               subtype%hint = HINT_FVEC_16
            case ('fvec32')
               subtype%hint = HINT_FVEC_32
            case ('fvec64')
               subtype%hint = HINT_FVEC_64
            case default
               call throw('Invalid type hint in type declaration', result%loc, .false.)
               return
            end select
         else
            if (subtype%size /= 1) then
               call throw('Nested types cannot have a specified size', result%loc, .false.)
               return
            end if
            subtype%type = parse_type_string(curr_ir, line(2:end_index - 1), result%loc, result%name)
         end if
         line => line(end_index + 1:)
         if (len(line) > 0) then
            if (line(:1) /= ' ') then
               subtype%count = atobi(line)
               if (index(line, ' ') /= 0) then
                  line => line(index(line, ' '):)
               else
                  line => line(len(line) + 1:)
               end if
            end if
         end if
         line => line(trim_index(line):)
         call result%subtypes%push(subtype)
      end do

      if (len_trim(line) /= 0) then
         call throw('Extraneous characters in type declaration', result%loc, .false.)
      end if

      result%loc%loc_chain(1)%end_line = i

      call curr_ir%types%push(result)
   end subroutine
end module
