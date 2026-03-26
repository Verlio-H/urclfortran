module ir_parse_helper
   use include, only: SMALL, BIG, annotated_string, location, trim_index, throw, atobi
   use ir, only: full_ir, full_ir_type, ir_type, base_comptime_val, ir_procedure, comptime_int, comptime_addr
   use data_mod, only: list

   implicit none (type, external)
contains
   subroutine skip_block(str, i)
      type(list), target, intent(in) :: str
      integer(BIG), intent(inout) :: i

      class(*), pointer :: line_str_poly
      class(annotated_string), pointer :: line_str
      character(:), pointer :: line

      line_str_poly => str%get(i)
      select type (line_str_poly)
      class is (annotated_string)
         line_str => line_str_poly
      class default
         error stop 'invalid str argument for skip_block'
      end select

      line => line_str%val(trim_index(line_str%val):)

      if (line(len_trim(line):len_trim(line)) /= '{') return
      do while (line(trim_index(line):trim_index(line)) /= '}')
         i = i + 1
         line_str_poly => str%get(i)
         select type (line_str_poly)
         class is (annotated_string)
            line_str => line_str_poly
         class default
            error stop 'invalid str argument for skip_block'
         end select
         line => line_str%val
      end do
   end subroutine

   subroutine parse_comptime_val(result, input, loc)
      class(base_comptime_val), allocatable, intent(inout) :: result
      character(*), target, intent(in) :: input
      type(location), intent(in) :: loc

      character(:), pointer :: name
      integer :: end_index
      integer(BIG) :: add_amount

      if (len(input) == 0) then
         call throw('Comptime value has 0 length', loc, .false.)
         result = comptime_int(0)
         return
      end if

      if (input(:1) == '-' .or. input(:1) >= '0' .and. input(:1) <= '9') then
         result = comptime_int(atobi(input))
         return
      end if

      end_index = index(input, '+')
      if (end_index /= 0) then
         name => input(:end_index - 1)
         add_amount = atobi(input(end_index + 1:))
      else
         name => input
         add_amount = 0
      end if

      result = comptime_addr(name=name, offset=add_amount)
   end subroutine

   function parse_type_string(curr_ir, input, loc, extra_type) result(result)
      type(full_ir), intent(in) :: curr_ir
      character(*), target, intent(in) :: input
      type(location), intent(in) :: loc
      character(*), intent(in), optional :: extra_type
      type(full_ir_type) :: result
      
      character(:), pointer :: val
      integer(SMALL) :: ptr_count
      integer(BIG) :: i
      integer :: end_index

      if (len(input) == 0) then
         call throw('Invalid type', loc, .false.)
         result%unknown = .true.
         return
      end if
      val => input
      ptr_count = 0
      if (val(:1) == '[') then
         end_index = index(val, ']')
         if (end_index == 0) then
            call throw('Invalid array specifier in type name', loc, .false.)
            val => val(2:)
         end if
         val => val(end_index  + 1:)
      end if
      do while (val(:1) == '*')
         ptr_count = ptr_count + 1_SMALL
         val => val(2:)
         if (val(:1) == '#') val => val(2:)
         if (val(:1) == '%') val => val(2:)
         if (val(:1) == '[') then
            end_index = index(val, ']')
            if (end_index == 0) then
               call throw('Invalid array specifier in type name', loc, .false.)
               val => val(2:)
            end if
            val => val(end_index + 1:)
         end if
      end do

      allocate(result%const_mask(0:ptr_count), result%restrict_mask(ptr_count), result%restrictish_mask(ptr_count), source=.false.)
      allocate(result%array_sizes(0:ptr_count), source=1_BIG)
      result%indirection_count = ptr_count

      val => input
      ptr_count = 0
      if (val(:1) == '#') then
         val => val(2:)
         result%const_mask(0) = .true.
      end if
      if (val(:1) == '[') then
         val => val(2:)
         end_index = index(val, ']')

         result%array_sizes(ptr_count) = atobi(val(:end_index - 1))
         if (result%array_sizes(ptr_count) < 0) then
            call throw('Invalid array size in type name', loc)
         end if
         val => val(end_index + 1:)
      end if
      do while (val(:1) == '*')
         ptr_count = ptr_count + 1_SMALL
         val => val(2:)
         if (val(:1) == '#') then
            val => val(2:)
            result%const_mask(ptr_count) = .true.
         end if
         if (val(:1) == '%') then
            val => val(2:)
            result%restrict_mask(ptr_count) = .true.
            result%restrictish_mask(ptr_count) = .true.
         end if
         if (val(:1) == '~') then
            val => val(2:)
            result%restrictish_mask(ptr_count) = .true.
         end if
         if (val(:1) == '[') then
            val => val(2:)
            end_index = index(val, ']')
            
            result%array_sizes(ptr_count) = atobi(val(:end_index - 1))
            if (result%array_sizes(ptr_count) < 0) then
               call throw('Invalid array size in type name', loc)
            end if
            val => val(end_index + 1:)
            
         end if
      end do

      if (val == '?') then
         result%unknown = .true.
         return
      end if

      do i = 1, curr_ir%types%size
         select type (t => curr_ir%types%get(i))
         class default
            error stop 'invalid curr_ir list in parse_type_string'
         type is (ir_type)
            if (t%name == val) then
               result%type = i
               return
            end if
         end select
      end do

      if (present(extra_type)) then
         if (extra_type == val) then
            result%type = curr_ir%types%size + 1
            return
         end if
      end if
      
      call throw('Unknown type: '//val, loc)
   end function
end module
