module backend_lower_bits
   use include, only: SMALL, BIG, sitoa
   use ir, only: full_ir, ir_subtype, ir_type, full_ir_type, HINT_INVALID
   use data_mod, only: list
   implicit none (type, external)

   type :: pair
      integer(SMALL) :: size = 0
      integer(BIG) :: index = 0 
   end type
contains
   subroutine ir_lower_bits(input, target_bits, src_hint_type, dest_hint_type)
      type(full_ir), intent(inout) :: input
      integer(SMALL), intent(in) :: target_bits
      integer(SMALL), intent(in) :: src_hint_type, dest_hint_type

      integer(BIG) :: i, base_type, idx
      class(*), allocatable :: base_type_type, subtype, temp_type, temp_subtype, temp_temp_type
      integer(SMALL) :: tail_size

      ! TODO: hashmap
      type(list) :: created_types

      created_types = list(pair())

      base_type_type = ir_type('___base_type'//sitoa(dest_hint_type)//'_'//sitoa(target_bits), subtypes = list(ir_subtype()))

      select type (base_type_type)
      type is (ir_type)
         subtype = ir_subtype(size = target_bits, hint = dest_hint_type)
         call base_type_type%subtypes%move_push(subtype)
      end select

      call input%types%move_push(base_type_type)
      base_type = input%types%size
      call created_types%push(pair(target_bits, base_type))

      do i = 1, base_type - 1
         select type (type => input%types%get(i))
         type is (ir_type)
            if (type%subtypes%size /= 1) cycle
            select type (subtype => type%subtypes%get(1))
            type is (ir_subtype)
               if (subtype%hint /= src_hint_type) cycle
               if (subtype%size <= target_bits) cycle
               ! find in map
               idx = find_type_index(created_types, subtype%size)
               if (idx == 0) then
                  ! create new type
                  temp_type = ir_type('___base_type'//sitoa(dest_hint_type)//'_'//sitoa(subtype%size), &
                     subtypes = list(ir_subtype()))
                  if (subtype%size >= target_bits) then
                     temp_subtype = ir_subtype(type=full_ir_type(type=base_type), count=subtype%size / target_bits)
                     select type (temp_type)
                     type is (ir_type)
                        call temp_type%subtypes%move_push(temp_subtype)
                     end select
                  end if
                  tail_size = mod(subtype%size, target_bits)
                  if (tail_size /= 0) then
                     idx = find_type_index(created_types, tail_size)
                     if (idx == 0) then
                        temp_temp_type = ir_type('___base_type'//sitoa(dest_hint_type)//'_'//sitoa(tail_size), &
                           subtypes = list(ir_subtype()))
                        temp_subtype = ir_subtype(size=tail_size, hint=dest_hint_type)
                        select type (temp_temp_type)
                        type is (ir_type)
                           call temp_temp_type%subtypes%move_push(temp_subtype)
                        end select
                        call input%types%move_push(temp_temp_type)
                        idx = input%types%size
                     end if
                     temp_subtype = ir_subtype(type=full_ir_type(type=idx))
                     select type (temp_type)
                     type is (ir_type)
                        call temp_type%subtypes%move_push(temp_subtype)
                     end select
                  end if
                  call input%types%push(temp_type)
                  idx = input%types%size
               end if
               subtype%hint = HINT_INVALID
               subtype%type = full_ir_type(type=idx)
               subtype%size = 1
            end select
         end select
      end do
      
   end subroutine

   function find_type_index(created_types, size) result(result)
      type(list), intent(in) :: created_types
      integer(SMALL), intent(in) :: size
      integer(BIG) :: result

      integer(BIG) :: i

      do i = 1, created_types%size
         select type (type_pair => created_types%get(i))
         class default
            error stop 'malformed created_type argument to find_type_index'
         type is (pair)
            if (type_pair%size == size) then
               result = type_pair%index
               return
            end if
         end select
      end do

      result = 0
   end function
end module
