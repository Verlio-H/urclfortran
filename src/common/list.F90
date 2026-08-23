module data_mod
   use iso_c_binding, only: c_char, c_sizeof, c_size_t

   implicit none (type, external)

   type :: box
      class(*), allocatable :: val
   end type

   type :: list
      class(*), allocatable :: type

      type(box), private, allocatable :: array(:)
      integer(c_size_t) :: size = 0
   contains
      procedure, non_overridable :: get
      procedure, non_overridable :: move_get
      procedure, non_overridable :: set
      procedure, non_overridable, private :: cont_span
      procedure, non_overridable, private :: non_cont_span
      generic :: span => cont_span, non_cont_span
      procedure, non_overridable :: complete_span

      procedure, non_overridable :: push
      procedure, non_overridable :: move_push

      procedure, non_overridable :: push_list
      procedure, non_overridable :: move_push_list

      procedure, non_overridable :: insert
      procedure, non_overridable :: move_insert

      procedure, non_overridable :: remove
      procedure, non_overridable :: fast_remove
      procedure, non_overridable :: pop

      procedure, non_overridable :: reserve
      procedure, non_overridable :: move
   end type

   private

   public :: box
   public :: list
contains
   function get(array, index)
      class(list), target, intent(in) :: array
      integer(c_size_t), value, intent(in) :: index
      class(*), pointer :: get

#ifdef DEBUG
      if (index > array%size .or. index <= 0) then
         error stop 'index out of range in get'
      end if
#endif

      get => array%array(index)%val
   end function

   function move_get(array, index)
      class(list), target, intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index
      class(*), allocatable :: move_get

#ifdef DEBUG
      if (index > array%size .or. index <= 0) then
         error stop 'index out of range in move_get'
      end if
#endif

      call move_alloc(array%array(index)%val, move_get) 
   end function

   subroutine set(array, index, val)
      class(list), target, intent(inout) :: array
      integer(c_size_t), intent(in) :: index
      class(*) :: val

#ifdef DEBUG
      if (.not.same_type_as(array%type, val)) then
         error stop 'incorrect type in list set'
      else if (index > array%size .or. index <= 0) then
         error stop 'index out of range in set'
      end if
#endif

      array%array(index)%val = val
   end subroutine

   function cont_span(array, index0, index1)
      class(list), target, intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index0, index1
      type(box), pointer :: cont_span(:)

#ifdef DEBUG
      if (index0 > array%size .or. index0 <= 0) then
         error stop 'lower index out of range in cont span'
      end if
      if (index1 > array%size .or. index1 <= 0) then
         error stop 'upper index out of range in cont span'
      end if
#endif

      cont_span => array%array(index0:index1)
   end function

   function non_cont_span(array, index0, index1, stride)
      class(list), target, intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index0, index1, stride
      type(box), pointer :: non_cont_span(:)

#ifdef DEBUG
      if (index0 > array%size .or. index0 <= 0) then
         error stop 'lower index out of range in non cont span'
      end if
      if (index1 > array%size .or. index1 <= 0) then
         error stop 'upper index out of range in non cont span'
      end if
#endif

       non_cont_span => array%array(index0:index1:stride)
   end function

   function complete_span(array)
      class(list), target, intent(inout) :: array
      type(box), contiguous, pointer :: complete_span(:)

      complete_span => array%array(:array%size)
   end function

   subroutine reallocate(array)
      class(list), intent(inout) :: array
      type(box), allocatable :: temp(:)

      integer(c_size_t) :: i

      if (.not.allocated(array%array)) then
         allocate(array%array(31 + array%size))
      else if (array%size >= size(array%array)) then ! resize
         allocate(temp(array%size + array%size / 2))
         do i = 1, size(array%array)
            call move_alloc(array%array(i)%val, temp(i)%val)
         end do
         call move_alloc(temp, array%array)
      end if
   end subroutine

   subroutine move_push(array, value)
      class(list), intent(inout) :: array
      class(*), allocatable, intent(inout) :: value

#ifdef DEBUG
      if (.not.allocated(array%type)) then
         error stop 'list type unallocated'
      else if (.not.same_type_as(array%type, value)) then
         error stop 'pushed invalid type to list'
      end if
#endif

      array%size = array%size + 1
      call reallocate(array)
      call move_alloc(value, array%array(array%size)%val)
   end subroutine

   subroutine push(array, value)
      class(list), intent(inout) :: array
      class(*), intent(in) :: value

      class(*), allocatable :: copy

      allocate(copy, source=value)
      call array%move_push(copy)
   end subroutine

   subroutine reserve(array, count)
      class(list), intent(inout) :: array
      integer(c_size_t), value, intent(in) :: count

#ifdef DEBUG
      if (count < 0) then
         error stop 'reserve amount must be non negative'
      end if
#endif

      array%size = array%size + count
      call reallocate(array)
      array%size = array%size - count
   end subroutine

   function pop(array)
      class(list), target, intent(inout) :: array
      class(*), allocatable :: pop

#ifdef DEBUG
      if (array%size == 0) then
         error stop 'pop on empty array'
      end if
#endif

      call move_alloc(array%array(array%size)%val, pop)
      array%size = array%size - 1
   end function

   subroutine move_insert(array, index, value)
      class(list), intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index
      class(*), allocatable, intent(inout) :: value

      integer(c_size_t) :: i

#ifdef DEBUG
      if (.not.allocated(array%type)) then
         error stop 'list type unallocated'
      else if (.not.same_type_as(array%type, value)) then
         error stop 'type mismatch in insert'
      end if

      if (index > array%size + 1 .or. index <= 0) then
         error stop 'index out of range in insert'
      end if
#endif

      array%size = array%size + 1
      call reallocate(array)
      do i = array%size, index + 1, -1
         call move_alloc(array%array(i - 1)%val, array%array(i)%val)
      end do

      call move_alloc(value, array%array(index)%val)
   end subroutine

   subroutine insert(array, index, value)
      class(list), intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index
      class(*), intent(in) :: value

      class(*), allocatable :: copy

      copy = value
      call array%move_insert(index, copy)
   end subroutine

   subroutine push_list(array, array2)
      class(list), intent(inout) :: array
      type(list), intent(in) :: array2

      integer(c_size_t) :: i
      
      if (array2%size == 0) return
      
      call array%reserve(array2%size) 

      do i = 1, array2%size
         call array%push(array2%array(i)%val)
      end do
   end subroutine

   subroutine move_push_list(array, array2)
      class(list), intent(inout) :: array
      type(list), intent(inout) :: array2

      integer(c_size_t) :: i
      
      if (array2%size == 0) return
      
      call array%reserve(array2%size) 

      do i = 1, array2%size
         call array%move_push(array2%array(i)%val)
      end do

      array2%size = 0
   end subroutine

   subroutine remove(array, index)
      class(list), intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index

      integer(c_size_t) :: i

#ifdef DEBUG
      if (index > array%size .or. index <= 0) then
         error stop 'index out of range in remove'
      end if
#endif

      deallocate(array%array(index)%val)

      do i = index, array%size - 1
         call move_alloc(array%array(i + 1)%val, array%array(i)%val)
      end do
      array%size = array%size - 1

   end subroutine

   subroutine fast_remove(array, index)
      class(list), intent(inout) :: array
      integer(c_size_t), value, intent(in) :: index

#ifdef DEBUG
      if (index > array%size .or. index <= 0) then
         error stop 'index out of range in fast remove'
      end if
#endif
      
      if (index == array%size) then
         deallocate(array%array(index)%val)
      else
         call move_alloc(array%array(array%size)%val, array%array(index)%val)
      end if
      array%size = array%size - 1
   end subroutine

   subroutine move(array1, array2)
      class(list), intent(inout) :: array1
      class(list), intent(inout) :: array2

#ifdef DEBUG
      if (.not.same_type_as(array1%type, array2%type)) then
         error stop 'cannot move between arrays of differing type'
      end if
#endif
      
      call move_alloc(array1%array, array2%array)
      array2%size = array1%size
      array1%size = 0
   end subroutine
end module
