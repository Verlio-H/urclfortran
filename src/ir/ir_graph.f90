module ir_graph
   use include, only: BIG
   use ir, only: full_ir, ir_procedure, ir_block
   use data_mod, only: list
   
   implicit none (type, external)

   type :: var_def
      integer :: deref_count
      integer(BIG) :: idx
   end type

   type :: definitions
      type(var_def), allocatable :: all_defined(:)
      type(var_def), allocatable :: defined_writeback(:)
      type(var_def), allocatable :: used_before(:)
   end type

   type :: proc_stats
      type(list), allocatable :: tree(:)
      integer(BIG), allocatable :: rtree(:)
      type(list), allocatable :: frontier(:)
      type(definitions), allocatable :: defs(:)
   end type
contains
   subroutine compute_stats(output, input)
      type(full_ir), intent(in) :: input
      type(proc_stats), allocatable, intent(out) :: output(:)

      integer(BIG) :: i

      allocate(output(input%procedures%size))
      do i = 1, input%procedures%size
         select type (proc => input%procedures%get(i))
         class default
            error stop 'invalid input argument to compute dominance tree'
         type is (ir_procedure)
            if (proc%blocks%size == 0) cycle
            call compute_proc_stats(output(i), input, proc)
         end select
      end do
   end subroutine

   subroutine compute_proc_stats(output, input, proc)
      type(proc_stats), intent(out) :: output
      type(full_ir), intent(in) :: input
      type(ir_procedure), intent(in) :: proc

      allocate(output%tree(proc%blocks%size))
      allocate(output%rtree(proc%blocks%size))
      call compute_proc_dominance_tree(output%tree, output%rtree, input, proc)
      allocate(output%frontier(proc%blocks%size))
      call compute_proc_dominance_frontier(output%frontier, output%tree, output%rtree, proc, input)
   end subroutine

   ! adapted from a blog post by Tanuj Khattar
   ! original algorithm by Thomas Lengauer and Robert Tarjan
   subroutine compute_proc_dominance_tree(tree, rtree, input, proc)
      type(list), intent(out) :: tree(:)
      integer(BIG), intent(out) :: rtree(:)
      type(full_ir), intent(in) :: input
      type(ir_procedure), intent(in) :: proc

      integer(BIG) :: arr(proc%blocks%size)
      integer(BIG) :: rev(size(arr))
      integer(BIG) :: parent(size(arr))
      integer(BIG) :: label(size(arr))
      integer :: sdom(size(arr))
      integer :: dom(size(arr))
      integer(BIG) :: dsu(size(arr))
      type(list) :: rg(size(arr))
      type(list) :: bucket(size(arr))
      integer :: count

      integer :: v
      integer(BIG) :: i, j, w

      arr = 0
      rev = 0
      parent = 0
      label = [(i, i = 1, size(label))]
      sdom = [(i, i = 1, size(sdom))]
      dom = [(i, i = 1, size(dom))]
      dsu = [(i, i = 1, size(dsu))]
      count = 0
      tree = list(0_BIG)
      rg = list(0_BIG)
      bucket = list(0_BIG)

      call dominance_tree_dfs(arr, rev, parent, rg, proc, 1_BIG, count, input)

      do i = size(arr), 1, -1
         do j = 1, rg(i)%size
            select type (rgij => rg(i)%get(j))
            class default
               error stop 'something very bad has happened'
            type is (integer(BIG))
               sdom(i) = min(sdom(i), sdom(find(dsu, sdom, label, rgij)))
            end select
         end do

         if (i > 1) then
            call bucket(sdom(i))%push(i)
         end if

         do j = 1, bucket(i)%size
            select type (bij => bucket(i)%get(j))
            class default
               error stop 'something has gone terribly wrong'
            type is (integer(BIG))
               w = bij
            end select

            v = find(dsu, sdom, label, w)

            if (sdom(v) == sdom(w)) then
               dom(w) = sdom(w)
            else
               dom(w) = v
            end if
         end do

         if (i > 1) call union(dsu, parent(i), i)
      end do

      do i = 2, size(arr)
         if (dom(i) /= sdom(i)) dom(i) = dom(dom(i))
         rtree(rev(i)) = rev(dom(i))
         call tree(rev(dom(i)))%push(rev(i))
      end do
   end subroutine

   recursive subroutine dominance_tree_dfs(arr, rev, parent, rg, proc, i, count, input)
      type(ir_procedure), intent(in) :: proc
      integer(BIG), intent(inout) :: arr(proc%blocks%size)
      integer(BIG), intent(inout) :: rev(size(arr))
      integer(BIG), intent(inout) :: parent(size(arr))
      type(list), intent(inout) :: rg(size(arr))
      integer(BIG), intent(in) :: i
      integer, intent(inout) :: count
      type(full_ir), intent(in) :: input
      
      type(ir_block), pointer :: blk
      integer :: j
      integer(BIG) :: w

      count = count + 1
      arr(i) = count
      rev(count) = i

      blk => proc%get_block(input, i)

      do j = 1, size(blk%child_blocks)
         w = blk%child_blocks(j)
         if (arr(w) == 0) then
            call dominance_tree_dfs(arr, rev, parent, rg, proc, w, count, input)
            parent(arr(w)) = arr(i)
         end if
         call rg(arr(w))%push(arr(i))
      end do
   end subroutine

   recursive function find(dsu, sdom, label, u) result(res)
      integer(BIG), intent(inout) :: dsu(:)
      integer, intent(in) :: sdom(:)
      integer(BIG), intent(inout) :: label(:)
      integer(BIG), intent(in) :: u
      integer(BIG) :: res

      integer(BIG) :: v

      if (dsu(u) /= u) then
         v = find(dsu, sdom, label, dsu(u))

         if (sdom(label(dsu(u))) < sdom(label(u))) then
            label(u) = label(dsu(u))
         end if

         dsu(u) = v
      end if

      res = label(u)
   end function

   subroutine union(dsu, u, v)
      integer(BIG), intent(inout) :: dsu(:)
      integer(BIG), intent(in) :: u
      integer(BIG), intent(in) :: v

      dsu(v) = u
   end subroutine

   subroutine print_dom_tree(curr_ir, proc, tree)
      type(full_ir), intent(in) :: curr_ir
      type(ir_procedure), intent(in) :: proc
      type(list), intent(in) :: tree(:)

      if (proc%blocks%size == 0) return
      call print_dom_tree_helper(curr_ir, proc, tree, 1_BIG, '   ')
   end subroutine

   recursive subroutine print_dom_tree_helper(curr_ir, proc, tree, i, pre)
      type(full_ir), intent(in) :: curr_ir
      type(ir_procedure), intent(in) :: proc
      type(list), intent(in) :: tree(:)
      integer(BIG), intent(in) :: i
      character(*), intent(in) :: pre

      character(:), allocatable :: line
      type(ir_block), pointer :: blk
      character(:), allocatable :: newpre
      integer(BIG) :: j

      blk => proc%get_block(curr_ir, i)

      line = pre
      line(len(line) - 2:) = '+- '
      line = line//blk%name
      if (tree(i)%size /= 0) line = line//':'
      write (*, '(A)') line

      newpre = pre//'|  '
      do j = 1, tree(i)%size
         if (j == tree(i)%size) then
            newpre(len(newpre) - 2:) = ''
         end if
         select type (newi => tree(i)%get(j))
         class default
            error stop 'invalid tree argument to print dom tree'
         type is (integer(BIG))
            call print_dom_tree_helper(curr_ir, proc, tree, newi, newpre)
         end select
      end do
   end subroutine

   function ir_strict_dominates(rtree, a, b) result(adomb)
      integer(BIG), intent(in) :: rtree(:)
      integer(BIG), intent(in) :: a 
      integer(BIG), intent(in) :: b
      logical :: adomb

      integer(BIG) :: i

      i = rtree(b)
      do while (i > 0)
         if (i == a) then
            adomb = .true.
            return
         end if

         i = rtree(i)
      end do

      adomb = .false.
   end function

   function ir_dominates(rtree, a, b) result(adomb)
      integer(BIG), intent(in) :: rtree(:)
      integer(BIG), intent(in) :: a
      integer(BIG), intent(in) :: b
      logical :: adomb

      if (a == b) then
         adomb = .true.
      else if (ir_strict_dominates(rtree, a, b)) then
         adomb = .true.
      else
         adomb = .false.
      end if
   end function

   subroutine compute_proc_dominance_frontier(frontier, tree, rtree, proc, input)
      type(list), intent(out) :: frontier(:)
      type(list), intent(in) :: tree(:)
      integer(BIG), intent(in) :: rtree(:)
      type(ir_procedure), intent(in) :: proc
      type(full_ir), intent(in) :: input

      integer(BIG) :: i, j
      type(list) :: temp_list

      logical :: included(size(tree))


      do i = 1, size(tree)
         frontier(i) = list(0_BIG)
         included = .false.

         call fill_frontier_list(included, tree, proc, input, i)

         do j = 1, size(included)
            if (.not.included(j)) cycle
            if (ir_strict_dominates(rtree, i, j)) cycle
            call frontier(i)%push(j)
         end do
      end do
   end subroutine

   recursive subroutine fill_frontier_list(included, tree, proc, input, i)
      logical, intent(inout) :: included(:)
      type(list), intent(in) :: tree(:)
      type(ir_procedure), intent(in) :: proc
      type(full_ir), intent(in) :: input
      integer(BIG), intent(in) :: i

      type(ir_block), pointer :: blk
      integer(BIG) :: j

      blk => proc%get_block(input, i)

      included(blk%child_blocks) = .true.

      do j = 1, tree(i)%size
         select type (child => tree(i)%get(j))
         class default
            error stop 'invalid tree argument to fill frontier list'
         type is (integer(BIG))
            call fill_frontier_list(included, tree, proc, input, child)
         end select
      end do
   end subroutine

   subroutine print_frontier(input, proc, frontier)
      type(full_ir), intent(in) :: input
      type(ir_procedure), intent(in) :: proc
      type(list), intent(in) :: frontier(:)

      integer(BIG) :: i, j
      type(ir_block), pointer :: blk
      character(:), allocatable :: line

      do i = 1, size(frontier)
         blk => proc%get_block(input, i)
         line = blk%name//': {'

         do j = 1, frontier(i)%size
            if (j /= 1) line = line//', '
            select type (idx => frontier(i)%get(j))
            class default
               error stop 'invalid frontier argument to print_frontier'
            type is (integer(BIG))
               blk => proc%get_block(input, idx)
               line = line//blk%name
            end select
         end do

         write(*, '(A)') line//'}'
      end do
   end subroutine

end module
