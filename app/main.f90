program compiler
   use include, only: read_file, string, BIG, annotated_string
   use fort_compile, only: compiledata
   use ir, only: full_ir, full_ir_empty, ir_procedure, HINT_INT
   use ir_parse, only: parse_ir
   use ir_write, only: write_ir
   use ir_graph, only: compute_stats, proc_stats, print_dom_tree, print_frontier
   use ir_ssa, only: ssaify
   use backend_lower, only: lower_ir_types
   use backend_lower_bits, only: ir_lower_bits
   use data_mod, only: list
   implicit none (type, external)

   character(256) :: argument, libloc
   character(:), allocatable :: ofname
   type(list), target :: ifnames
   logical :: link
   integer :: newunit, argnum, argcnt

   type(list) :: input
   integer(BIG) :: file_index, i
   type(full_ir) :: intermediate
   type(list) :: single_output, output

   ifnames = list(string())
   input = list(annotated_string())

   single_output = list(string())
   output = list(string())

   intermediate  = full_ir_empty()

   ! read arguments
   link = .true.
   argnum = 1
   ofname = 'output.ir'
   argcnt = command_argument_count()
   do while (argnum <= argcnt)
      call get_command_argument(argnum, argument)
      if (argument == '-o') then
         argnum = argnum + 1
         call get_command_argument(argnum, argument)
         ofname = trim(argument)
      else if (argument == '-c') then
         link = .false.
      else
         call ifnames%push(string(trim(argument)))
      end if
      argnum = argnum + 1
   end do

   if (ifnames%size == 0) then
      write(*, '(A)') 'error: no input files provided'
      stop
   end if

   do file_index = 1, ifnames%size
      ! initialize data
      select type (str => ifnames%get(file_index))
      class default
         error stop 'something really bad has happened with ifnames'
      type is (string)
         ! call compile function
         !output = output//compiledata(input, ifnames%array(argnum)%value)
         call read_file(input, str%val)

         single_output = list(string())

         intermediate  = full_ir_empty()
         call parse_ir(intermediate, input)

         block
            type(proc_stats), allocatable :: stats(:)
            type(list), allocatable :: associations(:)
            call compute_stats(stats, intermediate)

            call ssaify(associations, intermediate, stats)
            !do idx = 1, size(stats)
            !   select type (proc => intermediate%procedures%get(idx))
            !   type is (ir_procedure)
            !      write(*, '(A)') proc%name//':'
            !      call print_dom_tree(intermediate, proc, stats(idx)%tree)
            !      write(*, '(A)') 'dominance frontier:'
            !      call print_frontier(intermediate, proc, stats(idx)%frontier)
            !   end select
            !end do

            call ir_lower_bits(intermediate, 16, HINT_INT, HINT_INT)
            call lower_ir_types(intermediate, associations)
         end block

         call write_ir(single_output, intermediate)

         call output%push_list(single_output)
      end select
   end do

   ! output result
   open(newunit=newunit, file=ofname)
   do i = 1, output%size
      select type (str => output%get(i))
      class default
         error stop 'something really bad has happened with output'
      type is (string)
         write(newunit, '(A)') str%val
      end select
   end do
   close(newunit)

   if (link) then
      call get_environment_variable('LIBFORT_PATH',libloc)
      call execute_command_line('urcl-ld -o '//ofname//' '//&
         trim(libloc)//'/urcl16/main.urcl '//&
         trim(libloc)//'/urcl16/math.urcl '//ofname)
   end if
end program
