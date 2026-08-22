program compiler
   use include, only: read_file, string, BIG, annotated_string
   use fort_compile, only: compiledata
   use ir, only: full_ir, full_ir_empty, ir_procedure
   use ir_parse, only: parse_ir
   use ir_write, only: write_ir
   use ir_graph, only: compute_stats, proc_stats, print_dom_tree, print_frontier
   use ir_ssa, only: ssaify
   use backend_lower, only: lower_ir_types
   use data_mod, only: list

   ! backends
   use backend_type, only: backend_base_type
   use backend_ir, only: backend_ir_type
   use backend_urcl, only: backend_urcl_type
   use backend_arm, only: backend_arm_type
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

   type(proc_stats), allocatable :: stats(:)
   type(list), allocatable :: associations(:)

   class(backend_base_type), allocatable :: backend

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

   backend = backend_ir_type()

   do while (argnum <= argcnt)
      call get_command_argument(argnum, argument)
      select case (argument)
      case ('-o')
         argnum = argnum + 1
         if (argnum > argcnt) then
            write (*, '(A)') 'Missing file name after -o'
            error stop, quiet=.true.
         end if
         call get_command_argument(argnum, argument)
         ofname = trim(argument)
      case ('-t')
         argnum = argnum + 1
         if (argnum > argcnt) then
            write (*, '(A)') 'Missing target after -t'
            error stop, quiet=.true.
         end if
         call get_command_argument(argnum, argument)
         select case (argument)
         case ('urcl')
            backend = backend_urcl_type()
         case ('ir')
            backend = backend_ir_type()
         case ('aarch64', 'arm')
            backend = backend_arm_type()
         case default
            write (*, '(A)') 'Invalid target: '//argument
            error stop, quiet=.true.
         end select
      case ('-c')
         link = .false.
      case default
         call ifnames%push(string(trim(argument)))
      end select
      argnum = argnum + 1
   end do

   if (ifnames%size == 0) then
      write(*, '(A)') 'error: no input files provided'
      stop
   end if

   call backend%full_init()

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

         intermediate = full_ir_empty()
         call backend%ir_init(intermediate)
         call parse_ir(intermediate, input)

         call backend%pre_ssa(intermediate)

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

         !call ir_lower_bits(intermediate, 16, HINT_INT, HINT_INT)

         call backend%pre_lowering(intermediate, associations, stats)
         call lower_ir_types(intermediate, associations)

         call backend%instruction_selection(intermediate, associations, stats)
         !call phi_removal(intermediate, associations, stats)

         call backend%pre_write(intermediate, associations, stats)
         call backend%write(single_output, intermediate, associations)

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
