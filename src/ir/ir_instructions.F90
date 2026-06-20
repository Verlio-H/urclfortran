module ir_instructions
   use iso_c_binding, only: c_char
   use include, only: SMALL, location
   use data_mod, only: list
   implicit none (type, external)

   integer(SMALL), parameter :: INST_PHI = 0 ! op1 = phi op2...
   integer(SMALL), parameter :: INST_RET = 1 ! return op1...
   integer(SMALL), parameter :: INST_ASSIGN = 2 ! arg1 <- arg2, ...
   integer(SMALL), parameter :: INST_CALL = 3 ! arg1... = op2(op2...) where arg2 is a function
   integer(SMALL), parameter :: INST_ASM = 4 ! op1 is name followed by result, op2 are source vars
   integer(SMALL), parameter :: INST_JMP = 5 ! goto arg1
   integer(SMALL), parameter :: INST_BNZ = 7 ! goto arg2(1) if arg1 != 0 else goto arg2(2)
   integer(SMALL), parameter :: INST_CAST = 8 ! arg1 <~ arg2 where the two have the same size
   integer(SMALL), parameter :: INST_GET = 9 ! arg1 <* arg2
   integer(SMALL), parameter :: INST_SET = 10 ! arg1 *< arg2
   integer(SMALL), parameter :: INST_LBAR = 11 ! leaving barrier: outward jump, memory sync for multithreading
   integer(SMALL), parameter :: INST_JBAR = 12 ! joining barrier: inward jump from longjmp or entry

   ! leaving barriers involve synchronizing externally accessible state
   ! joining barriers involve synchronizing all state

   type :: ir_instruction
      integer(SMALL) :: inst_type = -1
      type(ir_op_container), allocatable :: op1(:)
      type(ir_op_container), allocatable :: op2(:)
      type(location) :: loc = location()
      type(list) :: invalidate = list() !operand_ir_var
      logical :: writtenback = .false.
   end type

   type, abstract :: ir_operand
      integer :: gvn_index = 0
   end type

   type :: ir_op_container
      class(ir_operand), allocatable :: val
   end type

end module
