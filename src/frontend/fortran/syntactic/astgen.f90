module fort_astgen
   use include, only: SMALL, BIG, throw
   use fort_asttypes, only: ast_node
   use fort_astutil, only: scan_for, next_line
   use fort_lexer, only: TOKEN_SIZE, TOKEN_LBRACKET, TOKEN_RBRACKET, TOKEN_LPAREN, TOKEN_RPAREN, TOKEN_IDENTIFIER, &
      TOKEN_NEXTLINE, TOKEN_ASSIGN
   use data_mod, only: list
   implicit none (type, external)
contains

   subroutine syntactic_analysis(result, input)
      type(ast_node), intent(out) :: result
      type(list), intent(in) :: input

      integer(BIG) :: i, idx

      i = 1
      do while (i < input%size)
         ! check for assignment
         idx = scan_for(input, i, TOKEN_ASSIGN, 0, 1, 1)
         if (idx /= -1) then
            ! assignment
            
            i = next_line(input, i)
            cycle
         end if
      end do
   end subroutine
end module
