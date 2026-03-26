module fort_astutil
   use include, only: SMALL, throw, BIG
   use fort_lexer, only: TOKEN_SIZE, TOKEN_LPAREN, TOKEN_LBRACKET, TOKEN_RPAREN, TOKEN_RBRACKET, TOKEN_IDENTIFIER, TOKEN_NEXTLINE, &
      token
   use fort_asttypes, only: ast_node
   use data_mod, only: list
   implicit none (type, external)
contains
   function next_line(tokens, start) result(result)
      type(list), target, intent(in) :: tokens
      integer(BIG), intent(in) :: start
      integer(BIG) :: result

      result = scan_for(tokens, start + 1, TOKEN_NEXTLINE, 0, huge(0), 0)
      if (result == -1) then
         select type (start_token => tokens%get(start))
         type is (token)
            call throw('Unclosed Group', start_token%loc)
         class default
            error stop 'invalid token list in next_line'
         end select
      end if
   end function

   function scan_for(tokens, start, token_type, target_depth, max_ident, min_ident) result(result)
      type(list), target, intent(in) :: tokens
      integer(BIG), intent(in) :: start
      integer(SMALL), intent(in) :: token_type
      integer, intent(in) :: target_depth
      integer, intent(in) :: max_ident
      integer, intent(in) :: min_ident
      integer(BIG) :: result

      integer(BIG) :: i
      integer :: depth
      integer :: ident_count

      depth = 0
      ident_count = 0
      result = -1
      do i = start, tokens%size
         select type (t => tokens%get(i))
         class default
            error stop 'invalid token list in scan_for'
         type is (token)
            if (ident_count > min_ident .and. depth == target_depth .and. t%type == token_type) then
               result = i
               return
            end if
            if (t%type == TOKEN_LPAREN .or. t%type == TOKEN_LBRACKET) then
               depth = depth + 1
            else if (t%type == TOKEN_RPAREN .or. t%type == TOKEN_RBRACKET) then
               depth = depth - 1
            else if (depth == target_depth .and. t%type == TOKEN_IDENTIFIER) then
               ident_count = ident_count + 1
               if (ident_count > max_ident) then
                  result = -1
                  return
               end if
            else if (t%type == TOKEN_NEXTLINE) then
               result = -1
               return
            end if
         end select
      end do
   end function
end module
