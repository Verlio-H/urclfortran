module fort_parse_expr
   use include, only: SMALL, throw, BIG
   use fort_lexer, only: TOKEN_SIZE, TOKEN_OP_ADD, TOKEN_OP_SUB, TOKEN_ASTERISK, TOKEN_OP_DIV, TOKEN_VALUE_INT, TOKEN_IDENTIFIER, &
       TOKEN_LPAREN, TOKEN_RPAREN, token
   use fort_asttypes, only: ast_node, NODE_INT_VAL, NODE_FNC_ARR, NODE_STRING
   use fort_astutil, only: scan_for
   use data_mod, only: list
   implicit none (type, external)
contains
   function binding_power(operator) result(result)
      integer(SMALL), intent(in) :: operator
      integer :: result(2)

      result = [0, 0]
      select case (operator)
      case (TOKEN_OP_ADD, TOKEN_OP_SUB)
          result = [80, 81]
      case (TOKEN_ASTERISK, TOKEN_OP_DIV)
          result = [100, 101]
      end select
   end function

   function is_atom(type) result(result)
      integer(SMALL), intent(in) :: type
      logical :: result

      select case (type)
      case (TOKEN_VALUE_INT, TOKEN_IDENTIFIER)
          result = .true.
      case default
          result = .false.
      end select
   end function

   subroutine parse_atom(dest, input, start, end)
      type(ast_node), intent(out) :: dest
      type(list), intent(in) :: input
      integer(BIG), intent(in) :: start
      integer(BIG), intent(in) :: end

      integer(BIG) :: rparen !, next_comma

      select type (tok => input%get(start))
      class default
         error stop 'invalid input list to parse_atom'  
      type is (token)
         select case (tok%type)
         case (TOKEN_VALUE_INT)
            ! TODO: deal with kind
            dest%type = NODE_INT_VAL
            dest%content_str = tok%value
            allocate(dest%children(1))
         case (TOKEN_IDENTIFIER)
            dest%content_str = tok%value
            if (start /= end) then
               select type (tok2 => input%get(start + 1))
               class default
                  error stop 'invalid input list to parse_atom'
               type is (token)
                  if (tok2%type == TOKEN_LPAREN) then
                     ! TODO: deal with arrays/functions
                     dest%type = NODE_FNC_ARR
                     allocate(dest%children(1))
                     rparen = scan_for(input, start + 2, TOKEN_RPAREN, 0, huge(0), 0)
                     if (rparen == -1) call throw('Missing closing parenthesis', tok2%loc)
                  else
                     dest%type = NODE_STRING
                  end if
               end select
            else
               dest%type = NODE_STRING
            end if
         end select
      end select
   end subroutine

   recursive subroutine parse_expr(dest, input, start, end)
      type(ast_node), intent(out) :: dest
      type(list), intent(in) :: input
      integer(BIG), intent(in) :: start
      integer(BIG), intent(in) :: end

      type(ast_node) :: lhs

      select type (t1 => input%get(start))
      class default
         error stop 'invalid input list to parse_expr'
      type is (token)
         if (start > end) then
            call throw('expected expression', t1%loc)
         end if
   
         if (.not.is_atom(t1%type)) then
            call throw('expected constant or variable name', t1%loc)
         end if
      end select

      call parse_atom(lhs, input, start, end)
   end subroutine
end module
