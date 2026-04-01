!TODO: add BOZ constants
module fort_lexer
   use include, only: SMALL, BIG, to_upper, throw, location, file_char, annotated_string
   use data_mod, only: list
   use iso_c_binding, only: c_char

   implicit none (type, external)

   type :: token
      type(location) :: loc = location()
      character(:), allocatable :: value
      integer(SMALL) :: type = 0 ! TOKEN_
   end type
   integer, parameter :: TOKEN_SIZE = storage_size(token()) / storage_size(c_char_'a')

   integer(SMALL), parameter :: TOKEN_IDENTIFIER = 0
   integer(SMALL), parameter :: TOKEN_VALUE_INT = 1
   integer(SMALL), parameter :: TOKEN_VALUE_REAL = 2
   integer(SMALL), parameter :: TOKEN_VALUE_LOGICAL = 3
   integer(SMALL), parameter :: TOKEN_VALUE_CHAR = 4
   integer(SMALL), parameter :: TOKEN_NEXTLINE = 5
   integer(SMALL), parameter :: TOKEN_ASSIGN = 6
   integer(SMALL), parameter :: TOKEN_ASTERISK = 7
   integer(SMALL), parameter :: TOKEN_LPAREN = 8
   integer(SMALL), parameter :: TOKEN_RPAREN = 9
   integer(SMALL), parameter :: TOKEN_LBRACKET = 10
   integer(SMALL), parameter :: TOKEN_RBRACKET = 11
   integer(SMALL), parameter :: TOKEN_TEMPLATE = 12
   integer(SMALL), parameter :: TOKEN_COLON = 13
   integer(SMALL), parameter :: TOKEN_DCOLON = 14
   integer(SMALL), parameter :: TOKEN_PTR_ASSIGN = 15
   integer(SMALL), parameter :: TOKEN_COMMA = 16
   integer(SMALL), parameter :: TOKEN_TERNARY = 17
   integer(SMALL), parameter :: TOKEN_MEMBER = 18
   integer(SMALL), parameter :: TOKEN_NIL = 19
   integer(SMALL), parameter :: TOKEN_OP_ADD = 100
   integer(SMALL), parameter :: TOKEN_OP_SUB = 101
   integer(SMALL), parameter :: TOKEN_OP_DIV = 102
   integer(SMALL), parameter :: TOKEN_OP_POW = 103
   integer(SMALL), parameter :: TOKEN_OP_EQUAL = 104
   integer(SMALL), parameter :: TOKEN_OP_NEQUAL = 105
   integer(SMALL), parameter :: TOKEN_OP_LESS = 106
   integer(SMALL), parameter :: TOKEN_OP_GREATER = 107
   integer(SMALL), parameter :: TOKEN_OP_LESS_EQUAL = 108
   integer(SMALL), parameter :: TOKEN_OP_GREATER_EQUAL = 109
   integer(SMALL), parameter :: TOKEN_OP_AND = 110
   integer(SMALL), parameter :: TOKEN_OP_OR = 111
   integer(SMALL), parameter :: TOKEN_OP_EQV = 112
   integer(SMALL), parameter :: TOKEN_OP_NEQV = 113
   integer(SMALL), parameter :: TOKEN_OP_CUSTOM = 114
   integer(SMALL), parameter :: TOKEN_OP_CONCAT = 115
   integer(SMALL), parameter :: TOKEN_OP_NOT = 116
contains
   subroutine get_token(result, inc, input, i)
      type(token), intent(out) :: result
      integer(SMALL), intent(out) :: inc
      type(annotated_string), target, intent(in) :: input
      integer, intent(In) :: i

      character :: c
      character(2) :: cc
      character(:), allocatable :: ccc
      integer :: end
      integer :: offset
      character(:), pointer :: string

      string => input%val
      result%loc = input%loc
      c = string(i:i)

      if (i == len(string)) then
         offset = 0
      else
         offset = 1
      end if

      if (c == '.') then
         offset = index(string(i + 1:), '.')
         if (offset == 0) continue !call throw('missing closing . ', fname, lnum, char)
         ccc = to_upper(string(i:i + offset))
         result%value = ccc
         inc = int(offset, SMALL) + 1_SMALL
         select case (ccc)
         case ('.EQ.')
               result%type = TOKEN_OP_EQUAL
         case ('.NE.')
               result%type = TOKEN_OP_NEQUAL
         case ('.GE.')
               result%type = TOKEN_OP_GREATER_EQUAL
         case ('.LE')
               result%type = TOKEN_OP_LESS_EQUAL
         case ('.GT.')
               result%type = TOKEN_OP_GREATER
         case ('.LT.')
               result%type = TOKEN_OP_LESS
         case ('.EQV.')
               result%type = TOKEN_OP_EQV
         case ('.NEQV.')
               result%type = TOKEN_OP_NEQV
         case ('.NIL.')
               result%type = TOKEN_NIL
         case ('.TRUE.', '.FALSE.')
               result%type = TOKEN_VALUE_LOGICAL
         case ('.AND.')
               result%type = TOKEN_OP_AND
         case ('.OR.')
               result%type = TOKEN_OP_OR
         case ('.NOT.')
               result%type = TOKEN_OP_NOT
         case default
               result%type = TOKEN_OP_CUSTOM
         end select
         return
      end if

      if (c == '''' .or. c == '"') then
         end = i + 1
         inc = 1_SMALL
         result%value = ''
         do
            if (end > len(string)) then
               !call throw('expected end of string', location([file_char(fname, lnum, char + int(end, SMALL))]))
            else if ((end == len(string) .and. string(end:end) /= c) .or. string(end:end) == achar(10)) then
               !call throw('expected end of string', fname, lnum, char + int(end, SMALL))
            else if (end == len(string)) then
               inc = inc + 1_SMALL
               result%type = TOKEN_VALUE_CHAR
               exit
            else if (string(end:end) == c .and. string(end + 1:end + 1) == c) then
               result%value = result%value//c
               inc = inc + 2_SMALL
               end = end + 2
            else if (string(end:end) == c) then
               inc = inc + 1_SMALL
               result%type = TOKEN_VALUE_CHAR
               exit
            else
               result%value = result%value//string(end:end)
               end = end + 1
            end if
         end do
         return
      end if

      if (c >= '0' .and. c <= '9') then
         end = ending_int(string(i + 1:)) + i
         if (string(end:end) == '.') then
            end = ending_int(string(end + 1:)) + end
            result%type = TOKEN_VALUE_REAL
         else
            result%type = TOKEN_VALUE_INT
         end if
         c = string(end:end)
         if (c == 'e' .or. c == 'E' .or. c == 'd' .or. c == 'D') then
            end = ending_int(string(end + 1:)) + end
            result%type = TOKEN_VALUE_REAL
         end if
         if (string(end:end) == '_') then
            end = ending(string(end + 1:)) + end
         end if
         result%value = string(i:end - 1)
         inc = int(end - i, SMALL)
         return
      end if

      if ((c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')) then
         inc = ending(string(i + 1:))
         result%type = TOKEN_IDENTIFIER
         result%value = to_upper(string(i:i + inc - 1))
         return
      end if

      inc = 2_SMALL
      cc = string(i:i + offset)
      result%value = cc
      select case (cc)
      case ('**')
         result%type = TOKEN_OP_POW
      case ('//')
         result%type = TOKEN_OP_CONCAT
      case ('::')
         result%type = TOKEN_DCOLON
      case ('=>')
         result%type = TOKEN_PTR_ASSIGN
      case ('==')
         result%type = TOKEN_OP_EQUAL
      case ('/=')
         result%type = TOKEN_OP_NEQUAL
      case ('>=')
         result%type = TOKEN_OP_GREATER_EQUAL
      case ('<=')
         result%type = TOKEN_OP_LESS_EQUAL
      case ('(/')
         result%type = TOKEN_LBRACKET
      case ('/)')
         result%type = TOKEN_RBRACKET
      case default
         inc = 1_SMALL
      end select

      if (inc == 2_SMALL) return

      result%value = c
      select case (c)
      case ('+')
         result%type = TOKEN_OP_ADD
      case ('-')
         result%type = TOKEN_OP_SUB
      case ('*')
         result%type = TOKEN_ASTERISK
      case ('/')
         result%type = TOKEN_OP_DIV
      case ('>')
         result%type = TOKEN_OP_GREATER
      case ('<')
         result%type = TOKEN_OP_LESS
      case ('=')
         result%type = TOKEN_ASSIGN
      case ('%')
         result%type = TOKEN_MEMBER
      case ('?')
         result%type = TOKEN_TERNARY
      case ('^')
         result%type = TOKEN_TEMPLATE
      case ('[')
         result%type = TOKEN_LBRACKET
      case (']')
         result%type = TOKEN_RBRACKET
      case ('(')
         result%type = TOKEN_LPAREN
      case (')')
         result%type = TOKEN_RPAREN
      case (':')
         result%type = TOKEN_COLON
      case (',')
         result%type = TOKEN_COMMA
      case default
         !call throw('unknown symbol '//c, location([file_span(fname, lnum, char)]), .false.)
         result%type = TOKEN_OP_CUSTOM
      end select
   end subroutine

   subroutine lexical_analysis(lexed, input)
      type(list), target, intent(out) :: lexed
      type(list), intent(in) :: input
      
      integer(BIG) :: index
      type(token) :: temptoken
      integer :: i
      integer(SMALL) :: inc
      logical :: continuation

      type(annotated_string), pointer :: line_str
      character(:), pointer :: line

      lexed = list(token())

      continuation = .false.
      outer: &
      do index = 1, input%size
         select type (line_str_poly => input%get(index))
         type is (annotated_string)
            line_str => line_str_poly
         class default
            error stop 'invalid input to lexical_analysis'
         end select
         line => line_str%val

         i = 1
         do while (i <= len(line))
            associate(c => line(i:i))
               if (c == '&' .and. continuation) then
                  continuation = .false.
                  inc = 1_SMALL
                  i = i + inc
                  cycle
               else if (c == ';' .or. c == '!') then
                  temptoken%loc = line_str%loc
                  temptoken%type = TOKEN_NEXTLINE
                  if (c == ';') then
                     temptoken%value = c
                  else
                     temptoken%value = ''
                     call lexed%push(temptoken)
                     cycle outer
                  end if
                  inc = 1_SMALL
               else if (c == ' ') then
                  inc = 1_SMALL
                  i = i + inc
                  cycle
               else if (c == '&') then
                  cycle outer
               else
                  call get_token(temptoken, inc, line_str, i)
               end if
                  
               i = i + inc
               if (temptoken%type == TOKEN_NEXTLINE .and. lexed%size /= 0) then
                  select type (prev => lexed%get(lexed%size))
                  class default
                     error stop 'lexed result is of incorrect type'
                  type is (token)
                     if (prev%type /= TOKEN_NEXTLINE) then
                        call lexed%push(temptoken)
                     end if
                  end select
               else
                  call lexed%push(temptoken)
               end if
            end associate
         end do
      end do outer
   end subroutine

   subroutine print_tokengroup(tokens)
      type(list), intent(in) :: tokens

      integer(BIG) :: i

      do i = 1, tokens%size
         select type (current => tokens%get(i))
         type is (token)
            call print_token(current)
         class default
            error stop 'invalid tokens to print_tokengroup'
         end select
         if (i /= tokens%size) write(*, '(A)', advance='no') ' '
      end do
   end subroutine

   subroutine print_token(dtv)
      class(token), intent(in) :: dtv

      select case (dtv%type)
      case (TOKEN_IDENTIFIER)
         write(*, '(A)', advance='no') 'i: '
      case (TOKEN_VALUE_INT)
         write(*, '(A)', advance='no') 'int: '
      case (TOKEN_VALUE_REAL)
         write(*, '(A)', advance='no') 'real: '
      case (TOKEN_VALUE_LOGICAL)
         write(*, '(A)', advance='no') 'bool: '
      case (TOKEN_VALUE_CHAR)
         write(*, '(A)', advance='no') 'char: '
      case (TOKEN_NEXTLINE)
         write(*, '(A)', advance='no') '(lf)'
      end select
      if (allocated(dtv%value)) write(*, '(A)', advance='no') dtv%value
      if (dtv%type == TOKEN_NEXTLINE) write(*, '(A)') ''
   end subroutine

   function ending_int(str) result(result)
      integer(SMALL) :: result
      character(*), intent(in) :: str

      result = ending(str)
      call ifearlier(str, result, '_')
   end function

   function ending(str) result(result)
      integer(SMALL) :: result
      character(*), intent(in) :: str

      result = int(len(str),SMALL)
      call ifearlier(str, result, ' ')
      call ifearlier(str, result, '/')
      call ifearlier(str, result, '*')
      call ifearlier(str, result, '+')
      call ifearlier(str, result, '-')
      call ifearlier(str, result, ',')
      call ifearlier(str, result, '.')
      call ifearlier(str, result, '(')
      call ifearlier(str, result, ')')
      call ifearlier(str, result, '[')
      call ifearlier(str, result, ']')
      call ifearlier(str, result, '%')
      call ifearlier(str, result, ';')
      call ifearlier(str, result, '>')
      call ifearlier(str, result, '<')
      call ifearlier(str, result, '=')
      call ifearlier(str, result, '&')
      call ifearlier(str, result, ':')
      call ifearlier(str, result, '!')
      call ifearlier(str, result, '?')
      call ifearlier(str, result, '^')
      call ifearlier(str, result, achar(10))
   end function

   subroutine ifearlier(str, pos, char)
      character(*), intent(in) :: str
      integer(SMALL), intent(inout) :: pos
      character, intent(in) :: char

      integer :: temp

      temp = index(str, char)
      if (temp /= 0 .and. temp < pos) pos = int(temp, SMALL)
   end subroutine
end module
