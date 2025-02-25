!todo: add BOZ

module lexer
    use include, only: SMALL, tocaps, throw
    implicit none

    type :: token
        integer(SMALL) :: type ! TOKEN_
        character(:), allocatable :: value
        integer(SMALL) :: line
        integer(SMALL) :: char
    end type

    type :: tokengroup
        type(token), allocatable :: tokens(:)
        integer :: size = 0
    contains
        procedure :: append => tokengroup_append
    end type

    integer(SMALL), parameter :: TOKEN_IDENTIFIER = 0
    integer(SMALL), parameter :: TOKEN_OPERATOR = 1
    integer(SMALL), parameter :: TOKEN_VALUE_INT = 2
    integer(SMALL), parameter :: TOKEN_VALUE_REAL = 3
    integer(SMALL), parameter :: TOKEN_VALUE_LOGICAL = 4
    integer(SMALL), parameter :: TOKEN_VALUE_CHAR = 5
    integer(SMALL), parameter :: TOKEN_LGROUP = 6
    integer(SMALL), parameter :: TOKEN_RGROUP = 7
    integer(SMALL), parameter :: TOKEN_NEXTLINE = 8
    integer(SMALL), parameter :: TOKEN_ASSIGN = 9
    integer(SMALL), parameter :: TOKEN_ASTERISK = 10

    type(tokengroup) :: lexed
contains

    subroutine lex(input, fname, startline)
        character(:), allocatable, intent(in) :: input
        character(*), intent(in) :: fname
        integer(SMALL), allocatable, optional, intent(in) :: startline
        
        integer(SMALL) :: lnum, char, offset
        type(token) :: temptoken
        integer :: i, end
        integer(SMALL) :: inc

        if (present(startline)) then
            lnum = startline
        else
            lnum = 1_SMALL
        end if

        lexed%size = 0
        if (allocated(lexed%tokens)) deallocate(lexed%tokens)
        char = 1_SMALL
        i = 1
        
        do while (i <= len(input))
            if (i /= len(input)) then
                offset = 1
            else
                offset = 0
            end if
            associate(c => input(i:i), cc => input(i:i+offset))
                temptoken%line = lnum
                temptoken%char = char
                if (cc == '**' .or. cc == '//' .or. cc == '::' .or. cc == '=>') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = cc
                    inc = 2_SMALL
                else if (cc == '==') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = '.EQ.'
                    inc = 2_SMALL
                else if (cc == '/=') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = '.NE.'
                    inc = 2_SMALL
                else if (cc == '>=') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = '.GE.'
                    inc = 2_SMALL
                else if (cc == '<=') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = '.LE.'
                    inc = 2_SMALL
                else if (c == '<') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = '.LT.'
                    inc = 1_SMALL
                else if (c == '>') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = '.GT.'
                    inc = 1_SMALL
                else if (c == '+' .or. c == '-' .or. c == '/' .or. c == ':' .or. c == '%' .or. c== ',' .or. c == '?') then
                    temptoken%type = TOKEN_OPERATOR
                    temptoken%value = c
                    inc = 1_SMALL
                else if (c == '*') then
                    temptoken%type = TOKEN_ASTERISK
                    temptoken%value = c
                    inc = 1_SMALL
                else if (cc == '(/') then
                    temptoken%type = TOKEN_LGROUP
                    temptoken%value = cc
                    inc = 2_SMALL
                else if (c == '(' .or. c == '[') then
                    temptoken%type = TOKEN_LGROUP
                    temptoken%value = c
                    inc = 1_SMALL
                else if (cc == '/)') then
                    temptoken%type = TOKEN_RGROUP
                    temptoken%value = cc
                    inc = 2_SMALL
                else if (c == ')' .or. c == ']') then
                    temptoken%type = TOKEN_RGROUP
                    temptoken%value = c
                    inc = 1_SMALL
                else if (c == ';' .or. c == achar(10) .or. c == '!') then
                    temptoken%type = TOKEN_NEXTLINE
                    if (c == ';') then
                        temptoken%value = c
                    else if (c == '!') then
                        i = index(input(i:), achar(10)) + i - 1
                        temptoken%value = ''
                    else
                        temptoken%value = ''
                    end if
                    inc = 1_SMALL
                    char = 0
                    lnum = lnum + 1_SMALL
                else if (c == '=') then
                    temptoken%type = TOKEN_ASSIGN
                    temptoken%value = c
                    inc = 1_SMALL
                else if (c == '.') then
                    end = index(input(i+1:), '.') + i
                    ! I would use an associate here but gfortran threw a runtime error for no reason
                    if (tocaps(input(i:end)) == '.TRUE.' .or. tocaps(input(i:end)) == '.FALSE.') then
                        temptoken%type = TOKEN_VALUE_LOGICAL
                    else
                        temptoken%type = TOKEN_OPERATOR
                    end if
                    temptoken%value = tocaps(input(i:end))
                    inc = int(end - i + 1, 2)
                else if (c == '''') then
                    end = i + 1
                    char = char + 1_SMALL
                    temptoken%value = ''
                    do
                        if (input(end:end) == '''' .and. input(end + 1:end + 1) /= '''') then
                            i = end + 1
                            char = char + 1_SMALL
                            inc = 0
                            temptoken%type = TOKEN_VALUE_CHAR
                            exit
                        else if (input(end:end) == '''' .and. input(end + 1:end + 1) == '''') then
                            temptoken%value = temptoken%value//''''
                            end = end + 2
                            char = char + 2_SMALL
                        else if (input(end:end) == achar(10)) then
                            call throw('expected end of string', fname, lnum, char, .false.)
                            i = end + 1
                            lnum = lnum + 1_SMALL
                            char = 1
                            inc = 0
                            temptoken%type = TOKEN_VALUE_CHAR
                            exit
                        else
                            temptoken%value = temptoken%value//input(end:end)
                            end = end + 1
                            char = char + 1_SMALL
                        end if
                    end do
                else if (c == """") then
                    end = i + 1
                    char = char + 1_SMALL
                    temptoken%value = ''
                    do
                        if (input(end:end) == """" .and. input(end + 1:end + 1) /= """") then
                            i = end + 1
                            char = char + 1_SMALL
                            inc = 0
                            temptoken%type = TOKEN_VALUE_CHAR
                            exit
                        else if (input(end:end) == """" .and. input(end + 1:end + 1) == """") then
                            temptoken%value = temptoken%value//""""
                            end = end + 2
                            char = char + 2_SMALL
                        else if (input(end:end) == achar(10)) then
                            call throw('expected end of string', fname, lnum, char, .false.)
                            i = end + 1
                            lnum = lnum + 1_SMALL
                            char = 1
                            inc = 0
                            temptoken%type = TOKEN_VALUE_CHAR
                            exit
                        else
                            temptoken%value = temptoken%value//input(end:end)
                            end = end + 1
                            char = char + 1_SMALL
                        end if
                    end do
                else if (c >= '0' .and. c <= '9') then
                    end = ending_int(input(i + 1:)) + i
                    if (input(end:end) == '.') then
                        end = ending_int(input(end + 1:)) + end
                        temptoken%type = TOKEN_VALUE_REAL
                    else
                        temptoken%type = TOKEN_VALUE_INT
                    end if
                    if (input(end:end) == '_') then
                        end = ending(input(end + 1:)) + end
                    end if
                    temptoken%value = input(i:end - 1)
                    i = end
                    inc = 0_SMALL
                else if (c == ' ') then
                    inc = 1_SMALL
                    i = i + inc
                    char = char + inc
                    cycle
                else if (c == achar(0)) then
                    do i = 1, lexed%size - 1
                        call print_token(lexed%tokens(i))
                    end do
                    return
                else if (( c >= 'a' .and. c <= 'z') .or. (c >= 'A' .and. c <= 'Z')) then
                    inc = ending(input(i + 1:))
                    temptoken%type = TOKEN_IDENTIFIER
                    temptoken%value = tocaps(input(i:i + inc - 1))
                else if (c == '&') then
                    i = i + 1
                    do while (i <= len(input) .and. input(i:i) /= achar(10) .and. input(i:i) /= ' ')
                        i = i + 1
                    end do
                    if (input(i:i) == achar(10)) i = i + 1
                    if (input(i:i) == '&') i = i + 1
                    char = 0
                    lnum = lnum + 1_SMALL
                    cycle
                else
                    call throw('unknown symbol '//c, fname, lnum, char, .false.)
                    i = ending(input(i + 1:)) + i + 1
                    inc = 0_SMALL
                    temptoken%type = TOKEN_IDENTIFIER
                    temptoken%value = ''
                end if
                i = i + inc
                char = char + inc
                call lexed%append(temptoken)
            end associate
        end do
    end subroutine

    subroutine print_token(dtv)
        class(token), intent(in) :: dtv

        select case (dtv%type)
        case (TOKEN_IDENTIFIER)
            write(*, '(A)', advance='no') 'IDENT: '
        case (TOKEN_OPERATOR)
            write(*, '(A)', advance='no') 'OP: '
        case (TOKEN_VALUE_INT)
            write(*, '(A)', advance='no') 'INT: '
        case (TOKEN_VALUE_REAL)
            write(*, '(A)', advance='no') 'REAL: '
        case (TOKEN_VALUE_LOGICAL)
            write(*, '(A)', advance='no') 'LOGICAL: '
        case (TOKEN_VALUE_CHAR)
            write(*, '(A)', advance='no') 'CHAR: '
        case (TOKEN_LGROUP)
            write(*, '(A)', advance='no') 'LGROUP: '
        case (TOKEN_RGROUP)
            write(*, '(A)', advance='no') 'RGROUP: '
        case (TOKEN_NEXTLINE)
            write(*, '(A)', advance='no') 'NEXTLINE: '
        case (TOKEN_ASSIGN)
            write(*, '(A)', advance='no') 'ASSIGNMENT: '
        case (TOKEN_ASTERISK)
            write(*, '(A)', advance='no') 'ASTERISK: '
        end select
        if (allocated(dtv%value)) write(*, '(A)') dtv%value
    end subroutine

    subroutine tokengroup_append(this, value)
        class(tokengroup) :: this
        type(token), intent(in) :: value

        type(token), allocatable :: tmp(:)
        if (iand(this%size, 15) == 0) then
            if (this%size == 0) then
                allocate(this%tokens(15))
                this%size = 1
            else
                allocate(tmp(this%size + 15))
                tmp(:this%size - 1) = this%tokens(:this%size - 1) !copy the data
                call move_alloc(tmp, this%tokens) !rename
            end if
        end if
        this%tokens(this%size) = value
        this%size = this%size + 1
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