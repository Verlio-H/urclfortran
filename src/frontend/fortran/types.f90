module fort_types
    implicit none
    integer, parameter :: TYPE_PARAMETERIZED = 0
    integer, parameter :: TYPE_ASSUMED = -1
    integer, parameter :: TYPE_DEFERRED = -2
    integer, parameter :: TYPE_CALCULATED = -3 ! TYPE_CALCULATED-(index in calculated array index table)

    ! TYPE in fortran, STRUCT in c
    integer, parameter :: TYPE_STRUCTURE = 0

    ! ENUMERATOR in fortran, ENUM in c
    integer, parameter :: TYPE_ENUMERATOR = 1

    ! EQUIVALENCE in fortran, UNION in c
    integer, parameter :: TYPE_UNION = 2

    ! ABSTRACT in fortran, none in c
    integer, parameter :: TYPE_ABSTRACT = 3

    ! a type without any modifiers. representable by type(name(kind,length))
    type :: base_type
        ! there are some types that need to be included for inheritance reasons but are not usable
        logical :: available = .true.

        character(:), allocatable :: name

        ! can be parameterized
        integer :: kind = 1

        ! can be parameterized, assumed, deferred, calculated
        integer :: length = 1

        ! if true then type(name(A)) specifies length A. if false specifies kind A
        logical :: length_first = .false.

        type(ext_type), allocatable :: components(:)

        ! inheritance
        type(base_type), pointer :: parent => null()
    end type

    type :: array_bound
        integer :: lflags ! can be deferred, assumed, calculated
        integer :: lbound
        integer :: uflags ! can be deferred, assumed, calculated
        integer :: ubound
    end type


    ! Determining "reference depth" of a value:
    ! Start at 0
    ! if (VALUE) sub 1
    ! if (ARG) add 1
    ! if (POINTER) add 1
    ! if (ALLOCATABLE) add 1

    ! other things of note
    ! if intent(out) then previous value unused
    ! all target variables that an ir pointer could point to need to be resolved

    ! VALUE attribute in fortran, applies to all function arguments in c
    integer, parameter :: VALPROP_VALUE = 2**0

    ! INTENT(INOUT) implies neither VALPROP_INTENTIN nor VALPROP_INTENTOUT
    ! INTENT(IN) attribute in fortran, CONST in c
    integer, parameter :: VALPROP_INTENTIN = 2**1

    ! INTENT(OUT) attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_INTENTOUT = 2**2

    ! ALLOCATABLE attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_ALLOCATABLE = 2**3

    ! POINTER attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_POINTER = 2**4

    ! PARAMETER attribute in fortran, CONSTEXPR attribute in c
    integer, parameter :: VALPROP_PARAMETER = 2**5

    ! SAVE attribute in fortran or implied save or module variable, STATIC and globals in c
    integer, parameter :: VALPROP_SAVE = 2**6

    ! OPTIONAL attribute in fortran, applies to variadic in c
    integer, parameter :: VALPROP_OPTIONAL = 2**8

    ! TARGET attribute in fortran, inverse of REGISTER in c
    integer, parameter :: VALPROP_TARGET = 2**9

    ! PRIVATE attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_PRIVATE = 2**10

    ! PUBLIC attribute in fortran, applies to all in c
    integer, parameter :: VALPROP_PUBLIC = 2**11

    ! PROTECTED attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_PROTECTED = 2**12

    ! EXTERNAL attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_EXTERN_PROC = 2**13

    ! VOLATILE attribute in fortran, VOLATILE in c
    integer, parameter :: VALPROP_VOLATILE = 2**14

    ! RANK(..) attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_DEFFERED_RANK = 2**15

    ! DEFERRED attribute in fortran (templates), applies to none in c
    integer, parameter :: VALPROP_DEFERRED = 2**16

    ! ASYNCHRONOUS attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_ASYNCHRONOUS = 2**17

    ! NON_OVERRIDABLE attribute in fortran, applies to none in c
    integer, parameter :: VALPROP_NON_OVERRIDABLE = 2**18

    ! applies to all in fortran, RESTRICT in c
    integer, parameter :: VALPROP_RESTRICT = 2**19

    ! applies to variables from external module in fortran, EXTERN in c
    integer, parameter :: VALPROP_EXTERN = 2**20

    ! marks variable as function arguments
    integer, parameter :: VALPROP_ARG = 2**21

    ! marks variable as class(foo) variable, nothing in c
    integer, parameter :: VALPROP_CLASS = 2**22

    ! PASS attribute fortran, nothing in c
    integer, parameter :: VALPROP_PASS = 2**23

    type :: ext_type
        type(base_type), pointer :: type

        integer :: flags !VALPROP_ values
        
        type(array_bound), allocatable :: array_bounds(:)
        logical :: coarray
    end type

    type :: string
        character(:), allocatable :: value
    end type

    type :: var
        character(:), allocatable :: name
        type(ext_type) :: type
        type(string), allocatable :: contents(:) ! if parameter
    end type

    type :: value
        type(ext_type) :: type
        type(string), allocatable :: contents(:)
    end type

    abstract interface
        function op_comptime(a) result(b)
            import value
            type(value), intent(in) :: a(:)
            type(value) :: b
        end function
    end interface

    integer, parameter :: INTERFACE_NORMAL = 0
    integer, parameter :: INTERFACE_GENERIC = 1
    integer, parameter :: INTERFACE_ABSTRACT = 2

    type :: interface
        character(:), allocatable :: name
        integer :: type
        type(function), allocatable :: functions
    end type

    ! Fortran related ones

    ! void function in c
    integer, parameter :: FUNC_SUBROUTINE = 2**0

    ! extern in c
    integer, parameter :: FUNC_PUBLIC = 2**0

    ! static and funcptr generated in c
    integer, parameter :: FUNC_PRIVATE = 2**1

    ! only intrinsic in c
    integer, parameter :: FUNC_PURE = 2**2

    ! only intrinsic in c
    integer, parameter :: FUNC_SIMPLE = 2**3

    ! only intrinsic in c
    integer, parameter :: FUNC_ELEMENTAL = 2**4

    ! C related ones

    ! only intrinsic in fortran
    integer, parameter :: FUNC_INLINE = 2**5

    ! states that casts should be inserted on arguments if applicable
    ! always true in c, never true in fortran (except in intrinsics)
    integer, parameter :: FUNC_CAST = 2**6

    ! intrinsic only ones
    ! means that a function in the form foo(a, b, ...) can be broken into foo(a.1, b.1, ...) and foo(a.2, b.2, ...)
    ! used for assignment and other internals
    integer, parameter :: FUNC_BREAKABLE = 2**7

    ! means that arguments may be passed in any order
    ! only applies to functions with two arguments
    ! is used in GVN to determine that eg a+b == b+a
    integer, parameter :: FUNC_COMMUTATIVE = 2**8
    
    ! means that func(a + i, b) = func(a, b) + func(i, b)
    ! is used for strength reduction and GVN
    ! *
    ! also in fortran specifically: / (real, complex)
    integer, parameter :: FUNC_DISTRIBUTES_ADD = 2**9

    ! means that func(a, 0) = a. 0 = 0 for int, real, cmplx. 0 = '' for char. 0 = .false. for logical
    ! +, -, .or., .neqv., //
    integer, parameter :: FUNC_IDENT_0 = 2**10

    ! means that func(a, 1) = a. 1 = 1 for int, real, cmplx. 1 is undefined for char, 1 = .true. for logical
    ! *, /, **, .and., .eqv.
    integer, parameter :: FUNC_IDENT_1 = 2**11

    ! means that func(0, a) = 0
    ! *, /, **, .and.
    integer, parameter :: FUNC_0_0 = 2**12

    ! means that func(1, a) = 1
    ! **, .or.
    integer, parameter :: FUNC_1_1 = 2**13

    ! means that func(a, 0) = 1
    ! **
    integer, parameter :: FUNC_0_1 = 2**14


    type :: function
        character(:), allocatable :: name
        integer :: flags ! FUNC_
        type(ext_type) :: return_type
        type(function), pointer :: inverse
        type(var), allocatable :: template_args(:)
        type(var), allocatable :: args(:)
        procedure(op_comptime), pointer, nopass :: comptime => null()
    end type
end module