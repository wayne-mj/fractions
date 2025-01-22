module fractions
  use iso_fortran_env
  implicit none
  private

  public :: say_hello, maxdenom, fractiontype, add_fraction, sub_fraction, multiply_fraction

  !! Define a type to represent a fraction
  type :: fractiontype
    integer                   :: unit, numerator, denominator
    integer(int64)            :: l_unit, l_numerator, l_denominator
    character(:), allocatable :: status
  end type fractiontype

  !! Define known constants
  integer, parameter :: maxint = huge(0)
  integer, parameter :: decimalplaces = 6
  integer, parameter :: maxdenom = 1 * (10 ** decimalplaces)

  interface add_fraction
    module procedure add_fraction_dt, add_fraction_int
  end interface
  
  interface sub_fraction 
    module procedure sub_fraction_dt, sub_fraction_int
  end interface

  interface multiply_fraction 
    module procedure multiply_fraction_dt, multiply_fraction_int
  end interface

contains
  subroutine say_hello
    type(fractiontype) :: my

    my%status = "Hello World"
    print *, my%status, len(my%status)
  end subroutine say_hello

  !> Add to fractions together using the data type fractiontype
  function add_fraction_dt(firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    integer(int64)                         :: l_num, l_denom 
    logical                         :: overflow

    ! Initialize the variables
    l_num = 0
    l_denom = 0
    overflow = .false.

    ! Make sure we are not dividing by zero anywhere
    if ((firstFraction%denominator .eq. 0) .or. (secondFraction%denominator .eq. 0)) then
        resultFraction%status = 'Error: Division by zero'
        return
    end if

    ! Convert everything to long integers to handle integer overflow
    ! and perform the calculations
    if (firstFraction%denominator /= secondFraction%denominator) then
        l_num = (int(firstFraction%numerator,KIND=int64) * int(secondFraction%denominator,kind=int64)) + (int(secondFraction%numerator,kind=int64) * int(firstFraction%denominator,kind=int64))
        l_denom = int(firstFraction%denominator,kind=int64) * int(secondFraction%denominator,kind=int64)
    else
        l_num = int(firstFraction%numerator,kind=int64) + int(secondFraction%numerator,kind=int64)
        l_denom = int(firstFraction%denominator,kind=int64)
    end if

    !Test for Integer Overflow
    overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

    ! If there is an overflow, set the result to 0
    ! Otherwise, set the result to the calculated values
    if (overflow) then
        resultFraction%numerator = 0
        resultFraction%denominator = 0
        resultFraction%unit = 0
        resultFraction%l_numerator = l_num
        resultFraction%l_denominator = l_denom
        resultFraction%status = 'Error: Integer Overflow'
    else
        resultFraction%numerator = int(l_num)
        resultFraction%denominator = int(l_denom)
        resultFraction%unit = 0
        resultFraction%status = 'OK'
    end if
  end function add_fraction_dt

  !> Add to fractions together using the data type integers and returns datatype
  function add_fraction_int(fn, fd, sn, sd) result(resultFraction)
    integer, intent(in)             :: fn, fd, sn, sd
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
    logical                         :: overflow

    ! Initialize the variables
    l_num = 0
    l_denom = 0
    overflow = .false.

    ! Make sure we are not dividing by zero anywhere
    if ((fd .eq. 0) .or. (sd .eq. 0)) then
        resultFraction%status = 'Error: Division by zero'
        return
    end if

    ! Convert everything to long integers to handle integer overflow
    ! and perform the calculations
    if (fd /= sd) then
        l_num = (int(fn,KIND=int64) * int(sd,kind=int64)) + (int(sn,kind=int64) * int(fd,kind=int64))
        l_denom = int(fd,kind=int64) * int(sd,kind=int64)
    else
        l_num = int(fn,kind=int64) + int(sn,kind=int64)
        l_denom = int(fd,kind=int64)
    end if

    !Test for Integer Overflow
    overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

    ! If there is an overflow, set the result to 0
    ! Otherwise, set the result to the calculated values
    if (overflow) then
        resultFraction%numerator = 0
        resultFraction%denominator = 0
        resultFraction%unit = 0
        resultFraction%l_numerator = l_num
        resultFraction%l_denominator = l_denom
        resultFraction%status = 'Error: Integer Overflow'
    else
        resultFraction%numerator = int(l_num)
        resultFraction%denominator = int(l_denom)
        resultFraction%unit = 0
        resultFraction%status = 'OK'
    end if
  end function add_fraction_int

  !> Subtraction dunction using datatype
  function sub_fraction_dt(firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
    logical                         :: overflow = .false.

    ! Initialize the variables
    l_num = 0
    l_denom = 0

    ! Make sure we are not dividing by zero anywhere
    if ((firstFraction%denominator .eq. 0) .or. (secondFraction%denominator .eq. 0)) then
        resultFraction%status = 'Error: Division by zero'
        return
    end if

    ! Convert everything to long integers to handle integer overflow
    ! and perform the calculations
    if (firstFraction%denominator /= secondFraction%denominator) then
        l_num = (int(firstFraction%numerator,int64) * int(secondFraction%denominator,int64)) - (int(secondFraction%numerator,int64) * int(firstFraction%denominator,int64))
        l_denom = int(firstFraction%denominator,int64) * int(secondFraction%denominator,int64)
    else
        l_num = int(firstFraction%numerator,int64) - int(secondFraction%numerator,int64)
        l_denom = int(firstFraction%denominator,int64)
    end if
    
    !Test for Integer Overflow
    overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

    ! If there is an overflow, set the result to 0
    ! Otherwise, set the result to the calculated values
    if (overflow) then
        resultFraction%numerator = 0
        resultFraction%denominator = 0
        resultFraction%unit = 0
        resultFraction%l_numerator = l_num
        resultFraction%l_denominator = l_denom
        resultFraction%status = 'Error: Integer Overflow'
    else
        resultFraction%numerator = int(l_num)
        resultFraction%denominator = int(l_denom)
        resultFraction%unit = 0
        resultFraction%status = 'OK'
    end if
  end function sub_fraction_dt

  !> Subtraction using integers and returns datatype
  function sub_fraction_int(fn, fd, sn, sd) result(resultFraction)
    integer, intent(in)             :: fn, fd, sn, sd
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
    logical                         :: overflow = .false.

    ! Initialize the variables
    l_num = 0
    l_denom = 0

    ! Make sure we are not dividing by zero anywhere
    if ((fd .eq. 0) .or. (sd .eq. 0)) then
        resultFraction%status = 'Error: Division by zero'
        return
    end if

    ! Convert everything to long integers to handle integer overflow
    ! and perform the calculations
    if (fd /= sd) then
        l_num = (int(fn,int64) * int(sd,int64)) - (int(sn,int64) * int(fd,int64))
        l_denom = int(fd,int64) * int(sd,int64)
    else
        l_num = int(fn,int64) - int(sn,int64)
        l_denom = int(fd,int64)
    end if
    
    !Test for Integer Overflow
    overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

    ! If there is an overflow, set the result to 0
    ! Otherwise, set the result to the calculated values
    if (overflow) then
        resultFraction%numerator = 0
        resultFraction%denominator = 0
        resultFraction%unit = 0
        resultFraction%l_numerator = l_num
        resultFraction%l_denominator = l_denom
        resultFraction%status = 'Error: Integer Overflow'
    else
        resultFraction%numerator = int(l_num)
        resultFraction%denominator = int(l_denom)
        resultFraction%unit = 0
        resultFraction%status = 'OK'
    end if
  end function sub_fraction_int

  !> Multiply fractions
  function multiply_fraction_dt (firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
    logical                         :: overflow = .false.

    ! Initialize the variables
    l_num = 0
    l_denom = 0
    
    ! Make sure we are not dividing by zero anywhere
    if ((firstFraction%denominator .eq. 0) .or. (secondFraction%denominator .eq. 0)) then
        resultFraction%status = 'Error: Division by zero'
        return
    end if

    l_num = int(firstFraction%numerator,int64) * int(secondFraction%numerator,int64)
    l_denom = int(firstFraction%denominator,int64) * int(secondFraction%denominator,int64)

    !Test for Integer Overflow
    overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

    ! If there is an overflow, set the result to 0
    ! Otherwise, set the result to the calculated values
    if (overflow) then
        resultFraction%numerator = 0
        resultFraction%denominator = 0
        resultFraction%unit = 0
        resultFraction%l_numerator = l_num
        resultFraction%l_denominator = l_denom
        resultFraction%status = 'Error: Integer Overflow'
    else
        resultFraction%numerator = int(l_num)
        resultFraction%denominator = int(l_denom)
        resultFraction%unit = 0
        resultFraction%status = 'OK'
    end if
  end function multiply_fraction_dt

  !> Multiply fractions
  function multiply_fraction_int (fn, fd, sn, sd) result(resultFraction)
    integer, intent(in)             :: fn, fd, sn, sd
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
    logical                         :: overflow = .false.

    ! Initialize the variables
    l_num = 0
    l_denom = 0
    
    ! Make sure we are not dividing by zero anywhere
    if ((fd .eq. 0) .or. (sd .eq. 0)) then
        resultFraction%status = 'Error: Division by zero'
        return
    end if

    l_num = int(fn,int64) * int(sn,int64)
    l_denom = int(fd,int64) * int(sd,int64)

    !Test for Integer Overflow
    overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

    ! If there is an overflow, set the result to 0
    ! Otherwise, set the result to the calculated values
    if (overflow) then
        resultFraction%numerator = 0
        resultFraction%denominator = 0
        resultFraction%unit = 0
        resultFraction%l_numerator = l_num
        resultFraction%l_denominator = l_denom
        resultFraction%status = 'Error: Integer Overflow'
    else
        resultFraction%numerator = int(l_num)
        resultFraction%denominator = int(l_denom)
        resultFraction%unit = 0
        resultFraction%status = 'OK'
    end if
  end function multiply_fraction_int

  ! This function checks for integer overflow
  ! Using long integers to check for overflow
  function chkoverflow (testInteger) result(bool)
    integer(int64), intent(in)  :: testInteger
    logical                     :: bool

    bool = .false.

    ! Check for integer overflow
    if (testInteger .ge. (maxint+1) ) then          ! Originally maxint -1
        bool = .true.
        return
    else if (testInteger .le. (-maxint-1)) then     ! originally -maxint + 1
        bool = .true.
        return
    end if
  end function chkoverflow

end module fractions
