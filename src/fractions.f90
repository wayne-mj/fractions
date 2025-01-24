module fractions
  use iso_fortran_env
  implicit none
  private

  public :: maxdenom, fractiontype, decimalplaces, &
            add_fraction, sub_fraction, multiply_fraction, divide_fraction, &
            lowest_common_denom, mixed_fraction, approx_fraction

  !! Define a type to represent a fraction
  type :: fractiontype
    integer                   :: unit, numerator, denominator
    integer(int64)            :: l_unit, l_numerator, l_denominator
    character(:), allocatable :: status
  end type fractiontype

  type :: closetfractiontype
    type(fractiontype)  :: lower
    type(fractiontype)  :: current
    type(fractiontype)  :: upper
  end type

  !! Define known constants
  integer, parameter  :: maxint = huge(0)
  integer, parameter  :: decimalplaces = 9
  integer, parameter  :: maxdenom = 1 * (10 ** decimalplaces)
  real, parameter     :: tolerance = 1 * 10**(-decimalplaces)

  !! For DEBUG purposes
  !! **********************************
  logical, parameter  :: DBG = .false.
  !! **********************************

  interface add_fraction
    module procedure add_fraction_dt, add_fraction_int
  end interface
  
  interface sub_fraction 
    module procedure sub_fraction_dt, sub_fraction_int
  end interface

  interface multiply_fraction 
    module procedure multiply_fraction_dt, multiply_fraction_int
  end interface

  interface divide_fraction 
    module procedure divide_fraction_dt, divide_fraction_int
  end interface

  interface lowest_common_denom
    module procedure lcd
  end interface

  interface mixed_fraction
    module procedure mixed_fraction_int, mixed_fraction_dt
  end interface

contains
  
  !> ************************************************************************************************************************** <!

  !> Add to fractions together using the data type fractiontype
  function add_fraction_dt(firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
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
    if (firstFraction%denominator .ne. secondFraction%denominator) then
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
    if (fd .ne. sd) then
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

  !> ************************************************************************************************************************** <!

  !> Subtraction dunction using datatype
  function sub_fraction_dt(firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
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
    if (firstFraction%denominator .ne. secondFraction%denominator) then
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
    if (fd .ne. sd) then
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

  !> ************************************************************************************************************************** <!

  !> Multiply fractions
  function multiply_fraction_dt (firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    integer(int64)                  :: l_num, l_denom 
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

  !> ************************************************************************************************************************** <!

  !> Division of fractions
  function divide_fraction_dt(firstFraction, secondFraction) result(resultFraction)
    type(fractiontype), intent(in)  :: firstFraction, secondFraction
    type(fractiontype)              :: resultFraction
    type(fractiontype)              :: tempFraction

    ! If the numerator is zero, then the result will be 
    ! a divide by zero error
    if (secondFraction%numerator .eq. 0) then
      resultFraction%status = 'Error: Division by zero'
      return
    end if

    ! Pass it off to the multiply function by swapping the
    ! numerator and denominator of the second fraction
    tempFraction%numerator = secondFraction%denominator
    tempFraction%denominator = secondFraction%numerator
    resultFraction = multiply_fraction(firstFraction, tempFraction)
  end function divide_fraction_dt

  !> Division of fractions
  function divide_fraction_int(fn, fd, sn, sd) result(resultFraction)
    integer, intent(in)   :: fn, fd, sn, sd
    type(fractiontype)    :: resultFraction

    ! If the numerator is zero, then the result will be 
    ! a divide by zero error
    if (sn .eq. 0) then
      resultFraction%status = 'Error: Division by zero'
      return
    end if

    ! Pass it off to the multiply function by swapping the
    ! numerator and denominator of the second fraction
    resultFraction = multiply_fraction(fn, fd, sd, sn)
  end function divide_fraction_int

  !> ************************************************************************************************************************** <!

  !> Lowest Common Denominator
  function lcd(numerator, denominator) result(resultFraction)
    integer, intent(in)   :: numerator, denominator
    integer               :: num, denum, gcd
    type(fractiontype)    :: resultFraction

    num = 0
    denum =0
    gcd = 0

    ! Make sure we are not dividing by zero anywhere
    if (denominator .eq. 0) then
      resultFraction%status = 'Error: Division by zero'
      return
    end if

    gcd = GCDfunction(abs(numerator), abs(denominator))

    ! Calculate the least common denominator
    num = numerator / gcd
    denum = denominator / gcd

    ! If the numerator is greater than the denominator, then
    ! we need to convert it to a mixed number
    resultFraction = mixed_fraction_int(num, denum)
  end function lcd

  !> ************************************************************************************************************************** <!

  !> Greated Common Denominator
  recursive function GCDfunction (x,y) result(result)
    integer, intent(in) :: x,y
    integer             :: result

    if (x .eq. 0) then
      result = y
    else if (y .eq. 0) then
      result = x
    else
      result = GCDfunction(y, mod(x,y))
    end if
  end function GCDfunction

  !> ************************************************************************************************************************** <!

  !> This function converts a fraction to a mixed number 1 & 3/4 from 7/4
  function mixed_fraction_int (numerator, denominator) result(resultFraction)
    integer, intent(in) :: numerator, denominator
    type(fractiontype)  :: resultFraction

    ! Make sure we are not dividing by zero anywhere
    if (denominator .eq. 0) then
      resultFraction%status = 'Error: Division by zero'
      return
    end if

    ! If the numerator is greater than the denominator, then
    ! we need to convert it to a mixed number
    if (numerator .gt. denominator) then
      resultFraction%unit = abs(numerator / denominator)
      resultFraction%numerator = abs(numerator) - (resultFraction%unit * denominator)
      resultFraction%denominator = denominator
    else
      resultFraction%unit = 0
      resultFraction%numerator = abs(numerator)
      resultFraction%denominator = denominator
    end if
    
    resultFraction%status = 'OK'
  end function mixed_fraction_int

  !> This function converts a fraction to a mixed number 1 & 3/4 from 7/4
  function mixed_fraction_dt (dt) result(resultFraction)
    type(fractiontype), intent(in)  :: dt
    type(fractiontype)              :: resultFraction

    ! Make sure we are not dividing by zero anywhere
    if (dt%denominator .eq. 0) then
      resultFraction%status = 'Error: Division by zero'
      return
    end if

    ! If the numerator is greater than the denominator, then
    ! we need to convert it to a mixed number
    if (dt%numerator .gt. dt%denominator) then
      resultFraction%unit = abs(dt%numerator / dt%denominator)
      resultFraction%numerator = abs(dt%numerator) - (resultFraction%unit * dt%denominator)
      resultFraction%denominator = dt%denominator
    else
      resultFraction%unit = 0
      resultFraction%numerator = abs(dt%numerator)
      resultFraction%denominator = dt%denominator
    end if
    
    resultFraction%status = 'OK'
  end function mixed_fraction_dt

  !> ************************************************************************************************************************** <!

  !> This function converts a decimal to an approx fraction
  function approx_fraction(decimal) result(resultFraction)
    real(real64), intent(in)    :: decimal
    type(fractiontype)          :: resultFraction
    integer(int64)              :: l_num, l_denom
    integer                     :: num, denum
    logical                     :: overflow
    
    num = 0
    denum = 0
    l_num = 0
    l_denom = 0
    overflow = .false.

    l_num = int(decimal * maxdenom, int64)
    l_denom = maxdenom

    if (DBG) then
      print *, decimal, l_num, l_denom
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
      num = int(l_num, int32)
      denum = int(l_denom, int32)
      resultFraction = lcd(num,denum)!(int(l_num, int32), int(l_denom, int32))
    end if
  end function approx_fraction
  
  !> ************************************************************************************************************************** <!

  ! function guess_fraction(decimal) result(rf)
  !   real(real32), intent(in)    :: decimal 
  !   type(fractiontype)  :: rf, tf 
  !   integer(int64)      :: l_num, l_denom 
  !   logical             :: overflow 

  !   overflow = .false.
  !   tf = approx_fraction(decimal)
  !   l_num = int(tf%numerator,int64)
  !   l_denom = int(tf%denominator, int64)

  ! end function guess_fraction

  !> ************************************************************************************************************************** <!

  !> This function checks for integer overflow
  !> Using long integers to check for overflow
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

  !> ************************************************************************************************************************** <!

end module fractions
