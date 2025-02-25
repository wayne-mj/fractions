module fractionmodule
    implicit none

    !! Define a type to represent a fraction
    type :: fractiontype
        integer :: unit, numerator, denominator
        integer(8) :: l_numerator, l_denominator
        character(32) :: status
    end type fractiontype

    !! Define known constants
    integer, parameter :: maxint = huge(0)
    integer, parameter :: maxdenom = 100000
    integer, parameter :: decimalplaces = 6

    contains

    function addfraction(firstFraction, secondFraction) result(resultFraction)
        type(fractiontype), intent(in) :: firstFraction, secondFraction
        type(fractiontype) :: resultFraction
        integer(8) :: l_num, l_denom 
        logical :: overflow = .false.

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
            l_num = (int(firstFraction%numerator,8) * int(secondFraction%denominator,8)) + (int(secondFraction%numerator,8) * int(firstFraction%denominator,8))
            l_denom = int(firstFraction%denominator,8) * int(secondFraction%denominator,8)
        else
            l_num = int(firstFraction%numerator,8) + int(secondFraction%numerator,8)
            l_denom = int(firstFraction%denominator,8)
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
    end function addfraction

    function subfraction(firstFraction, secondFraction) result(resultFraction)
        type(fractiontype), intent(in) :: firstFraction, secondFraction
        type(fractiontype) :: resultFraction
        integer(8) :: l_num, l_denom 
        logical :: overflow = .false.

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
            l_num = (int(firstFraction%numerator,8) * int(secondFraction%denominator,8)) - (int(secondFraction%numerator,8) * int(firstFraction%denominator,8))
            l_denom = int(firstFraction%denominator,8) * int(secondFraction%denominator,8)
        else
            l_num = int(firstFraction%numerator,8) - int(secondFraction%numerator,8)
            l_denom = int(firstFraction%denominator,8)
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
    end function subfraction

    function multiplyfraction (firstFraction, secondFraction) result(resultFraction)
        type(fractiontype), intent(in) :: firstFraction, secondFraction
        type(fractiontype) :: resultFraction
        integer(8) :: l_num, l_denom 
        logical :: overflow = .false.

        ! Initialize the variables
        l_num = 0
        l_denom = 0
        
        ! Make sure we are not dividing by zero anywhere
        if ((firstFraction%denominator .eq. 0) .or. (secondFraction%denominator .eq. 0)) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        l_num = int(firstFraction%numerator,8) * int(secondFraction%numerator,8)
        l_denom = int(firstFraction%denominator,8) * int(secondFraction%denominator,8)

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
    end function multiplyfraction

    function dividefraction(firstFraction, secondFraction) result(resultFraction)
        type(fractiontype), intent(in) :: firstFraction, secondFraction
        type(fractiontype) :: resultFraction
        type(fractiontype) :: tempFraction

        ! If the numerator is zero, then the result will be 
        ! a divide by zero error
        if (secondFraction%numerator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        ! Pass it off to the multiply function by swapping the
        ! numerator and denominator of the second fraction
        tempFraction%numerator = secondFraction%denominator
        tempFraction%denominator = secondFraction%numerator
        resultFraction = multiplyfraction(firstFraction, tempFraction)
    end function dividefraction

    function lcd(numerator, denominator) result(resultFraction)
        integer, intent(in) :: numerator, denominator
        integer :: num = 0, denum = 0, gcd =0
        ! integer :: temp
        type(fractiontype) :: resultFraction

        ! Make sure we are not dividing by zero anywhere
        if (denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        ! Find the greatest common divisor
        ! Old and slow way
        !do temp = 1, min(abs(numerator), abs(denominator)+1)
        !    if (mod(denominator, temp) == 0 .and. mod(numerator, temp) == 0) then
        !        gcd = temp
        !    end if
        !end do
        gcd = GCDfunction(abs(numerator), abs(denominator))

        ! Calculate the least common denominator
        num = numerator / gcd
        denum = denominator / gcd

        ! If the numerator is greater than the denominator, then
        ! we need to convert it to a mixed number
        resultFraction = mixedFraction(num, denum)
    end function lcd

    recursive function GCDfunction (x,y) result(result)
        integer, intent(in) :: x,y
        integer :: result

        if (x == 0) then
            result = y
        else if (y == 0) then
            result = x
        else
            result = GCDfunction(y, mod(x,y))
        end if
    end function GCDfunction

    ! This function converts a fraction to a mixed number 1 & 3/4 from 7/4
    function mixedFraction (numerator, denominator) result(resultFraction)
        integer, intent(in) :: numerator, denominator
        type(fractiontype) :: resultFraction

        ! Make sure we are not dividing by zero anywhere
        if (denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        ! If the numerator is greater than the denominator, then
        ! we need to convert it to a mixed number
        if (numerator > denominator) then
            resultFraction%unit = abs(numerator / denominator)
            resultFraction%numerator = abs(numerator) - (resultFraction%unit * denominator)
            resultFraction%denominator = denominator
        else
            resultFraction%unit = 0
            resultFraction%numerator = abs(numerator)
            resultFraction%denominator = denominator
        end if
        
        resultFraction%status = 'OK'

    end function mixedFraction

    ! Make a mixed fraction from a fraction type 7/4 to 1 & 3/4
    function makemixedFraction(fraction) result(resultFraction)
        type(fractiontype), intent(in) :: fraction
        type(fractiontype) :: resultFraction

        if (fraction%unit /= 0) then
            resultFraction%numerator = (fraction%unit * fraction%denominator) + fraction%numerator
            resultFraction%denominator = fraction%denominator
        else
            resultFraction%numerator = fraction%numerator
            resultFraction%denominator = fraction%denominator
        end if
    end function makemixedFraction

    ! This function converts a decimal to a fraction
    function decimaltofractionapprox(decimal) result(resultFraction)
        real, intent(in) :: decimal
        type(fractiontype) :: resultFraction
        integer(8) :: l_num, l_denom
        logical :: overflow = .false.

        l_num = int(nint(decimal * maxdenom),8)
        l_denom = maxdenom

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
            resultFraction = lcd(int(l_num,4), int(l_denom,4))
        end if
    end function decimaltofractionapprox

    ! This function checks for integer overflow
    ! Using long integers to check for overflow
    function chkoverflow (testInteger) result(bool)
        integer(8), intent(in) :: testInteger
        logical :: bool

        bool = .false.

        ! Check for integer overflow
        if (testInteger .ge. (maxint -1) ) then
            bool = .true.
        else if (testInteger .le. (-maxint + 1)) then
            bool = .true.
        else
            bool = .false.
        end if
    end function chkoverflow

    ! This function reads the input string and returns a fraction type
    ! If the input string is in the form of "unit numerator/denominator"
    ! it will return the fraction type with the unit, numerator and denominator
    ! If the input string is in the form of "numerator/denominator" then the unit
    ! will be set to 0
    function returnfraction(input) result(fraction)
        character(len=*), intent(in) :: input
        type(fractiontype) :: fraction
        integer :: length, i, io_status, lastpos
        character(len=100) :: temp

        i = 0
        length = 0
        lastpos = 0

        ! Initialize the fraction type to 0
        fraction%unit = 0
        fraction%numerator = 0
        fraction%denominator = 0
        fraction%status = "OK"

        length = len_trim(input)
        do i=1, length

            if (input(i:i) == ' ') then
                ! This is the unit part
                temp = input(1:i)
                lastpos = i
                read(temp, *, iostat=io_status) fraction%unit
                
                if (.not. validatefraction(fraction, io_status)) then
                    return
                end if
                
            else if (input(i:i) == '/') then
                if (lastpos == 0) then
                    ! There is no unit part
                    temp = input(1:i-1)
                    read(temp, *, iostat=io_status) fraction%numerator
                    
                    if( .not. validatefraction(fraction, io_status)) then
                        return
                    end if
                    
                    lastpos = i
                else
                    ! There is a unit part
                    temp = input(lastpos+1:i-1)
                    lastpos = i
                    read(temp, *, iostat=io_status) fraction%numerator
                    
                    if (.not. validatefraction(fraction, io_status)) then
                        return
                    end if
                end if
            else if (i == length) then
                ! This is the denominator part
                temp = input(lastpos+1:length)
                read(temp, *, iostat=io_status) fraction%denominator
                
                if (.not. validatefraction(fraction, io_status)) then
                    return
                end if
                exit
            end if
        end do
    end function returnfraction

    ! Moved the logic to validate the fraction to a separate function
    ! Basically, if the io_status is not 0, then the fraction is invalid
    function validatefraction(fraction, io_status) result(good)
        type(fractiontype), intent(inout) :: fraction
        integer, intent(in) :: io_status
        logical :: good
        
        good = .true.

        if (io_status .ne. 0) then
            fraction%unit = 0
            fraction%numerator = 0
            fraction%denominator = 0
            fraction%status = 'Error: Invalid fraction'
            good = .false.
        else
            fraction%status = "OK"
            good = .true.
        end if        
    end function validatefraction

    function validateresult(io_status) result(goodbad)
        integer, intent(in) :: io_status
        logical :: goodbad

        goodbad = .true.

        if (io_status .ne. 0) then
            goodbad = .false.
        else
            goodbad = .true.
        end if

    end function validateresult

    ! This function converts an integer to a string
    function int2string(input) result(str)
        integer, intent(in) :: input
        character(len=32) :: str
        
        write(str, '(I0)') input
    end function int2string

end module fractionmodule