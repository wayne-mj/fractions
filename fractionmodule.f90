module fractionmodule
    implicit none

    !! Define a type to represent a fraction
    type :: fractiontype
        integer :: unit, numerator, denominator
        character(32) :: status
    end type fractiontype

    !integer, parameter :: long = selected_int_kind(18)
    integer, parameter :: maxint = huge(0)

    contains

    function addfraction(firstFraction, secondFraction) result(resultFraction)
        type(fractiontype), intent(in) :: firstFraction, secondFraction
        type(fractiontype) :: resultFraction
        integer(8) :: l_num, l_denom, l_first_num, l_second_num, l_first_denom, l_second_denom
        logical :: overflow = .false.

        ! Make sure we are not dividing by zero anywhere
        if (firstFraction%denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        else if (secondFraction%denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        ! Convert everything to long integers to handle integer overflow
        ! and perform the calculations
        if (firstFraction%denominator /= secondFraction%denominator) then
            l_first_num = firstFraction%numerator
            l_first_denom = firstFraction%denominator
            l_second_num = secondFraction%numerator
            l_second_denom = secondFraction%denominator
            l_num = (l_first_num * l_second_denom) + (l_second_num * l_first_denom)
            l_denom = l_first_denom * l_second_denom            
        else
            l_first_num = firstFraction%numerator
            l_second_num = secondFraction%numerator
            l_num = l_first_num + l_second_num
            l_denom = firstFraction%denominator
        end if

        !Test for Integer Overflow
        overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

        ! Simple debug to see what the values of the numerator and denominator are
        !print *, l_num
        !print *, l_denom

        ! If there is an overflow, set the result to 0
        ! Otherwise, set the result to the calculated values
        if (overflow) then
            resultFraction%numerator = 0
            resultFraction%denominator = 0
            resultFraction%unit = 0
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
        integer(8) :: l_num, l_denom, l_first_num, l_second_num, l_first_denom, l_second_denom
        logical :: overflow = .false.

        ! Make sure we are not dividing by zero anywhere
        if (firstFraction%denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        else if (secondFraction%denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        ! Convert everything to long integers to handle integer overflow
        ! and perform the calculations
        if (firstFraction%denominator /= secondFraction%denominator) then
            l_first_num = firstFraction%numerator
            l_first_denom = firstFraction%denominator
            l_second_num = secondFraction%numerator
            l_second_denom = secondFraction%denominator
            l_num = (l_first_num * l_second_denom) - (l_second_num * l_first_denom)
            l_denom = l_first_denom * l_second_denom            
        else
            l_first_num = firstFraction%numerator
            l_second_num = secondFraction%numerator
            l_num = l_first_num - l_second_num
            l_denom = firstFraction%denominator
        end if
        
        !Test for Integer Overflow
        overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

        ! Simple debug to see what the values of the numerator and denominator are
        !print *, l_num
        !print *, l_denom

        ! If there is an overflow, set the result to 0
        ! Otherwise, set the result to the calculated values
        if (overflow) then
            resultFraction%numerator = 0
            resultFraction%denominator = 0
            resultFraction%unit = 0
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
        integer(8) :: l_num, l_denom, l_first_num, l_second_num, l_first_denom, l_second_denom
        logical :: overflow = .false.

        ! Make sure we are not dividing by zero anywhere
        if (firstFraction%denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        else if (secondFraction%denominator == 0) then
            resultFraction%status = 'Error: Division by zero'
            return
        end if

        l_first_num = firstFraction%numerator
        l_first_denom = firstFraction%denominator
        l_second_num = secondFraction%numerator
        l_second_denom = secondFraction%denominator
        l_num = l_first_num * l_second_num
        l_denom = l_first_denom * l_second_denom

        !Test for Integer Overflow
        overflow = chkoverflow(l_num) .or. chkoverflow(l_denom)

        ! Simple debug to see what the values of the numerator and denominator are
        !print *, l_num
        !print *, l_denom

        ! If there is an overflow, set the result to 0
        ! Otherwise, set the result to the calculated values
        if (overflow) then
            resultFraction%numerator = 0
            resultFraction%denominator = 0
            resultFraction%unit = 0
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

    function chkoverflow (testInteger) result(bool)
        integer(8), intent(in) :: testInteger
        logical :: bool

        bool = .false.

        ! Check for integer overflow
        if (testInteger > maxint) then
            bool = .true.
        else if (testInteger < -maxint) then
            bool = .true.
        else
            bool = .false.
        end if
    end function chkoverflow

end module fractionmodule