program check
    use iso_fortran_env
    use fractions
    use check_addition
    use check_subtraction
    use check_multiplication
    use check_divsion
    use check_lcd
    use VerySimpleTestFramework
    implicit none
       
    ! **** Addition **** !
   
    call test_addition()
    ! fr = add_fraction(f1,f2)
    ! call test("1/2 + 1/3 = 5/6 Numerator = 5")
    ! call assert_equals(fr%numerator,5,0)
    ! call test("1/2 + 1/3 = 5/6 Denominator = 6")
    ! call assert_equals(fr%denominator,6,0)

    ! fr = add_fraction(1, 3, 1, 4)
    ! call test("1/3 + 1/4 = 7/12 Numerator = 7")
    ! call assert_equals(fr%numerator,7,0)
    ! call test("1/3 + 1/4 = 7/12 Denominator = 12")
    ! call assert_equals(fr%denominator,12,0)

    ! fr=add_fraction(1, i, 2, i)
    ! call test("1/MAXINT + 2/MAXINT = 3/MAXINT")
    ! call assert_equals(fr%numerator,3,0)
    ! call test("1/MAXINT + 2/MAXINT = 3/MAXINT")
    ! call assert_equals(fr%denominator,i,0)

    ! **** Subtraction **** !

    call test_subtraction()
    ! fr=sub_fraction(f1, f2)
    ! call test("1/2 - 1/3 = 1/6")
    ! call assert_equals(fr%numerator, 1, 0)
    ! call test("1/2 - 1/3 = 1/6")
    ! call assert_equals(fr%denominator, 6, 0)

    ! fr = sub_fraction(1, 3, 1, 4)
    ! call test("1/3 - 1/4 =1/12 Numerator = 1")
    ! call assert_equals(fr%numerator,1,0)
    ! call test("1/3 - 1/4 = 1/12 Denominator = 12")
    ! call assert_equals(fr%denominator,12,0)
    
    ! fr=sub_fraction(2, i, 1, i)
    ! call test("2/MAXINT - 1/MAXINT = 1/MAXINT")
    ! call assert_equals(fr%numerator,1,0)
    ! call test("2/MAXINT - 1/MAXINT = 3/MAXINT")
    ! call assert_equals(fr%denominator,i,0)

    ! **** Multiply Fraction **** !

    call test_multiplication()

    ! fr = multiply_fraction(f1,f2)
    ! call test("1/2 * 1/3 = 1/6 Numerator = 1")
    ! call assert_equals(fr%numerator,1,0)
    ! call test("1/2 * 1/3 = 1/6 Denominator = 6")
    ! call assert_equals(fr%denominator,6,0)

    ! fr = multiply_fraction(1, 3, 1, 4)
    ! call test("1/3 * 1/4 = 1/12 Numerator = 1")
    ! call assert_equals(fr%numerator,1,0)
    ! call test("1/3 * 1/4 = 1/12 Denominator = 12")
    ! call assert_equals(fr%denominator,12,0)

    ! fr=multiply_fraction(1, i, 2, i)
    ! call test("1/MAXINT * 2/MAXINT = 2/(MAXINT*MAXINT) AKA Integer Overflow")
    ! call assert_equals(fr%status,'Error: Integer Overflow')
    ! call test("1/MAXINT * 2/MAXINT = 2/(MAXINT*MAXINT) AKA Integer Overflow")
    ! call assert_equals(fr%status,'Error: Integer Overflow')

    ! **** Divide Fraction **** !

    call test_divsion()

    ! fr = divide_fraction(f1,f2)
    ! call test("1/2 / 1/3 = 3/2 Numerator = 3")
    ! call assert_equals(fr%numerator,3,0)
    ! call test("1/2 / 1/3 = 3/2 Denominator = 2")
    ! call assert_equals(fr%denominator,2,0)

    ! fr = divide_fraction(1, 3, 1, 4)
    ! call test("1/3 / 1/4 = 4/3 Numerator = 4")
    ! call assert_equals(fr%numerator,4,0)
    ! call test("1/3 / 1/4 = 4/3 Denominator = 3")
    ! call assert_equals(fr%denominator,3,0)

    ! fr=divide_fraction(2, i, 1, i)
    ! call test("2/MAXINT / 1/MAXINT = 2*MAXINT/MAXINT")
    ! call assert_equals(fr%status,'Error: Integer Overflow')
    ! call test("2/MAXINT / 1/MAXINT = 2*MAXINT/MAXINT")
    ! call assert_equals(fr%status,'Error: Integer Overflow')

    ! **** Lowest Common Denominator *** !

    call test_lcd()
    
    print *, ""
    call suite("Begin tests of Fraction functions")
    call test("Test of assert pass")

    call test("Test that the max denominator is what it is suppose to be 1 * 10^9")
    call assert_equals(maxdenom, 1*10**decimalplaces,0)
    call results()

! print *, "Put some tests in here!"
end program check