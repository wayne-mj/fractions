program check
    use iso_fortran_env
    use fractions
    use VerySimpleTestFramework
    implicit none
    type(fractiontype)      :: fr, f1, f2
    integer                 :: i

    i = huge(1)


    call suite("Begin tests of Fraction functions")
    call test("Test of assert pass")

    call test("Test that the max denominator is what it is suppose to be 1 * 10^9")
    call assert_equals(maxdenom, 1*10**decimalplaces,0)

    f1%numerator=1
    f1%denominator=2
    f2%numerator=1
    f2%denominator=3
    
    ! **** Addition **** !
    
    fr = add_fraction(f1,f2)
    call test("Fraction Model: 1/2 + 1/3 = 5/6 Numerator = 5")
    call assert_equals(fr%numerator,5,0)
    call test("Fraction Model: 1/2 + 1/3 = 5/6 Denominator = 6")
    call assert_equals(fr%denominator,6,0)

    fr = add_fraction(1, 3, 1, 4)
    call test("1/3 + 1/4 = 7/12 Numerator = 7")
    call assert_equals(fr%numerator,7,0)
    call test("1/3 + 1/4 = 7/12 Denominator = 12")
    call assert_equals(fr%denominator,12,0)

    fr=add_fraction(1, i, 2, i)
    call test("1/MAXINT + 2/MAXINT = 3/MAXINT")
    call assert_equals(fr%numerator,3,0)
    call test("1/MAXINT + 2/MAXINT = 3/MAXINT")
    call assert_equals(fr%denominator,i,0)

    ! **** Subtraction **** !

    fr=sub_fraction(f1, f2)
    call test("Fraction Model: 1/2 - 1/3 = 1/6")
    call assert_equals(fr%numerator, 1, 0)
    call test("Fraction Model: 1/2 - 1/3 = 1/6")
    call assert_equals(fr%denominator, 6, 0)

    fr = sub_fraction(1, 3, 1, 4)
    call test("1/3 - 1/4 =1/12 Numerator = 1")
    call assert_equals(fr%numerator,1,0)
    call test("1/3 - 1/4 = 1/12 Denominator = 12")
    call assert_equals(fr%denominator,12,0)
    
    fr=sub_fraction(2, i, 1, i)
    call test("2/MAXINT - 1/MAXINT = 1/MAXINT")
    call assert_equals(fr%numerator,1,0)
    call test("2/MAXINT - 1/MAXINT = 3/MAXINT")
    call assert_equals(fr%denominator,i,0)

    ! **** Multiply Fraction **** !

    fr = multiply_fraction(f1,f2)
    call test("Fraction Model: 1/2 * 1/3 = 1/6 Numerator = 1")
    call assert_equals(fr%numerator,1,0)
    call test("Fraction Model: 1/2 * 1/3 = 1/6 Denominator = 6")
    call assert_equals(fr%denominator,6,0)

    fr = multiply_fraction(1, 3, 1, 4)
    call test("1/3 * 1/4 = 1/12 Numerator = 1")
    call assert_equals(fr%numerator,1,0)
    call test("1/3 * 1/4 = 1/12 Denominator = 12")
    call assert_equals(fr%denominator,12,0)

    fr=multiply_fraction(1, i, 2, i)
    call test("1/MAXINT * 2/MAXINT = 2/(MAXINT*MAXINT) AKA Integer Overflow")
    call assert_equals(fr%status,'Error: Integer Overflow')
    call test("1/MAXINT * 2/MAXINT = 2/(MAXINT*MAXINT) AKA Integer Overflow")
    call assert_equals(fr%status,'Error: Integer Overflow')

     ! **** Divide Fraction **** !

    fr = divide_fraction(f1,f2)
    call test("Fraction Model: 1/2 / 1/3 = 3/2 Numerator = 3")
    call assert_equals(fr%numerator,3,0)
    call test("Fraction Model: 1/2 / 1/3 = 3/2 Denominator = 2")
    call assert_equals(fr%denominator,2,0)

    fr = divide_fraction(1, 3, 1, 4)
    call test("1/3 / 1/4 = 4/3 Numerator = 4")
    call assert_equals(fr%numerator,4,0)
    call test("1/3 / 1/4 = 4/3 Denominator = 3")
    call assert_equals(fr%denominator,3,0)

    fr=divide_fraction(2, i, 1, i)
    call test("2/MAXINT / 1/MAXINT = 2*MAXINT/MAXINT")
    call assert_equals(fr%status,'Error: Integer Overflow')
    call test("2/MAXINT / 1/MAXINT = 2*MAXINT/MAXINT")
    call assert_equals(fr%status,'Error: Integer Overflow')

    ! **** Lowest Common Denominator *** !
    
    fr = lowest_common_denom(8, 16)
    call test("LCD: 8/16 = 1/2")
    call assert_equals(fr%numerator, 1, 0)
    call test("LCD: 8/16 = 1/2")
    call assert_equals(fr%denominator, 2, 0)

    fr = lowest_common_denom(9, 27)
    call test("LCD: 9/27 = 1/3")
    call assert_equals(fr%numerator, 1, 0)
    call test("LCD: 9/27 = 1/3")
    call assert_equals(fr%denominator, 3, 0)

    fr = lowest_common_denom(3, 2)
    call test("LCD: 3/2 = 1 1/2")
    call assert_equals(fr%unit, 1, 0)
    call test("LCD: 3/2 = 1 1/2")
    call assert_equals(fr%numerator, 1, 0)
    call test("LCD: 3/2 = 1 1/2")
    call assert_equals(fr%denominator, 2, 0)

    fr = approx_fraction(0.5_real64)
    call test("Approx Fraction: 0.5 = 1/2")
    call assert_equals(fr%numerator, 1, 0)
    call test("Approx Fraction: 0.5 = 1/2")
    call assert_equals(fr%denominator, 2, 0)
    
    fr = approx_fraction(0.75_real64)
    call test("Approx Fraction: 0.75 = 3/4")
    call assert_equals(fr%numerator, 3, 0)
    call test("Approx Fraction: 0.75 = 3/4")
    call assert_equals(fr%denominator, 4, 0)
    
    fr = approx_fraction(0.33333333333_real64)
    call test("Approx Fraction: 0.33333333333 = 33333333/1000000000")
    call assert_equals(fr%numerator, 333333333, 0)
    call test("Approx Fraction: 0.33333333333 = 33333333/1000000000")
    call assert_equals(fr%denominator, maxdenom, 0)

    fr = approx_fraction(0.16666667_real64)
    call test("Approx Fraction: 0.16666667 = 16666667/100000000")
    call assert_equals(fr%numerator, 16666667, 0)
    call test("Approx Fraction: 0.16666667 = 16666667/100000000")
    call assert_equals(fr%denominator, 100000000, 0)

    fr = approx_fraction(0.125_real64)
    call test("Approx Fraction: 0.125 = 1/8")
    call assert_equals(fr%numerator, 1, 0)
    call test("Approx Fraction: 0.125 = 1/8")
    call assert_equals(fr%denominator, 8, 0)
    
    call results()

! print *, "Put some tests in here!"
end program check