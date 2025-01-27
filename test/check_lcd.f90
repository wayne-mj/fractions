module check_lcd
    use iso_fortran_env
    use fractions
    use VerySimpleTestFramework
    implicit none
    type(fractiontype)      :: fr, f1, f2
    integer                 :: i

    contains

    subroutine test_lcd()
        i = huge(1)


        call suite("Begin tests of Fraction functions")
        
        f1%numerator=1
        f1%denominator=2
        f2%numerator=1
        f2%denominator=3
        
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
    end subroutine
end module check_lcd