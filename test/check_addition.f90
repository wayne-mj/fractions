module check_addition
    use iso_fortran_env
    use fractions
    use VerySimpleTestFramework
    implicit none
    type(fractiontype)      :: fr, f1, f2
    integer                 :: i

    contains

    subroutine test_addition()
        i = huge(1)


        call suite("Begin tests of Fraction functions")
        
        f1%numerator=1
        f1%denominator=2
        f2%numerator=1
        f2%denominator=3
        
        ! **** Addition **** !
        
        fr = add_fraction(f1,f2)
        call test("1/2 + 1/3 = 5/6 Numerator = 5")
        call assert_equals(fr%numerator,5,0)
        call test("1/2 + 1/3 = 5/6 Denominator = 6")
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
        
        call results()
    end subroutine
end module check_addition