module check_subtraction
    use iso_fortran_env
    use fractions
    use VerySimpleTestFramework
    implicit none
    type(fractiontype)      :: fr, f1, f2
    integer                 :: i

    contains

    subroutine test_subtraction()
        i = huge(1)


        call suite("Begin tests of Fraction functions")
        
        f1%numerator=1
        f1%denominator=2
        f2%numerator=1
        f2%denominator=3
        
        ! **** Subtraction **** !

        fr=sub_fraction(f1, f2)
        call test("1/2 - 1/3 = 1/6")
        call assert_equals(fr%numerator, 1, 0)
        call test("1/2 - 1/3 = 1/6")
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
        
        call results()
    end subroutine
end module check_subtraction