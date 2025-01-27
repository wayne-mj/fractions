module check_divsion
    use iso_fortran_env
    use fractions
    use VerySimpleTestFramework
    implicit none
    type(fractiontype)      :: fr, f1, f2
    integer                 :: i

    contains

    subroutine test_divsion()
        i = huge(1)


        call suite("Begin tests of Fraction functions")
        
        f1%numerator=1
        f1%denominator=2
        f2%numerator=1
        f2%denominator=3
        
        ! **** Divide Fraction **** !

        fr = divide_fraction(f1,f2)
        call test("1/2 / 1/3 = 3/2 Numerator = 3")
        call assert_equals(fr%numerator,3,0)
        call test("1/2 / 1/3 = 3/2 Denominator = 2")
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
        
        call results()
    end subroutine
end module check_divsion