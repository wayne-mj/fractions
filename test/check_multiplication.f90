module check_multiplication
    use iso_fortran_env
    use fractions
    use VerySimpleTestFramework
    implicit none
    type(fractiontype)      :: fr, f1, f2
    integer                 :: i

    contains

    subroutine test_multiplication()
        i = huge(1)


        call suite("Begin tests of Fraction functions")
        
        f1%numerator=1
        f1%denominator=2
        f2%numerator=1
        f2%denominator=3
        
        ! **** Multiply Fraction **** !

        fr = multiply_fraction(f1,f2)
        call test("1/2 * 1/3 = 1/6 Numerator = 1")
        call assert_equals(fr%numerator,1,0)
        call test("1/2 * 1/3 = 1/6 Denominator = 6")
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
        
        call results()
    end subroutine
end module check_multiplication