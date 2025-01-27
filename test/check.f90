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
    
    ! **** Subtraction **** !

    call test_subtraction()
    
    ! **** Multiply Fraction **** !

    call test_multiplication()

    ! **** Divide Fraction **** !

    call test_divsion()

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