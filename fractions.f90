program fractions
    use fractionmodule
    implicit none

    type :: localfractions
        integer :: root_num1, root_num2, root_den1, root_den2
    end type localfractions

    type :: testing
        type(fractiontype) :: int_f1, int_f2
    end type testing

    type(fractiontype) :: f1, f3
    !type(localfractions) :: mychoices
    type(testing) :: mychoices2
    integer :: num1, den1
    integer :: choice = 0, io_status
    character(len=100) :: input_line
    logical :: valid = .true.
    character(1), parameter :: newline = achar(10)

    do while (valid)
        call menu()
        read(*, '(A)') input_line

        read(input_line, '(I10)', iostat=io_status) choice
        if (io_status ==0) then
            if (choice == 0) then
                valid = .false.
                print *, newline // "Goodbye!" // newline
            
            else if (choice == 1) then
                print *, newline // "Add Fractions"
                mychoices2 = readoptions()
                f3 = addfraction(mychoices2%int_f1, mychoices2%int_f2)
                print *, newline // "The result is: ", trim(int2str(f3%unit)), " " , trim(int2str(f3%numerator)), "/", trim(int2str(f3%denominator)), " ", f3%status
            
            else if (choice == 2) then
                print *, newline // "Subtract Fractions"
                mychoices2 = readoptions()
                f3 = subfraction(mychoices2%int_f1, mychoices2%int_f2)
                print *, newline // "The result is: ", trim(int2str(f3%unit)), " " , trim(int2str(f3%numerator)), "/", trim(int2str(f3%denominator)), " " ,f3%status
            
            else if (choice == 3) then
                print *, newline //"Multiply Fractions"
                mychoices2 = readoptions()
                f3 = multiplyfraction(mychoices2%int_f1, mychoices2%int_f2)
                print *, newline // "The result is: ", trim(int2str(f3%unit)), " " , trim(int2str(f3%numerator)), "/", trim(int2str(f3%denominator)), " ", f3%status
            
            else if (choice == 4) then
                print *, newline // "Divide Fractions"
                mychoices2 = readoptions()
                f3 = dividefraction(mychoices2%int_f1, mychoices2%int_f2)
                print *, newline // "The result is: ", trim(int2str(f3%unit)), " " , trim(int2str(f3%numerator)), "/", trim(int2str(f3%denominator)), " ", f3%status
            
            else if (choice == 5) then
                print *, newline // "Mixed Fractions"
                write (*, '(A)', advance='no') "Enter the fraction numerator: "
                read (*, *) num1
                write (*, '(A)', advance='no') "Enter the fraction denominator: "
                read (*, *) den1
                f1 = mixedFraction(num1, den1)
                print *, newline // "The mixed fraction is: ", trim(int2str(f1%unit)), " ", trim(int2str(f1%numerator)), "/", trim(int2str(f1%denominator))
            
            else if (choice == 6) then
                print *, newline // "Simplify Fractions"
                write (*, '(A)', advance='no') "Enter the fraction numerator: "
                read (*, *) num1
                write (*, '(A)', advance='no') "Enter the fraction denominator: "
                read (*, *) den1
                f1 = lcd(num1, den1)
                print *, newline // "The simplifed fraction is: ", trim(int2str(f1%unit)), " ", trim(int2str(f1%numerator)), "/", trim(int2str(f1%denominator))
            
            else
                print *, "Invalid choice"
            end if
        end if
    end do

    contains

    subroutine menu()
        character(132) :: menutext
        character(128) :: prompt, banner

        banner = newline // & 
                 "********************************" // newline // &
                 "****  Fraction Calculator   ****" // newline // &
                 "********************************"
        menutext = newline // & 
                   "1. Add Fractions" // newline // &
                   "2. Subtract Fractions" // newline // &
                   "3. Multiply Fractions" // newline // &
                   "4. Divide Fractions" // newline // &
                   "5. Mixed Fractions" // newline // &
                   "6. Simplify Fractions" // newline // &
                   "0. Exit"
        prompt = newline // "Enter your choice: "

        write (*, '(A)') trim(adjustl(banner))
        print *, trim(adjustl(menutext))
        write (*, '(A)', advance='no') trim(adjustl(prompt)) // " "
    end subroutine menu   

    function readoptions() result(choices2)    
        type(testing) :: choices2
        write (*, '(A)', advance='no') "Enter the first fraction numerator: "
        read (*, *) choices2%int_f1%numerator
        write (*, '(A)', advance='no') "Enter the first fraction denominator: "
        read (*, *) choices2%int_f1%denominator
        write (*, '(A)', advance='no') "Enter the second fraction numerator: "
        read (*, *) choices2%int_f2%numerator
        write (*, '(A)', advance='no') "Enter the second fraction denominator: "
        read (*, *) choices2%int_f2%denominator
    end function readoptions
    
    function int2str(i) result(str)
        integer, intent(in) :: i
        character(len=10) :: str
        write (str, '(I10)') i
        str = adjustl(str)
        str = trim(str)
    end function int2str
end program fractions