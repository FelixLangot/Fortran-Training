! program hello
!     ! A comment
!     print *, 'Hello World!'
! end program hello

! program variables
!     implicit none
!     integer :: amount
!     real :: pi
!     complex :: frequency
!     character :: initial
!     logical :: isOkay

!     amount = 10
!     pi = 3.1415927
!     frequency = (1.0, -0.5)
!     initial = 'A'
!     isOkay = .false.

!     print *, 'The value of amount (integer) is: ', amount
!     print *, 'The value of pi (real) is: ', pi
!     print *, 'The value of frequency (complex) is : ', frequency
!     print *, 'The value of inital (character) is: ', initial
!     print *, 'The value of isOkay (logical) is; ', isOkay
! end program variables

program read_value
    implicit none
    integer :: age

    print *, 'Please enter your age: '
    read(*,*) age

    print *, 'Your age is: ', age
end program read_value