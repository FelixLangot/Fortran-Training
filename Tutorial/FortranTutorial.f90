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

! program read_value
!     implicit none
!     integer :: age

!     print *, 'Please enter your age: '
!     read(*,*) age

!     print *, 'Your age is: ', age
! end program read_value

! program arithmetic
!     implicit none
!     real :: pi
!     real :: radius
!     real :: height
!     real :: area
!     real :: volume

!     pi = 3.1415627

!     print *, 'Enter cylinder base radius: '
!     read(*,*) radius

!     print *, 'Enter cylinder height: '
!     read(*,*) height

!     area = pi * radius**2.0
!     volume = area * height 

!     print *, 'Cylinder radius is: ', radius
!     print *, 'Cylinder height is: ', height
!     print *, 'Cylinder base area is: ', area
!     print *, 'Cylinder volume is: ', volume

! end program arithmetic

! program float
!   use, intrinsic :: iso_fortran_env, only: sp=>real32, dp=>real64
!   implicit none

!   real(sp) :: float32
!   real(dp) :: float64

!   float32 = 1.0_sp  ! Explicit suffix for literal constants
!   float64 = 1.0_dp

! end program float

! program array_slice
!   implicit none

!   integer :: i
!   integer :: array1(10)  ! 1D integer array of 10 elements
!   integer :: array2(10, 10)  ! 2D integer array of 100 elements

!   array1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  ! Array constructor
!   array1 = [(i, i = 1, 10)]  ! Implied do loop constructor
!   array1(:) = 0  ! Set all elements to zero
!   array1(1:5) = 1  ! Set first five elements to one
!   array1(6:) = 1  ! Set all elements after five to one

!   print *, array1(1:10:2)  ! Print out elements at odd indices
!   print *, array2(:,1)  ! Print out the first column in a 2D array
!   print *, array1(10:1:-1)  ! Print an array in reverse

! end program array_slice

! program loops_conditions
!     implicit none
!     integer :: i

!     do i = 1, 100
!         if (i>10) then
!             exit
!         end if
!         print *, i
!     end do
! end program loops_conditions

! program nested
!     implicit none
!     integer :: i, j

!     outer_loop: do i = 1, 10
!         print *, 'i = ',i
!         inner_loop: do j = 1, 10
!             if (j == 5) then
!                 ! cycle outer_loop
!                 print *, 'j = ', j
!             end if
!         end do inner_loop
!     end do outer_loop
! end program nested


! Print matrix A to screen

! subroutine print_matrix(n,m,A)
!   implicit none
!   integer, intent(in) :: n
!   integer, intent(in) :: m
!   real, intent(in) :: A(n, m)

!   integer :: i

!   do i = 1, n
!     print *, A(i, 1:m)
!   end do

! end subroutine print_matrix

! program call_sub
!   implicit none

!   real :: mat(10, 20)

!   mat(:,:) = 0.0

!   call print_matrix(10, 20, mat)

! end program call_sub

module my_mod
  implicit none

  private  ! All entities are now module-private by default
  public public_var, print_matrix  ! Explicitly export public entities

  real, parameter :: public_var = 2
  integer :: private_var

contains
    
  ! Print matrix A to screen
  subroutine print_matrix(A)
    real, intent(in) :: A(:,:)  ! An assumed-shape dummy argument

    integer :: i

    do i = 1, size(A,1)
      print *, A(i,:)
    end do

  end subroutine print_matrix

end module my_mod

program use_mod
  use my_mod
  implicit none

  real :: mat(10, 10)

  mat(:,:) = public_var

  call print_matrix(mat)

end program use_mod