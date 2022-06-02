!>@file   t_powers_4_coefficients.f90
!!@brief  module t_powers_4_coefficients
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!>@brief  dimensionless number list for each term
!!
!!@verbatim
!!      subroutine alloc_coef_power_list(coef_list)
!!      subroutine dealloc_coef_power_list(coef_list)
!!      subroutine copy_power_and_names_from_ctl(coef_ctl, coef_list)
!!      subroutine check_power_and_name_list(coef_list)
!!        type(ctl_array_cr), intent(in) :: coef_ctl
!!        type(powers_4_coefficients), intent(inout) :: coef_list
!!
!!      real(kind = kreal) function coefficient_from_dimless            &
!!     &                          (dless_list, power_list)
!!        type(list_of_dimless), intent(in) :: dless_list
!!        type(powers_4_coefficients), intent(in) :: power_list
!!@endverbatim
!!
      module t_powers_4_coefficients
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!>      Structure of powers for coefficients
      type powers_4_coefficients
!>        Number of parameters
        integer(kind=kint) :: num
!>        List of parameter name
        character(len=kchara), allocatable :: name(:)
!>        List of power
        real (kind = kreal), allocatable :: power(:)
      end type powers_4_coefficients
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_coef_power_list(coef_list)
!
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      allocate(coef_list%name(coef_list%num))
      allocate(coef_list%power(coef_list%num))
      if(coef_list%num .gt. 0) coef_list%power = 0.0d0
!
      end subroutine alloc_coef_power_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_coef_power_list(coef_list)
!
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      deallocate(coef_list%name, coef_list%power)
!
      end subroutine dealloc_coef_power_list
!
! -----------------------------------------------------------------------
!
      subroutine copy_power_and_names_from_ctl(coef_ctl, coef_list)
!
      use t_control_array_charareal
!
      type(ctl_array_cr), intent(in) :: coef_ctl
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      call alloc_coef_power_list(coef_list)
      if (coef_list%num .le. 0) return
!
      coef_list%name(1:coef_list%num) = coef_ctl%c_tbl(1:coef_list%num)
      coef_list%power(1:coef_list%num) = coef_ctl%vect(1:coef_list%num)
!
      end subroutine copy_power_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      subroutine check_power_and_name_list(coef_list)
!
      type(powers_4_coefficients), intent(in) :: coef_list
      integer(kind = kint) :: i
!
      write(*,*) '# of coefficients: ', coef_list%num
!
      do i = 1, coef_list%num
        write(*,*) i, trim(coef_list%name(i)), '  ', coef_list%power(i)
      end do
!
      end subroutine check_power_and_name_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      real(kind = kreal) function coefficient_from_dimless              &
     &                          (dless_list, power_list)
!
      use m_error_IDs
      use t_list_of_dimless_numbers
      use skip_comment_f
!
      type(list_of_dimless), intent(in) :: dless_list
      type(powers_4_coefficients), intent(in) :: power_list
!
      real(kind = kreal) :: coef
      integer(kind=kint) :: i, j, iflag
!
!
      coef = one
      do i = 1, power_list%num
        if     (cmp_no_case(power_list%name(i), 'One'))  then
          coef = coef * one
        else if(cmp_no_case(power_list%name(i), 'Two') ) then
          coef = coef * two**power_list%power(i)
        else if(cmp_no_case(power_list%name(i), 'Zero')) then
          coef = coef * zero
        else
          iflag = 0
          do j = 1, dless_list%num
            if ( power_list%name(i) .eq. dless_list%name(j) ) then
              coef = coef * dless_list%value(j)**power_list%power(i)
              iflag = 1
            end if
          end do
          if (iflag .eq. 0) then
            write(e_message,*) 'there is missing dimensionless number'
          end if
        end if
      end do
      coefficient_from_dimless = coef
!
      end function coefficient_from_dimless
!
! -----------------------------------------------------------------------
!
      end module t_powers_4_coefficients
