!>@file   construct_MHD_coefficient.f90
!!@brief  module construct_MHD_coefficient
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Subroutine to construct coeeficient from dimansionless numbers
!!
!!@verbatim
!!      subroutine construct_coefficient                                &
!!     &         (coef, dless_list, power_list, r_low_t, r_high_t)
!!        type(list_of_dimless), intent(in) :: dless_list
!!        type(powers_4_coefficients), intent(inout) :: power_list
!!      subroutine set_implicit_4_inf_viscous(coef_field,               &
!!     &          coef_imp, coef_exp)
!!@endverbatim
!!
!!@n @param coef          Coefficient of term (output)
!!
!!@n @param dimless_list  List of dimensionless numbers
!!
!!@n @param num_coef      Number of dimensionless number to construct
!!@n @param coef_name(num_coef)
!!                        Name of dimensionless number to construct
!!@n @param coef_power(num_coef)
!!                        Power of dimensionless number to construct
!!
!!@n @param r_low_t       Radius at low reference tomperature
!!@n @param r_high_t      Radius at high reference tomperature
!!
!!@n @param coef_field    Coefficient for evolution of field
!!@n @param coef_imp      Contribution of implicit term
!!@n @param coef_exp      Contribution of explicit term
!!
!
      module construct_MHD_coefficient
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine construct_coefficient                                  &
     &         (coef, dless_list, power_list, r_low_t, r_high_t)
!
      use calypso_mpi
      use m_error_IDs
      use t_normalize_parameter
      use skip_comment_f
!
      type(list_of_dimless), intent(in) :: dless_list
      type(powers_4_coefficients), intent(inout) :: power_list
      real(kind = kreal), intent(in) :: r_low_t, r_high_t
!
      real(kind = kreal), intent(inout) :: coef
!
      integer(kind=kint) :: i, j, iflag
!
!
      do i = 1, power_list%num
        if     (cmp_no_case(power_list%name(i), 'One'))  then
          coef = coef * one
        else if(cmp_no_case(power_list%name(i), 'Two') ) then
          coef = coef * two**power_list%power(i)
        else if(cmp_no_case(power_list%name(i), 'Zero')) then
          coef = coef * zero
        else if(cmp_no_case(power_list%name(i), 'Radial_parameter'))    &
     &       then
          coef = coef * (one - r_high_t/r_low_t)
        else if(cmp_no_case(power_list%name(i), 'Radial_35')) then
          coef = coef * (one - 0.35d0)
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
            call calypso_MPI_abort(ierr_dless, e_message)
          end if
        end if
      end do
!
      call dealloc_coef_power_list(power_list)
!
      end subroutine construct_coefficient
!
! -----------------------------------------------------------------------
!
      subroutine set_implicit_4_inf_viscous(coef_field,                 &
     &          coef_imp, coef_exp)
!
      real(kind = kreal), intent(in) ::   coef_field
      real(kind = kreal), intent(inout) :: coef_imp, coef_exp
!
!
        if (coef_field .eq. zero) then
          coef_imp = one
          coef_exp = zero
        end if
!
       end subroutine set_implicit_4_inf_viscous
!
! -----------------------------------------------------------------------
!
      end module construct_MHD_coefficient
