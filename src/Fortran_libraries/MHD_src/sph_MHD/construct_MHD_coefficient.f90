!>@file   construct_MHD_coefficient.f90
!!@brief  module construct_MHD_coefficient
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Subroutine to construct coeeficient from dimansionless numbers
!!
!!@verbatim
!!      subroutine construct_coefficient(coef, num_dimless, dimless,    &
!!     &     name_dimless, num_coef, coef_name, coef_power,             &
!!     &     r_low_t, r_high_t)
!!      subroutine set_implicit_4_inf_viscous(coef_field,               &
!!     &          coef_imp, coef_exp)
!!@endverbatim
!!
!!@n @param coef          Coefficient of term (output)
!!
!!@n @param num_dimless   Number of inout dimewnsionless numbers
!!@n @param dimless(num_dimless)
!!                        Value of the dimewnsionless numbers
!!@n @param name_dimless(num_dimless)
!!                        Name of dimensionless numbers
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
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine construct_coefficient(coef, num_dimless, dimless,      &
     &          name_dimless, num_coef, coef_name, coef_power,          &
     &          r_low_t, r_high_t)
!
      use calypso_mpi
!
      integer (kind = kint), intent(in) :: num_dimless
      character(len=kchara), intent(in) :: name_dimless(num_dimless)
      real(kind = kreal), intent(in) :: dimless(num_dimless)
!
      integer (kind = kint), intent(in) :: num_coef
      character(len=kchara), intent(in) :: coef_name(num_coef)
      real(kind = kreal), intent(in) :: coef_power(num_coef)
      real(kind = kreal), intent(in) :: r_low_t, r_high_t
!
      real(kind = kreal), intent(inout) :: coef
!
      integer(kind=kint) :: i, j, iflag
!
!
       do i = 1, num_coef
         if (coef_name(i).eq.'One') then
           coef = coef * one
         else if (coef_name(i).eq.'Two') then
           coef = coef * two**coef_power(i)
         else if (coef_name(i).eq.'Zero') then
           coef = coef * zero
         else if (coef_name(i).eq.'Radial_parameter') then
           coef = coef * (one - r_high_t/r_low_t)
         else if (coef_name(i).eq.'Radial_35') then
           coef = coef * (one - 0.35d0)
         else
           iflag = 0
           do j = 1, num_dimless
             if ( coef_name(i) .eq. name_dimless(j) ) then
               coef = coef * dimless(j)**coef_power(i)
               iflag = 1
             end if
           end do
           if (iflag .eq. 0) then
             write(*,*) 'there is missing dimensionless number'
             call calypso_MPI_abort(1000, 'normalization problem')
           end if
         end if
       end do
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
