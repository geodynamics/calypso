!>@file   cal_sph_FDM_viscosity_mat.f90
!!@brief  module cal_sph_FDM_viscosity_mat
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine set_sph_FDM_pressure_grad_mat                        &
!!     &         (n_in, n_out, jmax, r_nod, g_sph_rj,                   &
!!     &          coef_p, fdm_e2n_d1_mat, mat_grad_p)
!!      subroutine set_sph_FDM_fix_viscous_mat(n_in, n_out, jmax,       &
!!     &          a2_radius, g_sph_rj, fdm_d2_mat, mat_viscous)
!!        real(kind = kreal), intent(in) :: r_nod
!!        real(kind = kreal), intent(in) :: a2_radius
!!        real(kind = kreal), intent(in) :: fdm_e2n_d1_mat(n_in:n_out)
!!      subroutine add_sph_FDM_val_viscous_mat(n_in, n_out,             &
!!     &          flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          jmax, a1_radius, relative_d, h_nu, h_rho, h_drhodr,   &
!!     &          fdm_d1_mat, mat_viscous)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: n_in, n_out
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: a1_radius(nri)
!!        real(kind = kreal), intent(in) :: a2_radius(nri)
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
!!        real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
!!        real(kind = kreal), intent(in) :: fdm_d1_mat(n_in:n_out)
!!        real(kind = kreal), intent(in) :: fdm_d2_mat(n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: mat_grad_p(jmax,n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_viscous(jmax,n_in:n_out)
!!@endverbatim
!!
      module cal_sph_FDM_viscosity_mat
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_FDM_pressure_grad_mat                          &
     &         (n_in, n_out, jmax, r_nod, g_sph_rj,                     &
     &          coef_p, fdm_e2n_d1_mat, mat_grad_p)
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: r_nod
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: fdm_e2n_d1_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: mat_grad_p(jmax,n_in:n_out)
!
      integer(kind = kint) :: i_next
!
!
      do i_next = n_in, n_out
        mat_grad_p(1:jmax,i_next) = coef_p * g_sph_rj(1:jmax,13)        &
     &                     * r_nod * r_nod  * fdm_e2n_d1_mat(i_next)
      end do
!
      end subroutine set_sph_FDM_pressure_grad_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_FDM_fix_viscous_mat(n_in, n_out, jmax,         &
     &          a2_radius, g_sph_rj, fdm_d2_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a2_radius
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: fdm_d2_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(jmax,n_in:n_out)
!
      integer(kind = kint) :: i_next
!
!
      do i_next = n_in, n_out
        mat_viscous(1:jmax,i_next) = fdm_d2_mat(i_next)
      end do
      mat_viscous(1:jmax,0) = mat_viscous(1:jmax,0)                     &
     &                       - g_sph_rj(1:jmax,3) * a2_radius
!
      end subroutine set_sph_FDM_fix_viscous_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_sph_FDM_val_viscous_mat(n_in, n_out,               &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          jmax, a1_radius, relative_d, h_nu, h_rho, h_drhodr,     &
     &          fdm_d1_mat, mat_viscous)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1_radius
      real(kind = kreal), intent(in) :: relative_d, h_nu
      real(kind = kreal), intent(in) :: h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm_d1_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(jmax,n_in:n_out)
!
      real(kind = kreal) :: mat_tmp(n_in:n_out)
      real(kind = kreal) :: c_d1, c_d0
      integer(kind = kint) :: i_next
!
!
      if(flag_ref_density_valiation) then
        c_d0 = -(four / three)                                          &
     &        * (h_rho*a1_radius + h_drhodr)
        c_d1 = - h_rho / three
        mat_tmp(n_in:n_out) = c_d1 * fdm_d1_mat(n_in:n_out)
        mat_tmp(0) = mat_tmp(0) + c_d0
!
        if(flag_viscous_variation) then
          mat_tmp(0) = mat_tmp(0)                                       &
     &                - (four / three) * h_rho * h_nu
        end if
        do i_next = n_in, n_out
          mat_viscous(1:jmax,i_next) = mat_viscous(1:jmax,i_next)       &
     &                                +  mat_tmp(i_next)
        end do
      end if
!
      if(flag_viscous_variation) then
        c_d0 = - four * h_nu * a1_radius
        c_d1 =   two *  h_nu
        mat_tmp(n_in:n_out) = c_d1 * fdm_d1_mat(n_in:n_out)
        mat_tmp(0) =          mat_tmp(0) + c_d0
        do i_next = n_in, n_out
          mat_viscous(1:jmax,i_next) = mat_viscous(1:jmax,i_next)       &
     &                                +  mat_tmp(i_next)
        end do
!
        mat_viscous(1:jmax,n_in:n_out)                                  &
     &          = relative_d * mat_viscous(1:jmax,n_in:n_out)
      end if
!
      end subroutine add_sph_FDM_val_viscous_mat
!
!  -------------------------------------------------------------------
!
      end module cal_sph_FDM_viscosity_mat
