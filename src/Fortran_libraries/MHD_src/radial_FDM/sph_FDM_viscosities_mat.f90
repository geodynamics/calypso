!>@file   sph_FDM_viscosities_mat.f90
!!@brief  module sph_FDM_viscosities_mat
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine set_sph_FDM_viscosity_mat(n_in, n_out, jmax,         &
!!     &          flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          a1_radius, a2_radius, g_sph_rj, coef_d,               &
!!     &          relative_d, h_nu, h_rho, h_drhodr,                    &
!!     &          fdm_d1_mat, fdm_d2_mat, mat_viscous)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: n_in, n_out, jmax
!!        real(kind = kreal), intent(in) :: a1_radius, a2_radius
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: relative_d, h_nu
!!        real(kind = kreal), intent(in) :: h_rho, h_drhodr
!!        real(kind = kreal), intent(in) :: fdm_d1_mat(n_in:n_out)
!!        real(kind = kreal), intent(in) :: fdm_d2_mat(n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_viscous(jmax,n_in:n_out)
!!      subroutine set_sph_FDM_hdiv_viscosity_mat(n_in, n_out,          &
!!     &          flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          jmax, a1r_ele_rj, a2r_ele_rj, a3r_ele_rj,             &
!!     &          g_sph_rj, coef_d, relative_d, h_nu, h_rho, h_drhodr,  &
!!     &          fdm3e_d0_mat, fdm3e_d1_mat,                           &
!!     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: n_in, n_out, jmax
!!        real(kind = kreal), intent(in) :: a1r_ele_rj
!!        real(kind = kreal), intent(in) :: a2r_ele_rj
!!        real(kind = kreal), intent(in) :: a3r_ele_rj
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
!!        real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
!!        real(kind = kreal), intent(in) :: fdm3e_d0_mat(n_in:n_out)
!!        real(kind = kreal), intent(in) :: fdm3e_d1_mat(n_in:n_out)
!!        real(kind = kreal), intent(in) :: fdm3e_d2_mat(n_in:n_out)
!!        real(kind = kreal), intent(in) :: fdm3e_d3_mat(n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!!@endverbatim
!!
      module sph_FDM_viscosities_mat
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_physical_property
      use t_fdm_coefs
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_FDM_viscosity_mat(n_in, n_out, jmax,           &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          a1_radius, a2_radius, g_sph_rj, coef_d,                 &
     &          relative_d, h_nu, h_rho, h_drhodr,                      &
     &          fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      use cal_sph_FDM_viscosity_mat
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out, jmax
      real(kind = kreal), intent(in) :: a1_radius, a2_radius
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d, h_nu
      real(kind = kreal), intent(in) :: h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm_d1_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm_d2_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout) :: mat_viscous(jmax,n_in:n_out)
!
!
      call set_sph_FDM_fix_viscous_mat(n_in, n_out, jmax,               &
     &    a2_radius, g_sph_rj, fdm_d2_mat, mat_viscous)
!
      if(flag_viscous_variation .or. flag_ref_density_valiation) then
        call add_sph_FDM_val_viscous_mat(n_in, n_out,                   &
     &      flag_viscous_variation, flag_ref_density_valiation,         &
     &      jmax, a1_radius, relative_d, h_nu, h_rho, h_drhodr,         &
     &      fdm_d1_mat, mat_viscous)
      end if
!
      mat_viscous(1:jmax,n_in:n_out)                                    &
     &       = coef_d * mat_viscous(1:jmax,n_in:n_out)
!
      end subroutine set_sph_FDM_viscosity_mat
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_FDM_hdiv_viscosity_mat(n_in, n_out,            &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          jmax, a1r_ele_rj, a2r_ele_rj, a3r_ele_rj,               &
     &          g_sph_rj, coef_d, relative_d, h_nu, h_rho, h_drhodr,    &
     &          fdm3e_d0_mat, fdm3e_d1_mat,                             &
     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out, jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: a3r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
      real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(jmax,n_in:n_out)
!
!
      call set_sph_FDM_fix_hdiv_vscs_mat(n_in, n_out, jmax,             &
     &    a2r_ele_rj, a3r_ele_rj, g_sph_rj,                             &
     &    fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      if(flag_viscous_variation .or. flag_ref_density_valiation) then
        call add_sph_FDM_val_hdiv_vscs_mat(n_in, n_out,                 &
     &      flag_viscous_variation,  flag_ref_density_valiation,        &
     &      jmax, a1r_ele_rj, a2r_ele_rj, g_sph_rj,                     &
     &      relative_d, h_nu, h_rho, h_drhodr,                          &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
      end if
!
      hdiv_visous_mat(1:jmax,n_in:n_out)                                &
     &       = coef_d * hdiv_visous_mat(1:jmax,n_in:n_out)
!
      end subroutine set_sph_FDM_hdiv_viscosity_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_sph_FDM_val_hdiv_vscs_mat(n_in, n_out,             &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          jmax, a1r_ele_rj, a2r_ele_rj, g_sph_rj,                 &
     &          relative_d, h_nu, h_rho, h_drhodr,                      &
     &          fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,               &
     &          hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
      real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      real(kind = kreal) :: mat_tmp(n_in:n_out)
      real(kind = kreal) :: relative_de, h_nu_e
      real(kind = kreal) :: h_rho_e, h_drhodr_e
      integer(kind = kint) :: i_next
!
!
      relative_de = half * (relative_d(1) + relative_d(2))
      h_nu_e =      half * (h_nu(1) +     h_nu(2))
      h_rho_e =     half * (h_rho(1) +    h_rho(2))
      h_drhodr_e =  half * (h_drhodr(1) + h_drhodr(2))
!
      if(flag_ref_density_valiation) then
        call add_sph_hdiv_viscous_rho_depend(n_in, n_out, jmax,         &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho_e, h_drhodr_e,      &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
!
        if(flag_viscous_variation) then
          mat_tmp(n_in:n_out)                                           &
     &       = h_nu_e * h_rho_e * fdm3e_d1_mat(n_in:n_out)
          do i_next = n_in, n_out
            hdiv_visous_mat(1:jmax,i_next)                              &
     &        = hdiv_visous_mat(1:jmax,i_next) + mat_tmp(i_next)
          end do
        end if
      end if
      if(flag_viscous_variation) then
        call add_sph_hdiv_viscous_nu_depend(n_in, n_out, jmax,          &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_de, h_nu_e,      &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
      end if
!
      end subroutine add_sph_FDM_val_hdiv_vscs_mat
!
! -----------------------------------------------------------------------
!
      end module sph_FDM_viscosities_mat
