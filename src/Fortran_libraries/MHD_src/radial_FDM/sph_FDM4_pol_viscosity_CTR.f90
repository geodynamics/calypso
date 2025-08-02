!>@file   sph_FDM4_pol_viscosity_CTR.f90
!!@brief  module sph_FDM4_pol_viscosity_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine sph_exp_FDM4_vpol_viscosity_CTR(sph_rj, g_sph_rj,    &
!!     &         flag_viscous_variation, flag_ref_density_valiation,    &
!!     &         coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,     &
!!     &         fdm_e3, fdm4_pol_CTR, fdm3e_center, fdm3e_center1,     &
!!     &         d_vpol, press_e, mat4_viscous_CTR, hdiv_visous_mat_CTR,&
!!     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        real(kind=kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!!        type(fdm_matrices), intent(in) :: fdm_e3
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center1
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_CTR(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!!
!!      subroutine sph_FDM4_vpol_viscosity_mat_CTR(sph_rj, g_sph_rj,    &
!!     &          flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,    &
!!     &          fdm_3e, fdm_e3, fdm4_pol_CTR, fdm3e_center,           &
!!     &          mat4_grad_p_CTR, mat4_viscous_CMB1,                   &
!!     &          hdiv_visous_mat_CTR, mat9)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        real(kind=kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!!        type(fdm_matrices), intent(in) :: fdm_3e
!!        type(fdm_matrices), intent(in) :: fdm_e3
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center1
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_grad_p_CTR(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!      real(kind = kreal), intent(inout)                               &
!!     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!@endverbatim
!
      module sph_FDM4_pol_viscosity_CTR
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
      subroutine sph_exp_FDM4_vpol_viscosity_CTR(sph_rj, g_sph_rj,      &
     &         flag_viscous_variation, flag_ref_density_valiation,      &
     &         coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,       &
     &         fdm_e3, fdm4_pol_CTR, fdm3e_center, fdm3e_center1,       &
     &         d_vpol, press_e, mat4_viscous_CTR, hdiv_visous_mat_CTR,  &
     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_coef_fdm4_vpol_centre
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use sum_sph_pol_vscs_FDM4_exp
      use sum_sph_pol_grad_p_FDM4_exp
      use sum_sph_hdiv_vscs_FDM_exp
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!
      type(fdm_matrices), intent(in) :: fdm_e3
      type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center1
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_CTR(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat3_grad_p_CTR(sph_rj%nidx_rj(2),-1:2)
!
      integer(kind = kint) :: kr
!
!
      kr = 1
      call set_sph_FDM_hdiv_viscosity_mat(izero, ione,                  &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                    &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    coef_d, relative_d(kr), h_nu(kr), h_rho(kr), h_drhodr(kr),    &
     &    fdm3e_center%dmat_vp0(-2,1),                                  &
     &    fdm3e_center%dmat_vp0(-2,2),                                  &
     &    fdm3e_center%dmat_vp0(-2,3),                                  &
     &    fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call sum_exp_sph_hdiv_viscous_CTR                                 &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    hdiv_visous_mat_CTR(1,0), hdiv_viscous_e)
!
      kr = 2
      call set_sph_FDM_hdiv_viscosity_mat(-itwo, ione,                  &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                    &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    coef_d, relative_d(kr), h_nu(kr), h_rho(kr), h_drhodr(kr),    &
     &    fdm3e_center1%dmat_vp0(-2,1),                                 &
     &    fdm3e_center1%dmat_vp0(-2,2),                                 &
     &    fdm3e_center1%dmat_vp0(-2,3),                                 &
     &    fdm3e_center1%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call sum_exp_sph_hdiv_viscous_ICB                                 &
     &   (itwo, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), d_vpol,           &
     &    hdiv_visous_mat_CTR(1,-2), hdiv_viscous_e)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (izero, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(1), g_sph_rj, coef_p,                   &
     &    fdm_e3%dmat(0,1,1), mat3_grad_p_CTR)
      call sum_exp4_sph_pol_grad_p_CTR1                                 &
     &   (ione, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &    press_e, mat3_grad_p_CTR(1,0), d_grad_p)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (-ione, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(2), g_sph_rj, coef_p,                   &
     &    fdm_e3%dmat(-1,2,1), mat3_grad_p_CTR)
      call sum_exp4_sph_pol_grad_p                                      &
     &   (itwo, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &    press_e, mat3_grad_p_CTR(1,-1), d_grad_p)
!
      kr = 1
      call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),    &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),                 &
     &    g_sph_rj, coef_d, relative_d(kr+1), h_nu(kr+1),               &
     &    h_rho(kr+1), h_drhodr(kr+1),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,2),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,3), mat4_viscous_CTR)
      call sum_exp4_sph_viscous_CTR1                                    &
     &   (ione, sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,              &
     &    mat4_viscous_CTR(1,0), d_viscous_p)
!
      kr = 2
      call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),    &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),                 &
     &    g_sph_rj, coef_d, relative_d(kr+1), h_nu(kr+1),               &
     &    h_rho(kr+1), h_drhodr(kr+1),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,2),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,3), mat4_viscous_CTR)
      call sum_exp4_sph_viscous_CTR2                                    &
     &   (itwo, sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,              &
     &    mat4_viscous_CTR(1,-1), d_viscous_p)
!
      end subroutine sph_exp_FDM4_vpol_viscosity_CTR
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sph_FDM4_vpol_viscosity_mat_CTR(sph_rj, g_sph_rj,      &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,      &
     &          fdm_3e, fdm_e3, fdm4_pol_CTR, fdm3e_center,             &
     &          mat4_grad_p_CTR, mat4_viscous_CMB1,                     &
     &          hdiv_visous_mat_CTR, mat9)
!
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_coef_fdm4_vpol_centre
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use set_sph_pol_vscs_FDM4_mat
      use set_sph_hdiv_vscs_FDM_mat9
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!
      type(fdm_matrices), intent(in) :: fdm_3e
      type(fdm_matrices), intent(in) :: fdm_e3
      type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_grad_p_CTR(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
!
      integer(kind = kint) :: kr
!
!
      kr = 1
      call set_sph_FDM_hdiv_viscosity_mat(izero, ione,                  &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                    &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    coef_d, relative_d(kr), h_nu(kr), h_rho(kr), h_drhodr(kr),    &
     &    fdm3e_center%dmat_vp0(-2,1),                                  &
     &    fdm3e_center%dmat_vp0(-2,2),                                  &
     &    fdm3e_center%dmat_vp0(-2,3),                                  &
     &    fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call add_sph_ele_pressure_FDM_mat9                                &
     &   (ione, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p, mat9)
      call sub_sph_hdiv_viscous_mat9_CTR                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_CTR(1,0), mat9)
!
      kr = 1
      call set_sph_FDM_pressure_grad_mat                                &
     &   (izero, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(1), g_sph_rj, coef_p,                   &
     &    fdm_e3%dmat(0,1,1), mat4_grad_p_CTR(1,0))
      call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),    &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),                 &
     &    g_sph_rj, coef_d, relative_d(kr+1), h_nu(kr+1),               &
     &    h_rho(kr+1), h_drhodr(kr+1),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,2),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,3), mat4_viscous_CMB1)
      call sub_sph_pol_viscous_mat9_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat4_grad_p_CTR(1,0), mat4_viscous_CMB1(1,0), mat9)
!
!
      kr = 2
      call add_sph_ele_pressure_FDM_mat9                                &
     &   (itwo, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p, mat9)
      call set_sph_FDM_hdiv_viscosity_mat(-ione, ione,                  &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                    &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    coef_d, relative_d(kr), h_nu(kr), h_rho(kr), h_drhodr(kr),    &
     &    fdm_3e%dmat(-itwo,kr,0), fdm_3e%dmat(-itwo,kr,1),             &
     &    fdm_3e%dmat(-itwo,kr,2), fdm_3e%dmat(-itwo,kr,3),             &
     &    hdiv_visous_mat_CTR(1,-1))
      call sub_sph_hdiv_viscous_mat9_CTR1                               &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_CTR(1,-1), mat9)
!
!
      kr = 2
      call set_sph_FDM_pressure_grad_mat                                &
     &   (-ione, fdm_e3%n_plus, sph_rj%nidx_rj(2),                      &
     &    sph_rj%radius_1d_rj_r(kr), g_sph_rj, coef_p,                  &
     &    fdm_e3%dmat(-1,kr,1), mat4_grad_p_CTR(1,-1))
      call set_sph_FDM_viscosity_mat(-ione, itwo, sph_rj%nidx_rj(2),    &
     &    flag_viscous_variation, flag_ref_density_valiation,           &
     &    sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2),                 &
     &    g_sph_rj, coef_d, relative_d(kr+1), h_nu(kr+1),               &
     &    h_rho(kr+1), h_drhodr(kr+1),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,2),                                  &
     &    fdm4_pol_CTR%dmat_vp1(-1,3), mat4_viscous_CMB1)
      call sub_sph_pol_viscous_mat9_CTR2                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat4_grad_p_CTR(1,-1), mat4_viscous_CMB1(1,-1), mat9)
!
      end subroutine sph_FDM4_vpol_viscosity_mat_CTR
!
!  -------------------------------------------------------------------
!
      end module sph_FDM4_pol_viscosity_CTR

