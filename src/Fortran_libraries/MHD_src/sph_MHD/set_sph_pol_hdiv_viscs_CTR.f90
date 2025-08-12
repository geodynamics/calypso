!>@file   set_sph_pol_hdiv_viscs_CTR.f90
!!@brief  module set_sph_pol_hdiv_viscs_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine sph_exp_FDM2_vpol_viscosity_CTR                      &
!!     &        (sph_rj, fl_prop, radial_variation,                     &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e1,                      &
!!     &         fdm2_center, fdm3e_center, d_vpol, press_e,            &
!!     &         mat2_viscous_CTR, hdiv_visous_mat_CTR,                 &
!!     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e1(0:1)
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat2_viscous_CTR(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: hdiv_viscous_e(sph_rj%nnod_rj)
!!      subroutine sph_exp_FDM4_vpol_viscosity_CTR                      &
!!     &        (sph_rj, fl_prop, radial_variation,                     &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e3,                      &
!!     &         fdm4_pol_CTR, fdm3e_center, d_vpol, press_e,           &
!!     &         mat4_viscous_CTR, hdiv_visous_mat_CTR,                 &
!!     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e3(0:1)
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
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
!!      subroutine sph_FDM2_vpol_viscosity_mat_CTR                      &
!!     &        (sph_rj, fl_prop, radial_variation, g_sph_rj,           &
!!     &         coef_d, fdm_3e, fdm_e1, fdm2_center, fdm3e_CTR,        &
!!     &         mat_grad_p_CTR, mat2_viscous_CTR1, hdiv_visous_mat_CTR,&
!!     &         mat7)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e1(0:1)
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
!!        type(fdm2_center_mat), intent(in) :: fdm2_pol_CTR
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_grad_p_CTR(sph_rj%nidx_rj(2),0:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat2_viscous_CTR1(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!      subroutine sph_FDM4_vpol_viscosity_mat_CMB                      &
!!     &        (sph_rj, fl_prop, radial_variation,                     &
!!     &         g_sph_rj, coef_p, coef_d, fdm_e3,                      &
!!     &         fdm4_pol_CTR, fdm3e_center, fdm3e_center1,             &
!!     &         mat4_viscous_CMB1, hdiv_visous_mat_CTR, mat9)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        type(fdm_matrix), intent(in) :: fdm_e3(0:1)
!!        type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
!!        type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center1
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!!      real(kind = kreal), intent(inout)                               &
!!     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!@endverbatim
!
      module set_sph_pol_hdiv_viscs_CTR
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
      subroutine sph_exp_FDM2_vpol_viscosity_CTR                        &
     &        (sph_rj, fl_prop, radial_variation,                       &
     &         g_sph_rj, coef_p, coef_d, fdm_e1,                        &
     &         fdm2_center, fdm3e_center, d_vpol, press_e,              &
     &         mat2_viscous_CTR, hdiv_visous_mat_CTR,                   &
     &         d_grad_p, d_viscous_p, hdiv_viscous_e)
!
      use t_coef_fdm2_centre
      use t_coef_fdm3_n2e_zero_vp_CTR
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use sum_sph_pol_vscs_FDM2_exp
      use sum_sph_pol_grad_p_FDM2_exp
      use sum_sph_hdiv_vscs_FDM_exp
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e1(0:1)
      type(fdm2_center_mat), intent(in) :: fdm2_center
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous_CTR(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                    :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                    :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat1_grad_p_CTR(sph_rj%nidx_rj(2),0:1)
!
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call sum_exp_sph_hdiv_viscous_CTR                                 &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    hdiv_visous_mat_CTR(1,0), hdiv_viscous_e)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call sum_exp_sph_hdiv_viscous_ICB                                 &
     &   (itwo, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), d_vpol,           &
     &    hdiv_visous_mat_CTR(1,-1), hdiv_viscous_e)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (ione, fdm_e1(1)%n_minus, fdm_e1(1)%n_plus,                    &
     &    sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_p, fdm_e1(1)%nri_mat, fdm_e1(1)%dmat, mat1_grad_p_CTR)
      call sum_exp2_sph_pol_grad_p                                      &
     &   (ione, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &    press_e, mat1_grad_p_CTR(1,0), d_grad_p)
!
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm2_center%dmat_fix_dr(-1,2),                          &
     &    fdm2_center%dmat_fix_dr(-1,3), mat2_viscous_CTR)
      call sum_exp2_sph_viscous_CTR1                                    &
     &   (ione, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &    d_vpol, mat2_viscous_CTR(1,0), d_viscous_p)
!
      end subroutine sph_exp_FDM2_vpol_viscosity_CTR
!
!  -------------------------------------------------------------------
!
      subroutine sph_exp_FDM4_vpol_viscosity_CTR                        &
     &        (sph_rj, fl_prop, radial_variation,                       &
     &         g_sph_rj, coef_p, coef_d, fdm_e3,                        &
     &         fdm4_pol_CTR, fdm3e_center, d_vpol, press_e,             &
     &         mat4_viscous_CTR, hdiv_visous_mat_CTR,                   &
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
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e3(0:1)
      type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
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
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call sum_exp_sph_hdiv_viscous_CTR                                 &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,                    &
     &    hdiv_visous_mat_CTR(1,0), hdiv_viscous_e)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call sum_exp_sph_hdiv_viscous_ICB                                 &
     &   (itwo, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), d_vpol,           &
     &    hdiv_visous_mat_CTR(1,-1), hdiv_viscous_e)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (ione, izero, fdm_e3(1)%n_plus,                                &
     &    sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_p, fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call sum_exp4_sph_pol_grad_p_CTR1                                 &
     &   (ione, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &    press_e, mat3_grad_p_CTR(1,0), d_grad_p)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (itwo, -ione, fdm_e3(1)%n_plus,                                &
     &    sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_p, fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call sum_exp4_sph_pol_grad_p                                      &
     &   (itwo, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &    press_e, mat3_grad_p_CTR(1,-1), d_grad_p)
!
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CTR)
      call sum_exp4_sph_viscous_CTR1                                    &
     &   (ione, sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,              &
     &    mat4_viscous_CTR(1,0), d_viscous_p)
!
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CTR)
      call sum_exp4_sph_viscous_CTR2                                    &
     &   (itwo, sph_rj%nnod_rj, sph_rj%nidx_rj(2), d_vpol,              &
     &    mat4_viscous_CTR(1,-1), d_viscous_p)
!
      end subroutine sph_exp_FDM4_vpol_viscosity_CTR
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sph_FDM2_vpol_viscosity_mat_CTR                        &
     &        (sph_rj, fl_prop, radial_variation, g_sph_rj,             &
     &         coef_d, fdm_3e, fdm_e1, fdm2_center, fdm3e_CTR,          &
     &         mat_grad_p_CTR, mat2_viscous_CTR1, hdiv_visous_mat_CTR,  &
     &         mat7)
!
      use t_coef_fdm2_centre
      use t_coef_fdm3_n2e_zero_vp_CTR
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use set_sph_pol_vscs_FDM2_mat
      use set_sph_hdiv_vscs_FDM_mat7
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
!
      type(fdm_matrix), intent(in) :: fdm_3e(0:3)
      type(fdm_matrix), intent(in) :: fdm_e1(0:1)
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_CTR
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_grad_p_CTR(sph_rj%nidx_rj(2),0:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous_CTR1(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
!
      call set_sph_ele_pressure_FDM_mat7                                &
     &   (ione, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                   &
     &    fl_prop%coef_press, mat7)
      call each_sph_FDM_hdiv_viscosity_mat                              &
     &   (ione, izero, ione, sph_rj, fl_prop,                           &
     &    radial_variation, g_sph_rj, coef_d,                           &
     &    fdm3e_CTR%dmat_vp0( 0,1), fdm3e_CTR%dmat_vp0( 0,2),           &
     &    fdm3e_CTR%dmat_vp0( 0,3), fdm3e_CTR%dmat_vp0( 0,4),           &
     &    hdiv_visous_mat_CTR(1,0))
      call sub_sph_hdiv_viscous_mat7_CTR                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_CTR(1,0), mat7)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (ione, fdm_e1(1)%n_minus, fdm_e1(1)%n_plus,                    &
     &    sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    fl_prop%coef_press, fdm_e1(1)%nri_mat, fdm_e1(1)%dmat,        &
     &    mat_grad_p_CTR(1,0))
      call each_sph_FDM_viscosity_mat(izero, ione, ione,                &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    fdm2_center%dmat_fix_fld(0,2),                                &
     &    fdm2_center%dmat_fix_fld(0,3), mat2_viscous_CTR1(1,0))
      call sub_sph_pol_viscous_mat7_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat_grad_p_CTR(1,0), mat2_viscous_CTR1(1,0), mat7)
!
      call set_sph_ele_pressure_FDM_mat7                                &
     &   (itwo, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                   &
     &    fl_prop%coef_press, mat7)
      call set_sph_FDM_hdiv_viscosity_mat(itwo, -itwo, ione,            &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    fdm_3e(0)%nri_mat, fdm_3e(0)%dmat, fdm_3e(1)%dmat,            &
     &    fdm_3e(2)%dmat, fdm_3e(3)%dmat, hdiv_visous_mat_CTR(1,-2))
      call sub_sph_hdiv_viscous_mat7_CTR1                               &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_CTR(1,-1), mat7)
!
      end subroutine sph_FDM2_vpol_viscosity_mat_CTR
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM4_vpol_viscosity_mat_CMB                        &
     &        (sph_rj, fl_prop, radial_variation,                       &
     &         g_sph_rj, coef_p, coef_d, fdm_e3,                        &
     &         fdm4_pol_CTR, fdm3e_center, fdm3e_center1,               &
     &         mat4_viscous_CMB1, hdiv_visous_mat_CTR, mat9)
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
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
!
      type(fdm_matrix), intent(in) :: fdm_e3(0:1)
      type(fdm4_centre_vpol), intent(in) :: fdm4_pol_CTR
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center
      type(fdm3_n2e_CTR_vpol), intent(in) :: fdm3e_center1
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat4_viscous_CMB1(sph_rj%nidx_rj(2),-2:2)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat_CTR(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      real(kind = kreal) :: mat3_grad_p_CTR(sph_rj%nidx_rj(2),-1:2)
!
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (ione, izero, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center%dmat_vp0(-2,1),                                &
     &      fdm3e_center%dmat_vp0(-2,2),                                &
     &      fdm3e_center%dmat_vp0(-2,3),                                &
     &      fdm3e_center%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,0))
      call add_sph_ele_pressure_FDM_mat9                                &
     &   (ione, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p, mat9)
      call sub_sph_hdiv_viscous_mat9_CTR                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_CTR(1,0), mat9)
!
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (itwo, -ione, ione, sph_rj, fl_prop,                         &
     &      radial_variation, g_sph_rj, coef_d, ione,                   &
     &      fdm3e_center1%dmat_vp0(-2,1),                               &
     &      fdm3e_center1%dmat_vp0(-2,2),                               &
     &      fdm3e_center1%dmat_vp0(-2,3),                               &
     &      fdm3e_center1%dmat_vp0(-2,4), hdiv_visous_mat_CTR(1,-1))
      call add_sph_ele_pressure_FDM_mat9                                &
     &   (itwo, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p, mat9)
      call sub_sph_hdiv_viscous_mat9_CTR1                               &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    hdiv_visous_mat_CTR(1,-1), mat9)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (ione, izero, fdm_e3(1)%n_plus,                                &
     &    sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_p, fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CMB1)
      call sub_sph_pol_viscous_mat9_CTR1                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat3_grad_p_CTR(1,0), mat4_viscous_CMB1(1,0), mat9)
!
      call set_sph_FDM_pressure_grad_mat                                &
     &   (itwo, -ione, fdm_e3(1)%n_plus,                                &
     &    sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_p, fdm_e3(1)%nri_mat, fdm_e3(1)%dmat, mat3_grad_p_CTR)
      call set_sph_FDM_viscosity_mat(-ione, itwo, itwo,                 &
     &    sph_rj, fl_prop, radial_variation, g_sph_rj, coef_d,          &
     &    ione, fdm4_pol_CTR%dmat_vp1(-2,2),                            &
     &    fdm4_pol_CTR%dmat_vp1(-2,3), mat4_viscous_CMB1)
      call sub_sph_pol_viscous_mat9_CTR2                                &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    mat3_grad_p_CTR(1,-1), mat4_viscous_CMB1(1,-1), mat9)
!
      end subroutine sph_FDM4_vpol_viscosity_mat_CMB
!
!  -------------------------------------------------------------------
!
      end module set_sph_pol_hdiv_viscs_CTR

