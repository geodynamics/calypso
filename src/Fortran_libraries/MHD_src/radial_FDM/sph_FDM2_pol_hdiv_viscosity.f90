!>@file   sph_FDM2_pol_hdiv_viscosity.f90
!!@brief  module sph_FDM2_pol_hdiv_viscosity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix in each layer
!!
!!@verbatim
!!      subroutine sph_exp_FDM2_vpol_viscosity                          &
!!     &         (kr_st, kr_ed, sph_rj, g_sph_rj,                       &
!!     &          flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,    &
!!     &          fdm_2, fdm_3e, fdm_e1, d_vpol, press_e,               &
!!     &          mat2_viscous, hdiv_visous_mat,                        &
!!     &          d_grad_p, d_viscous_p, hdiv_viscous_e)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        type(phys_data), intent(in) :: radial_variation
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
!!        real(kind = kreal), intent(in)                               &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        real(kind=kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!!        type(fdm_matrices), intent(in) :: fdm_2
!!        type(fdm_matrices), intent(in) :: fdm_3e(0:3)
!!        type(fdm_matrices), intent(in) :: fdm_e1
!!        real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                            &
!!     &           :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                            &
!!     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!!        real(kind = kreal) intent(inout) :: d_viscous_p(sph_rj%nnod_rj)
!!        real(kind = kreal), intent(inout)                            &
!!     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!!      subroutine sph_FDM2_vpol_viscosity_mat                          &
!!     &         (kr_st, kr_ed, sph_rj, g_sph_rj,                       &
!!     &          flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,    &
!!     &          fdm_2, fdm_3e, fdm_e1, mat2_viscous, hdiv_visous_mat, &
!!     &          mat7)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        type(phys_data), intent(in) :: radial_variation
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        real(kind=kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
!!        real(kind=kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!!        type(fdm_matrices), intent(in) :: fdm_2
!!        type(fdm_matrices), intent(in) :: fdm_3e
!!        type(fdm_matrices), intent(in) :: fdm_e1
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!!@endverbatim
!!
      module sph_FDM2_pol_hdiv_viscosity
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
      subroutine sph_exp_FDM2_vpol_viscosity                            &
     &         (kr_st, kr_ed, sph_rj, g_sph_rj,                         &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,      &
     &          fdm_2, fdm_3e, fdm_e1, d_vpol, press_e,                 &
     &          mat2_viscous, hdiv_visous_mat,                          &
     &          d_grad_p, d_viscous_p, hdiv_viscous_e)
!
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use sum_sph_pol_vscs_FDM2_exp
      use sum_sph_pol_grad_p_FDM2_exp
      use sum_sph_hdiv_vscs_FDM_exp
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!
      type(fdm_matrices), intent(in) :: fdm_2
      type(fdm_matrices), intent(in) :: fdm_3e
      type(fdm_matrices), intent(in) :: fdm_e1
      real(kind = kreal), intent(in) :: d_vpol(sph_rj%nnod_rj)
      real(kind = kreal), intent(in) :: press_e(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout) :: d_grad_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: d_viscous_p(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_viscous_e(sph_rj%nnod_rj)
!
      real(kind = kreal) :: mat1_grad_p(sph_rj%nidx_rj(2),0:1)
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr,hdiv_visous_mat)
      do kr = kr_st+2, kr_ed-1
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (fdm_3e%n_minus, fdm_3e%n_plus,                              &
     &      flag_viscous_variation, flag_ref_density_valiation,         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,   &
     &      coef_d, relative_d(kr), h_nu(kr), h_rho(kr), h_drhodr(kr),  &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,0),                           &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,1),                           &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,2),                           &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,3), hdiv_visous_mat)
        call sum_exp_sph_hdiv_viscous                                   &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &      d_vpol, hdiv_visous_mat, hdiv_viscous_e)
      end do
!$omp end parallel do
!
!$omp parallel do private(kr,mat1_grad_p,mat2_viscous)
      do kr = kr_st+1, kr_ed-1
        call set_sph_FDM_pressure_grad_mat                              &
     &     (fdm_e1%n_minus, fdm_e1%n_plus, sph_rj%nidx_rj(2),           &
     &      sph_rj%radius_1d_rj_r(kr), g_sph_rj, coef_p,                &
     &      fdm_e1%dmat(fdm_e1%n_minus,kr,1), mat1_grad_p)
        call sum_exp2_sph_pol_grad_p                                    &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &      mat1_grad_p, press_e, d_grad_p)
      end do
!$omp end parallel do
!
!$omp parallel do private(kr,mat1_grad_p,mat2_viscous)
      do kr = kr_st+1, kr_ed-1
        call set_sph_FDM_viscosity_mat                                  &
     &     (fdm_2%n_minus, fdm_2%n_plus, sph_rj%nidx_rj(2),             &
     &      flag_viscous_variation, flag_ref_density_valiation,         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2), g_sph_rj,     &
     &      coef_d, relative_d(kr+1), h_nu(kr+1),                       &
     &      h_rho(kr+1), h_drhodr(kr+1),                                &
     &      fdm_2%dmat(fdm_2%n_minus,kr,1),                             &
     &      fdm_2%dmat(fdm_2%n_minus,kr,2), mat2_viscous)
        call sum_exp2_sph_pol_viscous                                   &
     &     (kr, sph_rj%nnod_rj, sph_rj%nidx_rj(2),                      &
     &      d_vpol, mat2_viscous, d_viscous_p)
      end do
!$omp end parallel do
!
      end subroutine sph_exp_FDM2_vpol_viscosity
!
! -----------------------------------------------------------------------
!
      subroutine sph_FDM2_vpol_viscosity_mat                            &
     &         (kr_st, kr_ed, sph_rj, g_sph_rj,                         &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,      &
     &          fdm_2, fdm_3e, fdm_e1, mat2_viscous, hdiv_visous_mat,   &
     &          mat7)
!
      use sph_FDM_viscosities_mat
      use cal_sph_FDM3e_hdiv_viscous
      use cal_sph_FDM_viscosity_mat
      use set_sph_pol_vscs_FDM2_mat
      use set_sph_hdiv_vscs_FDM_mat7
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1)+1)
      real(kind = kreal), intent(in) :: h_drhodr(sph_rj%nidx_rj(1)+1)
!
      type(fdm_matrices), intent(in) :: fdm_2
      type(fdm_matrices), intent(in) :: fdm_3e
      type(fdm_matrices), intent(in) :: fdm_e1
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat2_viscous(sph_rj%nidx_rj(2),-1:1)
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_mat(sph_rj%nidx_rj(2),-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1), sph_rj%nidx_rj(2))
!
      real(kind = kreal) :: mat1_grad_p(sph_rj%nidx_rj(2),0:1)
!
      integer(kind = kint) :: kr
!
!
!$omp parallel do private(kr,hdiv_visous_mat)
      do kr = kr_st+2, kr_ed-1
        call set_sph_ele_pressure_FDM_mat7                              &
     &     (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), coef_p, mat7)
        call set_sph_FDM_hdiv_viscosity_mat                             &
     &     (fdm_3e%n_minus, fdm_3e%n_plus,                              &
     &      flag_viscous_variation, flag_ref_density_valiation,         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,   &
     &      coef_d, relative_d(kr), h_nu(kr), h_rho(kr), h_drhodr(kr),  &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,0),                           &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,1),                           &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,2),                           &
     &      fdm_3e%dmat(fdm_3e%n_minus,kr,3), hdiv_visous_mat)
        call sub_sph_hdiv_viscous_FDM_mat7                              &
     &     (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                   &
     &      hdiv_visous_mat, mat7)
      end do
!$omp end parallel do
!
!$omp parallel do private(kr,mat1_grad_p,mat2_viscous)
      do kr = kr_st+1, kr_ed-1
        call set_sph_FDM_pressure_grad_mat                              &
     &     (fdm_e1%n_minus, fdm_e1%n_plus, sph_rj%nidx_rj(2),           &
     &      sph_rj%radius_1d_rj_r(kr), g_sph_rj, coef_p,                &
     &      fdm_e1%dmat(fdm_e1%n_minus,kr,1), mat1_grad_p)
!
        call set_sph_FDM_viscosity_mat                                  &
     &     (fdm_2%n_minus, fdm_2%n_plus, sph_rj%nidx_rj(2),             &
     &      flag_viscous_variation, flag_ref_density_valiation,         &
     &      sph_rj%ar_1d_rj(kr,1), sph_rj%ar_1d_rj(kr,2), g_sph_rj,     &
     &      coef_d, relative_d(kr+1), h_nu(kr+1),                       &
     &      h_rho(kr+1), h_drhodr(kr+1),                                &
     &      fdm_2%dmat(fdm_2%n_minus,kr,1),                             &
     &      fdm_2%dmat(fdm_2%n_minus,kr,2), mat2_viscous)
        call sub_sph_pol_viscous_FDM2_mat                               &
     &     (kr, sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                   &
     &      mat1_grad_p, mat2_viscous, mat7)
      end do
!$omp end parallel do
!
      end subroutine sph_FDM2_vpol_viscosity_mat
!
! -----------------------------------------------------------------------
!
      end module sph_FDM2_pol_hdiv_viscosity
