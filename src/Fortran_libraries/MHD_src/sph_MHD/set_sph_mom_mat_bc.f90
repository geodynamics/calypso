!>@file   set_sph_mom_mat_bc.f90
!!@brief  module set_sph_mom_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for fixed velocity at boundaries
!!
!!@verbatim
!!      subroutine set_icb_wt_sph_evo_mat
!!      subroutine set_icb_vp_sph_poisson_mat
!!      subroutine set_icb_p_sph_poisson_mat
!!
!!      subroutine set_cmb_wt_sph_evo_mat
!!      subroutine set_cmb_vp_sph_poisson_mat
!!      subroutine set_cmb_p_sph_poisson_mat
!!@endverbatim
!
      module set_sph_mom_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_physical_property
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
      use m_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_wt_sph_evo_mat
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
!        wt_evo_mat(2,nlayer_ICB,  j)                                   &
!     &                   = one + coef_imp_v*dt*coef_d_velo             &
!     &                      * g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)
!        wt_evo_mat(1,nlayer_ICB+1,j) =  zero
!
        wt_evo_mat(2,nlayer_ICB,  j)                                   &
     &                   = one + coef_imp_v*dt*coef_d_velo             &
     &                           * ( -coef_fdm_fix_dr_ICB_2( 0,3)      &
     &                      + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2))
        wt_evo_mat(1,nlayer_ICB+1,j)                                   &
     &                   =     - coef_imp_v*dt*coef_d_velo             &
     &                          *    coef_fdm_fix_dr_ICB_2( 1,3)
      end do
!
      end subroutine set_icb_wt_sph_evo_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_vp_sph_poisson_mat
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(2,nlayer_ICB,  j) = one
        vs_poisson_mat(1,nlayer_ICB+1,j) = zero
      end do
!
      end subroutine set_icb_vp_sph_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_icb_p_sph_poisson_mat
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        p_poisson_mat(2,nlayer_ICB,  j) = coef_press                    &
     &                           * (coef_fdm_fix_dr_ICB_2( 0,3)         &
     &                           - g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2) &
     &                           + two * ar_1d_rj(nlayer_ICB,1)         &
     &                            * coef_fdm_fix_dr_ICB_2( 0,2) )
        p_poisson_mat(1,nlayer_ICB+1,j) = coef_press                    &
     &                           * (coef_fdm_fix_dr_ICB_2( 1,3)         &
     &                           + two * ar_1d_rj(nlayer_ICB,1)         &
     &                            * coef_fdm_fix_dr_ICB_2( 1,2) )
!
        p_poisson_mat(3,nlayer_ICB,j  ) = coef_press                    &
     &                    * (d2nod_mat_fdm_2(nlayer_ICB+1,-1)           &
     &                   + two*ar_1d_rj(nlayer_ICB+1,1)                 &
     &                    * d1nod_mat_fdm_2(nlayer_ICB+1,-1) )
        p_poisson_mat(2,nlayer_ICB+1,j) = coef_press                    &
     &                           * ( d2nod_mat_fdm_2(nlayer_ICB+1, 0)   &
     &                   + two*ar_1d_rj(nlayer_ICB+1,1)                 &
     &                    * d1nod_mat_fdm_2(nlayer_ICB+1, 0)            &
     &                   - g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB+1,2) )
        p_poisson_mat(1,nlayer_ICB+2,j) = coef_press                    &
     &                           * ( d2nod_mat_fdm_2(nlayer_ICB+1, 1)   &
     &                   + two*ar_1d_rj(nlayer_ICB+1,1)                 &
     &                    * d1nod_mat_fdm_2(nlayer_ICB+1, 1) )
      end do
!
      end subroutine set_icb_p_sph_poisson_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_cmb_wt_sph_evo_mat
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
!        wt_evo_mat(3,nlayer_CMB-1,j) = zero
!        wt_evo_mat(2,nlayer_CMB,  j)                                   &
!     &                   = one + coef_imp_v*dt*coef_d_velo             &
!     &                        * g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)
!
        wt_evo_mat(3,nlayer_CMB-1,j)                                   &
     &                   =     - coef_imp_v*dt*coef_d_velo             &
     &                          *    coef_fdm_fix_dr_CMB_2(-1,3)
        wt_evo_mat(2,nlayer_CMB,  j)                                   &
     &                   = one + coef_imp_v*dt*coef_d_velo             &
     &                          * ( -coef_fdm_fix_dr_CMB_2( 0,3)       &
     &                      + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2))
      end do
!
      end subroutine set_cmb_wt_sph_evo_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_vp_sph_poisson_mat
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        vs_poisson_mat(3,nlayer_CMB-1,j) = zero
        vs_poisson_mat(2,nlayer_CMB,  j) = one
      end do
!
      end subroutine set_cmb_vp_sph_poisson_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_cmb_p_sph_poisson_mat
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        p_poisson_mat(3,nlayer_CMB-1,j) = coef_press                    &
     &                           * ( coef_fdm_fix_dr_CMB_2(-1,3)        &
     &                           + two* ar_1d_rj(nlayer_CMB,1)          &
     &                            * coef_fdm_fix_dr_CMB_2(-1,2) )
        p_poisson_mat(2,nlayer_CMB,  j) = coef_press                    &
     &                           * ( coef_fdm_fix_dr_CMB_2( 0,3)        &
     &                           - g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2) &
     &                           + two * ar_1d_rj(nlayer_CMB,1)         &
     &                            * coef_fdm_fix_dr_CMB_2( 0,2) )
      end do
!
      end subroutine set_cmb_p_sph_poisson_mat
!
! -----------------------------------------------------------------------
!
      end module set_sph_mom_mat_bc
