!>@file   set_sph_magne_mat_bc.f90
!!@brief  module set_sph_magne_mat_bc
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for magnetic field at boundaries
!!
!!@verbatim
!!    Boundary condition for approaching to center
!!      subroutine set_magne_center_rmat_sph
!!
!!    Boundary condition to connect potential field
!!      subroutine set_ins_magne_icb_rmat_sph
!!      subroutine set_ins_magne_cmb_rmat_sph
!!
!!    Boundary condition for radial magnetic field
!!      subroutine set_qvacume_magne_icb_rmat_sph
!!      subroutine set_qvacume_magne_cmb_rmat_sph
!!@endverbatim
!
      module set_sph_magne_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_physical_property
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_magne_center_rmat_sph
!
      use m_fdm_coefs
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        bs_evo_mat(2,ione,j) = one + coef_imp_b*dt*coef_d_magne         &
     &                               * ( -d2nod_mat_fdm_2(1,0)          &
     &                              + g_sph_rj(j,3)*ar_1d_rj(ione,2) )
        bs_evo_mat(1,itwo,j) =     - coef_imp_b*dt*coef_d_magne         &
     &                               *    d2nod_mat_fdm_2(1,1)
!
        bt_evo_mat(2,ione,j) = one + coef_imp_b*dt*coef_d_magne         &
     &                               * ( -d2nod_mat_fdm_2(1,0)          &
     &                              + g_sph_rj(j,3)*ar_1d_rj(ione,2) )
        bt_evo_mat(1,itwo,j) =     - coef_imp_b*dt*coef_d_magne         &
     &                               *    d2nod_mat_fdm_2(1,1)
      end do
!
      end subroutine set_magne_center_rmat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_ins_magne_icb_rmat_sph
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        bs_evo_mat(2,nlayer_ICB,  j)                                    &
     &                   = one + coef_imp_b*dt*coef_d_magne             &
     &                          * ( -coef_fdm_fix_dr_ICB_2( 0,3)        &
     &                         + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2)   &
     &                         - (g_sph_rj(j,1)+one)                    &
     &                          * ar_1d_rj(nlayer_ICB,1)                &
     &                          *    coef_fdm_fix_dr_ICB_2(-1,3) )
        bs_evo_mat(1,nlayer_ICB+1,j)                                    &
     &                   =     - coef_imp_b*dt*coef_d_magne             &
     &                          *    coef_fdm_fix_dr_ICB_2( 1,3)
!
        bt_evo_mat(2,nlayer_ICB,  j) = one
        bt_evo_mat(1,nlayer_ICB+1,j) = zero
      end do
!
      end subroutine set_ins_magne_icb_rmat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_ins_magne_cmb_rmat_sph
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        bs_evo_mat(3,nlayer_CMB-1,j)                                    &
     &                   =     - coef_imp_b*dt*coef_d_magne             &
     &                          *    coef_fdm_fix_dr_CMB_2(-1,3)
        bs_evo_mat(2,nlayer_CMB,  j)                                    &
     &                   = one + coef_imp_b*dt*coef_d_magne             &
     &                          * ( -coef_fdm_fix_dr_CMB_2( 0,3)        &
     &                         + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2)   &
     &                         + g_sph_rj(j,1)*ar_1d_rj(nlayer_CMB,1)   &
     &                          *    coef_fdm_fix_dr_CMB_2( 1,3) )
!
        bt_evo_mat(3,nlayer_CMB-1,j) = zero
        bt_evo_mat(2,nlayer_CMB,  j) = one
      end do
!
      end subroutine set_ins_magne_cmb_rmat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_qvacume_magne_icb_rmat_sph
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        bs_evo_mat(2,nlayer_ICB,  j)                                    &
     &                   = one + coef_imp_b*dt*coef_d_magne             &
     &                          * ( -coef_fdm_fix_dr_ICB_2( 0,3)        &
     &                         + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2) )
        bs_evo_mat(1,nlayer_ICB+1,j)                                    &
     &                   =     - coef_imp_b*dt*coef_d_magne             &
     &                          *    coef_fdm_fix_dr_ICB_2( 1,3)
!
        bt_evo_mat(2,nlayer_ICB,  j) = one
        bt_evo_mat(1,nlayer_ICB+1,j) = zero
      end do
!
      end subroutine set_qvacume_magne_icb_rmat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_qvacume_magne_cmb_rmat_sph
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint) :: j
!
!
      do j = 1, nidx_rj(2)
        bs_evo_mat(3,nlayer_CMB-1,j)                                    &
     &                   =     - coef_imp_b*dt*coef_d_magne             &
     &                          *    coef_fdm_fix_dr_CMB_2(-1,3)
        bs_evo_mat(2,nlayer_CMB,  j)                                    &
     &                   = one + coef_imp_b*dt*coef_d_magne             &
     &                          * ( -coef_fdm_fix_dr_CMB_2( 0,3)        &
     &                         + g_sph_rj(j,3)*ar_1d_rj(nlayer_CMB,2) )
!
        bt_evo_mat(3,nlayer_CMB-1,j) = zero
        bt_evo_mat(2,nlayer_CMB,  j) = one
      end do
!
      end subroutine set_qvacume_magne_cmb_rmat_sph
!
! -----------------------------------------------------------------------
!
      end module set_sph_magne_mat_bc
