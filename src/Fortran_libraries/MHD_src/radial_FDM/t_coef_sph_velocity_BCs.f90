!>@file   t_coef_sph_velocity_BCs.f90
!!@brief  module t_coef_sph_velocity_BCs
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2024
!
!>@brief  Structure for basic boundary conditions for velocity
!!
!!
!!@verbatim
!!      subroutine check_sph_fdm_boundaries(id_file, kr_in, kr_out,     &
!!     &          nri, radius_1d_rj_r, bc_fdms_U)
!!      subroutine check_sph_4th_fdm_boundaries(id_file, bc_fdms_U)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!!        type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!!
!!      subroutine set_sph_fdm_velocity_bc                              &
!!     &         (kr_in, kr_out, h_rho_in, h_rho_out, sph_rj, bc_fdms_U)
!!      subroutine set_boundary_sph_4th_fdm                             &
!!     &         (kr_in, kr_out, h_rho_in, h_rho_out,                   &
!!     &          sph_rj, fdm_4th, bc_fdms_U)
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: h_rho_in, h_rho_out
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: fdm_4th
!!        type(velocity_boundary_FDMs), intent(inout) :: bc_fdms_U
!!@endverbatim
!!
      module t_coef_sph_velocity_BCs
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_rj_data
!
      use t_coef_fdm2_free_slip_ICB
      use t_coef_fdm2_free_slip_CMB
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm3_n2e_free_vp_CMB
!
      use t_coef_fdm4_zero_vpol_ICB
      use t_coef_fdm4_zero_vpol_CMB
      use t_coef_fdm4_free_vpol_ICB
      use t_coef_fdm4_free_vpol_CMB
      use t_coef_fdm4_vpol_centre
!
      implicit none
!
!>      Structure for Additional boundary condition matrices for velocity
      type velocity_boundary_FDMs
!>        Structure for FDM matrix of free slip boundary at ICB
        type(fdm2_ICB_free_slip) :: fdm2_free_ICB
!>        Structure for FDM matrix of free slip boundary at CMB
        type(fdm2_CMB_free_slip) :: fdm2_free_CMB
!
!>        Matrix to evaluate radial derivative at ICB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_ICB(0:1,2)
!>        Matrix to evaluate radial derivative at CMB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_CMB(-1:0,2)
!
!>        Structure for FDM matrix of free slip boundary at center
        type(fdm3_n2e_CTR_vpol) :: fdm3e_CTR
!>        Structure for FDM matrix at ICB element with zero poloidal
        type(fdm3_n2e_ICB_zero_vpol) :: fdm3e_vp0_ICB
!>        Structure for FDM matrix at CMB element with zero poloidal
        type(fdm3_n2e_CMB_zero_vpol) :: fdm3e_vp0_CMB
!
!>        Structure for FDM matrix of free slip boundary at ICB element
        type(fdm3_n2e_ICB_free_vpol) :: fdm3e_free_ICB
!>        Structure for FDM matrix of free slip boundary at CMB element
        type(fdm3_n2e_CMB_free_vpol) :: fdm3e_free_CMB
!
!
!>        Structure for FDM matrix of free slip boundary at center
        type(fdm4_centre_vpol) :: fdm4_CTR
!>        Structure for 4th order FDM matrix of non-slip boundary at ICB
        type(fdm4_ICB_zero_vpol) :: fdm4_noslip_ICB
!>        Structure for 4th order FDM matrix of non-slip boundary at CMB
        type(fdm4_CMB_zero_vpol) :: fdm4_noslip_CMB
!
!>        Structure for 4th order FDM matrix of free slip boundary at ICB
        type(fdm4_ICB_free_vpol) :: fdm4_free_vp_ICB
!>        Structure for 4th order FDM matrix of free slip boundary at CMB
        type(fdm4_CMB_free_vpol) :: fdm4_free_vp_CMB
      end type velocity_boundary_FDMs
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_fdm_boundaries(id_file, kr_in, kr_out,       &
     &                                    sph_rj, bc_fdms_U)
!
      use t_coef_fdm1_free_rotate_ICB
      use t_coef_fdm1_free_rotate_CMB
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: kr_in, kr_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!
      real(kind = kreal) :: r, dr_bc, c_dr, c_dr2, c_dr3
!
      r =     sph_rj%radius_1d_rj_r(kr_in  )
      dr_bc = sph_rj%radius_1d_rj_r(kr_in+1) - r
      c_dr =  r /   (dr_bc * (r + dr_bc))
      c_dr2 = 2.0 / (dr_bc * (r + dr_bc))
      write(*,*) 'free ICB for Vp', c_dr, c_dr2
      c_dr =  2.0 / (dr_bc * dr_bc)
      c_dr2 = -(c_dr / r) * (r + 2.0*dr_bc)
      c_dr3 =  2.0 / r
      write(*,*) 'free ICB for DVt/DR (ICB)', c_dr3
      write(*,*) 'free ICB for D2Vt/DR2 (ICB, ICB+1)', c_dr2, c_dr
      call check_fdm2_coef_free_slip_ICB(id_file,                       &
     &                                  bc_fdms_U%fdm2_free_ICB)
!
      r =         sph_rj%radius_1d_rj_r(kr_out  )
      dr_bc = r - sph_rj%radius_1d_rj_r(kr_out-1)
      c_dr =  r /   (dr_bc * (r - dr_bc))
      c_dr2 = 2.0 / (dr_bc * (r - dr_bc))
      write(*,*) 'free CMB', c_dr, c_dr2
      c_dr =  2.0 / (dr_bc * dr_bc)
      c_dr2 = (c_dr / r) * (-r + 2.0*dr_bc)
      c_dr3 =  2.0 / r
      write(*,*) 'free CMB for DVt/DR (CMB)', c_dr3
      write(*,*) 'free CMB for D2Vt/DR2 (CMB-1, CMB)', c_dr, c_dr2
      call check_fdm2_coef_free_slip_CMB(id_file,                       &
     &                                  bc_fdms_U%fdm2_free_CMB)
!
      call check_fdm1_ICB_fixed_field(id_file,                          &
     &                                bc_fdms_U%fdm1_fix_fld_ICB)
      call check_fdm1_CMB_fixed_field(id_file,                          &
     &                                bc_fdms_U%fdm1_fix_fld_CMB)
!
      call check_fdm3_n2e_ICB_zero_vpol(id_file,                        &
     &                                  bc_fdms_U%fdm3e_vp0_ICB)
      call check_fdm3_n2e_CMB_zero_vpol(id_file,                        &
     &                                  bc_fdms_U%fdm3e_vp0_CMB)
!
      call check_fdm3_n2e_ICB_free_vpol(id_file,                        &
     &                                  bc_fdms_U%fdm3e_free_ICB)
      call check_fdm3_n2e_CMB_free_vpol(id_file,                        &
     &                                  bc_fdms_U%fdm3e_free_CMB)
      call check_fdm3_n2e_CTR_zero_vpol(id_file, bc_fdms_U%fdm3e_CTR)
!
      end subroutine check_sph_fdm_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_4th_fdm_boundaries(id_file, bc_fdms_U)
!
      integer(kind = kint), intent(in) :: id_file
      type(velocity_boundary_FDMs), intent(in) :: bc_fdms_U
!
!
      call check_4th_ICB_nonslip_vp_fdm(id_file,                        &
     &                                  bc_fdms_U%fdm4_noslip_ICB)
      call check_4th_CMB_nonslip_vp_fdm(id_file,                        &
     &                                  bc_fdms_U%fdm4_noslip_CMB)
      call check_4th_ICB_free_vp_fdm(id_file,                           &
     &                               bc_fdms_U%fdm4_free_vp_ICB)
      call check_4th_CMB_free_vp_fdm(id_file,                           &
     &                               bc_fdms_U%fdm4_free_vp_CMB)
      call check_fdm4_vpol_centre(id_file, bc_fdms_U%fdm4_CTR)
!
      end subroutine check_sph_4th_fdm_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_fdm_velocity_bc                                &
     &         (kr_in, kr_out, h_rho_in, h_rho_out, sph_rj, bc_fdms_U)
!
      use t_coef_fdm1_free_rotate_ICB
      use t_coef_fdm1_free_rotate_CMB
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: h_rho_in, h_rho_out
!
      type(velocity_boundary_FDMs), intent(inout) :: bc_fdms_U
!
!
      call cal_fdm1_coef_fix_fld_ICB(sph_rj%radius_1d_rj_r(kr_in),      &
     &                               bc_fdms_U%fdm1_fix_fld_ICB)
      call cal_fdm1_coef_fix_fld_CMB(sph_rj%radius_1d_rj_r(kr_out-1),   &
     &                               bc_fdms_U%fdm1_fix_fld_CMB)
!
      call cal_fdm2_ICB_free_vp(h_rho_in, sph_rj%radius_1d_rj_r(kr_in), &
     &                          bc_fdms_U%fdm2_free_ICB)
      call cal_fdm2_ICB_free_vt(h_rho_in,                               &
     &    sph_rj%radius_1d_rj_r(kr_in), bc_fdms_U%fdm2_free_ICB)
!
      call cal_fdm3e_ICB_hdiv_vp(sph_rj%radius_1d_rj_r(kr_in),          &
     &                           bc_fdms_U%fdm3e_vp0_ICB)
      call cal_fdm3e_ICB_free_hdiv_vp                                   &
     &   (bc_fdms_U%fdm2_free_ICB%dmat_vp, bc_fdms_U%fdm3e_vp0_ICB,     &
     &    bc_fdms_U%fdm3e_free_ICB)
!
      call cal_fdm2_CMB_free_vp(h_rho_out,                              &
     &    sph_rj%radius_1d_rj_r(kr_out-1), bc_fdms_U%fdm2_free_CMB)
      call cal_fdm2_CMB_free_vt(h_rho_out,                              &
     &    sph_rj%radius_1d_rj_r(kr_out-1), bc_fdms_U%fdm2_free_CMB)
!
      call cal_fdm3e_CMB_hdiv_vp(sph_rj%radius_1d_rj_r(kr_out-2),       &
     &                           bc_fdms_U%fdm3e_vp0_CMB)
      call cal_fdm3e_CMB_free_hdiv_vp                                   &
     &   (bc_fdms_U%fdm2_free_CMB%dmat_vp, bc_fdms_U%fdm3e_vp0_CMB,     &
     &    bc_fdms_U%fdm3e_free_CMB)
!
! Set 3-rd order FDM matrices for Center
      if(iflag_debug .gt. 0) write(*,*) 'cal_2nd_to_center_fixed_fdm'
      call cal_fdm3e_CTR_hdiv_vp(sph_rj%radius_1d_rj_r(1),              &
     &                           bc_fdms_U%fdm3e_CTR)
!
      end subroutine set_sph_fdm_velocity_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_boundary_sph_4th_fdm                               &
     &         (kr_in, kr_out, h_rho_in, h_rho_out,                     &
     &          sph_rj, fdm_4th, bc_fdms_U)
!
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: h_rho_in, h_rho_out
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: fdm_4th
!
      type(velocity_boundary_FDMs), intent(inout) :: bc_fdms_U
!
!
      call cal_fdm4_ICB0_nonslip_vp(sph_rj%radius_1d_rj_r(kr_in),       &
     &                              bc_fdms_U%fdm4_noslip_ICB)
      call cal_fdm4_ICB1_nonslip_vp(sph_rj%radius_1d_rj_r(kr_in),       &
     &                              bc_fdms_U%fdm4_noslip_ICB)
!
      call cal_fdm4_CMB0_nonslip_vp(sph_rj%radius_1d_rj_r(kr_out-3),    &
     &                              bc_fdms_U%fdm4_noslip_CMB)
      call cal_fdm4_CMB1_nonslip_vp(sph_rj%radius_1d_rj_r(kr_out-3),    &
     &                              bc_fdms_U%fdm4_noslip_CMB)
!
      call cal_fdm4_ICB0_free_vp                                        &
     &   (h_rho_in, sph_rj%radius_1d_rj_r(kr_in),                       &
     &    bc_fdms_U%fdm4_free_vp_ICB)
      call cal_fdm4_ICB1_free_vp(sph_rj%radius_1d_rj_r(kr_in),          &
     &                           bc_fdms_U%fdm4_free_vp_ICB)
!
      call cal_fdm4_CMB0_free_vp                                        &
     &   (h_rho_out, sph_rj%radius_1d_rj_r(kr_out-3),                   &
     &    bc_fdms_U%fdm4_free_vp_CMB)
      call cal_fdm4_CMB1_free_vp(sph_rj%radius_1d_rj_r(kr_out-3),       &
     &                           bc_fdms_U%fdm4_free_vp_CMB)
!
      call cal_coef_fdm4_vpol_centre(sph_rj%radius_1d_rj_r(1),          &
     &                               fdm_4th, bc_fdms_U%fdm4_CTR)
!
      end subroutine set_boundary_sph_4th_fdm
!
! -----------------------------------------------------------------------
!
      end module t_coef_sph_velocity_BCs
!
