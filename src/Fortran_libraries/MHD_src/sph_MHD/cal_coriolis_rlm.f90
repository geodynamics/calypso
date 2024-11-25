!>@file   cal_coriolis_rlm.f90
!!@brief  module cal_coriolis_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1995
!@n     Modified in Dec., 2013
!
!>@brief  Evaluate Coriolis force in (r,l,m) parallelization
!!
!!@verbatim
!!************************************************
!!      subroutine s_cal_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,   &
!!     &          fl_prop, sph_bc_U, omega_sph, b_trns, f_trns,         &
!!     &          leg, gt_cor, n_WR, WR, cor_rlm)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(phys_address), intent(in) :: b_trns, f_trns
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!!        integer(kind = kint), intent(in) :: ncomp_trans, n_WR
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
!!
!!      subroutine copy_coriolis_terms_rlm(ncomp_trans, sph_rlm,        &
!!     &          comm_rlm, b_trns, cor_rlm, n_WS, WS)
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(phys_address), intent(in) :: b_trns
!!        type(coriolis_rlm_data), intent(in) :: cor_rlm
!!        integer(kind = kint), intent(in) :: ncomp_trans, n_WS
!!        real(kind = kreal), intent(inout) :: WS(n_WS)
!!
!!************************************************
!!
!!  Radial componenet of the Coriolis term
!!     (wsr) = wsr(jc,1,j3)*dw*dusb/r**2
!!  Horizontal poloidal component of the Coriolos term
!!     (wtr) = wtr(j3)*dw*wtb/r**2
!!
!!  Toroidal of componennt of the Coriolos term
!!     (wss) = wss(jc,1,j3)*w*dyb/r**2
!!            + wss(jc,2,j3)*dw*yb/r**2
!!
!!************************************************
!!
!!************************************************
!!
!!     wss(jc,1,j3) = sw_rj(jc,1,j3)
!!     wss(jc,2,j3) = sw_rj(jc,2,j3)
!!     wts(jc,j3)   = sw_rj(jc,3,j3)
!!     wst(jc,1,j3) = tw_rj(jc,1,j3)
!!     wst(jc,2,j3) = tw_rj(jc,2,j3)
!!     wtt(jc,1,j3) = tw_rj(jc,3,j3)
!!     wtt(jc,2,j3) = tw_rj(jc,4,j3)
!!
!!     wsd(jc,1,j3) = sd_rj(jc,1,j3)
!!     wsd(jc,2,j3) = sd_rj(jc,2,j3)
!!     wtd(jc,j3)   = td_rj(jc,j3)
!!
!!     wsr(jc,j3) =   sr_rj(jc,j3)
!!     wtr(jc,j3) =   tr_rj(jc,j3)
!!
!!************************************************
!!@endverbatim
!!
!
!
      module cal_coriolis_rlm
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_spheric_rlm_data
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_physical_property
      use t_boundary_params_sph_MHD
      use t_gaunt_coriolis_rlm
      use t_coriolis_terms_rlm
      use t_phys_address
!
      implicit none
!
!
!>      Address for poloidal componenet of Coriolis term
      integer(kind = kint), parameter, private :: ip_rlm_coriolis = 1
!>      Address for horizontal componenet of Coriolis term
      integer(kind = kint), parameter, private :: ih_rlm_coriolis = 2
!>      Address for toroidal componenet of Coriolis term
      integer(kind = kint), parameter, private :: it_rlm_coriolis = 3
!
!>      Address for poloidal componenet of Coriolis term
      integer(kind = kint), parameter, private :: ip_rlm_rot_cor = 4
!>      Address for horizontal componenet of Coriolis term
      integer(kind = kint), parameter, private :: ih_rlm_rot_cor = 5
!>      Address for toroidal componenet of Coriolis term
      integer(kind = kint), parameter, private :: it_rlm_rot_cor = 6
!
      private :: sum_coriolis_rlm, sum_rot_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,     &
     &          fl_prop, sph_bc_U, omega_sph, b_trns, f_trns,           &
     &          leg, gt_cor, n_WR, WR, cor_rlm)
!
      use t_physical_property
      use t_boundary_params_sph_MHD
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(phys_address), intent(in) :: b_trns, f_trns
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: ncomp_trans, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      if(fl_prop%flag_coriolis .eqv. .FALSE.) return
!
!$omp parallel workshare
      cor_rlm%d_cor_rlm(1:sph_rlm%nnod_rlm,                             &
     &                  1:cor_rlm%ncomp_coriolis_rlm) = 0.0d0
!$omp end parallel workshare
!
      if(f_trns%forces%i_Coriolis .gt. izero) then
        call sum_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,           &
     &      fl_prop, sph_bc_U, omega_sph, b_trns,                       &
     &      gt_cor, n_WR, WR, cor_rlm)
      end if
!
      if(f_trns%rot_forces%i_Coriolis .gt. izero) then
        call sum_rot_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,       &
     &      fl_prop, sph_bc_U, omega_sph, b_trns,                       &
     &      leg, gt_cor,n_WR, WR, cor_rlm)
      end if
!
      end subroutine s_cal_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      subroutine copy_coriolis_terms_rlm(ncomp_trans, sph_rlm,          &
     &          comm_rlm, f_trns, cor_rlm, n_WS, WS)
!
      use m_sph_communicators
      use sel_spherical_SRs
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(phys_address), intent(in) :: f_trns
      type(coriolis_rlm_data), intent(in) :: cor_rlm
!
      integer(kind = kint), intent(in) :: ncomp_trans, n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
!
      if(f_trns%forces%i_Coriolis .gt. izero) then
        call sel_calypso_to_send_vector                                 &
     &    (ncomp_trans, sph_rlm%nnod_rlm, n_WS,                         &
     &     comm_rlm%nneib_domain, comm_rlm%istack_sr, comm_rlm%item_sr, &
     &     cor_rlm%ncomp_coriolis_rlm, ip_rlm_coriolis,                 &
     &     f_trns%forces%i_Coriolis, cor_rlm%d_cor_rlm(1,1), WS(1))
      end if
!
      if(f_trns%rot_forces%i_Coriolis .gt. izero) then
        call sel_calypso_to_send_vector                                 &
     &    (ncomp_trans, sph_rlm%nnod_rlm, n_WS,                         &
     &     comm_rlm%nneib_domain, comm_rlm%istack_sr, comm_rlm%item_sr, &
     &     cor_rlm%ncomp_coriolis_rlm, ip_rlm_rot_cor,                  &
     &     f_trns%rot_forces%i_Coriolis, cor_rlm%d_cor_rlm(1,1), WS(1))
      end if
!
      end subroutine copy_coriolis_terms_rlm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sum_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,       &
     &          fl_prop, sph_bc_U, omega_sph, b_trns, gt_cor,           &
     &          n_WR, WR, cor_rlm)
!
      use sum_coriolis_terms_rlm
      use sum_boundary_coriolis_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(phys_address), intent(in) :: b_trns
!
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: ncomp_trans, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      call select_sum_coriolis_rlm_10                                   &
     &   (b_trns, sph_rlm, omega_sph, gt_cor, fl_prop%coef_cor,         &
     &    ncomp_trans, n_WR, comm_rlm%irev_sr, WR,                      &
     &    cor_rlm%d_cor_rlm(1,ip_rlm_coriolis),                         &
     &    cor_rlm%d_cor_rlm(1,ih_rlm_coriolis),                         &
     &    cor_rlm%d_cor_rlm(1,it_rlm_coriolis))
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call inner_core_rot_z_coriolis_rlm(b_trns,                      &
     &      sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,      &
     &      sph_rlm%radius_1d_rlm_r, omega_sph%ws_rlm,                  &
     &      fl_prop%coef_cor, ncomp_trans, n_WR, comm_rlm%irev_sr, WR,  &
     &      cor_rlm%idx_rlm_ICB, cor_rlm%idx_rlm_degree_one,            &
     &      cor_rlm%d_cor_rlm(1,it_rlm_coriolis))
      end if
!
      end subroutine sum_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      subroutine sum_rot_coriolis_rlm(ncomp_trans, sph_rlm, comm_rlm,   &
     &          fl_prop, sph_bc_U, omega_sph, b_trns, leg, gt_cor,      &
     &          n_WR, WR, cor_rlm)
!
      use t_physical_property
      use t_boundary_params_sph_MHD
      use sum_rot_coriolis_terms_rlm
      use sum_boundary_coriolis_rlm
!      use sum_div_coriolis_terms_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rotation), intent(in) :: omega_sph
      type(phys_address), intent(in) :: b_trns
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
!
      integer(kind = kint), intent(in) :: ncomp_trans, n_WR
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      call select_sum_rot_coriolis_rlm_10(b_trns,                       &
     &    sph_rlm, leg, omega_sph, gt_cor, fl_prop%coef_cor,            &
     &    ncomp_trans, n_WR, comm_rlm%irev_sr, WR,                      &
     &    cor_rlm%d_cor_rlm(1,ip_rlm_rot_cor),                          &
     &    cor_rlm%d_cor_rlm(1,it_rlm_rot_cor))
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call inner_core_rot_z_coriolis_rlm(b_trns,                      &
     &      sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%istep_rlm,      &
     &      sph_rlm%radius_1d_rlm_r, omega_sph%ws_rlm,                  &
     &      fl_prop%coef_cor, ncomp_trans, n_WR, comm_rlm%irev_sr, WR,  &
     &      cor_rlm%idx_rlm_ICB, cor_rlm%idx_rlm_degree_one,            &
     &      cor_rlm%d_cor_rlm(1,ip_rlm_rot_cor))
      end if
!
!      call sum_r_coriolis_bc_rlm_10(b_trns,                            &
!     &    sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%idx_gl_1d_rlm_j, &
!     &    omega_sph%ws_rlm, fl_prop%coef_cor,                          &
!     &    gt_cor%jgi_rlm, gt_cor%jei_rlm, gt_cor%sr_rlm, gt_cor%tr_rlm,&
!     &    ncomp_trans, kr_in_U_rlm, n_WR, comm_rlm%irev_sr,            &
!     &    WR, cor_rlm%d_cor_in_rlm)
!      call sum_r_coriolis_bc_rlm_10(b_trns,                            &
!     &    sph_rlm%nnod_rlm, sph_rlm%nidx_rlm, sph_rlm%idx_gl_1d_rlm_j, &
!     &    omega_sph%ws_rlm, fl_prop%coef_cor,                          &
!     &    gt_cor%jgi_rlm, gt_cor%jei_rlm, gt_cor%sr_rlm, gt_cor%tr_rlm,&
!     &    ncomp_trans, kr_out_U_rlm, n_WR, comm_rlm%irev_sr,           &
!     &    WR, cor_rlm%d_cor_out_rlm)
!
      end subroutine sum_rot_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      end module cal_coriolis_rlm
