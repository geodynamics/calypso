!> @file  const_sph_diffusion.f90
!!      module const_sph_diffusion
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate diffusion terms explicitly
!!
!!@verbatim
!!      subroutine const_sph_viscous_diffusion                          &
!!     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,&
!!     &          g_sph_rj, coef_diffuse, is_velo, it_velo, is_viscous, &
!!     &          rj_fld)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!      subroutine const_sph_vorticirty_diffusion                       &
!!     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,&
!!     &          g_sph_rj, coef_diffuse, is_vort, is_w_diffuse, rj_fld)
!!     &          is_vort, is_w_diffuse, rj_fld)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse
!!
!!      subroutine const_sph_magnetic_diffusion(sph_rj, r_2nd, sph_bc_B,&
!!     &          g_sph_rj, coef_diffuse, is_magne, is_ohmic, rj_fld)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!
!!      subroutine const_sph_scalar_diffusion                           &
!!     &         (sph_rj, r_2nd, sph_bc, fdm2_center,                   &
!!     &          g_sph_rj, coef_diffuse, is_fld, is_diffuse, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity field
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal voeticity
!!@param is_viscous  Spherical hermonics data address
!!                   for poloidal visous diffusion
!!@param is_w_diffuse  Spherical hermonics data address
!!                   for poloidal diffusion term for vorticity
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_ohmic    Spherical hermonics data address
!!                   for poloidal ohmic dissipation
!!
!!@param is_fld       Input spectr field address
!!@param is_diffuse   Input spectr diffusiton term address
!!
!!@param rj_fld     Structure of spectr data
!
      module const_sph_diffusion
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_coef_fdm2_MHD_boundaries
!
      use cal_sph_exp_diffusion
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_viscous_diffusion                            &
     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,  &
     &          g_sph_rj, coef_diffuse, is_velo, it_velo, is_viscous,   &
     &          rj_fld)
!
      use cal_sph_exp_1st_diff
      use cal_sph_exp_fixed_scalar
      use select_exp_velocity_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: is_velo, it_velo, is_viscous
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_viscous + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_diffuse, is_velo, is_viscous,                            &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(2)%dmat, &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    is_viscous, idp_diffuse, sph_rj%nidx_rj, r_2nd%fdm(1)%dmat,   &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_bc_sph_viscous_diffusion(sph_rj, r_2nd, sph_bc_U,        &
     &    fdm2_free_ICB, fdm2_free_CMB, g_sph_rj, coef_diffuse,         &
     &    is_velo, it_velo, is_viscous, idp_diffuse, rj_fld)
!
      end subroutine const_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticirty_diffusion                         &
     &         (sph_rj, r_2nd, sph_bc_U, fdm2_free_ICB, fdm2_free_CMB,  &
     &          g_sph_rj, coef_diffuse, is_vort, is_w_diffuse, rj_fld)
!
      use cal_sph_exp_1st_diff
      use select_exp_velocity_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(fdm2_free_slip), intent(in) :: fdm2_free_ICB, fdm2_free_CMB
!
      integer(kind = kint), intent(in) :: is_vort, is_w_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_w_diffuse + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_diffuse, is_vort, is_w_diffuse,                          &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(2)%dmat, &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    is_w_diffuse, idp_diffuse, sph_rj%nidx_rj, r_2nd%fdm(1)%dmat, &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_bc_sph_vort_diffusion(sph_rj, r_2nd, sph_bc_U,           &
     &    fdm2_free_ICB, fdm2_free_CMB, g_sph_rj,                       &
     &    coef_diffuse, is_vort, is_w_diffuse, idp_diffuse, rj_fld)
!
      end subroutine const_sph_vorticirty_diffusion
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_magnetic_diffusion(sph_rj, r_2nd, sph_bc_B,  &
     &          g_sph_rj, coef_diffuse, is_magne, is_ohmic, rj_fld)
!
      use cal_sph_exp_1st_diff
      use select_exp_magne_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_ohmic
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_ohmic + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &     coef_diffuse, is_magne, is_ohmic, sph_rj%nidx_rj,            &
     &     sph_rj%ar_1d_rj, g_sph_rj, r_2nd%fdm(2)%dmat,                &
     &     rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_sph_nod_vect_dr_2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    is_ohmic, idp_diffuse, sph_rj%nidx_rj, r_2nd%fdm(1)%dmat,     &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_bc_sph_magnetic_diffusion(sph_rj, r_2nd, sph_bc_B,       &
     &    g_sph_rj, coef_diffuse, is_magne, is_ohmic, idp_diffuse,      &
     &    rj_fld)
!
      end subroutine const_sph_magnetic_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_diffusion                             &
     &         (sph_rj, r_2nd, sph_bc, fdm2_center,                     &
     &          g_sph_rj, coef_diffuse, is_fld, is_diffuse, rj_fld)
!
      use t_coef_fdm2_MHD_boundaries
      use select_exp_scalar_bc
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_boundary_type), intent(in) :: sph_bc
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: coef_diffuse
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_nod_scalar_diffuse2(sph_bc%kr_in, sph_bc%kr_out,     &
     &    coef_diffuse, is_fld, is_diffuse,                             &
     &    sph_rj%nidx_rj, sph_rj%ar_1d_rj, g_sph_rj,                    &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat,                         &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      call sel_bc_sph_scalar_diffusion(sph_rj, sph_bc, fdm2_center,     &
     &    g_sph_rj, coef_diffuse, is_fld, is_diffuse, rj_fld)
!
      end subroutine const_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      end module const_sph_diffusion
