!> @file  const_sph_rotation.f90
!!      module const_sph_rotation
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate curl of fields
!!
!!@verbatim
!!      subroutine const_sph_vorticity(sph_bc_U, is_velo, is_vort)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_sph_current(sph_bc_B, is_magne, is_current)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_sph_rotation_uxb(isph_bc_B, s_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_rotation_no_bc(sph_bc, is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_force_rot2(sph_bc_U, is_fld, is_rot)
!!        Input:    is_fld, it_fld
!!        Solution: is_rot, it_rot, ids_rot
!!
!!      subroutine const_sph_viscous_by_vort2(sph_bc_U, coef_diffuse,   &
!!     &          is_velo, is_vort, is_viscous)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!
!!      subroutine const_sph_mag_diffuse_by_j(sph_bc_B, coef_diffuse,   &
!!     &          is_magne, is_current, is_ohmic)
!!        Input:    ipol%i_current, itor%i_current
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!
!!      subroutine const_sph_viscous_by_vort2(sph_bc_U, coef_diffuse,   &
!!     &          is_velo, is_vort, is_viscous)
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!@param sph_bc  Structure for basic boundary condition parameters
!!
!!@param kr_inside     Radial ID for inner boundary
!!@param kr_outside    RaAdial ID for outer boundary
!!@param coef_fdm_fix_in_2(0:2,3)
!!             Finite difference matrix for inner boundary
!!@param coef_fdm_fix_out_2(0:2,3)
!!             Finite difference matrix for outer boundary
!!
!!@param coef_diffuse   Diffusion coefficient
!!
!!@param is_fld      Input spectr field address
!!@param is_rot      Address of curl of field
!!
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity field
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal voeticity
!!@param is_viscous  Spherical hermonics data address
!!                   for poloidal visous diffusion
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current  Spherical hermonics data address
!!                   for poloidal current density
!!@param is_ohmic    Spherical hermonics data address
!!                   for poloidal ohmic dissipation
!
      module const_sph_rotation
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use cal_sph_exp_rotation
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticity(sph_bc_U, is_velo, is_vort)
!
      use t_boundary_params_sph_MHD
      use select_exp_velocity_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
!
      call sel_bc_sph_vorticity(sph_bc_U, is_velo, is_vort)
!
      call cal_sph_nod_vect_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    is_velo, is_vort)
!
      end subroutine const_sph_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_current(sph_bc_B, is_magne, is_current)
!
      use t_boundary_params_sph_MHD
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
!
!
      call sel_bc_sph_current(sph_bc_B, is_magne, is_current)
!
      call cal_sph_nod_vect_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    is_magne, is_current)
!
      end subroutine const_sph_current
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_uxb(sph_bc_B, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      call sel_bc_sph_rotation_uxb(sph_bc_B, is_fld, is_rot)
!
      call cal_sph_nod_vect_w_div_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out, &
     &    is_fld, is_rot)
!
      end subroutine const_sph_rotation_uxb
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_rotation_no_bc(sph_bc, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_none_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
!
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      call cal_sph_nod_nobc_in_rot2(nidx_rj(2),                         &
     &    sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,          &
     &    is_fld, is_rot)
      call cal_sph_nod_nobc_out_rot2(nidx_rj(2),                        &
     &    sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,         &
     &    is_fld, is_rot)
!
      call cal_sph_nod_vect_rot2(sph_bc%kr_in, sph_bc%kr_out,           &
     &     is_fld, is_rot)
!
      end subroutine const_sph_rotation_no_bc
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_force_rot2(sph_bc_U, is_fld, is_rot)
!
      use t_boundary_params_sph_MHD
      use select_exp_velocity_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_fld, is_rot
!
!
      call sel_bc_sph_vorticity(sph_bc_U, is_fld, is_rot)
!
      call cal_sph_nod_vect_w_div_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    is_fld, is_rot)
!
      end subroutine const_sph_force_rot2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_viscous_by_vort2(sph_bc_U, coef_diffuse,     &
     &          is_velo, is_vort, is_viscous)
!
      use t_boundary_params_sph_MHD
      use const_sph_diffusion
      use select_exp_velocity_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, is_vort, is_viscous
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_viscous + 1
!
      call cal_sph_nod_diffuse_by_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &    coef_diffuse, is_vort, is_viscous)
!
      call sel_bc_sph_viscous_diffusion(sph_bc_U, coef_diffuse,         &
     &    is_velo, is_vort, is_viscous, idp_diffuse)
!
      end subroutine const_sph_viscous_by_vort2
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_mag_diffuse_by_j(sph_bc_B, coef_diffuse,     &
     &          is_magne, is_current, is_ohmic)
!
      use t_boundary_params_sph_MHD
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
      integer(kind = kint), intent(in) :: is_ohmic
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_ohmic + 1
!
      call cal_sph_nod_diffuse_by_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out, &
     &    coef_diffuse, is_current, is_ohmic)
!
      call sel_bc_sph_magnetic_diffusion(sph_bc_B, coef_diffuse,        &
     &    is_magne, is_ohmic, idp_diffuse)
!
      end subroutine const_sph_mag_diffuse_by_j
!
! -----------------------------------------------------------------------
!
      end module const_sph_rotation
