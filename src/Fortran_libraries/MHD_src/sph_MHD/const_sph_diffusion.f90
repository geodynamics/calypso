!> @file  const_sph_diffusion.f90
!!      module const_sph_diffusion
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate diffusion terms explicitly
!!
!!@verbatim
!!      subroutine const_sph_viscous_diffusion(sph_bc_U, coef_diffuse,  &
!!     &          is_velo, is_viscous)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
!!      subroutine const_sph_vorticirty_diffusion(sph_bc_U,             &
!!     &          coef_diffuse, is_vort, is_w_diffuse)
!!        Input:    ipol%i_vort, itor%i_vort
!!        Solution: ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse
!!
!!      subroutine const_sph_magnetic_diffusion(sph_bc_B, coef_diffuse, &
!!     &          is_magne, is_ohmic)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
!!
!!      subroutine const_sph_scalar_diffusion(sph_bc, coef_diffuse,     &
!!     &          is_fld, is_diffuse)
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
!
      module const_sph_diffusion
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use cal_sph_exp_diffusion
      use cal_sph_exp_1st_diff
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_viscous_diffusion(sph_bc_U, coef_diffuse,    &
     &          is_velo, it_velo, is_viscous)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_fixed_scalar
      use select_exp_velocity_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_velo, it_velo, is_viscous
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_viscous + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_diffuse, is_velo, is_viscous)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    d_rj(1,is_viscous), d_rj(1,idp_diffuse) )
!
      call sel_bc_sph_viscous_diffusion(sph_bc_U, coef_diffuse,         &
     &    is_velo, it_velo, is_viscous, idp_diffuse)
!
      end subroutine const_sph_viscous_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_vorticirty_diffusion(sph_bc_U,               &
     &          coef_diffuse, is_vort, is_w_diffuse)
!
      use t_boundary_params_sph_MHD
      use select_exp_velocity_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_vort, is_w_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_w_diffuse + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    coef_diffuse, is_vort, is_w_diffuse)
      call cal_sph_nod_vect_dr_2(sph_bc_U%kr_in, sph_bc_U%kr_out,       &
     &    d_rj(1,is_w_diffuse), d_rj(1,idp_diffuse) )
!
      call sel_bc_sph_vort_diffusion(sph_bc_U, coef_diffuse,            &
     &          is_vort, is_w_diffuse, idp_diffuse)
!
      end subroutine const_sph_vorticirty_diffusion
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_magnetic_diffusion(sph_bc_B, coef_diffuse,   &
     &          is_magne, is_ohmic)
!
      use t_boundary_params_sph_MHD
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_ohmic
      real(kind = kreal), intent(in) :: coef_diffuse
!
      integer(kind = kint) :: idp_diffuse
!
!
      idp_diffuse = is_ohmic + 1
!
      call cal_sph_nod_vect_diffuse2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &     coef_diffuse, is_magne, is_ohmic)
      call cal_sph_nod_vect_dr_2(sph_bc_B%kr_in, sph_bc_B%kr_out,       &
     &    d_rj(1,is_ohmic), d_rj(1,idp_diffuse) )
!
      call sel_bc_sph_magnetic_diffusion(sph_bc_B, coef_diffuse,        &
     &    is_magne, is_ohmic, idp_diffuse)
!
      end subroutine const_sph_magnetic_diffusion
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_diffusion(sph_bc, coef_diffuse,       &
     &          is_fld, is_diffuse)
!
      use t_boundary_params_sph_MHD
      use select_exp_scalar_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_diffuse
      real(kind = kreal), intent(in) :: coef_diffuse
!
!
      call cal_sph_nod_scalar_diffuse2(sph_bc%kr_in, sph_bc%kr_out,     &
     &    coef_diffuse, is_fld, is_diffuse)
!
      call sel_bc_sph_scalar_diffusion(sph_bc, coef_diffuse,            &
     &    is_fld, is_diffuse)
!
      end subroutine const_sph_scalar_diffusion
!
! -----------------------------------------------------------------------
!
      end module const_sph_diffusion
