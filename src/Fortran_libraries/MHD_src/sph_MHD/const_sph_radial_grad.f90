!> @file  const_sph_radial_grad.f90
!!      module const_sph_radial_grad
!!
!! @author  H. Matsui
!! @date Programmed in Oct. 2009
!
!> @brief Evaluate radial delivatives
!!
!!@verbatim
!!      subroutine const_radial_grad_scalar(sph_bc, is_fld, is_grad)
!!        Input:    is_fld
!!        Solution: is_grad
!!
!!      subroutine const_grad_vp_and_vorticity(is_velo, is_vort)
!!        Input:    ipol%i_velo, itor%i_velo
!!        Solution: idpdr%i_velo, ipol%i_vort, itor%i_vort, idpdr%i_vort
!!
!!      subroutine const_grad_bp_and_current(sph_bc_B,                  &
!!     &           is_magne, is_current)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne,
!!                  ipol%i_current, itor%i_current, idpdr%i_current
!!
!!      subroutine const_grad_poloidal_moment(is_fld)
!!        Input:    is_fld, is_fld+2
!!        Solution: is_fld+1
!!
!!      subroutine const_grad_poloidal_magne(sph_bc_B, is_magne)
!!        Input:    ipol%i_magne, itor%i_magne
!!        Solution: idpdr%i_magne
!!
!!      subroutine const_pressure_gradient(sph_bc_U, is_press, is_grad)
!!        Input:    ipol%i_press
!!        Solution: ipol%i_press_grad
!!
!!      subroutine const_sph_gradient_no_bc(sph_bc, is_fld, is_grad)
!!        Input:    is_fld
!!        Solution: is_grad, it_grad, ids_grad
!!@endverbatim
!!
!!@param sph_bc  Structure for basic boundary condition parameters
!!@param sph_bc_B  Structure for basic boundary condition parameters
!!                 for magnetic field
!!@param sph_bc_U  Structure for basic boundary condition parameters
!!                 for velocity
!!
!!@param is_fld      Spherical hermonics data address for input vector
!!@param is_grad     Spherical hermonics data address for gradient
!!@param is_velo     Spherical hermonics data address
!!                   for poloidal velocity
!!@param is_vort     Spherical hermonics data address
!!                   for poloidal vorticity
!!@param is_magne    Spherical hermonics data address
!!                   for poloidal magnetic field
!!@param is_current  Spherical hermonics data address
!!                   for poloidal current density
!!@param is_press    Spherical hermonics data address
!!                   for pressure
!
      module const_sph_radial_grad
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
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
      subroutine const_radial_grad_scalar(sph_bc, is_fld, is_grad)
!
      use t_boundary_params_sph_MHD
      use select_exp_scalar_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &     d_rj(1,is_fld), d_rj(1,is_grad) )
      call sel_bc_radial_grad_scalar(sph_bc, is_fld, is_grad)
      call normalize_sph_average_grad(d_rj(1,is_grad))
!
      end subroutine const_radial_grad_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity(is_velo, is_vort)
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_rotation
      use select_exp_velocity_bc
!
      integer(kind = kint), intent(in) :: is_velo, is_vort
!
!
      call sel_bc_grad_vp_and_vorticity(is_velo, is_vort)
      call cal_sph_diff_pol_and_rot2(sph_bc_U%kr_in, sph_bc_U%kr_out,   &
     &    is_velo, is_vort)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_bp_and_current(sph_bc_B,                    &
     &           is_magne, is_current)
!
      use t_boundary_params_sph_MHD
      use extend_potential_field
      use cal_sph_exp_rotation
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne, is_current
!
!
      call sel_bc_grad_bp_and_current(sph_bc_B, is_magne, is_current)
      call cal_sph_diff_pol_and_rot2(sph_bc_B%kr_in, sph_bc_B%kr_out,   &
     &    is_magne, is_current)
!
!      Extend potential field
      call ext_outside_potential_with_j                                 &
     &   (sph_bc_B%kr_out, d_rj(1,is_magne), d_rj(1,is_current))
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j                                &
     &     (sph_bc_B%kr_in, d_rj(1,is_magne), d_rj(1,is_current))
      end if
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_moment(is_fld)
!
      use m_boundary_params_sph_MHD
      use cal_sph_exp_rotation
      use select_exp_velocity_bc
!
      integer(kind = kint), intent(in) :: is_fld
!
!
      call sel_bc_grad_poloidal_moment(is_fld)
      call cal_sph_diff_poloidal2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    is_fld)
!
      end subroutine const_grad_poloidal_moment
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_magne(sph_bc_B, is_magne)
!
      use t_boundary_params_sph_MHD
      use extend_potential_field
      use cal_sph_exp_rotation
      use select_exp_magne_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      integer(kind = kint), intent(in) :: is_magne
!
!
      call sel_bc_grad_poloidal_magne(sph_bc_B, is_magne)
!
      call cal_sph_diff_poloidal2(sph_bc_B%kr_in, sph_bc_B%kr_out,      &
     &    is_magne)
!
!      Extend potential field
      call ext_outside_potential(sph_bc_B%kr_out, d_rj(1,is_magne))
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call ext_inside_potential(sph_bc_B%kr_in, d_rj(1,is_magne))
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_pressure_gradient(sph_bc_U, is_press, is_grad)
!
      use m_physical_property
      use t_boundary_params_sph_MHD
      use cal_sph_exp_1st_diff
      use cal_sph_exp_nod_none_bc
      use const_wz_coriolis_rtp
!
      type(sph_boundary_type), intent(in) :: sph_bc_U
      integer(kind = kint), intent(in) :: is_press, is_grad
!
!
      call cal_sph_nod_gradient_2(sph_bc_U%kr_in, sph_bc_U%kr_out,      &
     &    d_rj(1,is_press), d_rj(1,is_grad) )
      call normalize_sph_average_grad(d_rj(1,is_grad))
!
      call delete_bc_rj_vector(nidx_rj(2), sph_bc_U%kr_in,  is_grad)
      call delete_bc_rj_vector(nidx_rj(2), sph_bc_U%kr_out, is_grad)
!
!$omp parallel
      call ovwrt_rj_coef_prod_vect_smp( (-coef_press), is_grad)
!$omp end parallel
!
      end subroutine const_pressure_gradient
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_gradient_no_bc(sph_bc, is_fld, is_grad)
!
      use t_boundary_params_sph_MHD
      use cal_sph_exp_nod_none_bc
!
      type(sph_boundary_type), intent(in) :: sph_bc
!
      integer(kind = kint), intent(in) :: is_fld, is_grad
!
!
      call cal_sph_nod_nobc_in_grad2(nidx_rj(2),                        &
     &    sph_bc%kr_in, sph_bc%r_ICB, sph_bc%fdm2_fix_fld_ICB,          &
     &    is_fld, is_grad)
      call cal_sph_nod_nobc_out_grad2(nidx_rj(2),                       &
     &    sph_bc%kr_out, sph_bc%r_CMB, sph_bc%fdm2_fix_fld_CMB,         &
     &    is_fld, is_grad)
!
      call cal_sph_nod_gradient_2(sph_bc%kr_in, sph_bc%kr_out,          &
     &   d_rj(1,is_fld), d_rj(1,is_grad))
      call normalize_sph_average_grad(d_rj(1,is_grad))
!
      end subroutine const_sph_gradient_no_bc
!
! -----------------------------------------------------------------------
!
      end module const_sph_radial_grad
