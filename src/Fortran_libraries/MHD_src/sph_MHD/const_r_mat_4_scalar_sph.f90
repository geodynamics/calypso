!>@file   const_r_mat_4_scalar_sph.f90
!!@brief  module const_r_mat_4_scalar_sph
!!
!!@date  Programmed by H.Matsui on Apr., 2009
!
!>@brief Construct matrix for time evolution of scalar fields
!!
!!@verbatim
!!      subroutine const_sph_radial_mat_4_press                         &
!!     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, fdm2_center,        &
!!     &          g_sph_rj, band_p_poisson)
!!      subroutine const_sph_radial_mat_4_scalar(mat_name, dt, sph_rj,  &
!!     &          g_sph_rj, r_2nd, fdm2_center, scl_prop, sph_bc, bcs_S,&
!!     &          k_ratio, dk_dr, band_s_evo)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(scalar_property), intent(in) :: scl_prop
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_boundary_data) :: bcs_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        real(kind = kreal), intent(in) :: dt
!!        real(kind=kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!!        real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
!!        character(len=kchara), intent(in) :: mat_name
!!        type(band_matrices_type), intent(inout) :: band_p_poisson
!!        type(band_matrices_type), intent(inout) :: band_s_evo
!!@endverbatim
!
      module const_r_mat_4_scalar_sph
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
!
      use t_physical_property
      use t_scalar_property
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_sph_matrices
      use t_sph_center_matrix
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_centre
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_radial_mat_4_press                           &
     &         (sph_rj, r_2nd, fl_prop, sph_bc_U, fdm2_center,          &
     &          g_sph_rj, band_p_poisson)
!
      use m_ludcmp_3band
      use set_sph_unit_radial_mat
      use cal_inner_core_rotation
      use center_sph_matrices
      use mat_product_3band_mul
      use add_sph_scalar_radial_mat
      use sel_sph_r_mat_scalar_bc
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(band_matrices_type), intent(inout) :: band_p_poisson
!
      real(kind = kreal) :: coef_p
!
!
      write(band_p_poisson%mat_name,'(a)') 'pressure_poisson'
      coef_p = - fl_prop%coef_press
!
      call alloc_band_mat_sph(ithree, sph_rj, band_p_poisson)
!
      call set_unit_mat_4_poisson                                       &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                         &
     &    sph_bc_U%kr_in, sph_bc_U%kr_out, band_p_poisson%mat)
      call add_scalar_poisson_mat_sph                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,        &
     &    g_sph_rj, sph_bc_U%kr_in, sph_bc_U%kr_out, coef_p,            &
     &    r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_p_poisson%mat)
!
      call sel_sph_radial_mat_press_bc(sph_rj, sph_bc_U, fdm2_center,   &
     &    g_sph_rj, coef_p, band_p_poisson)
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_p_poisson)
!
      end subroutine const_sph_radial_mat_4_press
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_radial_mat_4_scalar(mat_name, dt, sph_rj,    &
     &          g_sph_rj, r_2nd, fdm2_center, scl_prop, sph_bc, bcs_S,  &
     &          k_ratio, dk_dr, band_s_evo)
!
      use m_ludcmp_3band
      use set_sph_unit_radial_mat
      use center_sph_matrices
      use add_sph_scalar_radial_mat
      use sel_sph_r_mat_scalar_bc
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(scalar_property), intent(in) :: scl_prop
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_boundary_data) :: bcs_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
      real(kind = kreal), intent(in) :: k_ratio(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: dk_dr(0:sph_rj%nidx_rj(1))
      character(len=kchara), intent(in) :: mat_name
!
      type(band_matrices_type), intent(inout) :: band_s_evo
!
      real(kind = kreal) :: coef
!
!
      band_s_evo%mat_name = mat_name
      call alloc_band_mat_sph(ithree, sph_rj, band_s_evo)
      call set_unit_on_diag(band_s_evo)
!
      if(scl_prop%coef_advect .eq. zero) then
        coef = one
        call set_unit_mat_4_poisson                                     &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2),                       &
     &      sph_bc%kr_in, sph_bc%kr_out, band_s_evo%mat)
      else
        coef = scl_prop%coef_imp * scl_prop%coef_diffuse * dt
        call set_unit_mat_4_time_evo                                    &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), band_s_evo%mat)
      end if
!
      if(scl_prop%flag_val_diffuse) then
        call add_scalar_r_diffuse_mat_sph                               &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,      &
     &      g_sph_rj, sph_bc%kr_in, sph_bc%kr_out,                      &
     &      coef, k_ratio(1), dk_dr(1),                                 &
     &      r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s_evo%mat)
      else
        call add_scalar_poisson_mat_sph                                 &
     &     (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), sph_rj%ar_1d_rj,      &
     &      g_sph_rj, sph_bc%kr_in, sph_bc%kr_out, coef,                &
     &      r_2nd%fdm(1)%dmat, r_2nd%fdm(2)%dmat, band_s_evo%mat)
      end if
!
      call sel_sph_radial_mat_scalar_ICB(scl_prop%flag_val_diffuse,     &
     &    sph_rj, sph_bc, bcs_S, fdm2_center, g_sph_rj,                 &
     &    coef, k_ratio(sph_bc%kr_in), dk_dr(sph_bc%kr_in), band_s_evo)
      call sel_sph_radial_mat_scalar_CMB(scl_prop%flag_val_diffuse,     &
     &    sph_rj, sph_bc, bcs_S, fdm2_center, g_sph_rj,                 &
     &    coef, k_ratio(sph_bc%kr_out), band_s_evo)
!
      call ludcmp_3band_mul_t                                           &
     &   (np_smp, sph_rj%istack_rj_j_smp, band_s_evo)
!
      end subroutine const_sph_radial_mat_4_scalar
!
! -----------------------------------------------------------------------
!
      end module const_r_mat_4_scalar_sph
