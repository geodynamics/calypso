!>@file   init_reference_scalar.f90
!!@brief  module init_reference_scalar
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!!@n    Modified in Jan, 2023
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine s_init_reference_scalar(takepiro, sph_params, sph_rj,&
!!     &          r_2nd, sc_prop, sph_bc_S, fdm2_center, mat_name,      &
!!     &          ref_param, iref_scalar, iref_grad,                    &
!!     &          iref_source, ref_field, bcs_S, flag_write_ref)
!!        character(len=kchara), intent(in) :: mat_name
!!        integer(kind = kint), intent(in) :: iref_scalar, iref_grad
!!        integer(kind = kint), intent(in) :: iref_source
!!        type(takepiro_model_param), intent(in) :: takepiro
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(phys_data), intent(in) :: rj_fld
!!        type(scalar_property), intent(in) :: sc_prop
!!        type(sph_boundary_type), intent(in) :: sph_bc_S
!!        type(fdm2_center_mat), intent(in) :: fdm2_center
!!        type(reference_scalar_param), intent(inout) :: ref_param
!!        type(phys_data), intent(inout) :: ref_field
!!        type(sph_scalar_boundary_data), intent(inout) :: bcs_S
!!        logical, intent(inout) :: flag_write_ref
!!@endverbatim
!!
      module init_reference_scalar
!
      use m_precision
      use m_constants
      use m_spheric_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_fdm_coefs
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_reference_scalar_param
      use t_sph_matrix
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine s_init_reference_scalar(takepiro, sph_params, sph_rj,  &
     &          r_2nd, sc_prop, sph_bc_S, fdm2_center, mat_name,        &
     &          ref_param, iref_scalar, iref_grad,                      &
     &          iref_source, ref_field, bcs_S, flag_write_ref)
!
      use set_reference_sph_mhd
      use set_reference_temp_sph
      use const_r_mat_4_scalar_sph
      use const_radial_references
      use const_diffusive_profile
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: mat_name
      integer(kind = kint), intent(in) :: iref_scalar, iref_grad
      integer(kind = kint), intent(in) :: iref_source
!
      type(takepiro_model_param), intent(in) :: takepiro
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
!      type(phys_data), intent(in) :: rj_fld
      type(scalar_property), intent(in) :: sc_prop
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
      type(reference_scalar_param), intent(inout) :: ref_param
      type(phys_data), intent(inout) :: ref_field
      type(sph_scalar_boundary_data), intent(inout) :: bcs_S
      logical, intent(inout) :: flag_write_ref
!
      character(len=kchara) :: file_name
      type(band_matrix_type) :: band_s00_poisson
!
!
      if (ref_param%iflag_reference .ne. id_sphere_ref_temp             &
     & .and. ref_param%iflag_reference .ne. id_takepiro_temp            &
     & .and. ref_param%iflag_reference .ne. id_numerical_solution       &
     &    ) return
      flag_write_ref = .TRUE.
!
!      Set reference temperature and adjust boundary conditions
      if (ref_param%iflag_reference .eq. id_sphere_ref_temp) then
        if(iflag_debug .gt. 0) write(*,*) 'set_ref_temp_sph_mhd'
        call set_ref_temp_sph_mhd                                       &
     &   (ref_param%low_value, ref_param%depth_top,                     &
     &    ref_param%high_value, ref_param%depth_bottom,                 &
     &    sph_rj%nidx_rj, sph_rj%radius_1d_rj_r, sph_rj%ar_1d_rj,       &
     &    ref_field%d_fld(1,iref_scalar), ref_field%d_fld(1,iref_grad))
      else if(ref_param%iflag_reference .eq. id_takepiro_temp) then
        call set_stratified_sph_mhd(takepiro%stratified_sigma,          &
     &    takepiro%stratified_width, takepiro%stratified_outer_r,       &
     &    sph_rj%nidx_rj, sph_params%radius_ICB, sph_params%radius_CMB, &
     &    sph_params%nlayer_ICB, sph_params%nlayer_CMB,                 &
     &    sph_rj%radius_1d_rj_r, ref_field%d_fld(1,iref_scalar),        &
     &    ref_field%d_fld(1,iref_grad))
      else if(ref_param%iflag_reference                                 &
     &                             .eq. id_numerical_solution) then
        call const_r_mat00_scalar_sph                                   &
     &     (mat_name, sc_prop%diffusie_reduction_ICB,                   &
     &      sph_params, sph_rj, r_2nd, sph_bc_S, fdm2_center,           &
     &      band_s00_poisson)
        file_name = add_dat_extension(mat_name)
        call const_diffusive_profiles(sph_rj, sc_prop, sph_bc_S, bcs_S, &
     &      fdm2_center, r_2nd, band_s00_poisson,                       &
     &      iref_scalar, iref_grad, iref_source, ref_field)
        call dealloc_band_matrix(band_s00_poisson)
      else if(ref_param%iflag_reference .eq. id_read_file) then
        call gradient_of_radial_reference                               &
     &     (sph_rj, r_2nd, sph_bc_S, bcs_S, fdm2_center,                &
     &      ref_field%d_fld(1,iref_scalar),                             &
     &      ref_field%d_fld(1,iref_grad))
      else
        call no_ref_temp_sph_mhd(sph_rj%nidx_rj(1),                     &
     &      sph_params%radius_ICB, sph_params%radius_CMB,               &
     &      ref_param%depth_top, ref_param%depth_bottom,                &
     &      ref_field%d_fld(1,iref_scalar),                             &
     &      ref_field%d_fld(1,iref_grad))
      end if
!
      call adjust_sph_temp_bc_by_reftemp                                &
     &   (sph_rj%idx_rj_degree_zero, sph_rj%nidx_rj(1),                 &
     &    ref_field%d_fld(1,iref_scalar),                               &
     &    ref_field%d_fld(1,iref_grad),                                 &
     &    sph_bc_S, bcs_S%ICB_Sspec, bcs_S%CMB_Sspec,                   &
     &    bcs_S%ICB_Sevo, bcs_S%CMB_Sevo)
!
!      if (i_ref*i_gref .gt. izero) then
!        call set_reftemp_4_sph(sph_rj%idx_rj_degree_zero,              &
!     &    sph_rj%inod_rj_center, sph_rj%nnod_rj, sph_rj%nidx_rj,       &
!     &    ref_field%d_fld(1,iref_scalar), ref_field%d_fld(1,iref_grad),&
!     &    rj_fld%d_fld(1,i_ref), rj_fld%d_fld(1,i_gref))
!      end if
!
      end subroutine s_init_reference_scalar
!
! -----------------------------------------------------------------------
!
      end module init_reference_scalar
