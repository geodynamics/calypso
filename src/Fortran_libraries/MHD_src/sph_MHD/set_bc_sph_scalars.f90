!>@file   set_bc_sph_scalars.f90
!!@brief  module set_bc_sph_scalars
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for scalar fields
!!
!!@verbatim
!!      subroutine set_sph_bc_temp_sph
!!      subroutine set_sph_bc_composition_sph
!!      subroutine adjust_sph_temp_bc_by_reftemp
!!@endverbatim
!
      module set_bc_sph_scalars
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_control_params_sph_MHD
      use m_boundary_condition_IDs
      use m_phys_labels
!
      implicit none
!
      private :: set_homogenious_scalar_bc, set_homogenious_grad_bc
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_temp_sph
!
      use m_spheric_parameter
      use m_boundary_params_sph_MHD
      use m_bc_data_list
      use m_surf_data_list
      use m_constants
      use m_sph_spectr_data
      use m_sph_boundary_input_data
!
!
      integer(kind = kint) :: i
!
!
      call allocate_temp_bc_array( nidx_rj(2) )
!
      do i = 1, h_flux_surf%num_bc
        if ( h_flux_surf%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        h_flux_surf%bc_name(i), h_flux_surf%bc_magnitude(i),      &
     &        nidx_rj(2), h_flux_ICB_bc, sph_bc_T%iflag_icb)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        h_flux_surf%bc_name(i), h_flux_surf%bc_magnitude(i),      &
     &        nidx_rj(2), h_flux_CMB_bc, sph_bc_T%iflag_cmb)
        else if (h_flux_surf%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_temp, ICB_nod_grp_name, ICB_sf_grp_name,              &
     &        nidx_rj(2), h_flux_ICB_bc, sph_bc_T%iflag_icb)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_temp, CMB_nod_grp_name, CMB_sf_grp_name,              &
     &        nidx_rj(2), h_flux_CMB_bc, sph_bc_T%iflag_cmb)
        else if ( h_flux_surf%ibc_type(i) .eq. iflag_sph_2_center       &
     &       .and. h_flux_surf%bc_name(i) .eq. CTR_sf_grp_name) then
         sph_bc_T%iflag_icb = iflag_sph_fill_center
        else if ( h_flux_surf%ibc_type(i) .eq. iflag_sph_clip_center    &
     &       .and. h_flux_surf%bc_name(i) .eq. CTR_sf_grp_name) then
         sph_bc_T%iflag_icb = iflag_sph_fix_center
        end if
      end do
!
      do i = 1, temp_nod%num_bc
        if ( temp_nod%ibc_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        temp_nod%bc_name(i), temp_nod%bc_magnitude(i),            &
     &        nidx_rj(2), h_flux_ICB_bc, sph_bc_T%iflag_icb)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        temp_nod%bc_name(i), temp_nod%bc_magnitude(i),            &
     &        nidx_rj(2), h_flux_CMB_bc, sph_bc_T%iflag_cmb)
!
        else if ( temp_nod%ibc_type(i)  .eq. iflag_bc_file_flux) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_h_flux, ICB_nod_grp_name, ICB_sf_grp_name,            &
     &        nidx_rj(2), h_flux_ICB_bc, sph_bc_T%iflag_icb)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_h_flux, CMB_nod_grp_name, CMB_sf_grp_name,            &
     &        nidx_rj(2), h_flux_CMB_bc, sph_bc_T%iflag_cmb)
!
!
        else if ( temp_nod%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc(ICB_nod_grp_name,              &
     &        temp_nod%bc_name(i), temp_nod%bc_magnitude(i),            &
     &        nidx_rj(2), temp_ICB_bc, sph_bc_T%iflag_icb)
          call set_homogenious_scalar_bc(CMB_nod_grp_name,              &
     &        temp_nod%bc_name(i), temp_nod%bc_magnitude(i),            &
     &        nidx_rj(2), temp_CMB_bc, sph_bc_T%iflag_cmb)
!
        else if ( temp_nod%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_scalar_bc_by_file(fhd_temp, ICB_nod_grp_name,  &
     &        nidx_rj(2), temp_ICB_bc, sph_bc_T%iflag_icb)
          call set_fixed_scalar_bc_by_file(fhd_temp, CMB_nod_grp_name,  &
     &        nidx_rj(2), temp_CMB_bc, sph_bc_T%iflag_cmb)
!
        else if ( temp_nod%ibc_type(i) .eq. iflag_sph_2_center          &
     &       .and. temp_nod%bc_name(i) .eq. CTR_sf_grp_name) then
         sph_bc_T%iflag_icb = iflag_sph_fill_center
        else if ( temp_nod%ibc_type(i) .eq. iflag_sph_clip_center       &
     &       .and. temp_nod%bc_name(i) .eq. CTR_nod_grp_name) then
         sph_bc_T%iflag_icb = iflag_sph_fix_center
        end if
      end do
!
      h_flux_ICB_bc(1:nidx_rj(2)) = - h_flux_ICB_bc(1:nidx_rj(2))
!
      end subroutine set_sph_bc_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_composition_sph
!
      use m_spheric_parameter
      use m_boundary_params_sph_MHD
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
      use m_sph_boundary_input_data
!
      integer(kind = kint) :: i
!
!
      call allocate_dscalar_bc_array( nidx_rj(2) )
!
!      Boundary setting using surface group data
!
      do i = 1, light_surf%num_bc
        if (light_surf%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        light_surf%bc_name(i), light_surf%bc_magnitude(i),        &
     &        nidx_rj(2), c_flux_ICB_bc, sph_bc_C%iflag_icb)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        light_surf%bc_name(i), light_surf%bc_magnitude(i),        &
     &        nidx_rj(2), c_flux_CMB_bc, sph_bc_C%iflag_cmb)
        else if (light_surf%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_light, ICB_nod_grp_name, ICB_sf_grp_name,             &
     &        nidx_rj(2), c_flux_ICB_bc, sph_bc_C%iflag_icb)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_light, CMB_nod_grp_name, CMB_sf_grp_name,             &
     &        nidx_rj(2), c_flux_CMB_bc, sph_bc_C%iflag_cmb)
        else if ( light_surf%ibc_type(i) .eq. iflag_sph_2_center        &
     &       .and. light_surf%bc_name(i) .eq. CTR_sf_grp_name) then
          sph_bc_C%iflag_icb = iflag_sph_fill_center
        else if ( light_surf%ibc_type(i) .eq. iflag_sph_clip_center     &
     &       .and. light_surf%bc_name(i) .eq. CTR_sf_grp_name) then
          sph_bc_C%iflag_icb = iflag_sph_fix_center
        end if
      end do
!
!      Boundary setting using boundary group data
!
      do i = 1, light_nod%num_bc
        if ( light_nod%ibc_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        light_nod%bc_name(i), light_nod%bc_magnitude(i),          &
     &        nidx_rj(2), c_flux_ICB_bc, sph_bc_C%iflag_icb)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        light_nod%bc_name(i), light_nod%bc_magnitude(i),          &
     &        nidx_rj(2), c_flux_CMB_bc, sph_bc_C%iflag_cmb)
!
        else if ( light_nod%ibc_type(i)  .eq. iflag_bc_file_flux) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_c_flux, ICB_nod_grp_name, ICB_sf_grp_name,            &
     &        nidx_rj(2), c_flux_ICB_bc, sph_bc_C%iflag_icb)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_c_flux, CMB_nod_grp_name, CMB_sf_grp_name,            &
     &        nidx_rj(2), c_flux_CMB_bc, sph_bc_C%iflag_cmb)
!
!
        else if ( light_nod%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc(ICB_nod_grp_name,              &
     &        light_nod%bc_name(i), light_nod%bc_magnitude(i),          &
     &        nidx_rj(2), composition_ICB_bc, sph_bc_C%iflag_icb)
          call set_homogenious_scalar_bc(CMB_nod_grp_name,              &
     &        light_nod%bc_name(i), light_nod%bc_magnitude(i),          &
     &        nidx_rj(2), composition_CMB_bc, sph_bc_C%iflag_cmb)
!
        else if ( light_nod%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_scalar_bc_by_file(fhd_light, ICB_nod_grp_name, &
     &        nidx_rj(2), composition_ICB_bc, sph_bc_C%iflag_icb)
          call set_fixed_scalar_bc_by_file(fhd_light, CMB_nod_grp_name, &
     &        nidx_rj(2), composition_CMB_bc, sph_bc_C%iflag_cmb)
!
        else if ( light_nod%ibc_type(i) .eq. iflag_sph_2_center         &
     &       .and. light_nod%bc_name(i) .eq. CTR_sf_grp_name) then
         sph_bc_C%iflag_icb = iflag_sph_fill_center
        else if ( light_nod%ibc_type(i) .eq. iflag_sph_clip_center      &
     &       .and. light_nod%bc_name(i) .eq. CTR_nod_grp_name) then
         sph_bc_C%iflag_icb = iflag_sph_fix_center
        end if
      end do
!
      c_flux_ICB_bc(1:nidx_rj(2)) = -c_flux_ICB_bc(1:nidx_rj(2))
!
      end subroutine set_sph_bc_composition_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_homogenious_scalar_bc(reference_grp, bc_name,      &
     &          bc_magnitude,jmax, bc_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use m_spheric_parameter
!
      character(len=kchara), intent(in) :: reference_grp
      character(len=kchara), intent(in) :: bc_name
      real(kind = kreal), intent(in) :: bc_magnitude
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
!
      if     (bc_name .eq. reference_grp) then
        iflag_bc_scalar =  iflag_fixed_field
        if(idx_rj_degree_zero .gt. 0) then
          bc_data(idx_rj_degree_zero) = bc_magnitude
        end if
      end if
!
      end subroutine set_homogenious_scalar_bc
!
! -----------------------------------------------------------------------
!
      subroutine set_homogenious_grad_bc(ref_nod_grp, ref_sf_grp,       &
     &          bc_name, bc_magnitude, jmax, bc_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use m_spheric_parameter
!
      character(len=kchara), intent(in) :: ref_nod_grp, ref_sf_grp
      character(len=kchara), intent(in) :: bc_name
      real(kind = kreal), intent(in) :: bc_magnitude
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
!
      if     (bc_name .eq. ref_nod_grp                                  &
     &   .or. bc_name .eq. ref_sf_grp ) then
        iflag_bc_scalar =  iflag_fixed_flux
        if(idx_rj_degree_zero .gt. 0) then
          bc_data(idx_rj_degree_zero) = bc_magnitude
        end if
      end if
!
      end subroutine set_homogenious_grad_bc
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_scalars
