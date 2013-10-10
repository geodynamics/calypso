!>@file   set_bc_sph_mhd.f90
!!@brief  module set_bc_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine s_set_bc_sph_mhd
!!@endverbatim
!
      module set_bc_sph_mhd
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
      character(len=kchara), parameter :: ICB_nod_grp_name = 'ICB'
      character(len=kchara), parameter :: CMB_nod_grp_name = 'CMB'
      character(len=kchara), parameter :: ICB_sf_grp_name = 'ICB_surf'
      character(len=kchara), parameter :: CMB_sf_grp_name = 'CMB_surf'
!
      private :: ICB_nod_grp_name, ICB_sf_grp_name
      private :: CMB_nod_grp_name, CMB_sf_grp_name
      private :: set_sph_bc_temp_sph
      private :: set_sph_bc_magne_sph, set_sph_bc_composition_sph
!
      private :: set_homogenious_scalar_bc, set_homogenious_grad_bc
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_bc_sph_mhd
!
      use set_bc_flag_sph_velo
!
!
      if (iflag_t_evo_4_velo .gt.     id_no_evolution) then
        call set_sph_bc_velo_sph
      end if
!
      if (iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call set_sph_bc_temp_sph
      end if
!
      if (iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call set_sph_bc_magne_sph
      end if
!
      if (iflag_t_evo_4_composit .gt. id_no_evolution) then
        call set_sph_bc_composition_sph
      end if
!
      end subroutine s_set_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_temp_sph
!
      use m_spheric_parameter
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
      if(iflag_debug .gt. 0) then
        write(*,*) 'h_flux_surf%num_bc',       h_flux_surf%num_bc
        write(*,*) 'h_flux_surf%ibc_type',     h_flux_surf%ibc_type
        write(*,*) 'h_flux_surf%bc_magnitude', h_flux_surf%bc_magnitude
      end if
!
      do i = 1, h_flux_surf%num_bc
        if ( h_flux_surf%ibc_type(i)  .eq. iflag_surf_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        h_flux_surf%bc_name(i), h_flux_surf%bc_magnitude(i),      &
     &        nidx_rj(2), h_flux_ICB_bc, iflag_icb_temp)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        h_flux_surf%bc_name(i), h_flux_surf%bc_magnitude(i),      &
     &        nidx_rj(2), h_flux_CMB_bc, iflag_cmb_temp)
        else if (h_flux_surf%ibc_type(i)  .eq. -iflag_surf_fix_s) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_temp, ICB_nod_grp_name, ICB_sf_grp_name,              &
     &        nidx_rj(2), h_flux_ICB_bc, iflag_icb_temp)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_temp, CMB_nod_grp_name, CMB_sf_grp_name,              &
     &        nidx_rj(2), h_flux_CMB_bc, iflag_cmb_temp)
        end if
      end do
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'iflag_icb_temp', iflag_icb_temp
        write(*,*) 'iflag_cmb_temp', iflag_cmb_temp
        write(*,*)  h_flux_CMB_bc(1)
      end if
!
      do i = 1, num_bc_e
        if ( ibc_e_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        bc_e_name(i), bc_e_magnitude(i),                          &
     &        nidx_rj(2), h_flux_ICB_bc, iflag_icb_temp)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        bc_e_name(i), bc_e_magnitude(i),                          &
     &        nidx_rj(2), h_flux_CMB_bc, iflag_cmb_temp)
!
        else if ( ibc_e_type(i)  .eq. -iflag_bc_fix_flux) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_h_flux, ICB_nod_grp_name, ICB_sf_grp_name,            &
     &        nidx_rj(2), h_flux_ICB_bc, iflag_icb_temp)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_h_flux, CMB_nod_grp_name, CMB_sf_grp_name,            &
     &        nidx_rj(2), h_flux_CMB_bc, iflag_cmb_temp)
!
!
        else if ( ibc_e_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc(ICB_nod_grp_name,              &
     &        bc_e_name(i), bc_e_magnitude(i),                          &
     &        nidx_rj(2), temp_ICB_bc, iflag_icb_temp)
          call set_homogenious_scalar_bc(CMB_nod_grp_name,              &
     &        bc_e_name(i), bc_e_magnitude(i),                          &
     &        nidx_rj(2), temp_CMB_bc, iflag_cmb_temp)
!
        else if ( ibc_e_type(i)  .eq. -iflag_bc_fix_s) then
          call set_fixed_scalar_bc_by_file(fhd_temp, ICB_nod_grp_name,  &
     &        nidx_rj(2), temp_ICB_bc, iflag_icb_temp)
          call set_fixed_scalar_bc_by_file(fhd_temp, CMB_nod_grp_name,  &
     &        nidx_rj(2), temp_CMB_bc, iflag_cmb_temp)
        end if
      end do
!
      h_flux_ICB_bc(1:nidx_rj(2)) = - h_flux_ICB_bc(1:nidx_rj(2))
!
      if(idx_rj_degree_zero .gt. 0                                      &
     &      .and. iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        temp_ICB_bc(idx_rj_degree_zero)                                 &
     &   = temp_ICB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_ICB,0)
        temp_CMB_bc(idx_rj_degree_zero)                                 &
     &   = temp_CMB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_CMB,0)
        h_flux_ICB_bc(idx_rj_degree_zero)                               &
     &   = h_flux_ICB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_ICB,1)
        h_flux_CMB_bc(idx_rj_degree_zero)                               &
     &   = h_flux_CMB_bc(idx_rj_degree_zero) - reftemp_rj(nlayer_CMB,1)
      end if
!
      if(iflag_debug .gt. 0 .and. idx_rj_degree_zero .gt. 0) then
        write(*,*) 'iflag_icb_temp', iflag_icb_temp,                    &
     &             reftemp_rj(nlayer_ICB,0),                            &
     &             temp_ICB_bc(idx_rj_degree_zero),                     &
     &             h_flux_ICB_bc(idx_rj_degree_zero)
        write(*,*) 'iflag_cmb_temp', iflag_cmb_temp,                    &
     &             reftemp_rj(nlayer_CMB,0),                            &
     &             temp_CMB_bc(idx_rj_degree_zero),                     &
     &             h_flux_CMB_bc(idx_rj_degree_zero)
      end if
!
      end subroutine set_sph_bc_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_magne_sph
!
      use m_bc_data_list
      use m_surf_data_list
!
!
      integer(kind = kint) :: i
!
!
      iflag_icb_magne = iflag_sph_insulator
      iflag_cmb_magne = iflag_sph_insulator
!
      do i = 1, num_bc_b
        if(bc_b_name(i) .eq. 'ICB') then
          if(ibc_b_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_icb_magne =  iflag_radial_magne
          end if
        end if
!
        if(bc_b_name(i) .eq. 'CMB') then
          if(ibc_b_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_cmb_magne =  iflag_radial_magne
          end if
        end if
!
        if(bc_b_name(i) .eq. 'to_Center') then
          if      (ibc_b_type(i) .eq. iflag_sph_2_center) then
            iflag_icb_magne =  iflag_sph_fill_center
          end if
        end if
      end do
!
!
      do i = 1, magne_surf%num_bc
        if(magne_surf%bc_name(i) .eq. 'ICB') then
          if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_icb_magne =  iflag_radial_magne
          end if
        end if
!
        if(magne_surf%bc_name(i) .eq. 'CMB') then
          if(magne_surf%ibc_type(i) .eq. iflag_pseudo_vacuum) then
            iflag_cmb_magne =  iflag_radial_magne
          end if
        end if
!
        if(magne_surf%bc_name(i) .eq. 'to_Center') then
          if(magne_surf%ibc_type(i) .eq. iflag_sph_2_center) then
            iflag_icb_magne =  iflag_sph_fill_center
          end if
        end if
      end do
!
!
      end subroutine set_sph_bc_magne_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_composition_sph
!
      use m_spheric_parameter
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
        if (light_surf%ibc_type(i)  .eq. iflag_surf_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        light_surf%bc_name(i), light_surf%bc_magnitude(i),        &
     &        nidx_rj(2), c_flux_ICB_bc, iflag_icb_composition)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        light_surf%bc_name(i), light_surf%bc_magnitude(i),        &
     &        nidx_rj(2), c_flux_CMB_bc, iflag_cmb_composition)
        else if (light_surf%ibc_type(i)  .eq. -iflag_surf_fix_s)        &
     &         then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_light, ICB_nod_grp_name, ICB_sf_grp_name,             &
     &        nidx_rj(2), c_flux_ICB_bc, iflag_icb_composition)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_light, CMB_nod_grp_name, CMB_sf_grp_name,             &
     &        nidx_rj(2), c_flux_CMB_bc, iflag_cmb_composition)
        end if
      end do
!
!      Boundary setting using boundary group data
!
      do i = 1, num_bc_composit
        if ( ibc_composit_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (ICB_nod_grp_name, ICB_sf_grp_name,                        &
     &        bc_composit_name(i), bc_composit_magnitude(i),            &
     &        nidx_rj(2), c_flux_ICB_bc, iflag_icb_composition)
          call set_homogenious_grad_bc                                  &
     &       (CMB_nod_grp_name, CMB_sf_grp_name,                        &
     &        bc_composit_name(i), bc_composit_magnitude(i),            &
     &        nidx_rj(2), c_flux_CMB_bc, iflag_cmb_composition)
!
        else if ( ibc_composit_type(i)  .eq. -iflag_bc_fix_flux) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_c_flux, ICB_nod_grp_name, ICB_sf_grp_name,            &
     &        nidx_rj(2), c_flux_ICB_bc, iflag_icb_composition)
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_c_flux, CMB_nod_grp_name, CMB_sf_grp_name,            &
     &        nidx_rj(2), c_flux_CMB_bc, iflag_cmb_composition)
!
!
        else if ( ibc_composit_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc(ICB_nod_grp_name,              &
     &        bc_composit_name(i), bc_composit_magnitude(i),            &
     &        nidx_rj(2), composition_ICB_bc, iflag_icb_composition)
          call set_homogenious_scalar_bc(CMB_nod_grp_name,              &
     &        bc_composit_name(i), bc_composit_magnitude(i),            &
     &        nidx_rj(2), composition_CMB_bc, iflag_cmb_composition)
!
        else if ( ibc_composit_type(i)  .eq. -iflag_bc_fix_s) then
          call set_fixed_scalar_bc_by_file(fhd_light, ICB_nod_grp_name, &
     &        nidx_rj(2), composition_ICB_bc, iflag_icb_composition)
          call set_fixed_scalar_bc_by_file(fhd_light, CMB_nod_grp_name, &
     &        nidx_rj(2), composition_CMB_bc, iflag_cmb_composition)
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
      end module set_bc_sph_mhd
