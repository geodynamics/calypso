!>@file   init_sph_radius_variations.f90
!!@brief  module init_sph_radius_variations
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Set radial variations for densityuy and diffusivities
!!
!!@verbatim
!!      subroutine init_radius_variations_sph_mhd(sph, r_2nd,           &
!!     &                                          MHD_prop, refs)
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(radial_reference_field), intent(inout) :: refs
!!@endverbatim
!!
!
      module init_sph_radius_variations
!
      use m_precision
      use m_constants
      use t_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_sph_radial_interpolate
      use t_field_data_IO
      use t_fdm_coefs
      use t_radial_reference_field
!
      implicit none
!
      private :: check_r_variation_data_list
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_radius_variations_sph_mhd(sph, r_2nd,             &
     &                                          MHD_prop, refs)
!
      use m_base_field_labels
      use m_diffusion_term_labels
      use transfer_to_long_integers
      use set_sph_radial_variations
      use init_radial_infos_sph_mhd
      use radial_reference_field_IO
      use calypso_mpi_real
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd

      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(radial_reference_field), intent(inout) :: refs
!
      character(len=kchara), parameter :: radius_name = 'radius'
      type(sph_radial_interpolate) :: r_itp
      type(field_IO) :: fld_IO
!
      integer(kind = kint) :: k
!
!
      if(iflag_debug .gt. 0) then
        call check_r_variation_data_list(MHD_prop, refs%ref_field)
      end if
!
      if(my_rank .eq. 0) then
        call set_sph_radial_density(my_rank, radius_name,               &
     &      density%name, MHD_prop%fl_prop%ir_rho,                      &
     &      sph%sph_rj, r_2nd, MHD_prop%polytrope_param,                &
     &      refs%ref_field, r_itp, fld_IO)
!
        if(MHD_prop%val_viscous_param%iflag_radial_diffusion            &
     &      .eq. iflag_constant) then
          call copy_const_diffusivity_to_ref                            &
     &       (MHD_prop%fl_prop%ir_nu, MHD_prop%fl_prop%ir_dnu_norm,     &
     &        refs%ref_field)
        else
          call set_sph_radial_diffusivity(my_rank, radius_name,         &
     &        kinetic_viscosity%name, MHD_prop%fl_prop%ir_nu,           &
     &        sph%sph_rj, r_2nd, MHD_prop%val_viscous_param,            &
     &        refs%ref_field, r_itp, fld_IO)
        end if
!
        if(MHD_prop%val_mag_diffuse_param%iflag_radial_diffusion        &
     &      .eq. iflag_constant) then
          call copy_const_diffusivity_to_ref                            &
     &       (MHD_prop%cd_prop%ir_eta, MHD_prop%cd_prop%ir_deta_norm,   &
     &        refs%ref_field)
        else
          call set_sph_radial_diffusivity(my_rank, radius_name,         &
     &        magnetic_diffusivity%name, MHD_prop%cd_prop%ir_eta,       &
     &        sph%sph_rj, r_2nd, MHD_prop%val_mag_diffuse_param,        &
     &        refs%ref_field, r_itp, fld_IO)
        end if
!
        if(MHD_prop%val_thermal_diffuse_param%iflag_radial_diffusion    &
     &      .eq. iflag_constant) then
          call copy_const_diffusivity_to_ref(MHD_prop%ht_prop%ir_kappa, &
     &        MHD_prop%ht_prop%ir_dkappa_norm, refs%ref_field)
        else if(MHD_prop%val_thermal_diffuse_param%iflag_radial_diffusion    &
     &      .eq. iflag_ICB_reduction) then
          call r_diffusivity_w_ICB_reduction                            &
     &       (sph%sph_params, MHD_prop%ht_prop, refs%iref_radius,       &
     &      MHD_prop%ht_prop%ir_kappa, MHD_prop%ht_prop%ir_dkappa_norm, &
     &      refs%ref_field)
        else
          call set_sph_radial_diffusivity(my_rank, radius_name,         &
     &        thermal_diffusivity%name, MHD_prop%ht_prop%ir_kappa,      &
     &        sph%sph_rj, r_2nd, MHD_prop%val_thermal_diffuse_param,    &
     &        refs%ref_field, r_itp, fld_IO)
        end if
!
        if(MHD_prop%val_comp_diffuse_param%iflag_radial_diffusion       &
     &      .eq. iflag_constant) then
          call copy_const_diffusivity_to_ref(MHD_prop%cp_prop%ir_kappa, &
     &        MHD_prop%cp_prop%ir_dkappa_norm, refs%ref_field)
        else if(MHD_prop%val_comp_diffuse_param%iflag_radial_diffusion  &
     &      .eq. iflag_ICB_reduction) then
          call r_diffusivity_w_ICB_reduction                            &
     &     (sph%sph_params, MHD_prop%cp_prop, refs%iref_radius,         &
     &      MHD_prop%cp_prop%ir_kappa, MHD_prop%cp_prop%ir_dkappa_norm, &
     &      refs%ref_field)
        else
          call set_sph_radial_diffusivity(my_rank, radius_name,         &
     &        chemical_diffusivity%name, MHD_prop%cp_prop%ir_kappa,     &
     &        sph%sph_rj, r_2nd, MHD_prop%val_comp_diffuse_param,       &
     &        refs%ref_field, r_itp, fld_IO)
        end if
      end if
!
      do k = 1, refs%ref_field%ntot_phys
        call calypso_mpi_bcast_real(refs%ref_field%d_fld(1,k),          &
     &      cast_long(refs%ref_field%n_point), 0)
      end do
!
      end subroutine init_radius_variations_sph_mhd
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine check_r_variation_data_list(MHD_prop, ref_field)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_data), intent(in) :: ref_field
      integer(kind = kint) :: icou_ref
!
      write(*,*) 'ir_rho', MHD_prop%fl_prop%ir_rho
      write(*,*) 'ir_drho_norm',  MHD_prop%fl_prop%ir_drho_norm
      write(*,*) 'ir_d2rho_norm', MHD_prop%fl_prop%ir_d2rho_norm
!
      write(*,*) 'ir_nu', MHD_prop%fl_prop%ir_nu
      write(*,*) 'ir_dnu_norm', MHD_prop%fl_prop%ir_dnu_norm
      write(*,*) 'ir_eta', MHD_prop%cd_prop%ir_eta
      write(*,*) 'ir_deta_norm', MHD_prop%cd_prop%ir_deta_norm
      write(*,*) 'ir_kappa', MHD_prop%ht_prop%ir_kappa
      write(*,*) 'ir_dkappa_norm', MHD_prop%ht_prop%ir_dkappa_norm
      write(*,*) 'ir_kappa', MHD_prop%cp_prop%ir_kappa
      write(*,*) 'ir_dkappa_norm', MHD_prop%cp_prop%ir_dkappa_norm
      do icou_ref = 1, ref_field%num_phys
        write(*,*) icou_ref, trim(ref_field%phys_name(icou_ref))
      end do
!
      end subroutine check_r_variation_data_list
!
! -----------------------------------------------------------------------
!
      end module init_sph_radius_variations
