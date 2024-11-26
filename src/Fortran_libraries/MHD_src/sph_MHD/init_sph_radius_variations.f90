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
!!      subroutine init_radius_variations_sph_mhd(sph, r_2nd, MHD_prop, &
!!     &                                          radial_variation)
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(phys_data), intent(inout) :: radial_variation
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
!
      implicit none
!
      private :: count_r_variation_data
      private :: set_r_variation_data_address
      private :: check_r_variation_data_list
!
      character(len = kchara), parameter                                &
     &        :: r_valiation_file_name = 'Radial_variations_out.dat'
!
      private :: output_radial_variation
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_radius_variations_sph_mhd(sph, r_2nd, MHD_prop,   &
     &                                          radial_variation)
!
      use calypso_mpi_real
      use transfer_to_long_integers
      use set_sph_radial_variations
      use m_base_field_labels
      use m_diffusion_term_labels
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd

      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(phys_data), intent(inout) :: radial_variation
!
      character(len=kchara), parameter :: radius_name = 'radius'
      type(sph_radial_interpolate) :: r_itp
      type(field_IO) :: fld_IO
!
      integer(kind = kint) :: k
!
!
      radial_variation%num_phys =      count_r_variation_data(MHD_prop)
      radial_variation%num_phys_viz =  radial_variation%num_phys
      radial_variation%ntot_phys =     radial_variation%num_phys
      call alloc_phys_name(radial_variation)
      call alloc_phys_data((sph%sph_rj%nidx_rj(1)+1), radial_variation)
!
      call set_r_variation_data_address(MHD_prop, radial_variation)
      
      if(iflag_debug .gt. 0) then
        call check_r_variation_data_list(MHD_prop, radial_variation)
      end if
!
      if(my_rank .eq. 0) then
        radial_variation%d_fld(1,1) = 0.0d0
        do k = 1, sph%sph_rj%nidx_rj(1)
          radial_variation%d_fld(k+1,1) = sph%sph_rj%radius_1d_rj_r(k)
        end do
!
        call set_sph_radial_density(my_rank, radius_name,               &
     &      density%name, MHD_prop%fl_prop%ir_rho,                      &
     &      sph%sph_rj, r_2nd, MHD_prop%polytrope_param,                &
     &      radial_variation, r_itp, fld_IO)
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      kinetic_viscosity%name, MHD_prop%fl_prop%ir_nu,             &
     &      sph%sph_rj, r_2nd, MHD_prop%val_viscous_param,              &
     &      radial_variation, r_itp, fld_IO)
!
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      magnetic_diffusivity%name, MHD_prop%cd_prop%ir_eta,         &
     &      sph%sph_rj, r_2nd, MHD_prop%val_mag_diffuse_param,          &
     &      radial_variation, r_itp, fld_IO)
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      thermal_diffusivity%name, MHD_prop%ht_prop%ir_kappa,        &
     &      sph%sph_rj, r_2nd, MHD_prop%val_thermal_diffuse_param,      &
     &      radial_variation, r_itp, fld_IO)
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      chemical_diffusivity%name, MHD_prop%cp_prop%ir_kappa,       &
     &      sph%sph_rj, r_2nd, MHD_prop%val_comp_diffuse_param,         &
     &      radial_variation, r_itp, fld_IO)
      end if
!
      do k = 1, radial_variation%ntot_phys
        call calypso_mpi_bcast_real(radial_variation%d_fld(1,k),        &
     &      cast_long(radial_variation%n_point), 0)
      end do
!
      call output_radial_variation                                      &
     &   (r_valiation_file_name, radial_variation, fld_IO)
!
      end subroutine init_radius_variations_sph_mhd
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      integer(kind = kint) function count_r_variation_data(MHD_prop)
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
      integer(kind = kint) :: icou_ref = 0
!
      icou_ref = 1
      if(MHD_prop%flag_ref_density_valiation)  icou_ref = icou_ref + 3
      if(MHD_prop%flag_viscous_variation)      icou_ref = icou_ref + 2
      if(MHD_prop%flag_mag_diffuse_variation)  icou_ref = icou_ref + 2
      if(MHD_prop%flag_term_diffuse_variation) icou_ref = icou_ref + 2
      if(MHD_prop%flag_comp_diffuse_variation) icou_ref = icou_ref + 2
      count_r_variation_data = icou_ref
!
      end function count_r_variation_data
!
!  -------------------------------------------------------------------
!
      subroutine set_r_variation_data_address(MHD_prop,                 &
     &                                        radial_variation)
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(phys_data), intent(inout) :: radial_variation
!
      integer(kind = kint) :: i
      integer(kind = kint) :: icou_ref = 0
!
!
      radial_variation%istack_component(0) = 0
      do i = 1, radial_variation%num_phys
        radial_variation%num_component(i) =    1
        radial_variation%istack_component(i) = i
      end do
!
      icou_ref = 1
      radial_variation%phys_name(icou_ref) = 'radius'
!
      if(MHD_prop%flag_ref_density_valiation) then
        write(*,*) 'flag_ref_density_valiation ON'
        MHD_prop%fl_prop%ir_rho =        icou_ref + 1
        MHD_prop%fl_prop%ir_drho_norm =  icou_ref + 2
        MHD_prop%fl_prop%ir_d2rho_norm = icou_ref + 3
        radial_variation%phys_name(MHD_prop%fl_prop%ir_rho)             &
    &                           = 'density'
        radial_variation%phys_name(MHD_prop%fl_prop%ir_drho_norm)       &
    &                           = 'normalized_drho_dr'
        radial_variation%phys_name(MHD_prop%fl_prop%ir_drho_norm)       &
    &                           = 'normalized_d2rho_dr2'
        icou_ref = icou_ref + 3
      end if
!
      if(MHD_prop%flag_viscous_variation) then
        write(*,*) 'flag_ref_density_valiation ON'
        MHD_prop%fl_prop%ir_nu =       icou_ref + 1
        MHD_prop%fl_prop%ir_dnu_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%fl_prop%ir_nu)              &
    &                           = 'viscousity'
        radial_variation%phys_name(MHD_prop%fl_prop%ir_dnu_norm)        &
    &                           = 'normalized_dnu_dr'
        icou_ref = icou_ref + 2
      end if
      if(MHD_prop%flag_mag_diffuse_variation) then
        write(*,*) 'flag_mag_diffuse_variation ON'
        MHD_prop%cd_prop%ir_eta =       icou_ref + 1
        MHD_prop%cd_prop%ir_deta_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%cd_prop%ir_eta)             &
    &                           = 'magnetic_diffusivity'
        radial_variation%phys_name(MHD_prop%cd_prop%ir_deta_norm)       &
    &                           = 'normalized_deta_dr'
        icou_ref = icou_ref + 2
      end if
      if(MHD_prop%flag_term_diffuse_variation) then
        write(*,*) 'flag_term_diffuse_variation ON'
        MHD_prop%ht_prop%ir_kappa =       icou_ref + 1
        MHD_prop%ht_prop%ir_dkappa_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%ht_prop%ir_kappa)           &
    &                           = 'thermal_diffusivity'
        radial_variation%phys_name(MHD_prop%ht_prop%ir_dkappa_norm)     &
    &                           = 'normalized_dkappa_T_dr'
        icou_ref = icou_ref + 2
      end if
      if(MHD_prop%flag_comp_diffuse_variation) then
        write(*,*) 'flag_comp_diffuse_variation ON'
        MHD_prop%cp_prop%ir_kappa =       icou_ref + 1
        MHD_prop%cp_prop%ir_dkappa_norm = icou_ref + 2
        radial_variation%phys_name(MHD_prop%cp_prop%ir_kappa)           &
    &                           = 'compositional_diffusivity'
        radial_variation%phys_name(MHD_prop%cp_prop%ir_dkappa_norm)     &
    &                           = 'normalized_dkappa_C_dr'
        icou_ref = icou_ref + 2
      end if
!
      end subroutine set_r_variation_data_address
!
!  -------------------------------------------------------------------
!
      subroutine output_radial_variation                                &
     &         (file_prefix, radial_variation, ref_fld_IO)
!
      use calypso_mpi
      use field_file_IO
!
      use copy_rj_phys_data_4_IO
      use set_sph_extensions
!
      character(len = kchara), intent(in) :: file_prefix
      type(phys_data), intent(in) :: radial_variation
      type(field_IO), intent(inout) :: ref_fld_IO
!
      type(time_data) :: time_IO
!
!
      if(my_rank .ne. 0) return
      if(radial_variation%num_phys .le. 1) return
!
      time_IO%i_time_step = izero
      time_IO%time = zero
      time_IO%dt = zero
!
      write(*,*) 'radial_variation%num_phys', radial_variation%num_phys
      write(*,*) 'radial_variation%num_component', radial_variation%num_component
      write(*,*) 'radial_variation%istack_component', radial_variation%istack_component
!
      call copy_rj_phys_name_to_IO                                      &
     &   (radial_variation%num_phys, radial_variation, ref_fld_IO)
      call alloc_phys_data_IO(ref_fld_IO)
      call copy_rj_phys_data_to_IO                                      &
     &   (radial_variation%num_phys, radial_variation, ref_fld_IO)
!
      call write_step_field_file(file_prefix, my_rank,                  &
     &                           time_IO, ref_fld_IO)
!
      call dealloc_phys_data_IO(ref_fld_IO)
      call dealloc_phys_name_IO(ref_fld_IO)
!
      end subroutine output_radial_variation
!
! -----------------------------------------------------------------------
!
      subroutine check_r_variation_data_list(MHD_prop,                  &
     &                                       radial_variation)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_data), intent(in) :: radial_variation
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
      do icou_ref = 1, radial_variation%num_phys
        write(*,*) icou_ref, trim(radial_variation%phys_name(icou_ref))
      end do
!
      end subroutine check_r_variation_data_list
!
! -----------------------------------------------------------------------
!
      end module init_sph_radius_variations
