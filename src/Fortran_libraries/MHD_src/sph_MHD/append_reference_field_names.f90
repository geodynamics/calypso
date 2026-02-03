!>@file   append_reference_field_names.f90
!!@brief  module append_reference_field_names
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  reference temperature as a function of r
!!
!!
!!@verbatim
!!      subroutine append_ref_field_names(radius_name, ipol_base,       &
!!     &                                  MHD_prop, refs)
!!        character(len = kchara), intent(in) :: radius_name
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(radial_reference_field), intent(inout) :: refs
!!@endverbatim
!
      module append_reference_field_names
!
      use m_precision
      use m_constants
      use t_radial_reference_field
      use t_phys_address
      use t_base_field_labels
      use t_grad_field_labels
      use t_field_component_labels
      use t_file_IO_parameter
      use t_control_parameter
!
      implicit  none
!
      character(len = kchara), parameter, private                       &
     &     :: drho_dr_name =   'normalized_drho_dr'
      character(len = kchara), parameter, private                       &
     &     :: d2rho_dr2_name = 'normalized_d2rho_dr2'
!
      private :: append_reference_scalar_list
      private :: append_const_magnetic_fld_list
      private :: append_r_diffusivities_list
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine append_ref_field_names(radius_name, ipol_base,         &
     &                                  MHD_prop, refs)
!
      use append_phys_data
!
      character(len = kchara), intent(in) :: radius_name
      type(base_field_address), intent(in) :: ipol_base
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(radial_reference_field), intent(inout) :: refs
!
!
      refs%iref_radius = refs%ref_field%ntot_phys + 1
      call append_field_name_list(radius_name,                          &
     &    ione, .TRUE., .FALSE., izero, refs%ref_field)
!
      call append_reference_scalar_list                                 &
     &   (ipol_base, refs%iref_base, refs%iref_grad, refs%ref_field)
      call append_const_magnetic_fld_list(ipol_base,                    &
     &    refs%iref_cmp, refs%ref_field)
      call append_r_diffusivities_list(MHD_prop,                        &
     &    refs%iref_diffusivity, refs%iref_grad_diffusivity,            &
     &    refs%ref_field)
!
      end subroutine append_ref_field_names
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_reference_scalar_list                           &
     &         (ipol_base, iref_base, iref_grad, ref_field)
!
      use m_base_field_labels
      use m_grad_field_labels
      use append_phys_data
!
      type(base_field_address), intent(in) :: ipol_base
!
      type(base_field_address), intent(inout) :: iref_base
      type(gradient_field_address), intent(inout) :: iref_grad
      type(phys_data), intent(inout) :: ref_field
!
!
      if(ipol_base%i_heat_source .gt. 0) then
        iref_base%i_heat_source = ref_field%ntot_phys + 1
        call append_field_name_list(heat_source%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(ipol_base%i_light_source .gt. 0) then
        iref_base%i_light_source = ref_field%ntot_phys + 1
        call append_field_name_list(composition_source%name,            &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      if(ipol_base%i_temp .gt. 0) then
        iref_base%i_temp =         ref_field%ntot_phys + 1
        call append_field_name_list(temperature%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        iref_grad%i_grad_temp =     ref_field%ntot_phys + 1
        call append_field_name_list(grad_temp%name,                     &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(ipol_base%i_light .gt. 0) then
        iref_base%i_light =         ref_field%ntot_phys + 1
        call append_field_name_list(composition%name,                   &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        iref_grad%i_grad_composit = ref_field%ntot_phys + 1
        call append_field_name_list(grad_composition%name,              &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      end subroutine append_reference_scalar_list
!
! -----------------------------------------------------------------------
!
      subroutine append_const_magnetic_fld_list(ipol_base,              &
     &                                          iref_cmp, ref_field)
!
      use m_field_component_labels
      use append_phys_data
!
      type(base_field_address), intent(in) :: ipol_base
!
      type(field_component_address), intent(inout) :: iref_cmp
      type(phys_data), intent(inout) :: ref_field
!
!
      if(ipol_base%i_back_B .gt. 0) then
        iref_cmp%i_magne_y = ref_field%ntot_phys + 1
        call append_field_name_list(y_magnetic_f%name,                  &
     &      ithree, .TRUE., .FALSE., izero, ref_field)
        iref_cmp%i_magne_z = ref_field%ntot_phys + 1
        call append_field_name_list(z_magnetic_f%name,                  &
     &      ithree, .TRUE., .FALSE., izero, ref_field)
        iref_cmp%i_magne_x = ref_field%ntot_phys + 1
        call append_field_name_list(x_magnetic_f%name,                  &
     &      ithree, .TRUE., .FALSE., izero, ref_field)
      end if
!
      end subroutine append_const_magnetic_fld_list
!
! -----------------------------------------------------------------------
!
      subroutine append_r_diffusivities_list(MHD_prop,                  &
     &          iref_diffusivity, iref_grad_diffusivity, ref_field)
!
      use m_base_field_labels
      use m_diffusion_term_labels
      use append_phys_data
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(diffusivity_adress), intent(inout) :: iref_diffusivity
      type(diffusivity_adress), intent(inout) :: iref_grad_diffusivity
      type(phys_data), intent(inout) :: ref_field
!
!
      if(MHD_prop%flag_ref_density_valiation) then
        write(*,*) 'flag_ref_density_valiation ON'
        MHD_prop%fl_prop%ir_rho =        ref_field%ntot_phys + 1
        call append_field_name_list(density%name,                       &
     &      ione, .TRUE., .FALSE., izero, ref_field)
        MHD_prop%fl_prop%ir_drho_norm =  ref_field%ntot_phys + 2
        call append_field_name_list(drho_dr_name,                       &
     &      ione, .TRUE., .FALSE., izero, ref_field)
        MHD_prop%fl_prop%ir_d2rho_norm = ref_field%ntot_phys + 3
        call append_field_name_list(d2rho_dr2_name,                     &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      if(MHD_prop%flag_viscous_variation) then
        MHD_prop%fl_prop%ir_nu = ref_field%ntot_phys + 1
        iref_diffusivity%i_K_viscosity = MHD_prop%fl_prop%ir_nu
        call append_field_name_list(kinetic_viscosity%name,             &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        MHD_prop%fl_prop%ir_dnu_norm = ref_field%ntot_phys + 1
        iref_grad_diffusivity%i_K_viscosity                             &
     &                          = MHD_prop%fl_prop%ir_dnu_norm
        call append_field_name_list(grad_kinetic_viscosity%name,        &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(MHD_prop%flag_mag_diffuse_variation) then
        MHD_prop%cd_prop%ir_eta = ref_field%ntot_phys + 1
        iref_diffusivity%i_B_diffusivity = MHD_prop%cd_prop%ir_eta
        call append_field_name_list(magnetic_diffusivity%name,          &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        MHD_prop%cd_prop%ir_deta_norm = ref_field%ntot_phys + 1
        iref_grad_diffusivity%i_B_diffusivity                           &
     &                          = MHD_prop%cd_prop%ir_deta_norm
        call append_field_name_list(grad_magnetic_diffusivity%name,     &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(MHD_prop%flag_term_diffuse_variation) then
        MHD_prop%ht_prop%ir_kappa = ref_field%ntot_phys + 1
        iref_diffusivity%i_T_diffusivity = MHD_prop%ht_prop%ir_kappa
        call append_field_name_list(thermal_diffusivity%name,           &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        MHD_prop%ht_prop%ir_dkappa_norm = ref_field%ntot_phys + 1
        iref_grad_diffusivity%i_T_diffusivity                           &
     &                          = MHD_prop%ht_prop%ir_dkappa_norm
        call append_field_name_list(grad_thermal_diffusivity%name,      &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
      if(MHD_prop%flag_comp_diffuse_variation) then
        MHD_prop%cp_prop%ir_kappa = ref_field%ntot_phys + 1
        iref_diffusivity%i_C_diffusivity = MHD_prop%cp_prop%ir_kappa
        call append_field_name_list(chemical_diffusivity%name,          &
     &      ione, .TRUE., .FALSE., izero, ref_field)
!
        MHD_prop%cp_prop%ir_dkappa_norm = ref_field%ntot_phys + 1
        iref_grad_diffusivity%i_C_diffusivity                           &
     &                          = MHD_prop%cp_prop%ir_dkappa_norm
        call append_field_name_list(grad_chemical_diffusivity%name,     &
     &      ione, .TRUE., .FALSE., izero, ref_field)
      end if
!
      end subroutine append_r_diffusivities_list
!
! -----------------------------------------------------------------------
!
      end module append_reference_field_names
