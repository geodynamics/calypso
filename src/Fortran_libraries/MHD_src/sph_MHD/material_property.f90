!>@file   material_property.f90
!!@brief  module material_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Subroutines to set coeffiecient of each term
!!
!!@verbatim
!!      subroutine set_material_property                                &
!!     &         (iphys, depth_top, depth_bottom, MHD_prop)
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!@endverbatim
!!
!
      module material_property
!
      use m_precision
      use m_constants
!
      use t_phys_address
      use t_control_parameter
      use t_physical_property
      use calypso_mpi
!
      implicit none
!
      private :: set_fluid_property, set_thermal_property
      private :: set_conductive_property, set_composition_property
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_material_property                                  &
     &         (iphys, depth_top, depth_bottom, MHD_prop)
!
      use construct_MHD_coefficient
!
      real(kind = kreal), intent(in) :: depth_top, depth_bottom
      type(phys_address), intent(in) :: iphys
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
!    For thermal
      if (my_rank .eq. 0) write(*,*) ''
      call set_thermal_property(iphys, depth_top, depth_bottom,         &
     &    MHD_prop%MHD_coef_list, MHD_prop%ht_prop)
!
!    For convection
      call set_fluid_property(depth_top, depth_bottom,                  &
     &    MHD_prop%MHD_coef_list, MHD_prop%fl_prop)
!
!   For Induction
      call set_conductive_property(depth_top, depth_bottom,             &
     &    MHD_prop%MHD_coef_list, MHD_prop%cd_prop)
!
!   For light element
      call set_composition_property(iphys, depth_top, depth_bottom,     &
     &    MHD_prop%MHD_coef_list, MHD_prop%cp_prop)
      if (my_rank .eq. 0) write(*,*) ''
!
      end subroutine set_material_property
!
! -----------------------------------------------------------------------
!
      subroutine set_fluid_property                                     &
     &         (depth_top, depth_bottom, MHD_coef_list, fl_prop)
!
      use construct_MHD_coefficient
!
      real(kind = kreal), intent(in) :: depth_top, depth_bottom
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
      type(fluid_property), intent(inout) :: fl_prop
!
!    For convection
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
!
        fl_prop%coef_velo =     one
        fl_prop%coef_diffuse =  one
        fl_prop%coef_buo =      one
        fl_prop%coef_comp_buo = one
        fl_prop%coef_cor =      one
        fl_prop%coef_lor =      one
        fl_prop%coef_press =    one
        fl_prop%acoef_press =   one
!
        call construct_coefficient(fl_prop%coef_velo,                   &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_momentum,   &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(fl_prop%coef_press,                  &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_pressure,   &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(fl_prop%coef_diffuse,                &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_v_diffuse,  &
     &      depth_top, depth_bottom)
!
        call set_implicit_4_inf_viscous(fl_prop%coef_velo,              &
     &      fl_prop%coef_imp, fl_prop%coef_exp)
!
        fl_prop%acoef_press = one / fl_prop%coef_press
        fl_prop%coef_nega_v = - fl_prop%coef_velo
!
        if (fl_prop%iflag_4_gravity .gt. id_turn_OFF                    &
     &     .or. fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop%coef_buo,                  &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_buoyancy,  &
     &      depth_top, depth_bottom)
        end if
!
        if (fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop%coef_comp_buo,             &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_comp_buo,  &
     &       depth_top, depth_bottom)
        end if
!
        if (fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop%coef_cor,                  &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_Coriolis,  &
     &       depth_top, depth_bottom)
        end if
!
        if (fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call construct_coefficient(fl_prop%coef_lor,                  &
     &       MHD_coef_list%dimless_list, MHD_coef_list%coefs_Lorentz,   &
     &       depth_top, depth_bottom)
        end if
!
      end if
!
!  Check
!
      if (my_rank .eq. 0) then
        if(fl_prop%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for velocity:            ',           &
     &              fl_prop%coef_velo
          write(*,*) 'coefficient for pressure:            ',           &
     &              fl_prop%coef_press
          write(*,*) 'coefficient for viscous diffusion:   ',           &
     &              fl_prop%coef_diffuse
        if (fl_prop%iflag_4_gravity .gt. id_turn_OFF) write(*,*)        &
     &         'coefficient for buoyancy:            ',                 &
     &              fl_prop%coef_buo
        if (fl_prop%iflag_4_composit_buo .gt. id_turn_OFF)  write(*,*)  &
     &         'coefficient for composit buoyancy:   ',                 &
     &              fl_prop%coef_comp_buo
        if (fl_prop%iflag_4_coriolis .gt. id_turn_OFF) write(*,*)       &
     &         'coefficient for coriolis force:      ',                 &
     &              fl_prop%coef_cor
        if (fl_prop%iflag_4_lorentz .gt. id_turn_OFF) write(*,*)        &
     &         'coefficient for Lorentz force:       ',                 &
     &              fl_prop%coef_lor
        end if
      end if
!
      end subroutine set_fluid_property
!
! -----------------------------------------------------------------------
!
      subroutine set_conductive_property                                &
     &         (depth_top, depth_bottom, MHD_coef_list, cd_prop)
!
      use construct_MHD_coefficient
!
      real(kind = kreal), intent(in) :: depth_top, depth_bottom
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
      type(conductive_property), intent(inout) :: cd_prop
!
!   For Induction
!
      if(    cd_prop%iflag_Bevo_scheme .gt. id_no_evolution             &
     &  .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
!
        cd_prop%coef_magne =   one
        cd_prop%coef_mag_p =   one
        cd_prop%coef_diffuse = one
        cd_prop%coef_induct =  one
!
        call construct_coefficient(cd_prop%coef_magne,                  &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_magnetic,   &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(cd_prop%coef_mag_p,                  &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_magne_p,    &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(cd_prop%coef_diffuse,                &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_m_diffuse,  &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(cd_prop%coef_induct,                 &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_induction,  &
     &      depth_top, depth_bottom)
      end if
!
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call set_implicit_4_inf_viscous(cd_prop%coef_magne,            &
     &      cd_prop%coef_imp, cd_prop%coef_exp)
      end if
      if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call set_implicit_4_inf_viscous(cd_prop%coef_magne,            &
     &      cd_prop%coef_imp, cd_prop%coef_exp)
      end if
!
!  Check
!
      if (my_rank .eq. 0) then
        if(     cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for magnetic field:      ',           &
     &              cd_prop%coef_magne
          write(*,*) 'coefficient for magnetic potential:  ',           &
     &              cd_prop%coef_mag_p
          write(*,*) 'coefficient for magnetic diffusion:  ',           &
     &              cd_prop%coef_diffuse
          write(*,*) 'coefficient for induction:           ',           &
     &              cd_prop%coef_induct
        end if
!
      end if
!
      end subroutine set_conductive_property
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_thermal_property                                   &
     &         (iphys, depth_top, depth_bottom, MHD_coef_list, ht_prop)
!
      use construct_MHD_coefficient
!
      type(phys_address), intent(in) :: iphys
      real(kind = kreal), intent(in) :: depth_top, depth_bottom
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
      type(scalar_property), intent(inout) :: ht_prop
!
!    For thermal
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
!
        ht_prop%coef_advect =  one
        ht_prop%coef_diffuse = one
        ht_prop%coef_source =  one
!
        call construct_coefficient(ht_prop%coef_advect,                 &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_termal,     &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(ht_prop%coef_diffuse,                &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_t_diffuse,  &
     &      depth_top, depth_bottom)
!
        call construct_coefficient(ht_prop%coef_source,                 &
     &      MHD_coef_list%dimless_list, MHD_coef_list%coefs_h_source,   &
     &      depth_top, depth_bottom)
!
        call set_implicit_4_inf_viscous(ht_prop%coef_advect,            &
     &      ht_prop%coef_imp, ht_prop%coef_exp)
!
        ht_prop%coef_nega_adv = - ht_prop%coef_advect
      end if
!
      if (my_rank .eq. 0) then
        if (ht_prop%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for temperature:         ',           &
     &              ht_prop%coef_advect
          write(*,*) 'coefficient for thermal diffusion:   ',           &
     &              ht_prop%coef_diffuse
          if(iphys%i_heat_source .gt. 0) write(*,*)                     &
     &         'coefficient for heat source:         ',                 &
     &              ht_prop%coef_source
        end if
      end if
!
      end subroutine set_thermal_property
!
! -----------------------------------------------------------------------
!
      subroutine set_composition_property                               &
     &         (iphys, depth_top, depth_bottom, MHD_coef_list, cp_prop)
!
      use construct_MHD_coefficient
!
      type(phys_address), intent(in) :: iphys
      real(kind = kreal), intent(in) :: depth_top, depth_bottom
      type(coef_parameters_list), intent(inout) :: MHD_coef_list
      type(scalar_property), intent(inout) :: cp_prop
!
!   For light element
!
      if (cp_prop%iflag_scheme .gt. id_no_evolution) then
        cp_prop%coef_advect =   one
        cp_prop%coef_diffuse =  one
        cp_prop%coef_source =   one
!
        call construct_coefficient(cp_prop%coef_advect,                 &
     &     MHD_coef_list%dimless_list, MHD_coef_list%coefs_composition, &
     &     depth_top, depth_bottom)
!
        call construct_coefficient(cp_prop%coef_diffuse,                &
     &     MHD_coef_list%dimless_list, MHD_coef_list%coefs_c_diffuse,   &
     &     depth_top, depth_bottom)
!
        call construct_coefficient(cp_prop%coef_source,                 &
     &     MHD_coef_list%dimless_list, MHD_coef_list%coefs_c_source,    &
     &     depth_top, depth_bottom)
!
        call set_implicit_4_inf_viscous(cp_prop%coef_advect,            &
     &      cp_prop%coef_imp, cp_prop%coef_exp)
!
        cp_prop%coef_nega_adv = - cp_prop%coef_advect
      end if
!
!  Check
!
      if (my_rank .eq. 0) then
        if (cp_prop%iflag_scheme .gt. id_no_evolution) then
          write(*,*) 'coefficient for composition:         ',           &
     &              cp_prop%coef_advect
          write(*,*) 'coefficient for composite diffusion: ',           &
     &              cp_prop%coef_diffuse
          if(iphys%i_light_source .gt. 0) write(*,*)                    &
     &         'coefficient for light element source:',                 &
     &              cp_prop%coef_source
        end if
      end if
!
      end subroutine set_composition_property
!
! -----------------------------------------------------------------------
!
      end module material_property
