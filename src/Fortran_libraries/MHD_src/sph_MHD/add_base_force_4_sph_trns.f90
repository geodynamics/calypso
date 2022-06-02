!>@file   add_base_force_4_sph_trns.f90
!!@brief  module add_base_force_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Force addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_base_force_4_MHD_sph_trns                        &
!!     &         (d_rj, ipol_frc, iphys_frc, f_trns_frc, trns)
!!      subroutine add_base_force_sph_trns_snap                         &
!!     &         (d_rj, ipol_frc, iphys_frc, f_trns_frc, trns)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!!        type(base_force_address), intent(inout) :: f_trns_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_rot_coriolis_MHD_sph_trns(d_rj,                  &
!!     &          ipol_rot_frc, iphys_rot_frc, f_trns_rot_frc, trns)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(base_force_address), intent(in) :: iphys_rot_frc
!!        type(base_force_address), intent(inout) :: f_trns_rot_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_div_coriolis_MHD_sph_trns(d_rj,                  &
!!     &          ipol_div_frc, iphys_div_frc, f_trns_div_frc, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(base_force_address), intent(inout) :: f_trns_div_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_base_force_fwd_trns_snap                         &
!!     &         (d_rj, ipol_frc, iphys_frc, f_trns_frc, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!!        type(base_force_address), intent(inout) :: f_trns_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_linear_force_bwd_trns_snap                       &
!!     &         (d_rj, ipol_frc, iphys_frc, b_trns_frc, trns)
!!      subroutine add_mag_induct_bwd_trns_snap                         &
!!     &         (d_rj, ipol_frc, iphys_frc, b_trns_frc, trns)
!!      subroutine add_scalar_flux_bwd_trns_snap                        &
!!     &         (d_rj, ipol_frc, iphys_frc, b_trns_frc, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!!        type(base_force_address), intent(inout) :: b_trns_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_rot_force_4_sph_trns_snap(d_rj,                  &
!!     &          ipol_rot_frc, iphys_rot_frc, b_trns_rot_frc, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(base_force_address), intent(in) :: iphys_rot_frc
!!        type(base_force_address), intent(inout) :: b_trns_rot_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_div_force_4_sph_trns_snap(d_rj,                  &
!!     &          ipol_div_frc, iphys_div_frc, b_trns_div_frc, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_force_address), intent(in) :: ipol_div_frc
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(base_force_address), intent(inout) :: b_trns_div_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_base_force_4_sph_trns
!
      use m_precision
      use m_constants
!
      use t_phys_data
      use t_base_force_labels
      use t_addresses_sph_transform
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_base_force_4_MHD_sph_trns                          &
!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                    &
     &         (d_rj, ipol_frc, iphys_frc, f_trns_frc, trns)
!
      use add_field_to_sph_trans_list
!
!      type(fluid_property), intent(in) :: fl_prop
!      type(conductive_property), intent(in) :: cd_prop
!      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!
      type(base_force_address), intent(inout) :: f_trns_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!   advection
!      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
!        if(fl_prop%iflag_4_inertia) then
          call add_field_4_sph_trns_by_pol(d_rj,                        &
     &        ipol_frc%i_m_advect, iphys_frc%i_m_advect,                &
     &        f_trns_frc%i_m_advect, trns)
!        end if
!   Coriolis force
!        if(fl_prop%iflag_4_coriolis) then
          call add_field_4_sph_trns_by_pol(d_rj,                        &
     &        ipol_frc%i_coriolis, iphys_frc%i_coriolis,                &
     &        f_trns_frc%i_coriolis, trns)
!        end if
!   Lorentz force
!        if(fl_prop%iflag_4_lorentz) then
          call add_field_4_sph_trns_by_pol(d_rj,                        &
     &        ipol_frc%i_lorentz, iphys_frc%i_lorentz,                  &
     &        f_trns_frc%i_lorentz, trns)
!        end if
!      end if
!
!   induction
!      if((cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)              &
!     &    .and. cd_prop%iflag_4_induction) then
          call add_field_4_sph_trns_by_pol(d_rj,                        &
     &      ipol_frc%i_vp_induct, iphys_frc%i_vp_induct,                &
     &      f_trns_frc%i_vp_induct, trns)
!      end if
!
!   heat flux
!      if((ht_prop%iflag_scheme .gt. id_no_evolution)                   &
!     &     .and. ht_prop%iflag_4_advection) then
          call add_field_4_sph_trns_by_pol(d_rj,                        &
     &      ipol_frc%i_h_flux, iphys_frc%i_h_flux, f_trns_frc%i_h_flux, &
     &      trns)
!      end if
!
!   composition flux
!      if((cp_prop%iflag_scheme .gt. id_no_evolution)                   &
!     &     .and. cp_prop%iflag_4_advection) then
          call add_field_4_sph_trns_by_pol(d_rj,                        &
     &      ipol_frc%i_c_flux, iphys_frc%i_c_flux, f_trns_frc%i_c_flux, &
     &      trns)
!      end if
!
      end subroutine add_base_force_4_MHD_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_base_force_sph_trns_snap                           &
     &         (d_rj, ipol_frc, iphys_frc, f_trns_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!
      type(base_force_address), intent(inout) :: f_trns_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_frc%i_ph_flux, iphys_frc%i_ph_flux,                      &
     &    f_trns_frc%i_ph_flux, trns)
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_frc%i_pc_flux, iphys_frc%i_pc_flux,                      &
     &    f_trns_frc%i_pc_flux, trns)
!
      end subroutine add_base_force_sph_trns_snap
!
!-----------------------------------------------------------------------
!
!      subroutine add_rot_coriolis_MHD_sph_trns(fl_prop, d_rj,          &
      subroutine add_rot_coriolis_MHD_sph_trns(d_rj,                    &
     &          ipol_rot_frc, iphys_rot_frc, f_trns_rot_frc, trns)
!
      use add_field_to_sph_trans_list
!
!      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(base_force_address), intent(in) :: iphys_rot_frc
!
      type(base_force_address), intent(inout) :: f_trns_rot_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!   rotation of Coriolis force
!      if(fl_prop%iflag_scheme .gt. id_no_evolution                     &
!     &      .and. fl_prop%iflag_4_coriolis) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_rot_frc%i_Coriolis, iphys_rot_frc%i_Coriolis,          &
     &      f_trns_rot_frc%i_Coriolis, trns)
!      end if
!
      end subroutine add_rot_coriolis_MHD_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_div_coriolis_MHD_sph_trns(d_rj,                    &
     &          ipol_div_frc, iphys_div_frc, f_trns_div_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_div_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(base_force_address), intent(inout) :: f_trns_div_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!   divergence of Coriolis force
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_div_frc%i_Coriolis, iphys_div_frc%i_Coriolis,            &
     &    f_trns_div_frc%i_Coriolis, trns)
!
      end subroutine add_div_coriolis_MHD_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_base_force_fwd_trns_snap                           &
     &         (d_rj, ipol_frc, iphys_frc, f_trns_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!
      type(base_force_address), intent(inout) :: f_trns_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_coriolis, iphys_frc%i_coriolis,                    &
     &    f_trns_frc%i_coriolis, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_mag_stretch, iphys_frc%i_mag_stretch,              &
     &    f_trns_frc%i_mag_stretch, trns)
!
      end subroutine add_base_force_fwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_linear_force_bwd_trns_snap                         &
     &         (d_rj, ipol_frc, iphys_frc, b_trns_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!
      type(base_force_address), intent(inout) :: b_trns_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_buoyancy, iphys_frc%i_buoyancy,                    &
     &    b_trns_frc%i_buoyancy, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_comp_buo, iphys_frc%i_comp_buo,                    &
     &    b_trns_frc%i_comp_buo, trns)
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_press_grad, iphys_frc%i_press_grad,                &
     &    b_trns_frc%i_press_grad, trns)
!
      end subroutine add_linear_force_bwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_mag_induct_bwd_trns_snap                           &
     &         (d_rj, ipol_frc, iphys_frc, b_trns_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!
      type(base_force_address), intent(inout) :: b_trns_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_induction, iphys_frc%i_induction,                  &
     &    b_trns_frc%i_induction, trns)
!
      end subroutine add_mag_induct_bwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_scalar_flux_bwd_trns_snap                          &
     &         (d_rj, ipol_frc, iphys_frc, b_trns_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_frc, iphys_frc
!
      type(base_force_address), intent(inout) :: b_trns_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_h_advect, iphys_frc%i_h_advect,                    &
     &    b_trns_frc%i_h_advect, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_frc%i_c_advect, iphys_frc%i_c_advect,                    &
     &    b_trns_frc%i_c_advect, trns)
!
      end subroutine add_scalar_flux_bwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_rot_force_4_sph_trns_snap(d_rj,                    &
     &          ipol_rot_frc, iphys_rot_frc, b_trns_rot_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(base_force_address), intent(in) :: iphys_rot_frc
!
      type(base_force_address), intent(inout) :: b_trns_rot_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_rot_frc%i_m_advect, iphys_rot_frc%i_m_advect,            &
     &    b_trns_rot_frc%i_m_advect, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_rot_frc%i_Coriolis, iphys_rot_frc%i_Coriolis,            &
     &    b_trns_rot_frc%i_Coriolis, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_rot_frc%i_lorentz, iphys_rot_frc%i_lorentz,              &
     &    b_trns_rot_frc%i_lorentz, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_rot_frc%i_buoyancy, iphys_rot_frc%i_buoyancy,            &
     &    b_trns_rot_frc%i_buoyancy, trns)
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_rot_frc%i_comp_buo, iphys_rot_frc%i_comp_buo,            &
     &    b_trns_rot_frc%i_comp_buo, trns)
!
      end subroutine add_rot_force_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_div_force_4_sph_trns_snap(d_rj,                    &
     &          ipol_div_frc, iphys_div_frc, b_trns_div_frc, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_force_address), intent(in) :: ipol_div_frc
      type(base_force_address), intent(in) :: iphys_div_frc
!
      type(base_force_address), intent(inout) :: b_trns_div_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_div_frc%i_Coriolis, iphys_div_frc%i_Coriolis,            &
     &    b_trns_div_frc%i_Coriolis, trns)
!
      end subroutine add_div_force_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_base_force_4_sph_trns
