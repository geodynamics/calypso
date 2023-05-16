!>@file   rot_self_buoyancies_sph.f90
!!@brief  module rot_self_buoyancies_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in July, 2011
!
!>@brief Evaluate rotation of buoyancy with radial self gravity
!!
!!@verbatim
!!      subroutine sel_rot_buoyancy_sph_MHD(sph_rj,                     &
!!     &          ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(base_force_address), intent(in) :: ipol_rot_frc
!!        type(sph_boundary_type), intent(in) :: sph_bc_U
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param sph_bc_U  Structure for basic velocity
!!                 boundary condition parameters
!!@param kr_in     Radial ID for inner boundary
!!@param kr_out    Radial ID for outer boundary
!
      module rot_self_buoyancies_sph
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_physical_property
      use t_spheric_rj_data
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
      use t_boundary_params_sph_MHD
!
      implicit  none
!
      private :: sel_rot_self_buoyancy_sph
      private :: sel_rot_r_const_buoyancy_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_buoyancy_sph_MHD(sph_rj,                       &
     &          ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%i_grav .eq. iflag_radial_g) then
        if(iflag_debug.gt.0) write(*,*) 'sel_rot_self_buoyancy_sph'
        call sel_rot_r_const_buoyancy_sph                               &
     &     (sph_rj, ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
      else
        if(iflag_debug.gt.0) write(*,*) 'sel_rot_self_buoyancy_sph'
        call sel_rot_self_buoyancy_sph                                  &
     &     (sph_rj, ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
      end if
!
      end subroutine sel_rot_buoyancy_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_self_buoyancy_sph(sph_rj,                      &
     &          ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
!
      use cal_buoyancies_sph_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: it_rot_buo
!
!
      if(fl_prop%iflag_4_gravity                                        &
     &     .and. fl_prop%iflag_4_composit_buo) then
!
        if (iflag_debug.eq.1)                                           &
     &    write(*,*)'rot_self_dbl_buoyancy_sph_MHD', ipol_base%i_temp
          it_rot_buo = ipol_rot_frc%i_buoyancy + 2
          call rot_self_dbl_buoyancy_sph_MHD                            &
     &      (sph_bc_U%kr_in, sph_bc_U%kr_out,                           &
     &       fl_prop%coef_buo, ipol_base%i_temp, fl_prop%coef_comp_buo, &
     &       ipol_base%i_light, it_rot_buo,                             &
     &       sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                     &
     &       rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_gravity) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_self_buoyancy_sph_MHD', ipol_base%i_temp
        it_rot_buo = ipol_rot_frc%i_buoyancy + 2
        call rot_self_buoyancy_sph_MHD                                  &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_base%i_temp, it_rot_buo,                               &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_composit_buo) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_self_buoyancy_sph_MHD', ipol_base%i_light
        it_rot_buo = ipol_rot_frc%i_comp_buo + 2
        call rot_self_buoyancy_sph_MHD(sph_bc_U%kr_in, sph_bc_U%kr_out, &
     &      fl_prop%coef_comp_buo, ipol_base%i_light, it_rot_buo,       &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_self_buoyancy_sph
!
!-----------------------------------------------------------------------
!
      subroutine sel_rot_r_const_buoyancy_sph(sph_rj,                   &
     &          ipol_base, ipol_rot_frc, fl_prop, sph_bc_U, rj_fld)
!
      use t_physical_property
      use t_spheric_rj_data
      use t_base_field_labels
      use t_base_force_labels
      use t_phys_data
      use t_boundary_params_sph_MHD
      use cal_buoyancies_sph_MHD
!
      type(fluid_property), intent(in) :: fl_prop
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(base_field_address), intent(in) :: ipol_base
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: it_rot_buo
!
!
      if(fl_prop%iflag_4_gravity                                        &
     &     .and. fl_prop%iflag_4_composit_buo) then
!
        if (iflag_debug.eq.1)                                           &
     &    write(*,*)'rot_r_cst_dbl_buoyancy_sph_MHD', ipol_base%i_temp
          it_rot_buo = ipol_rot_frc%i_buoyancy + 2
          call rot_r_cst_dbl_buoyancy_sph_MHD                           &
     &      (sph_bc_U%kr_in, sph_bc_U%kr_out,                           &
     &       fl_prop%coef_buo, ipol_base%i_temp, fl_prop%coef_comp_buo, &
     &       ipol_base%i_light, it_rot_buo, sph_rj%nidx_rj,             &
     &       rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_gravity) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_r_const_buoyancy_sph_MHD', ipol_base%i_temp
        it_rot_buo = ipol_rot_frc%i_buoyancy + 2
        call rot_r_const_buoyancy_sph_MHD                               &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_buo,          &
     &      ipol_base%i_temp, it_rot_buo, sph_rj%nidx_rj,               &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (fl_prop%iflag_4_composit_buo) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &      'rot_r_const_buoyancy_sph_MHD', ipol_base%i_light
        it_rot_buo = ipol_rot_frc%i_comp_buo + 2
        call rot_r_const_buoyancy_sph_MHD                               &
     &     (sph_bc_U%kr_in, sph_bc_U%kr_out, fl_prop%coef_comp_buo,     &
     &      ipol_base%i_light, it_rot_buo, sph_rj%nidx_rj,              &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sel_rot_r_const_buoyancy_sph
!
!-----------------------------------------------------------------------
!
      end module rot_self_buoyancies_sph
