!>@file   sum_rotation_of_forces.f90
!!@brief  module sum_rotation_of_forces
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine sum_forces_to_explicit                               &
!!     &         (fl_prop, ipol_exp, ipol_force, rj_fld)
!!      subroutine licv_forces_to_explicit                              &
!!     &         (fl_prop, ipol_exp, ipol_force, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_force
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module sum_rotation_of_forces
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_physical_property
      use t_phys_address
      use t_base_force_labels
      use t_explicit_term_labels
      use t_phys_data
!
      implicit none
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine sum_forces_to_explicit                                 &
     &         (fl_prop, ipol_exp, ipol_force, rj_fld)
!
      use copy_nodal_fields
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_force
      type(phys_data), intent(inout) :: rj_fld
!
!
      call clear_field_data                                             &
     &   (rj_fld, n_vector, ipol_exp%i_forces)
!
      if(fl_prop%flag_inertia) then
        call subtract_advection_to_force                                &
     &     (ipol_exp%i_forces, ipol_force%i_m_advect,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%flag_coriolis) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_Coriolis,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%flag_lorentz) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_lorentz,                    &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(fl_prop%flag_thermal_buoyancy) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_buoyancy,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%flag_comp_buoyancy) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_comp_buo,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine sum_forces_to_explicit
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_forces_to_explicit                                &
     &         (fl_prop, ipol_exp, ipol_force, rj_fld)
!
      use m_phys_constants
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_force
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(fl_prop%flag_coriolis) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_Coriolis,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%flag_thermal_buoyancy) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_buoyancy,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%flag_comp_buoyancy) then
        call add_each_force_to_forces                                   &
     &     (ipol_exp%i_forces, ipol_force%i_comp_buo,                   &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
!
      end subroutine licv_forces_to_explicit
!*
!*   ------------------------------------------------------------------
!
      end module sum_rotation_of_forces
