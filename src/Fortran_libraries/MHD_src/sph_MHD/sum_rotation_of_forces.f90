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
!!     &         (fl_prop, ipol_exp, ipol_rot_frc, rj_fld)
!!      subroutine licv_forces_to_explicit                              &
!!     &         (fl_prop, ipol_exp, ipol_rot_frc, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(explicit_term_address), intent(in) :: ipol_exp
!!        type(base_force_address), intent(in) :: ipol_rot_frc
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
     &         (fl_prop, ipol_exp, ipol_rot_frc, rj_fld)
!
      use copy_nodal_fields
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(       fl_prop%iflag_4_inertia                                 &
     &   .and.  fl_prop%iflag_4_gravity                                 &
     &   .and.  fl_prop%iflag_4_coriolis                                &
     &   .and.  fl_prop%iflag_4_lorentz) then
        call set_MHD_terms_to_force                                     &
     &     (ipol_exp, ipol_rot_frc, ipol_rot_frc%i_buoyancy,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(  fl_prop%iflag_4_inertia                                 &
     &   .and. (fl_prop%iflag_4_gravity  .eqv.   .FALSE.)               &
     &   .and.  fl_prop%iflag_4_composit_buo                            &
     &   .and.  fl_prop%iflag_4_coriolis                                &
     &   .and.  fl_prop%iflag_4_lorentz) then
        call set_MHD_terms_to_force                                     &
     &     (ipol_exp, ipol_rot_frc, ipol_rot_frc%i_comp_buo,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(  fl_prop%iflag_4_inertia                                 &
     &   .and.  fl_prop%iflag_4_gravity                                 &
     &   .and.  fl_prop%iflag_4_coriolis                                &
     &   .and. (fl_prop%iflag_4_lorentz  .eqv. .FALSE.)) then
        call set_rot_cv_terms_to_force                                  &
     &     (ipol_exp, ipol_rot_frc, ipol_rot_frc%i_buoyancy,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(  fl_prop%iflag_4_inertia                                 &
     &   .and. (fl_prop%iflag_4_gravity  .eqv.  .FALSE.)                &
     &   .and.  fl_prop%iflag_4_composit_buo                            &
     &   .and.  fl_prop%iflag_4_coriolis                                &
     &   .and. (fl_prop%iflag_4_lorentz  .eqv.  .FALSE.)) then
        call set_rot_cv_terms_to_force                                  &
     &     (ipol_exp, ipol_rot_frc, ipol_rot_frc%i_comp_buo,            &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
        call clear_field_data                                           &
     &     (rj_fld, n_vector, ipol_exp%i_forces)
!
!$omp parallel
        if(fl_prop%iflag_4_inertia) then
          call add_rot_advection_to_force                               &
     &     (ipol_exp%i_forces, ipol_rot_frc%i_m_advect,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
        if(fl_prop%iflag_4_coriolis) then
          call add_each_force_to_rot_forces                             &
     &       (ipol_exp%i_forces, ipol_rot_frc%i_Coriolis,               &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_lorentz) then
          call add_each_force_to_rot_forces                             &
     &       (ipol_exp%i_forces, ipol_rot_frc%i_lorentz,                &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
        if(fl_prop%iflag_4_gravity) then
          call add_buoyancy_to_vort_force                               &
     &       (ipol_exp%i_forces, ipol_rot_frc%i_buoyancy,               &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_composit_buo) then
          call add_buoyancy_to_vort_force                               &
     &       (ipol_exp%i_forces, ipol_rot_frc%i_comp_buo,               &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!$omp end parallel
      end if
!
      end subroutine sum_forces_to_explicit
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_forces_to_explicit                                &
     &         (fl_prop, ipol_exp, ipol_rot_frc, rj_fld)
!
      use m_phys_constants
      use cal_vorticity_terms_adams
!
      type(fluid_property), intent(in) :: fl_prop
      type(explicit_term_address), intent(in) :: ipol_exp
      type(base_force_address), intent(in) :: ipol_rot_frc
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(fl_prop%iflag_4_coriolis) then
        call add_each_force_to_rot_forces                               &
     &     (ipol_exp%i_forces, ipol_rot_frc%i_Coriolis,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%iflag_4_gravity) then
        call add_buoyancy_to_vort_force                                 &
     &     (ipol_exp%i_forces, ipol_rot_frc%i_buoyancy,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(fl_prop%iflag_4_composit_buo) then
        call add_buoyancy_to_vort_force                                 &
     &     (ipol_exp%i_forces, ipol_rot_frc%i_comp_buo,                 &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
!
      end subroutine licv_forces_to_explicit
!*
!*   ------------------------------------------------------------------
!
      end module sum_rotation_of_forces
