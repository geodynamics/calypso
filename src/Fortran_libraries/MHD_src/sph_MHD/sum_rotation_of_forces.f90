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
!!     &         (sph_rj, fl_prop, ipol, itor, rj_fld)
!!      subroutine licv_forces_to_explicit                              &
!!     &          (sph_rj, fl_prop, ipol, itor, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: ipol, itor
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
      use t_spheric_parameter
      use t_phys_address
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
     &         (sph_rj, fl_prop, ipol, itor, rj_fld)
!
      use cal_vorticity_terms_adams
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(      fl_prop%iflag_4_gravity  .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_coriolis .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_lorentz  .ne. id_turn_OFF) then
        call set_MHD_terms_to_force(ipol, itor, itor%i_rot_buoyancy,    &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .eq.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_composit_buo .ne. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_force(ipol, itor, itor%i_rot_comp_buo,    &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_coriolis .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_rot_cv_terms_to_force(ipol, itor, itor%i_rot_buoyancy, &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .eq.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_composit_buo .ne. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_lorentz  .eq.     id_turn_OFF) then
        call set_rot_cv_terms_to_force(ipol, itor, itor%i_rot_comp_buo, &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else
!$omp parallel
        if((ipol%i_forces*ipol%i_rot_inertia) .gt. 0) then
          call set_rot_advection_to_force                               &
     &     (ipol, itor, sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_coriolis .ne. id_turn_OFF) then
          call add_coriolis_to_vort_force(ipol, itor,                   &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_lorentz .ne.  id_turn_OFF) then
          call add_lorentz_to_vort_force (ipol, itor,                   &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_gravity .ne.  id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor, itor%i_rot_buoyancy,    &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        else if(fl_prop%iflag_4_composit_buo .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor, itor%i_rot_comp_buo,    &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        else if(fl_prop%iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor, itor%i_rot_filter_buo,  &
     &        sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
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
     &          (sph_rj, fl_prop, ipol, itor, rj_fld)
!
      use m_phys_constants
      use copy_nodal_fields
      use cal_vorticity_terms_adams
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: ipol, itor
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(fl_prop%iflag_4_coriolis .ne. id_turn_OFF) then
        call add_coriolis_to_vort_force(ipol, itor,                     &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(fl_prop%iflag_4_gravity .ne.  id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor, itor%i_rot_buoyancy,      &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      else if(fl_prop%iflag_4_composit_buo .ne. id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor, itor%i_rot_comp_buo,      &
     &      sph_rj%nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
!
      end subroutine licv_forces_to_explicit
!*
!*   ------------------------------------------------------------------
!
      end module sum_rotation_of_forces
