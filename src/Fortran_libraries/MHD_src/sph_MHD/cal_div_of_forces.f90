!>@file   cal_div_of_forces.f90
!!@brief  module cal_div_of_forces
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct. 2009
!
!>@brief Evaluate divergence of forces for pressure evaluation
!!
!!@verbatim
!!      subroutine sum_div_of_forces(fl_prop, ipol, rj_fld)
!!      subroutine sum_div_of_SGS_forces(fl_prop, ipol, rj_fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module cal_div_of_forces
!
      use m_precision
      use t_phys_address
!
      implicit  none
!
      private :: set_DMHD_terms_to_div_force
      private :: set_MHD_terms_to_div_force, set_div_cv_terms_to_force
      private :: add_term_to_div_force, set_div_advection_to_force
      private :: set_SGS_forces_to_div_force
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sum_div_of_forces(fl_prop, ipol, rj_fld)
!
      use t_physical_property
      use t_phys_data
!
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(      fl_prop%iflag_4_gravity  .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_composit_buo .ne. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_lorentz  .ne. id_turn_OFF) then
        call set_DMHD_terms_to_div_force                                &
     &     (ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .ne.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_composit_buo .eq. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_div_force(ipol, ipol%i_div_buoyancy,      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .eq.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_composit_buo .ne. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne.     id_turn_OFF            &
     &   .and. fl_prop%iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_div_force(ipol, ipol%i_div_comp_buo,      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_composit_buo .ne. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_div_dcv_terms_to_force                                 &
     &     (ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_composit_buo .eq. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_div_cv_terms_to_force(ipol, ipol%i_div_buoyancy,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else if( fl_prop%iflag_4_gravity  .eq. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_composit_buo .ne. id_turn_OFF            &
     &   .and. fl_prop%iflag_4_coriolis .ne. id_turn_OFF                &
     &   .and. fl_prop%iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_div_cv_terms_to_force(ipol, ipol%i_div_comp_buo,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
!
        call set_div_advection_to_force                                 &
     &     (ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        if(fl_prop%iflag_4_coriolis .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol, ipol%i_div_Coriolis,         &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_lorentz .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol, ipol%i_div_Lorentz,          &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(fl_prop%iflag_4_gravity .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol, ipol%i_div_buoyancy,         &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else if(fl_prop%iflag_4_composit_buo .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol, ipol%i_div_comp_buo,         &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else if(fl_prop%iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol, ipol%i_div_filter_buo,       &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      end if
!$omp end parallel
!
      end subroutine sum_div_of_forces
!
! ----------------------------------------------------------------------
!
      subroutine sum_div_of_SGS_forces(fl_prop, ipol, rj_fld)
!
      use t_physical_property
      use t_phys_data
!
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(ipol%i_SGS_rot_inertia  .ne. 0                                 &
     &  .and.  ipol%i_SGS_rot_Lorentz .ne. 0) then
        call set_SGS_forces_to_div_force                                &
     &     (ipol, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      else
!
        if(fl_prop%iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol, ipol%i_div_inertia,          &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(ipol%i_SGS_rot_inertia .ne. izero) then
          call add_term_to_div_force(ipol, ipol%i_div_Lorentz,          &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
      end if
!$omp end parallel
!
      end subroutine sum_div_of_SGS_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_DMHD_terms_to_div_force                            &
     &         (ipol, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_press) =  - d_rj(inod,ipol%i_div_inertia)      &
     &                             + d_rj(inod,ipol%i_div_Coriolis)     &
     &                             + d_rj(inod,ipol%i_div_Lorentz)      &
     &                             + d_rj(inod,ipol%i_div_buoyancy)     &
     &                             + d_rj(inod,ipol%i_div_comp_buo)
      end do
!$omp end do nowait
!
      end subroutine set_DMHD_terms_to_div_force
!
! ----------------------------------------------------------------------
!
      subroutine set_MHD_terms_to_div_force                             &
     &         (ipol, is_div_buo, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: is_div_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_press) =  - d_rj(inod,ipol%i_div_inertia)      &
     &                             + d_rj(inod,ipol%i_div_Coriolis)     &
     &                             + d_rj(inod,ipol%i_div_Lorentz)      &
     &                             + d_rj(inod,is_div_buo)
      end do
!$omp end do nowait
!
      end subroutine set_MHD_terms_to_div_force
!
! ----------------------------------------------------------------------
!
      subroutine set_div_dcv_terms_to_force                             &
     &         (ipol, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_press) =  - d_rj(inod,ipol%i_div_inertia)      &
     &                             + d_rj(inod,ipol%i_div_Coriolis)     &
     &                             + d_rj(inod,ipol%i_div_buoyancy)     &
     &                             + d_rj(inod,ipol%i_div_comp_buo)
      end do
!$omp end do nowait
!
      end subroutine set_div_dcv_terms_to_force
!
! ----------------------------------------------------------------------
!
      subroutine set_div_cv_terms_to_force                              &
     &         (ipol, is_div_buo, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: is_div_buo
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_press) =  - d_rj(inod,ipol%i_div_inertia)      &
     &                             + d_rj(inod,ipol%i_div_Coriolis)     &
     &                             + d_rj(inod,is_div_buo)
      end do
!$omp end do nowait
!
      end subroutine set_div_cv_terms_to_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_advection_to_force                             &
     &         (ipol, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
!        d_rj(inod,ipol%i_press) = - d_rj(inod,ipol%i_div_inertia)
        d_rj(inod,ipol%i_press) = zero
      end do
!$omp end do nowait
!
      end subroutine set_div_advection_to_force
!
! ----------------------------------------------------------------------
!
      subroutine add_term_to_div_force                                  &
     &          (ipol, is_div, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: is_div
!
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_press) = d_rj(inod,ipol%i_press)               &
     &                          + d_rj(inod,is_div)
       end do
!$omp end do nowait
!
      end subroutine add_term_to_div_force
!
! ----------------------------------------------------------------------
!
      subroutine set_SGS_forces_to_div_force                            &
     &         (ipol, nnod_rj, ntot_phys_rj, d_rj)
!
      type(phys_address), intent(in) :: ipol
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_press) =  d_rj(inod,ipol%i_press)              &
     &                           - d_rj(inod,ipol%i_SGS_rot_inertia)    &
     &                           + d_rj(inod,ipol%i_SGS_rot_Lorentz)
      end do
!$omp end do nowait
!
      end subroutine set_SGS_forces_to_div_force
!
! ----------------------------------------------------------------------
!
      end module cal_div_of_forces
