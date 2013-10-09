!>@file   cal_nonlinear.f90
!!@brief  module cal_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear
!!      subroutine licv_exp
!!@endverbatim
!
!
      module cal_nonlinear
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
!
      implicit none
!
      private :: nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear
!
      use m_sph_phys_address
      use cal_vorticity_terms_adams
      use const_coriolis_sph
!
      use cal_nonlinear_sph_MHD
!
      use m_work_time
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        call add_reftemp_advect_sph_MHD
      end if
!
!*  ----  set coriolis term
!*
      call start_eleps_time(13)
      if (iflag_debug.eq.1) write(*,*) 'sum_coriolis_rj_sph'
      if(iflag_4_coriolis .gt. id_turn_OFF) call sum_coriolis_rj_sph
      call end_eleps_time(13)
!
!$omp parallel
      if(      iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .ne. id_turn_OFF) then
        call set_MHD_terms_to_force(itor%i_rot_buoyancy)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_force(itor%i_rot_comp_buo)
      else if( iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_rot_cv_terms_to_force(itor%i_rot_buoyancy)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .eq.     id_turn_OFF) then
        call set_rot_cv_terms_to_force(itor%i_rot_comp_buo)
      else
        call set_rot_advection_to_force
        if(iflag_4_coriolis .ne. id_turn_OFF) then
          call add_coriolis_to_vort_force
        end if
        if(iflag_4_lorentz .ne.  id_turn_OFF) then
          call add_lorentz_to_vort_force
        end if
        if(iflag_4_gravity .ne.  id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_buoyancy)
        else if(iflag_4_composit_buo .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_comp_buo)
        else if(iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_buoyancy_to_vort_force(itor%i_rot_filter_buo)
        end if
      end if
!$omp end parallel
!
      end subroutine nonlinear
!*
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_momentum_eq_explicit
!
      use m_work_time
!
!   ----  lead nonlinear terms by phesdo spectrum
!
!
      call start_eleps_time(14)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD
      call end_eleps_time(14)
!
      call start_eleps_time(15)
      if (iflag_debug.ge.1) write(*,*) 's_cal_nonlinear_sph_MHD'
      call s_cal_nonlinear_sph_MHD
      call end_eleps_time(15)
!
      call start_eleps_time(16)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD
      call end_eleps_time(16)
!
      call start_eleps_time(17)
      if (iflag_debug.ge.1) write(*,*) 'cal_momentum_eq_exp_sph'
      call cal_momentum_eq_exp_sph
      call end_eleps_time(17)
!
      end subroutine nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_exp
!
      use m_sph_phys_address
      use cal_nonlinear_sph_MHD
      use cal_vorticity_terms_adams
      use const_coriolis_sph
!
      integer(kind = kint) :: inod
!
!*  ----  copy velocity for coriolis term ------------------
!*
      if (iflag_debug.eq.1) write(*,*) 'sum_coriolis_rj_sph'
      if(iflag_4_coriolis .ne. id_turn_OFF) call sum_coriolis_rj_sph
!
!   ----  lead nonlinear terms by phesdo spectrum
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_h_advect) = 0.0d0
        d_rj(inod,ipol%i_forces) =   0.0d0
        d_rj(inod,itor%i_forces) =   0.0d0
      end do
!$omp end parallel do
!
      if (iflag_4_ref_temp .eq. id_sphere_ref_temp) then
        call add_reftemp_advect_sph_MHD
      end if
!
!$omp parallel
      if(iflag_4_coriolis .ne. id_turn_OFF) then
        call add_coriolis_to_vort_force
      end if
      if(iflag_4_gravity .ne.  id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor%i_rot_buoyancy)
      else if(iflag_4_composit_buo .ne. id_turn_OFF) then
        call add_buoyancy_to_vort_force(itor%i_rot_comp_buo)
      end if
!$omp end parallel
!
!
      end subroutine licv_exp
!*
!*   ------------------------------------------------------------------
!
      end module cal_nonlinear
