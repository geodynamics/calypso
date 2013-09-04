!>@file   cal_div_of_forces.f90
!!@brief  module cal_div_of_forces
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct. 2009
!
!>@brief Evaluate divergence of forces for pressure evaluation
!!
!!@verbatim
!!      subroutine s_cal_div_of_forces
!!@endverbatim
!
      module cal_div_of_forces
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit  none
!
      private :: set_MHD_terms_to_div_force, set_div_cv_terms_to_force
      private :: add_term_to_div_force, set_div_advection_to_force
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_div_of_forces
!
      use m_control_parameter
!
!
!$omp parallel
      if(      iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .ne. id_turn_OFF) then
        call set_MHD_terms_to_div_force(ipol%i_div_buoyancy)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .ne.     id_turn_OFF) then
        call set_MHD_terms_to_div_force(ipol%i_div_comp_buo)
      else if( iflag_4_gravity  .ne. id_turn_OFF                        &
     &   .and. iflag_4_coriolis .ne. id_turn_OFF                        &
     &   .and. iflag_4_lorentz  .eq. id_turn_OFF) then
        call set_div_cv_terms_to_force(ipol%i_div_buoyancy)
      else if( iflag_4_gravity  .eq.     id_turn_OFF                    &
     &   .and. iflag_4_composit_buo .ne. id_turn_OFF                    &
     &   .and. iflag_4_coriolis .ne.     id_turn_OFF                    &
     &   .and. iflag_4_lorentz  .eq.     id_turn_OFF) then
        call set_div_cv_terms_to_force(ipol%i_div_comp_buo)
      else
!
        call set_div_advection_to_force
        if(iflag_4_coriolis .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol%i_div_Coriolis)
        end if
        if(iflag_4_lorentz .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol%i_div_Lorentz)
        end if
        if(iflag_4_gravity .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol%i_div_buoyancy)
        else if(iflag_4_composit_buo .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol%i_div_comp_buo)
        else if(iflag_4_filter_gravity .ne. id_turn_OFF) then
          call add_term_to_div_force(ipol%i_div_filter_buo)
        end if
!
      end if
!$omp end parallel
!
      end subroutine s_cal_div_of_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_MHD_terms_to_div_force(is_div_buo)
!
      integer(kind = kint), intent(in) :: is_div_buo
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
      subroutine set_div_cv_terms_to_force(is_div_buo)
!
      integer(kind = kint), intent(in) :: is_div_buo
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
      subroutine set_div_advection_to_force
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
      subroutine add_term_to_div_force(is_div)
!
      integer(kind = kint), intent(in) :: is_div
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
      end module cal_div_of_forces
