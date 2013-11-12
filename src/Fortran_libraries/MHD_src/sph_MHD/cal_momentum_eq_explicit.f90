!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine cal_momentum_eq_exp_sph
!!      subroutine cal_expricit_sph_adams
!!      subroutine cal_expricit_sph_euler(i_step)
!!@endverbatim
!!
!!@param i_step  time step
!
      module cal_momentum_eq_explicit
!
      use m_precision
!
      use m_control_parameter
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_momentum_eq_exp_sph
!
      use cal_explicit_terms
      use calypso_mpi
      use cal_sph_field_by_rotation
      use cal_nonlinear_sph_MHD
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_rot_of_forces_sph_2'
      call cal_rot_of_forces_sph_2
!
      call cal_rot_of_induction_sph
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph
!
      end subroutine cal_momentum_eq_exp_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_expricit_sph_adams
!
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
!
!
!$omp parallel
      if(iflag_t_evo_4_velo .gt.     id_no_evolution) then
        call cal_vorticity_eq_adams
      end if
!
      if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call cal_diff_induction_MHD_adams
      end if
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call sel_heat_diff_adv_src_adams
      end if
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        call sel_light_diff_adv_src_adams
      end if
!$omp end parallel
!
      end subroutine cal_expricit_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_expricit_sph_euler(i_step)
!
      use cal_explicit_terms
      use cal_vorticity_terms_adams
!
      integer(kind = kint), intent(in) :: i_step
!
!
!$omp parallel
      if(iflag_t_evo_4_velo .gt.     id_no_evolution) then
        call cal_vorticity_eq_euler
      end if
!
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call sel_heat_diff_adv_src_euler
      end if
      if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call cal_diff_induction_MHD_euler
      end if
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        call sel_light_diff_adv_src_euler
      end if
!
      if (i_step .eq. 1) then
        if(iflag_t_evo_4_velo .gt.     id_no_evolution) then
          call set_ini_adams_inertia
        end if
        if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
          call sel_ini_adams_heat_w_src
        end if
        if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
          call set_ini_adams_mag_induct
        end if
        if(iflag_t_evo_4_composit .gt. id_no_evolution) then
          call sel_ini_adams_light_w_src
        end if
      end if
!$omp end parallel
!
      end subroutine cal_expricit_sph_euler
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
