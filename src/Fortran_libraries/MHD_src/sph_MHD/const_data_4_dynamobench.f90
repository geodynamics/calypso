!>@file   const_data_4_dynamobench.f90
!!@brief  module const_data_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine s_const_data_4_dynamobench                           &
!!     &         (time, sph_params, sph_rj, sph_MHD_bc, leg, ipol, itor,&
!!     &          rj_fld, cdat, pwr, bench, WK_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(phys_data), intent(in) :: rj_fld
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(sph_mean_square_work), intent(inout) :: WK_pwr
!!@endverbatim
!
      module const_data_4_dynamobench
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_data_4_dynamobench                             &
     &         (time, sph_params, sph_rj, sph_MHD_bc, leg, ipol, itor,  &
     &          rj_fld, cdat, pwr, bench, WK_pwr)
!
      use field_at_mid_equator
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use field_at_mid_equator
      use t_boundary_data_sph_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
!
      use calypso_mpi
      use cal_rms_fields_by_sph
      use global_field_4_dynamobench
!
      real(kind=kreal), intent(in) :: time
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol, itor
      type(phys_data), intent(in) :: rj_fld
!
      type(circle_fld_maker), intent(inout) :: cdat
      type(sph_mean_squares), intent(inout) :: pwr
      type(dynamobench_monitor), intent(inout) :: bench
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(iflag_debug.gt.0)  write(*,*) 'mid_eq_transfer_dynamobench'
      call mid_eq_transfer_dynamobench                                  &
     &   (time, sph_rj, rj_fld, cdat, bench)
!
      pwr%v_spectr(1)%kr_inside =  sph_params%nlayer_ICB
      pwr%v_spectr(1)%kr_outside = sph_params%nlayer_CMB
      call cal_mean_squre_in_shell                                      &
     &   (sph_params%l_truncation, sph_rj, ipol, rj_fld, leg%g_sph_rj,  &
     &    pwr, WK_pwr)
      if(my_rank .eq. 0) then
        call copy_energy_4_dynamobench                                  &
     &     (pwr, bench%KE_bench, bench%ME_bench)
      end if
!
      if(sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call pick_inner_core_rotation(sph_rj%idx_rj_degree_one,         &
     &      sph_rj%nidx_rj, sph_params%nlayer_ICB, sph_rj%ar_1d_rj,     &
     &      itor%i_velo, rj_fld%n_point, rj_fld%ntot_phys,              &
     &      rj_fld%d_fld, bench%rotate_icore)
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        pwr%v_spectr(1)%kr_inside =  izero
        pwr%v_spectr(1)%kr_outside = sph_params%nlayer_ICB
        call cal_mean_squre_in_shell                                    &
     &     (sph_params%l_truncation, sph_rj, ipol, rj_fld,              &
     &      leg%g_sph_rj, pwr, WK_pwr)
        if(my_rank .eq. 0) then
          call copy_icore_energy_4_dbench(pwr, bench%mene_icore)
        end if
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center       &
     &   .and. sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic)   &
     & then
        call pick_mag_torque_inner_core                                 &
     &     (sph_rj%idx_rj_degree_one,  sph_rj%nidx_rj,                  &
     &      sph_params%nlayer_ICB, sph_rj%radius_1d_rj_r,               &
     &      itor%i_lorentz, rj_fld%n_point, rj_fld%ntot_phys,           &
     &      rj_fld%d_fld, bench%m_torque_icore)
      end if
!
      end subroutine s_const_data_4_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine mid_eq_transfer_dynamobench                            &
     &         (time, sph_rj, rj_fld, cdat, bench)
!
      use calypso_mpi
      use t_field_on_circle
      use t_spheric_rj_data
      use t_phys_data
      use t_circle_transform
      use field_at_mid_equator
      use t_field_4_dynamobench
!
      real(kind=kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
!    spherical transfer
!
      call sph_transfer_on_circle(sph_rj, rj_fld, cdat)
!
      if(my_rank .gt. 0) return
!
!   Evaluate drift frequencty by velocity 
!
      call cal_drift_by_v44(time, cdat%circle, bench%ibench_velo,       &
     &    bench%t_prev, bench%phase_vm4, bench%phase_vm4_prev,          &
     &    bench%omega_vm4)
!
!   find local point for dynamobench
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_field_4_dynamobench'
      call cal_field_4_dynamobench(cdat%circle, cdat%d_circle,          &
     &    bench%ibench_velo, bench%phi_zero, bench%phi_prev,            &
     &    bench%d_zero)
!
      end subroutine mid_eq_transfer_dynamobench
!
! ----------------------------------------------------------------------
!
      end module const_data_4_dynamobench
