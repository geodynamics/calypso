!>@file   const_data_4_dynamobench.f90
!!@brief  module const_data_4_dynamobench
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine const_dynamobench_data                               &
!!     &         (time_d, sph_params, sph_rj, sph_MHD_bc, trans_p, ipol,&
!!     &          rj_fld, pwr, cdat, bench)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!@endverbatim
!
      module const_data_4_dynamobench
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_dynamobench_data                                 &
     &         (time_d, sph_params, sph_rj, sph_MHD_bc, trans_p, ipol,  &
     &          rj_fld, pwr, cdat, bench)
!
      use field_at_mid_equator
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_time_data
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use field_at_mid_equator
      use t_boundary_data_sph_MHD
      use t_field_on_circle
      use t_field_4_dynamobench
      use t_work_4_sph_trans
!
      use calypso_mpi_real
      use sph_fwd_trans_mid_eq
      use cal_rms_fields_by_sph
      use global_field_4_dynamobench
      use transfer_to_long_integers
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
      integer(kind = kint) :: irank_copy
!
!
      if(bench%iflag_dynamobench .le. 0) return
!
      if(iflag_debug.gt.0)  write(*,*) 'mid_eq_transfer_dynamobench'
      call mid_eq_transfer_dynamobench(time_d%time, trans_p%iflag_FFT,  &
     &    sph_rj, rj_fld, ipol, cdat, bench)
!
      if(bench%ipwr_ocore .gt. 0) then
        irank_copy = pwr%v_spectr(bench%ipwr_ocore)%irank_m
        if(my_rank .eq. irank_copy)  then
          call copy_kin_energy_4_dbench(bench%ipwr_ocore, pwr,          &
     &                                  bench%KE_bench)
          call copy_mag_energy_4_dbench(bench%ipwr_ocore, pwr,          &
     &                                  bench%ME_bench)
        end if
        call calypso_mpi_bcast_real(bench%KE_bench, cast_long(ithree),  &
     &                              irank_copy)
        call calypso_mpi_bcast_real(bench%ME_bench, cast_long(ithree),  &
     &                              irank_copy)
      end if
!
      if(sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call pick_inner_core_rotation(sph_rj%idx_rj_degree_one,         &
     &      sph_rj%nidx_rj, sph_params%nlayer_ICB, sph_rj%ar_1d_rj,     &
     &      ipol%base%i_velo, rj_fld%n_point, rj_fld%ntot_phys,         &
     &      rj_fld%d_fld, bench%rotate_icore)
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        if(bench%ipwr_icore .gt. 0) then
          irank_copy = pwr%v_spectr(bench%ipwr_ocore)%irank_m
          if(my_rank .eq. irank_copy)  then
            call copy_mag_energy_4_dbench(bench%ipwr_icore, pwr,        &
     &                                    bench%mene_icore)
          end if
          call calypso_mpi_bcast_real(bench%mene_icore,                 &
     &                                cast_long(ithree), irank_copy)
        end if
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center       &
     &   .and. sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic)   &
     & then
        call pick_mag_torque_inner_core                                 &
     &     (sph_rj%idx_rj_degree_one,  sph_rj%nidx_rj,                  &
     &      sph_params%nlayer_ICB, sph_rj%radius_1d_rj_r,               &
     &      ipol%forces%i_lorentz, rj_fld%n_point, rj_fld%ntot_phys,    &
     &      rj_fld%d_fld, bench%m_torque_icore)
      end if
!
      end subroutine const_dynamobench_data
!
! ----------------------------------------------------------------------
!
      end module const_data_4_dynamobench
