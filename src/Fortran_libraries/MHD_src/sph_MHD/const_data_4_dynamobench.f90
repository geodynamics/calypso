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
!!     &          rj_fld, cdat, pwr, WK_pwr)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(phys_data), intent(in) :: rj_fld
!!        type(circle_fld_maker), intent(inout) :: cdat
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
! ----------------------------------------------------------------------
!
      subroutine s_const_data_4_dynamobench                             &
     &         (time, sph_params, sph_rj, sph_MHD_bc, leg, ipol, itor,  &
     &          rj_fld, cdat, pwr, WK_pwr)
!
      use m_field_at_mid_equator
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
      use t_boundary_data_sph_MHD
      use t_field_on_circle
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
      type(sph_mean_square_work), intent(inout) :: WK_pwr
!
!
      if(iflag_debug.gt.0)  write(*,*) 'mid_eq_transfer_dynamobench'
      call mid_eq_transfer_dynamobench(time, sph_rj, rj_fld, cdat)
!
      pwr%v_spectr(1)%kr_inside =  sph_params%nlayer_ICB
      pwr%v_spectr(1)%kr_outside = sph_params%nlayer_CMB
      call cal_mean_squre_in_shell                                      &
     &   (sph_params%l_truncation, sph_rj, ipol, rj_fld, leg%g_sph_rj,  &
     &    pwr, WK_pwr)
      if(my_rank .eq. 0) call copy_energy_4_dynamobench(pwr)
!
      if(sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call pick_inner_core_rotation(sph_rj%idx_rj_degree_one,         &
     &      sph_rj%nidx_rj, sph_params%nlayer_ICB, sph_rj%ar_1d_rj,     &
     &      itor%i_velo, rj_fld%n_point, rj_fld%ntot_phys,              &
     &      rj_fld%d_fld)
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center) then
        pwr%v_spectr(1)%kr_inside =  izero
        pwr%v_spectr(1)%kr_outside = sph_params%nlayer_ICB
        call cal_mean_squre_in_shell                                    &
     &     (sph_params%l_truncation, sph_rj, ipol, rj_fld,              &
     &      leg%g_sph_rj, pwr, WK_pwr)
        if(my_rank .eq. 0) call copy_icore_energy_4_dbench(pwr)
      end if
!
      if(sph_MHD_bc%sph_bc_B%iflag_icb .eq. iflag_sph_fill_center       &
     &   .and. sph_MHD_bc%sph_bc_U%iflag_icb .eq. iflag_rotatable_ic)   &
     & then
        call pick_mag_torque_inner_core                                 &
     &     (sph_rj%idx_rj_degree_one,  sph_rj%nidx_rj,                  &
     &      sph_params%nlayer_ICB, sph_rj%radius_1d_rj_r,               &
     &      itor%i_lorentz, rj_fld%n_point, rj_fld%ntot_phys,           &
     &      rj_fld%d_fld)
      end if
!
      end subroutine s_const_data_4_dynamobench
!
! ----------------------------------------------------------------------
!
      end module const_data_4_dynamobench
