!>@file   sph_fwd_trans_mid_eq.f90
!!@brief  module sph_fwd_trans_mid_eq
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine mid_eq_transfer_dynamobench                          &
!!     &         (time, iflag_FFT, sph_rj, rj_fld, ipol, cdat, bench)
!!        real(kind=kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(in) :: ipol
!!        type(circle_fld_maker), intent(inout) :: cdat
!!        type(dynamobench_monitor), intent(inout) :: bench
!!@endverbatim
!
      module sph_fwd_trans_mid_eq
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_field_on_circle
      use t_sph_circle_parameters
      use t_FFT_selector
      use t_field_on_circle
      use t_field_4_dynamobench
!
      implicit none
!
      private :: dbench_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mid_eq_transfer_dynamobench                            &
     &         (time, iflag_FFT, sph_rj, rj_fld, ipol, cdat, bench)
!
      use field_at_mid_equator
      use circle_bwd_transfer_rj
!
      real(kind=kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(circle_fld_maker), intent(inout) :: cdat
      type(dynamobench_monitor), intent(inout) :: bench
!
!    spherical transfer
      call dbench_leg_bwd_trans_rj(iflag_FFT, sph_rj, rj_fld, ipol,     &
     &    bench%iphys_dbench, cdat%circle, cdat%leg_circ,               &
     &    cdat%d_circle, cdat%WK_circle_fft)
!
      if(iflag_debug.gt.0) then
        call check_mid_eq_trans_dbench(ipol, cdat%circle,               &
     &      cdat%leg_circ, cdat%d_circle, bench)
      end if
!
!   Evaluate drift frequencty by velocity 
!
      call cal_drift_by_v44(time, sph_rj, rj_fld, ipol,                 &
     &    cdat%circle, bench%t_prev, bench%phase_vm4,                   &
     &    bench%phase_vm4_prev, bench%omega_vm4)
      if(my_rank .gt. 0) return
!
!   find local point for dynamobench
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_field_4_dynamobench'
      if(my_rank .eq. 0) then
        call cal_field_4_dynamobench                                    &
     &     (time, bench%t_prev, cdat%circle, cdat%d_circle,             &
     &      bench%iphys_dbench%i_velo, bench%phi_zero, bench%phi_prev,  &
     &      bench%phase_vr, bench%ave_phase_vr, bench%d_zero)
      end if
      bench%t_prev = time
!
      end subroutine mid_eq_transfer_dynamobench
!
! ----------------------------------------------------------------------
!
      subroutine dbench_leg_bwd_trans_rj                                &
     &         (iflag_FFT, sph_rj, rj_fld, ipol, iphys_dbench,          &
     &          circle, leg_circ, d_circle, WK_circle_fft)
!
      use t_base_field_labels
      use t_FFT_selector
      use calypso_mpi_real
      use transfer_to_long_integers
      use circle_bwd_transfer_rj
      use cal_circle_transform
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(in) :: ipol
      type(base_field_address), intent(in) :: iphys_dbench
      type(circle_parameters), intent(in) :: circle
!
      type(phys_data), intent(inout) :: d_circle
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
      type(working_FFTs), intent(inout) :: WK_circle_fft
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: i_comp
!
!
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_vector, ipol%base%i_velo, iphys_dbench%i_velo,              &
     &    sph_rj, rj_fld, circle, leg_circ%ltr_circle,                  &
     &    leg_circ%P_circ, leg_circ%dPdt_circ,                          &
     &    leg_circ%ar_circle, leg_circ%ar2_circle,                      &
     &    d_circle%ntot_phys, leg_circ%d_circ_lc)
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_vector, ipol%base%i_magne, iphys_dbench%i_magne,            &
     &    sph_rj, rj_fld, circle, leg_circ%ltr_circle,                  &
     &    leg_circ%P_circ, leg_circ%dPdt_circ,                          &
     &    leg_circ%ar_circle, leg_circ%ar2_circle,                      &
     &    d_circle%ntot_phys, leg_circ%d_circ_lc)
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_scalar, ipol%base%i_temp, iphys_dbench%i_temp,              &
     &    sph_rj, rj_fld, circle, leg_circ%ltr_circle,                  &
     &    leg_circ%P_circ, leg_circ%dPdt_circ,                          &
     &    leg_circ%ar_circle, leg_circ%ar2_circle,                      &
     &    d_circle%ntot_phys, leg_circ%d_circ_lc)
      call each_circle_leg_bwd_trans_rj                                 &
     &   (n_scalar, ipol%base%i_light, iphys_dbench%i_light,            &
     &    sph_rj, rj_fld, circle, leg_circ%ltr_circle,                  &
     &    leg_circ%P_circ, leg_circ%dPdt_circ,                          &
     &    leg_circ%ar_circle, leg_circ%ar2_circle,                      &
     &    d_circle%ntot_phys, leg_circ%d_circ_lc)
!
      num64                                                             &
     &  = cast_long(d_circle%ntot_phys * (2*leg_circ%ltr_circle+1))
      call calypso_mpi_reduce_real                                      &
     &   (leg_circ%d_circ_lc(-leg_circ%ltr_circle,1),                   &
     &    leg_circ%d_circ_gl(-leg_circ%ltr_circle,1),                   &
     &    num64, MPI_SUM, 0)
!
      if(my_rank .ne. 0) return
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        if(iphys_dbench%i_velo .gt. 0) then
          i_comp = iphys_dbench%i_velo
          call overwrt_circle_sph_vect_2_cyl                            &
     &      (leg_circ%theta_circle, leg_circ%ltr_circle,                &
     &       leg_circ%d_circ_gl(-leg_circ%ltr_circle,i_comp))
        end if
        if(iphys_dbench%i_magne .gt. 0) then
          i_comp = iphys_dbench%i_magne
          call overwrt_circle_sph_vect_2_cyl                            &
     &      (leg_circ%theta_circle, leg_circ%ltr_circle,                &
     &       leg_circ%d_circ_gl(-leg_circ%ltr_circle,i_comp))
        end if
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (d_circle%ntot_phys, leg_circ%ltr_circle, leg_circ%d_circ_gl,  &
     &    leg_circ%vrtm_mag, leg_circ%vrtm_phase)
      call copy_circle_spectrum_4_fft                                   &
     &   (d_circle%ntot_phys, leg_circ%ltr_circle,                      &
     &    leg_circ%d_circ_gl, circle%mphi_circle, d_circle%d_fld(1,1))
!
      do i_comp = 1, d_circle%ntot_phys
        call backward_FFT_select                                        &
     &     (iflag_FFT, np_smp, leg_circ%istack_circfft_smp, ione,       &
     &      circle%mphi_circle, d_circle%d_fld(1,i_comp),               &
     &      WK_circle_fft)
      end do
!
      end subroutine dbench_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      subroutine check_mid_eq_trans_dbench(ipol, circle, leg_circ,      &
     &                                     d_circle, bench)
!
      use t_field_on_circle
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_circle_transform
      use t_field_4_dynamobench
!
      type(phys_address), intent(in) :: ipol
      type(circle_parameters), intent(in) :: circle
      type(circle_transform_spectr), intent(in) :: leg_circ
      type(phys_data), intent(in) :: d_circle
      type(dynamobench_monitor), intent(in) :: bench
!
      integer :: i, j, n, ifld
!
!    spherical transfer
      if(my_rank .ne. 0) return
      i = bench%iphys_dbench%i_velo
      write(60,*) 'j, velo_new', ipol%base%i_velo, i
      do j = -leg_circ%ltr_circle, leg_circ%ltr_circle
        write(60,*) j, leg_circ%d_circ_gl(j,i:i+2)
      end do
      i = bench%iphys_dbench%i_magne
      write(60,*) 'j, magne_new', ipol%base%i_magne, i
      do j = -leg_circ%ltr_circle, leg_circ%ltr_circle
        write(60,*) j, leg_circ%d_circ_gl(j,i:i+2)
      end do
!!
      i = bench%iphys_dbench%i_temp
      write(60,*) 'j, temp_new', ipol%base%i_temp, i
      do j = -leg_circ%ltr_circle, leg_circ%ltr_circle
      write(60,*) j, leg_circ%d_circ_gl(j,i)
      end do
!
      do ifld = 1, d_circle%num_phys_viz
        i = d_circle%istack_component(ifld-1)
        n = d_circle%istack_component(ifld) - i
        write(60,*) 'j', trim(d_circle%phys_name(ifld)), ifld, i
        do j = 1, circle%mphi_circle
          write(60,*) j, d_circle%d_fld(j,i+1:i+n)
        end do
      end do
!
      end subroutine check_mid_eq_trans_dbench
!
! ----------------------------------------------------------------------
!!
      end module sph_fwd_trans_mid_eq
