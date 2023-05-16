!>@file   sph_fwd_trans_on_circles.f90
!!@brief  module sph_fwd_trans_on_circles
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in June, 2012
!
!>@brief Evaluate dynamo benchmark results
!!
!!@verbatim
!!      subroutine sph_forward_trans_on_circles(iflag_FFT,              &
!!     &          sph_rj, rj_fld, num_circles, cdat)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        integer(kind = kint), intent(in) :: num_circles
!!        type(circle_fld_maker), intent(inout) :: cdat(num_circles)
!!      subroutine check_mid_eq_trans_dbench(ipol, circle, leg_circ,    &
!!     &                                     d_circle, bench)
!!        type(phys_address), intent(in) :: ipol
!!        type(circle_parameters), intent(in) :: circle
!!        type(circle_transform_spectr), intent(in) :: leg_circ
!!        type(phys_data), intent(in) :: d_circle
!!        type(dynamobench_monitor), intent(in) :: bench
!!@endverbatim
!
      module sph_fwd_trans_on_circles
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
!
      implicit none
!
      private :: circle_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sph_forward_trans_on_circles(iflag_FFT,                &
     &          sph_rj, rj_fld, num_circles, cdat)
!
      use calypso_mpi_real
      use transfer_to_long_integers
      use circle_bwd_transfer_rj
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: num_circles
!
      type(circle_fld_maker), intent(inout) :: cdat(num_circles)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_circles
        call circle_leg_bwd_trans_rj(iflag_FFT, sph_rj, rj_fld,         &
     &    cdat(i)%ipol_circle_trns, cdat(i)%circle, cdat(i)%leg_circ,   &
     &    cdat(i)%d_circle, cdat(i)%WK_circle_fft)
      end do
!
      end subroutine sph_forward_trans_on_circles
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine circle_leg_bwd_trans_rj                                &
     &         (iflag_FFT, sph_rj, rj_fld, ipol_circle_trns,            &
     &          circle, leg_circ, d_circle, WK_circle_fft)
!
      use t_FFT_selector
      use calypso_mpi_real
      use transfer_to_long_integers
      use cal_circle_transform
      use circle_bwd_transfer_rj
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(circle_parameters), intent(in) :: circle
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
      type(phys_data), intent(inout) :: d_circle
      type(working_FFTs), intent(inout) :: WK_circle_fft
      integer(kind = kint), intent(in)                                  &
     &                      :: ipol_circle_trns(d_circle%num_phys_viz)
!
      integer(kind = kint) :: i_fld, num_comp, i_trns
      integer(kind = kint_gl) :: num64
!
!
      do i_fld = 1, d_circle%num_phys_viz
        num_comp = d_circle%istack_component(i_fld)                     &
     &            - d_circle%istack_component(i_fld-1)
        i_trns = d_circle%istack_component(i_fld-1) + 1
        call each_circle_leg_bwd_trans_rj                               &
     &     (num_comp, ipol_circle_trns(i_fld), i_trns,                  &
     &      sph_rj, rj_fld, circle, leg_circ%ltr_circle,                &
     &      leg_circ%P_circ, leg_circ%dPdt_circ,                        &
     &      leg_circ%ar_circle, leg_circ%ar2_circle,                    &
     &      d_circle%ntot_phys, leg_circ%d_circ_lc)
      end do
!
      num64                                                             &
     &  = cast_long(d_circle%ntot_phys * (2*leg_circ%ltr_circle+1))
      call calypso_mpi_reduce_real                                      &
     &   (leg_circ%d_circ_lc(-leg_circ%ltr_circle,1),                   &
     &    leg_circ%d_circ_gl(-leg_circ%ltr_circle,1),                   &
     &    num64, MPI_SUM, 0)
!
!
      if(my_rank .ne. 0) return
!
      if(circle%iflag_circle_coord .eq. iflag_circle_cyl) then
        do i_fld = 1, d_circle%num_phys_viz
          num_comp = d_circle%istack_component(i_fld)                   &
     &            - d_circle%istack_component(i_fld-1)
          i_trns = d_circle%istack_component(i_fld-1) + 1
          if(num_comp .eq. n_vector) then
            call overwrt_circle_sph_vect_2_cyl                          &
     &         (leg_circ%theta_circle, leg_circ%ltr_circle,             &
     &          leg_circ%d_circ_gl(-leg_circ%ltr_circle,i_trns))
!          else if(num_comp .eq. n_sym_tensor) then
!            call overwrt_circle_sph_vect_2_cyl                         &
!     &         (leg_circ%theta_circle, leg_circ%ltr_circle,            &
!     &          leg_circ%d_circ_gl(-leg_circ%ltr_circle,i_trns))
          end if
        end do
      end if
!
      call cal_circle_spectrum_vector                                   &
     &   (d_circle%ntot_phys, leg_circ%ltr_circle, leg_circ%d_circ_gl,  &
     &    leg_circ%vrtm_mag, leg_circ%vrtm_phase)
      call copy_circle_spectrum_4_fft                                   &
     &   (d_circle%ntot_phys, leg_circ%ltr_circle,                      &
     &    leg_circ%d_circ_gl, circle%mphi_circle, d_circle%d_fld(1,1))
!
      do i_fld = 1, d_circle%ntot_phys
        call backward_FFT_select                                        &
     &    (iflag_FFT, np_smp, leg_circ%istack_circfft_smp, ione,        &
     &     circle%mphi_circle, d_circle%d_fld(1,i_fld), WK_circle_fft)
      end do
!
      end subroutine circle_leg_bwd_trans_rj
!
! ----------------------------------------------------------------------
!
      end module sph_fwd_trans_on_circles
