!>@file   m_elapsed_labels_SPH_TRNS.f90
!!@brief  module m_elapsed_labels_SPH_TRNS
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2018
!
!>@brief  Labels for elapsed time monitor
!!
!!@verbatim
!!      subroutine elpsed_label_4_sph_trns
!!      subroutine elpsed_label_4_sph_detail
!!      subroutine elpsed_label_4_fft_detail
!!      subroutine elapsed_label_4_Legendre_trans
!!
!!      subroutine reset_elapse_after_init_SPH
!!      subroutine reset_elapse_after_init_SDT
!!      subroutine reset_elapse_after_init_FFT
!!      subroutine reset_elapse_after_init_LEG
!!@endverbatim
!!
      module m_elapsed_labels_SPH_TRNS
!
      use m_precision
      use m_work_time
!
      implicit none
!
!
      logical, save :: iflag_SPH_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SPH =   0
      integer(kind = kint), save :: ied_elapsed_SPH =   0
!
      logical, save :: iflag_SDT_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_SDT =   0
      integer(kind = kint), save :: ied_elapsed_SDT =   0
!
      logical, save :: iflag_FFT_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_FFT =   0
      integer(kind = kint), save :: ied_elapsed_FFT =   0
!
      logical, save :: iflag_LEG_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_LEG =   0
      integer(kind = kint), save :: ied_elapsed_LEG =   0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_sph_trns
!
      integer(kind = kint), parameter :: num_append = 10
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SPH, ied_elapsed_SPH)
!
      elps1%labels(ist_elapsed_SPH+ 1) = 'transfer rj  => rlm        '
      elps1%labels(ist_elapsed_SPH+ 2) = 'transfer rtm => rtp        '
      elps1%labels(ist_elapsed_SPH+ 3) = 'transfer rtp => rtm        '
      elps1%labels(ist_elapsed_SPH+ 4) = 'transfer rlm => rj         '
      elps1%labels(ist_elapsed_SPH+ 5) = 'Legendre backward transform'
      elps1%labels(ist_elapsed_SPH+ 6) = 'Legendre forward transform '
      elps1%labels(ist_elapsed_SPH+ 7) = 'Fourier transform          '
      elps1%labels(ist_elapsed_SPH+ 8) = 'mhd_spectr_to_sendbuf      '
      elps1%labels(ist_elapsed_SPH+ 9) = 'mhd_spectr_from_recvbuf    '
      elps1%labels(ist_elapsed_SPH+10) = 'Communication for transform'
!
      iflag_SPH_time = .TRUE.
!
      end subroutine elpsed_label_4_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_sph_detail
!
      integer(kind = kint), parameter :: num_append = 17
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SDT, ied_elapsed_SDT)
!
      elps1%labels(ist_elapsed_SDT+ 1) = 'order_b_trans_vector    '
      elps1%labels(ist_elapsed_SDT+ 2) = 'clear_b_trans_vector    '
      elps1%labels(ist_elapsed_SDT+ 3) = 'legendre_b_trans_vector '
      elps1%labels(ist_elapsed_SDT+ 4) = 'back_b_trans_vector     '
      elps1%labels(ist_elapsed_SDT+ 5) = 'order_f_trans_vector    '
      elps1%labels(ist_elapsed_SDT+ 6) = 'clear_f_trans_vector    '
      elps1%labels(ist_elapsed_SDT+ 7) = 'legendre_f_trans_vector '
      elps1%labels(ist_elapsed_SDT+ 8) = 'back_f_trans_vector     '
!
      elps1%labels(ist_elapsed_SDT+ 9) = 'set_sp_rlm_vec from recv '
      elps1%labels(ist_elapsed_SDT+10) = 'on-the-fly Legendre for bwd '
      elps1%labels(ist_elapsed_SDT+11) = 'matmul_bwd_leg_trans    '
      elps1%labels(ist_elapsed_SDT+12) = 'cal_vr_rtm_vec to send '
!
      elps1%labels(ist_elapsed_SDT+13) = 'set_vr_rtm_vec from recv '
      elps1%labels(ist_elapsed_SDT+14) = 'on-the-fly Legendre for fwd '
      elps1%labels(ist_elapsed_SDT+15) = 'matmul_fwd_leg_trans    '
      elps1%labels(ist_elapsed_SDT+16) = 'Get total from OpenMP area'
      elps1%labels(ist_elapsed_SDT+17) = 'cal_sp_rlm_vec to send '
!
      iflag_SDT_time = .TRUE.
!
      end subroutine elpsed_label_4_sph_detail
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_fft_detail
!
      integer(kind = kint), parameter :: num_append = 6
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_FFT, ied_elapsed_FFT)
!
      elps1%labels(ist_elapsed_FFT+ 1) = 'Copy_from_recv_bwd_FFT  '
      elps1%labels(ist_elapsed_FFT+ 2) = 'Backward_FFT            '
      elps1%labels(ist_elapsed_FFT+ 3) = 'Copy_to_data_bwd_FFT    '
!
      elps1%labels(ist_elapsed_FFT+ 4) = 'Copy_from_data_fwd_FFT  '
      elps1%labels(ist_elapsed_FFT+ 5) = 'Forward_FFT             '
      elps1%labels(ist_elapsed_FFT+ 6) = 'Copy_to_send_fwd_FFT    '
!
      iflag_FFT_time = .TRUE.
!
      end subroutine elpsed_label_4_fft_detail
!
!-----------------------------------------------------------------------
!
      subroutine elapsed_label_4_Legendre_trans
!
      integer(kind = kint), parameter :: num_append = 10
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_LEG, ied_elapsed_LEG)
!
      elps1%labels(ist_elapsed_LEG+1)                                   &
     &         = 'cal_sp_rlm_sym_mat_rin_loop1  '
      elps1%labels(ist_elapsed_LEG+2)                                   &
     &         = 'cal_sp_rlm_sym_mat_rin_loop2  '
      elps1%labels(ist_elapsed_LEG+3)                                   &
     &         = 'cal_sp_rlm_sym_mat_rin_loop3  '
      elps1%labels(ist_elapsed_LEG+4)                                   &
     &         = 'cal_sp_rlm_sym_mat_rin_loop4  '
      elps1%labels(ist_elapsed_LEG+5)                                   &
     &         = 'cal_sp_rlm_sym_mat_rin_loop5  '
      elps1%labels(ist_elapsed_LEG+6)                                   &
     &         = 'cal_sp_rlm_sym_mat_rin_loop6  '
      elps1%labels(ist_elapsed_LEG+7)                                   &
     &         = 'Copy field for fwd. trans.     '
      elps1%labels(ist_elapsed_LEG+8)                                   &
     &         = 'mat product for fwd. trans.    '
      elps1%labels(ist_elapsed_LEG+9)                                   &
     &         = 'Copy spectrum to fwd. trans.   '
!
      elps1%labels(ist_elapsed_LEG+10)                                  &
     &         = 'mhd_spectr_to_sendbuf.   '
!
      iflag_LEG_time = .TRUE.
!
      end subroutine elapsed_label_4_Legendre_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SPH
!
!
      if(iflag_SPH_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_SPH+1, ied_elapsed_SPH)
!
      end subroutine reset_elapse_after_init_SPH
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SDT
!
!
      if(iflag_SDT_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_SDT+1, ied_elapsed_SDT)
!
      end subroutine reset_elapse_after_init_SDT
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_FFT
!
!
      if(iflag_FFT_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_FFT+1, ied_elapsed_FFT)
!
      end subroutine reset_elapse_after_init_FFT
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_LEG
!
!
      if(iflag_LEG_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_LEG+1, ist_elapsed_LEG)
!
      end subroutine reset_elapse_after_init_LEG
!
!-----------------------------------------------------------------------
!
      end module m_elapsed_labels_SPH_TRNS
