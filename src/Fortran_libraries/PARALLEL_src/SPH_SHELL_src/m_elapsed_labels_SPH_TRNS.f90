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
!!
!!      subroutine reset_elapse_after_init_SPH
!!      subroutine reset_elapse_after_init_SDT
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_sph_trns
!
      integer(kind = kint), parameter :: num_append = 7
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SPH, ied_elapsed_SPH)
!
      elps1%labels(ist_elapsed_SPH+1) = 'transfer rj  => rlm        '
      elps1%labels(ist_elapsed_SPH+2) = 'transfer rtm => rtp        '
      elps1%labels(ist_elapsed_SPH+3) = 'transfer rtp => rtm        '
      elps1%labels(ist_elapsed_SPH+4) = 'transfer rlm => rj         '
      elps1%labels(ist_elapsed_SPH+5) = 'Legendre backward transform'
      elps1%labels(ist_elapsed_SPH+6) = 'Legendre forward transform '
      elps1%labels(ist_elapsed_SPH+7) = 'Fourier transform          '
!
      iflag_SPH_time = .TRUE.
!
      end subroutine elpsed_label_4_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_sph_detail
!
      integer(kind = kint), parameter :: num_append = 11
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_SDT, ied_elapsed_SDT)
!
      elps1%labels(ist_elapsed_SDT+1) = 'order_b_trans_vector    '
      elps1%labels(ist_elapsed_SDT+2) = 'clear_b_trans_vector    '
      elps1%labels(ist_elapsed_SDT+3) = 'legendre_b_trans_vector '
      elps1%labels(ist_elapsed_SDT+4) = 'back_b_trans_vector     '
      elps1%labels(ist_elapsed_SDT+5) = 'order_f_trans_vector    '
      elps1%labels(ist_elapsed_SDT+6) = 'clear_f_trans_vector    '
      elps1%labels(ist_elapsed_SDT+7) = 'legendre_f_trans_vector '
      elps1%labels(ist_elapsed_SDT+8) = 'back_f_trans_vector     '
!
      elps1%labels(ist_elapsed_SDT+ 9) = 'copy_FFT_real       '
      elps1%labels(ist_elapsed_SDT+10) = 'dfftw_execute       '
      elps1%labels(ist_elapsed_SDT+11) = 'copy_FFT_complex    '
!
      iflag_SDT_time = .TRUE.
!
      end subroutine elpsed_label_4_sph_detail
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
      end module m_elapsed_labels_SPH_TRNS
