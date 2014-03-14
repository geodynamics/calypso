!>@file   init_sph_MHD_elapsed_label.f90
!!@brief  module init_sph_MHD_elapsed_label
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine set_sph_MHD_elapsed_label
!!@endverbatim
!
      module init_sph_MHD_elapsed_label
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_MHD_elapsed_label
!
      use m_work_time
!
!
      num_elapsed = 36
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                 '
      elapse_labels(2) = 'Initialization time        '
      elapse_labels(3) = 'Time evolution loop time   '
      elapse_labels(4) = 'Data IO time               '
      elapse_labels(5) = 'Communication for RHS      '
!
      elapse_labels( 6) = 'snapshots_control         '
      elapse_labels( 7) = 'lead_fields_4_sph_mhd     '
      elapse_labels( 8) = 'output_sph_restart_control'
      elapse_labels( 9) = 'Field data output         '
      elapse_labels(10) = 'output_rms_sph_mhd_control'
      elapse_labels(11) = 'PSF_time                  '
      elapse_labels(12) = 'Nonliner_terms            '
!
      elapse_labels(13) = 'Coriolis term             '
      elapse_labels(14) = 'sph backward transform    '
      elapse_labels(15) = 'cal nonlinear terms       '
      elapse_labels(16) = 'sph forward transform     '
      elapse_labels(17) = 'obtain explicit terms     '
!
      elapse_labels(18) = 'transfer rj  => rlm        '
      elapse_labels(19) = 'transfer rtm => rtp        '
      elapse_labels(20) = 'transfer rtp => rtm        '
      elapse_labels(21) = 'transfer rlm => rj         '
      elapse_labels(22) = 'Legendre backward transform'
      elapse_labels(23) = 'Legendre forward transform '
      elapse_labels(24) = 'Fourier transform          '
!
      elapse_labels(25) = 'order_b_trans_vector_spin  '
      elapse_labels(26) = 'clear_b_trans_vector_spin  '
      elapse_labels(27) = 'legendre_b_trans_vector_spin '
      elapse_labels(28) = 'back_b_trans_vector_spin'
      elapse_labels(29) = 'order_f_trans_vector_spin  '
      elapse_labels(30) = 'clear_f_trans_vector_spin  '
      elapse_labels(31) = 'legendre_f_trans_vector_spin '
      elapse_labels(32) = 'back_f_trans_vector_spin'
!
      elapse_labels(33) = 'copy_FFT_real       '
      elapse_labels(34) = 'dfftw_execute       '
      elapse_labels(35) = 'copy_FFT_complex    '
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
      end subroutine set_sph_MHD_elapsed_label
!
! ----------------------------------------------------------------------
!
      end module  init_sph_MHD_elapsed_label
