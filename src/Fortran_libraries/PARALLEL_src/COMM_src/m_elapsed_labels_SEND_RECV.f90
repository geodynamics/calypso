!>@file   m_elapsed_labels_SEND_RECV.f90
!!@brief  module m_elapsed_labels_SEND_RECV
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2018
!
!>@brief  Labels for elapsed time monitor
!!
!!@verbatim
!!      subroutine elpsed_label_field_send_recv
!!      subroutine elpsed_label_calypso_send_recv
!!
!!      subroutine reset_elapse_after_init_SR
!!@endverbatim
!!
      module m_elapsed_labels_SEND_RECV
!
      use m_precision
      use m_work_time
!
      implicit none
!
!
      logical, save :: iflag_FSR_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_FSR =   0
      integer(kind = kint), save, private :: ied_elapsed_FSR =   0
!
      logical, save :: iflag_CSR_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_CSR =   0
      integer(kind = kint), save, private :: ied_elapsed_CSR =   0
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_field_send_recv
!
      integer(kind = kint), parameter :: num_append = 1
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_FSR, ied_elapsed_FSR)
!
      elps1%labels(ist_elapsed_FSR+1) = 'Communication for field'
!
      iflag_FSR_time = .TRUE.
!
      end subroutine elpsed_label_field_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_calypso_send_recv
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_CSR, ied_elapsed_CSR)
!
      elps1%labels(ist_elapsed_CSR+1) = 'set_to_send_buf_N    '
      elps1%labels(ist_elapsed_CSR+2) = 'calypso_send_recv_core    '
      elps1%labels(ist_elapsed_CSR+3) = 'set_from_recv_buf_rev_N    '
!
      iflag_CSR_time = .TRUE.
!
      end subroutine elpsed_label_calypso_send_recv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SR
!
!
      if(iflag_CSR_time) then
        call reset_elapsed_times(ist_elapsed_FSR+1, ied_elapsed_FSR)
      end if
      if(iflag_CSR_time) then
        call reset_elapsed_times(ist_elapsed_CSR+1, ied_elapsed_CSR)
      end if
!
      end subroutine reset_elapse_after_init_SR
!
!-----------------------------------------------------------------------
!
      end module m_elapsed_labels_SEND_RECV
