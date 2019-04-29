!>@file   m_elapsed_labels_4_VIZ.f90
!!@brief  module m_elapsed_labels_4_VIZ
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_VIZ
!!      subroutine reset_elapse_after_init_VIZ
!!@endverbatim
!
      module m_elapsed_labels_4_VIZ
!
      use m_precision
      use m_work_time
!
      implicit none
!
      logical, save :: iflag_VIZ_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_VIZ =   0
      integer(kind = kint), save :: ied_elapsed_VIZ =   0
!
      logical, save :: iflag_PVR_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_PVR =   0
      integer(kind = kint), save :: ied_elapsed_PVR =   0
!
      logical, save :: iflag_LIC_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_LIC =   0
      integer(kind = kint), save :: ied_elapsed_LIC =   0
!
      private :: elpsed_label_4_VIZ_outline
      private :: elpsed_label_4_PVR, elpsed_label_4_LIC
      private :: reset_elapse_after_init_VIZ_top
      private :: reset_elapse_after_init_PVR
      private :: reset_elapse_after_init_LIC
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ
!
!
      call elpsed_label_4_VIZ_outline
      call elpsed_label_4_PVR
      call elpsed_label_4_LIC
!
      end subroutine elpsed_label_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ
!
!
      call reset_elapse_after_init_VIZ_top
      call reset_elapse_after_init_PVR
      call reset_elapse_after_init_LIC
!
      end subroutine reset_elapse_after_init_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ_outline
!
      integer(kind = kint), parameter :: num_append = 10
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_VIZ, ied_elapsed_VIZ)
!
      elps1%labels(ist_elapsed_VIZ+ 1)                                  &
     &                    = 'Sectioning initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 2)                                  &
     &                    = 'Isosurfaceing initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 3)                                  &
     &                    = 'Volume rendering initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 4)                                  &
     &                    = 'fieldline initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 5)                                  &
     &                    = 'LIC rendering initialization.    '
!
      elps1%labels(ist_elapsed_VIZ+ 6) = 'Sectioning.    '
      elps1%labels(ist_elapsed_VIZ+ 7) = 'Isosurfaceing.    '
      elps1%labels(ist_elapsed_VIZ+ 8) = 'Volume rendering.    '
      elps1%labels(ist_elapsed_VIZ+ 9) = 'fieldline.    '
      elps1%labels(ist_elapsed_VIZ+10) = 'LIC rendering.    '
!
      iflag_VIZ_time = .TRUE.
!
      end subroutine elpsed_label_4_VIZ_outline
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_PVR
!
      integer(kind = kint), parameter :: num_append = 4
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_PVR, ied_elapsed_PVR)
!
      elps1%labels(ist_elapsed_PVR+1)                                  &
     &                    = 'Volume rendering w/o file output   '
      elps1%labels(ist_elapsed_PVR+2)                                  &
     &                    = 'Volume rendering file output   '
      elps1%labels(ist_elapsed_PVR+3)                                  &
     &                    = 'V. Rendering ray trace   '
      elps1%labels(ist_elapsed_PVR+4)                                  &
     &                    = 'V. Rendering subimage composit   '
!
      iflag_PVR_time = .TRUE.
!
      end subroutine elpsed_label_4_PVR
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_LIC
!
      integer(kind = kint), parameter :: num_append = 4
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_LIC, ied_elapsed_LIC)
!
      elps1%labels(ist_elapsed_LIC+1)                                   &
     &                    = 'LIC V. rendering w/o file output   '
      elps1%labels(ist_elapsed_LIC+2)                                   &
     &                    = 'LIC V. rendering file output   '
      elps1%labels(ist_elapsed_LIC+3)                                   &
     &                    = 'LIC V. Rendering ray trace   '
      elps1%labels(ist_elapsed_LIC+4)                                   &
     &                    = 'LIC V. Rendering subimage composit   '
!
      iflag_LIC_time = .TRUE.
!
      end subroutine elpsed_label_4_LIC
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ_top
!
!
      if(iflag_VIZ_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_VIZ+6, ied_elapsed_VIZ)
!
      end subroutine reset_elapse_after_init_VIZ_top
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_PVR
!
!
      if(iflag_PVR_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_PVR+1, ied_elapsed_PVR)
!
      end subroutine reset_elapse_after_init_PVR
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_LIC
!
!
      if(iflag_LIC_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_LIC+1, ied_elapsed_LIC)
!
      end subroutine reset_elapse_after_init_LIC
!
!-----------------------------------------------------------------------
!
      end module  m_elapsed_labels_4_VIZ
