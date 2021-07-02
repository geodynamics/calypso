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
      logical, save :: iflag_PSF_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_PSF =   0
      integer(kind = kint), save :: ied_elapsed_PSF =   0
!
      logical, save :: iflag_ISO_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_ISO =   0
      integer(kind = kint), save :: ied_elapsed_ISO =   0
!
      private :: elpsed_label_4_VIZ_outline
      private :: elpsed_label_4_PVR, elpsed_label_4_LIC
      private :: reset_elapse_after_init_VIZ_top
!
      private :: reset_elapse_after_init_PVR
      private :: reset_elapse_after_init_LIC
      private :: reset_elapse_after_init_PSF
      private :: reset_elapse_after_init_ISO
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
      call elpsed_label_4_PSF
      call elpsed_label_4_ISO
!
      end subroutine elpsed_label_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ
!
!
      call reset_elapse_after_init_VIZ_top
!      call reset_elapse_after_init_PVR
      call reset_elapse_after_init_LIC
!
      call reset_elapse_after_init_PSF
      call reset_elapse_after_init_ISO
!
      end subroutine reset_elapse_after_init_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ_outline
!
      integer(kind = kint), parameter :: num_append = 13
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
      elps1%labels(ist_elapsed_VIZ+11) = 'VTK output in viz module'
      elps1%labels(ist_elapsed_VIZ+12)                                  &
     &                    = 'ele. comm. table for LIC    '
      elps1%labels(ist_elapsed_VIZ+13)                                  &
     &                    = 'edge comm. table for surfacing    '
!
      iflag_VIZ_time = .TRUE.
!
      end subroutine elpsed_label_4_VIZ_outline
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_PVR
!
      integer(kind = kint), parameter :: num_append = 12
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_PVR, ied_elapsed_PVR)
!
      elps1%labels(ist_elapsed_PVR+1)                                   &
     &                    = 'Volume rendering w/o file output   '
      elps1%labels(ist_elapsed_PVR+2)                                   &
     &                    = 'Volume rendering file output   '
      elps1%labels(ist_elapsed_PVR+3)                                   &
     &                    = 'V. Rendering ray trace   '
      elps1%labels(ist_elapsed_PVR+4)                                   &
     &                    = 'V. Rendering subimage composit   '
!
      elps1%labels(ist_elapsed_PVR+5)                                  &
     &                    = 'bcast_pvr_controls  '
      elps1%labels(ist_elapsed_PVR+6)                                  &
     &                    = 'set_pvr_controls  '
      elps1%labels(ist_elapsed_PVR+7)                                  &
     &                    = 'each_PVR_initialize  '
      elps1%labels(ist_elapsed_PVR+8)                                  &
     &                    = 's_const_comm_tbl_img_output  '
      elps1%labels(ist_elapsed_PVR+9)                                  &
     &                    = 's_const_comm_tbl_img_composit  '
      elps1%labels(ist_elapsed_PVR+10)                                  &
     &                    = 'calypso_SR_type_int pvr_init  '
      elps1%labels(ist_elapsed_PVR+11)                                  &
     &                    = 'calypso_SR_type_1 pvr_init '
      elps1%labels(ist_elapsed_PVR+12)                                  &
     &                    = 'set_image_stacking_and_recv  '
!
      iflag_PVR_time = .TRUE.
!
      end subroutine elpsed_label_4_PVR
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_LIC
!
      integer(kind = kint), parameter :: num_append = 6
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
     &                    = 'Line integration for LIC   '
      elps1%labels(ist_elapsed_LIC+5)                                   &
     &                    = 'LIC V. Rendering subimage composit   '
      elps1%labels(ist_elapsed_LIC+6)                                   &
     &                    = 'LIC V. Rendering domain repartition   '
!
      iflag_LIC_time = .TRUE.
!
      end subroutine elpsed_label_4_LIC
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_PSF
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_PSF, ied_elapsed_PSF)
!
      elps1%labels(ist_elapsed_PSF+1)                                   &
     &                    = 'Find Section patch   '
      elps1%labels(ist_elapsed_PSF+2)                                   &
     &                    = 'Interpolate data on Section   '
      elps1%labels(ist_elapsed_PSF+3)                                   &
     &                    = 'Output Sectioning data   '
!
      iflag_PSF_time = .TRUE.
!
      end subroutine elpsed_label_4_PSF
!
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_ISO
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_ISO, ied_elapsed_ISO)
!
      elps1%labels(ist_elapsed_ISO+1)                                   &
     &                    = 'Find Isosurface patch   '
      elps1%labels(ist_elapsed_ISO+2)                                   &
     &                    = 'Interpolate data on isosurface   '
      elps1%labels(ist_elapsed_ISO+3)                                   &
     &                    = 'Output Isosurface data   '
!
      iflag_ISO_time = .TRUE.
!
      end subroutine elpsed_label_4_ISO
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
      subroutine reset_elapse_after_init_PSF
!
!
      if(iflag_PSF_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_PSF+2, ist_elapsed_PSF+2)
!
      end subroutine reset_elapse_after_init_PSF
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_ISO
!
!
      if(iflag_ISO_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_ISO+2, ist_elapsed_ISO+2)
!
      end subroutine reset_elapse_after_init_ISO
!
!-----------------------------------------------------------------------
!
      end module  m_elapsed_labels_4_VIZ
