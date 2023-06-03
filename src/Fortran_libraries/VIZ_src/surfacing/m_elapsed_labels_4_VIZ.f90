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
      logical, save :: iflag_MAP_time = .FALSE.
      integer(kind = kint), save :: ist_elapsed_MAP =   0
      integer(kind = kint), save :: ied_elapsed_MAP =   0
!
      private :: elpsed_label_4_VIZ_outline
      private :: elpsed_label_4_PVR, elpsed_label_4_LIC
      private :: reset_elapse_after_init_VIZ_top
!
      private :: reset_elapse_after_init_PVR
      private :: reset_elapse_after_init_LIC
      private :: reset_elapse_after_init_PSF
      private :: reset_elapse_after_init_ISO
      private :: reset_elapse_after_init_MAP
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
      call elpsed_label_4_MAP
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
      call reset_elapse_after_init_MAP
!
      end subroutine reset_elapse_after_init_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ_outline
!
      integer(kind = kint), parameter :: num_append = 15
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_VIZ, ied_elapsed_VIZ)
!
      elps1%labels(ist_elapsed_VIZ+ 1)                                  &
     &                    = 'Sectioning initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 2) = 'Sectioning.    '
!
      elps1%labels(ist_elapsed_VIZ+ 3)                                  &
     &                    = 'Isosurfaceing initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 4) = 'Isosurfaceing.    '
!
      elps1%labels(ist_elapsed_VIZ+ 5)                                  &
     &                    = 'Map projection initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 6) = 'Map projection.    '
!
      elps1%labels(ist_elapsed_VIZ+ 7)                                  &
     &                    = 'Volume rendering initialization.    '
      elps1%labels(ist_elapsed_VIZ+ 8) = 'Volume rendering.    '
!
      elps1%labels(ist_elapsed_VIZ+ 9)                                  &
     &                    = 'LIC rendering initialization.    '
      elps1%labels(ist_elapsed_VIZ+10) = 'LIC rendering.    '
!
      elps1%labels(ist_elapsed_VIZ+11)                                  &
     &                    = 'fieldline initialization.    '
      elps1%labels(ist_elapsed_VIZ+12) = 'fieldline.    '
!
      elps1%labels(ist_elapsed_VIZ+13) = 'VTK output in viz module'
      elps1%labels(ist_elapsed_VIZ+14)                                  &
     &                    = 'ele. comm. table for LIC    '
      elps1%labels(ist_elapsed_VIZ+15)                                  &
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
      elps1%labels(ist_elapsed_PVR+5)                                   &
     &                    = 'bcast_pvr_controls  '
      elps1%labels(ist_elapsed_PVR+6)                                   &
     &                    = 'set_pvr_controls  '
      elps1%labels(ist_elapsed_PVR+7)                                   &
     &                    = 'each_PVR_initialize  '
      elps1%labels(ist_elapsed_PVR+8)                                   &
     &                    = 's_const_comm_tbl_img_output  '
      elps1%labels(ist_elapsed_PVR+9)                                   &
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
      integer(kind = kint), parameter :: num_append = 9
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
     &                    = 'LIC V. Rendering domain repartition  '
      elps1%labels(ist_elapsed_LIC+7)                                   &
     &                    = 'LIC data transfer to new domain  '
      elps1%labels(ist_elapsed_LIC+8)                                   &
     &                    = 'FEM_mesh_initialization for LIC mesh  '
      elps1%labels(ist_elapsed_LIC+9)                                   &
     &                    = 'Data IO for line integration counts   '
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
!
      subroutine elpsed_label_4_MAP
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed_MAP, ied_elapsed_MAP)
!
      elps1%labels(ist_elapsed_MAP+1)                                   &
     &                    = 'Collect map data   '
      elps1%labels(ist_elapsed_MAP+2)                                   &
     &                    = 'Interpolate data on map   '
      elps1%labels(ist_elapsed_MAP+3)                                   &
     &                    = 'Output Map image   '
!
      iflag_MAP_time = .TRUE.
!
      end subroutine elpsed_label_4_MAP
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ_top
!
!
      if(iflag_VIZ_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_VIZ+ 2, ist_elapsed_VIZ+ 2)
      call reset_elapsed_times(ist_elapsed_VIZ+ 4, ist_elapsed_VIZ+ 4)
      call reset_elapsed_times(ist_elapsed_VIZ+ 6, ist_elapsed_VIZ+ 6)
      call reset_elapsed_times(ist_elapsed_VIZ+ 8, ist_elapsed_VIZ+ 8)
      call reset_elapsed_times(ist_elapsed_VIZ+10, ist_elapsed_VIZ+10)
      call reset_elapsed_times(ist_elapsed_VIZ+12, ist_elapsed_VIZ+12)
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
      subroutine reset_elapse_after_init_MAP
!
!
      if(iflag_ISO_time .eqv. .FALSE.) return
      call reset_elapsed_times(ist_elapsed_MAP+2, ist_elapsed_MAP+2)
!
      end subroutine reset_elapse_after_init_MAP
!
!-----------------------------------------------------------------------
!
      end module  m_elapsed_labels_4_VIZ
