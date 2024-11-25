!>@file   elapsed_labels_4_PVR.f90
!!@brief  module elapsed_labels_4_PVR
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_PVR(elps_PVR, elps)
!!      subroutine reset_elapse_after_init_PVR(elps_PVR, elps)
!!        type(elapsed_lables), intent(inout) :: elps_PVR
!!        type(elapsed_time_data), intent(inout) :: elps
!!
!!      subroutine elpsed_label_4_LIC(elps_LIC, elps)
!!      subroutine reset_elapse_after_init_LIC(elps_LIC, elps)
!!        type(elapsed_lables), intent(inout) :: elps_LIC
!!        type(elapsed_time_data), intent(inout) :: elps
!!
!!      subroutine elpsed_label_4_MAP(elps_map, elps)
!!      subroutine reset_elapse_after_init_MAP(elps_map, elps)
!!        type(elapsed_lables), intent(inout) :: elps_map
!!        type(elapsed_time_data), intent(inout) :: elps
!!@endverbatim
!
      module elapsed_labels_4_PVR
!
      use m_precision
      use m_work_time
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_PVR(elps_PVR, elps)
!
      type(elapsed_lables), intent(inout) :: elps_PVR
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint), parameter :: num_append = 12
!
!
      call append_elapsed_timer(num_append, elps_PVR%ist_elapsed,       &
     &                          elps_PVR%ied_elapsed, elps)
!
      elps%labels(elps_PVR%ist_elapsed+1)                               &
     &                    = 'Volume rendering w/o file output   '
      elps%labels(elps_PVR%ist_elapsed+2)                               &
     &                    = 'Volume rendering file output   '
      elps%labels(elps_PVR%ist_elapsed+3)                               &
     &                    = 'V. Rendering ray trace   '
      elps%labels(elps_PVR%ist_elapsed+4)                               &
     &                    = 'V. Rendering subimage composit   '
!
      elps%labels(elps_PVR%ist_elapsed+5)                               &
     &                    = 'bcast_pvr_controls  '
      elps%labels(elps_PVR%ist_elapsed+6)                               &
     &                    = 'set_pvr_controls  '
      elps%labels(elps_PVR%ist_elapsed+7)                               &
     &                    = 'each_PVR_initialize  '
      elps%labels(elps_PVR%ist_elapsed+8)                               &
     &                    = 's_const_comm_tbl_img_output  '
      elps%labels(elps_PVR%ist_elapsed+9)                               &
     &                    = 's_const_comm_tbl_img_composit  '
      elps%labels(elps_PVR%ist_elapsed+10)                              &
     &                    = 'calypso_SR_type_int pvr_init  '
      elps%labels(elps_PVR%ist_elapsed+11)                              &
     &                    = 'calypso_SR_type_1 pvr_init '
      elps%labels(elps_PVR%ist_elapsed+12)                              &
     &                    = 'set_image_stacking_and_recv  '
!
      elps_PVR%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_PVR
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_PVR(elps_PVR, elps)
!
      type(elapsed_lables), intent(inout) :: elps_PVR
      type(elapsed_time_data), intent(inout) :: elps
!
      if(elps_PVR%flag_elapsed .eqv. .FALSE.) return
      call reset_elapsed_timer(elps_PVR%ist_elapsed+1,                  &
     &                         elps_PVR%ied_elapsed, elps)
!
      end subroutine reset_elapse_after_init_PVR
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_LIC(elps_LIC, elps)
!
      type(elapsed_lables), intent(inout) :: elps_LIC
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint), parameter :: num_append = 9
!
!
      call append_elapsed_timer(num_append, elps_LIC%ist_elapsed,       &
     &                          elps_LIC%ied_elapsed, elps)
!
      elps%labels(elps_LIC%ist_elapsed+1)                               &
     &                    = 'LIC V. rendering w/o file output   '
      elps%labels(elps_LIC%ist_elapsed+2)                               &
     &                    = 'LIC V. rendering file output   '
      elps%labels(elps_LIC%ist_elapsed+3)                               &
     &                    = 'LIC V. Rendering ray trace   '
      elps%labels(elps_LIC%ist_elapsed+4)                               &
     &                    = 'Line integration for LIC   '
      elps%labels(elps_LIC%ist_elapsed+5)                               &
     &                    = 'LIC V. Rendering subimage composit   '
      elps%labels(elps_LIC%ist_elapsed+6)                               &
     &                    = 'LIC V. Rendering domain repartition  '
      elps%labels(elps_LIC%ist_elapsed+7)                               &
     &                    = 'LIC data transfer to new domain  '
      elps%labels(elps_LIC%ist_elapsed+8)                               &
     &                    = 'FEM_mesh_initialization for LIC mesh  '
      elps%labels(elps_LIC%ist_elapsed+9)                               &
     &                    = 'Data IO for line integration counts   '
!
      elps_LIC%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_LIC
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_LIC(elps_LIC, elps)
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(elapsed_time_data), intent(inout) :: elps
!
      if(elps_LIC%flag_elapsed .eqv. .FALSE.) return
      call reset_elapsed_timer(elps_LIC%ist_elapsed+1,                  &
     &                         elps_LIC%ied_elapsed, elps)
!
      end subroutine reset_elapse_after_init_LIC
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine elpsed_label_4_MAP(elps_map, elps)
!
      type(elapsed_lables), intent(inout) :: elps_map
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint), parameter :: num_append = 3
!
!
      call append_elapsed_timer(num_append, elps_map%ist_elapsed,       &
     &                          elps_map%ied_elapsed, elps)
!
      elps%labels(elps_map%ist_elapsed+1)                               &
     &                    = 'Collect map data   '
      elps%labels(elps_map%ist_elapsed+2)                               &
     &                    = 'Interpolate data on map   '
      elps%labels(elps_map%ist_elapsed+3)                               &
     &                    = 'Output Map image   '
!
      elps_map%flag_elapsed = .TRUE.
!
      end subroutine elpsed_label_4_MAP
!
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_MAP(elps_map, elps)
!
      type(elapsed_lables), intent(in) :: elps_map
      type(elapsed_time_data), intent(inout) :: elps
!
      if(elps_map%flag_elapsed .eqv. .FALSE.) return
      call reset_elapsed_timer(elps_map%ist_elapsed+2,                  &
     &                         elps_map%ist_elapsed+2, elps)
!
      end subroutine reset_elapse_after_init_MAP
!
!-----------------------------------------------------------------------
!
      end module  elapsed_labels_4_PVR
