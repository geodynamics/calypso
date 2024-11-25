!>@file   t_elapsed_labels_4_VIZ.f90
!!@brief  module t_elapsed_labels_4_VIZ
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine set_elpsed_label_4_VIZ(flag_detailed, elps_VIZ, elps)
!!      subroutine reset_elapse_after_init_VIZ(elps_VIZ, elps)
!!        logical, intent(in) :: flag_detailed
!!        type(elapsed_labels_4_VIZ), intent(inout) :: elps_VIZ
!!        type(elapsed_time_data), intent(inout) :: elps
!!@endverbatim
!
      module t_elapsed_labels_4_VIZ
!
      use m_precision
      use m_work_time
!
      implicit none
!
      type elapsed_labels_4_VIZ
        logical :: flag_elapsed_V = .FALSE.
        integer(kind = kint) :: ist_elapsed_V = 0
        integer(kind = kint) :: ied_elapsed_V = 0
!
        type(elapsed_lables) :: elps_PSF
        type(elapsed_lables) :: elps_ISO
!
        type(elapsed_lables) :: elps_PVR
        type(elapsed_lables) :: elps_LIC
        type(elapsed_lables) :: elps_MAP
!
        type(elapsed_lables) :: elps_FLINE
        type(elapsed_lables) :: elps_TRACER
      end type elapsed_labels_4_VIZ
!
      private :: elpsed_label_4_VIZ_outline
      private :: reset_elapse_after_init_VIZ_top
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_elpsed_label_4_VIZ(flag_detailed, elps_VIZ, elps)
!
      use elapsed_labels_4_PVR
      use elapsed_labels_4_FLINE
      use elapsed_labels_4_PSF
!
      logical, intent(in) :: flag_detailed
      type(elapsed_labels_4_VIZ), intent(inout) :: elps_VIZ
      type(elapsed_time_data), intent(inout) :: elps
!
!
      call elpsed_label_4_VIZ_outline(elps_VIZ, elps)
!
      if(flag_detailed) then
        call elpsed_label_4_PVR(elps_VIZ%elps_PVR, elps)
        call elpsed_label_4_LIC(elps_VIZ%elps_LIC, elps)
!
        call elpsed_label_4_PSF(elps_VIZ%elps_PSF, elps)
        call elpsed_label_4_ISO(elps_VIZ%elps_ISO, elps)
        call elpsed_label_4_MAP(elps_VIZ%elps_MAP, elps)
!
        call elpsed_label_4_FLINE(elps_VIZ%elps_FLINE, elps)
        call elpsed_label_4_TRACER(elps_VIZ%elps_TRACER, elps)
      else
        elps_VIZ%elps_PSF%flag_elapsed =    .FALSE.
        elps_VIZ%elps_ISO%flag_elapsed =    .FALSE.
        elps_VIZ%elps_PVR%flag_elapsed =    .FALSE.
        elps_VIZ%elps_LIC%flag_elapsed =    .FALSE.
        elps_VIZ%elps_MAP%flag_elapsed =    .FALSE.
        elps_VIZ%elps_FLINE%flag_elapsed =  .FALSE.
        elps_VIZ%elps_TRACER%flag_elapsed = .FALSE.
      end if
!
      end subroutine set_elpsed_label_4_VIZ
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ(elps_VIZ, elps)
!
      use elapsed_labels_4_PVR
      use elapsed_labels_4_PSF
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(elapsed_time_data), intent(inout) :: elps
!
!
      call reset_elapse_after_init_VIZ_top(elps_VIZ, elps)
!      call reset_elapse_after_init_PVR(elps_VIZ%elps_PVR, elps)
      call reset_elapse_after_init_LIC(elps_VIZ%elps_LIC, elps)
!
      call reset_elapse_after_init_PSF(elps_VIZ%elps_PSF, elps)
      call reset_elapse_after_init_ISO(elps_VIZ%elps_ISO, elps)
      call reset_elapse_after_init_MAP(elps_VIZ%elps_MAP, elps)
!
      end subroutine reset_elapse_after_init_VIZ
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_VIZ_outline(elps_VIZ, elps)
!
      type(elapsed_labels_4_VIZ), intent(inout) :: elps_VIZ
      type(elapsed_time_data), intent(inout) :: elps
      integer(kind = kint), parameter :: num_append = 17
!
!
      call append_elapsed_timer(num_append, elps_VIZ%ist_elapsed_V,     &
     &                          elps_VIZ%ied_elapsed_V, elps)
!
      elps%labels(elps_VIZ%ist_elapsed_V+ 1)                            &
     &                    = 'Sectioning initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+ 2) = 'Sectioning.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+ 3)                            &
     &                    = 'Isosurfaceing initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+ 4) = 'Isosurfaceing.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+ 5)                            &
     &                    = 'Map projection initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+ 6) = 'Map projection.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+ 7)                            &
     &                    = 'Volume rendering initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+ 8) = 'Volume rendering.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+ 9)                            &
     &                    = 'LIC rendering initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+10) = 'LIC rendering.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+11)                            &
     &                    = 'Fieldline initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+12) = 'Fieldline.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+13)                            &
     &                    = 'Tracer initialization.    '
      elps%labels(elps_VIZ%ist_elapsed_V+14) = 'Tracer.    '
!
      elps%labels(elps_VIZ%ist_elapsed_V+15)                            &
     &                    = 'VTK output in viz module'
      elps%labels(elps_VIZ%ist_elapsed_V+16)                            &
     &                    = 'ele. comm. table for LIC    '
      elps%labels(elps_VIZ%ist_elapsed_V+17)                            &
     &                    = 'edge comm. table for vizualization    '
!
      elps_VIZ%flag_elapsed_V = .TRUE.
!
      end subroutine elpsed_label_4_VIZ_outline
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_VIZ_top(elps_VIZ, elps)
!
      type(elapsed_labels_4_VIZ), intent(in) :: elps_VIZ
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint) :: i, i_viz
!
      if(elps_VIZ%flag_elapsed_V .eqv. .FALSE.) return
      
      do i = 1, 7
        i_viz = 2*i + elps_VIZ%ist_elapsed_V
        call reset_elapsed_timer(i_viz, i_viz, elps)
      end do
!
      end subroutine reset_elapse_after_init_VIZ_top
!
!-----------------------------------------------------------------------
!
      end module  t_elapsed_labels_4_VIZ
