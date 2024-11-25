!>@file   t_elapsed_labels_4_SECTIONS.f90
!!@brief  module t_elapsed_labels_4_SECTIONS
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Initialize elepsed time monitoring
!!
!!@verbatim
!!      subroutine elpsed_label_4_SECT(flag_detailed, elps_SECT, elps)
!!      subroutine reset_elapse_after_init_SECT(elps_SECT, elps)
!!        logical intent(in) :: flag_detailed
!!        type(elapsed_labels_4_SECTIONS), intent(inout) :: elps_SECT
!!        type(elapsed_time_data), intent(inout) :: elps
!!@endverbatim
!
      module t_elapsed_labels_4_SECTIONS
!
      use m_precision
      use m_work_time
!
      implicit none
!
      type elapsed_labels_4_SECTIONS
        logical :: flag_elapsed_S = .FALSE.
        integer(kind = kint) :: ist_elapsed_S = 0
        integer(kind = kint) :: ied_elapsed_S = 0
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
      end type elapsed_labels_4_SECTIONS
!
      private :: elpsed_label_4_SECTIONS
      private :: reset_section_elapse_after_init
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_SECT(flag_detailed, elps_SECT, elps)
!
      use elapsed_labels_4_PSF
!
      logical, intent(in) :: flag_detailed
      type(elapsed_labels_4_SECTIONS), intent(inout) :: elps_SECT
      type(elapsed_time_data), intent(inout) :: elps
!
!
      call elpsed_label_4_SECTIONS(elps_SECT, elps)
!
      if(flag_detailed) then
        call elpsed_label_4_PSF(elps_SECT%elps_PSF, elps)
        call elpsed_label_4_ISO(elps_SECT%elps_ISO, elps)
      else
        elps_SECT%elps_PSF%flag_elapsed = .FALSE.
        elps_SECT%elps_ISO%flag_elapsed = .FALSE.
      end if
!
      end subroutine elpsed_label_4_SECT
!
! ----------------------------------------------------------------------
!
      subroutine reset_elapse_after_init_SECT(elps_SECT, elps)
!
      use elapsed_labels_4_PSF
!
      type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
      type(elapsed_time_data), intent(inout) :: elps
!
!
      call reset_section_elapse_after_init(elps_SECT, elps)
      call reset_elapse_after_init_PSF(elps_SECT%elps_PSF, elps)
      call reset_elapse_after_init_ISO(elps_SECT%elps_ISO, elps)
!
      end subroutine reset_elapse_after_init_SECT
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_SECTIONS(elps_SECT, elps)
!
      type(elapsed_labels_4_SECTIONS), intent(inout) :: elps_SECT
      type(elapsed_time_data), intent(inout) :: elps
      integer(kind = kint), parameter :: num_append = 6
!
!
      call append_elapsed_timer(num_append, elps_SECT%ist_elapsed_S,    &
     &                          elps_SECT%ied_elapsed_S, elps)
!
      elps%labels(elps_SECT%ist_elapsed_S+ 1)                           &
     &                    = 'Sectioning initialization.    '
      elps%labels(elps_SECT%ist_elapsed_S+ 2) = 'Sectioning.    '
!
      elps%labels(elps_SECT%ist_elapsed_S+ 3)                           &
     &                    = 'Isosurfaceing initialization.    '
      elps%labels(elps_SECT%ist_elapsed_S+ 4) = 'Isosurfaceing.    '
!
      elps%labels(elps_SECT%ist_elapsed_S+5)                            &
     &                    = 'VTK output in viz module'
      elps%labels(elps_SECT%ist_elapsed_S+6)                            &
     &                    = 'edge comm. table for vizualization    '
!
      elps_SECT%flag_elapsed_S = .TRUE.
!
      end subroutine elpsed_label_4_SECTIONS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reset_section_elapse_after_init(elps_SECT, elps)
!
      type(elapsed_labels_4_SECTIONS), intent(in) :: elps_SECT
      type(elapsed_time_data), intent(inout) :: elps
!
      integer(kind = kint) :: i, i_viz
!
      if(elps_SECT%flag_elapsed_S .eqv. .FALSE.) return
      
      do i = 1, 7
        i_viz = 2*i + elps_SECT%ist_elapsed_S
        call reset_elapsed_timer(i_viz, i_viz, elps)
      end do
!
      end subroutine reset_section_elapse_after_init
!
!-----------------------------------------------------------------------
!
      end module  t_elapsed_labels_4_SECTIONS
