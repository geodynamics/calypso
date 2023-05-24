!>@file   t_sort_PVRs_by_type.f90
!!@brief  module t_sort_PVRs_by_type
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine alloc_sort_PVRs_by_type(num_pvr, PVR_sort)
!!      subroutine dealloc_sort_PVRs_list(PVR_sort)
!!      subroutine dealloc_sort_PVRs_by_type(PVR_sort)
!!      subroutine s_sort_PVRs_by_type(num_pvr, pvr_ctl, PVR_sort)
!!        integer(kind = kint), intent(in) :: num_pvr
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!!        type(sort_PVRs_by_type), intent(inout) :: PVR_sort
!!@endverbatim
      module t_sort_PVRs_by_type
!
      use m_precision
      use m_constants
!
      use t_control_data_4_pvr
!
      implicit none
!
      type sort_PVRs_by_type
        integer(kind = kint), allocatable :: istack_PVR_modes(:)
!>        Number of image files for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_images(:)
!
        integer(kind = kint), allocatable :: ipvr_sorted(:)
!
        integer(kind = kint), pointer :: nPVR_modes(:)
        integer(kind = kint), pointer :: nPVR_base
        integer(kind = kint), pointer :: nPVR_quilt
        integer(kind = kint), pointer :: nPVR_movie
        integer(kind = kint), pointer :: nPVR_movie_quilt
        integer(kind = kint), pointer :: nPVR_anaglyph
        integer(kind = kint), pointer :: nPVR_mov_anaglyph
      end type sort_PVRs_by_type
!
      private :: count_anaglyph_PVRs_by_type, count_quilt_PVRs_by_type
      private :: find_anaglyph_PVRs_by_type,  find_quilt_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sort_PVRs_by_type(num_pvr, PVR_sort)
!
      integer(kind = kint), intent(in) :: num_pvr
!
      type(sort_PVRs_by_type), intent(inout) :: PVR_sort
!
!
      allocate(PVR_sort%istack_pvr_images(0:num_pvr))
!$omp parallel workshare
      PVR_sort%istack_pvr_images(0:num_pvr) = 0
!$omp end parallel workshare
!
      allocate(PVR_sort%ipvr_sorted(num_pvr))
!$omp parallel workshare
      PVR_sort%ipvr_sorted(1:num_pvr) = 0
!$omp end parallel workshare
!
      allocate(PVR_sort%nPVR_modes(6))
      allocate(PVR_sort%istack_PVR_modes(0:6))
      PVR_sort%nPVR_modes(1:6) =       0
      PVR_sort%istack_PVR_modes(0:6) = 0
!
      PVR_sort%nPVR_base =>  PVR_sort%nPVR_modes(1)
      PVR_sort%nPVR_quilt => PVR_sort%nPVR_modes(2)
      PVR_sort%nPVR_movie => PVR_sort%nPVR_modes(3)
      PVR_sort%nPVR_movie_quilt =>  PVR_sort%nPVR_modes(4)
      PVR_sort%nPVR_anaglyph =>     PVR_sort%nPVR_modes(5)
      PVR_sort%nPVR_mov_anaglyph => PVR_sort%nPVR_modes(6)
!
      end subroutine alloc_sort_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sort_PVRs_list(PVR_sort)
!
      type(sort_PVRs_by_type), intent(inout) :: PVR_sort
!
!
      if(allocated(PVR_sort%ipvr_sorted) .eqv. .FALSE.) return
      deallocate(PVR_sort%ipvr_sorted, PVR_sort%istack_pvr_images)
!
      end subroutine dealloc_sort_PVRs_list
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sort_PVRs_by_type(PVR_sort)
!
      type(sort_PVRs_by_type), intent(inout) :: PVR_sort
!
!
      if(allocated(PVR_sort%istack_PVR_modes) .eqv. .FALSE.) return
      deallocate(PVR_sort%nPVR_modes, PVR_sort%istack_PVR_modes)
!
      end subroutine dealloc_sort_PVRs_by_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_sort_PVRs_by_type(num_pvr, pvr_ctl, PVR_sort)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl(num_pvr)
!
      type(sort_PVRs_by_type), intent(inout) :: PVR_sort
!
      integer(kind = kint) :: i_pvr, i, ntot
!
!
      do i_pvr = 1, num_pvr
        call count_anaglyph_PVRs_by_type(pvr_ctl(i_pvr),                &
     &                                   PVR_sort%nPVR_modes(1:6))
      end do
!
      call s_cal_total_and_stacks(isix, PVR_sort%nPVR_modes, izero,     &
     &    PVR_sort%istack_PVR_modes, ntot)
!
      PVR_sort%nPVR_modes(1:6) = 0
      do i_pvr = 1, num_pvr
        call find_anaglyph_PVRs_by_type                                 &
     &     (pvr_ctl(i_pvr), PVR_sort%istack_PVR_modes(0:6),             &
     &      PVR_sort%nPVR_modes(1:6), PVR_sort%ipvr_sorted(i_pvr))
      end do
!
      end subroutine s_sort_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      subroutine count_anaglyph_PVRs_by_type(pvr_ctl, icou_mode)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
      integer(kind = kint), intent(inout) :: icou_mode(6)
!
!
      if(yes_flag(pvr_ctl%anaglyph_ctl%charavalue)) then
        if(pvr_ctl%movie%movie_mode_ctl%iflag .gt. 0) then
          icou_mode(6) = icou_mode(6) + 1
        else
          icou_mode(5) = icou_mode(5) + 1
        end if
      else
        if(pvr_ctl%movie%movie_mode_ctl%iflag .gt. 0) then
          call count_quilt_PVRs_by_type                                 &
     &       (pvr_ctl, icou_mode(3) , icou_mode(4))
        else
          call count_quilt_PVRs_by_type                                 &
     &       (pvr_ctl, icou_mode(1) , icou_mode(2))
        end if
      end if
!
      end subroutine count_anaglyph_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      subroutine count_quilt_PVRs_by_type(pvr_ctl,                      &
     &                                    icou_base, icou_quilt)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
      integer(kind = kint), intent(inout) :: icou_base, icou_quilt
!
!
      if(yes_flag(pvr_ctl%quilt_ctl%charavalue)) then
        icou_quilt = icou_quilt + 1
      else
        icou_base =  icou_base + 1
      end if
!
      end subroutine count_quilt_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      subroutine find_anaglyph_PVRs_by_type(pvr_ctl, istack_mode,       &
     &                                      icou_mode, ipvr_sorted)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
      integer(kind = kint), intent(in) :: istack_mode(0:5)
!
      integer(kind = kint), intent(inout) :: icou_mode(6)
      integer(kind = kint), intent(inout) :: ipvr_sorted
!
!
      if(yes_flag(pvr_ctl%anaglyph_ctl%charavalue)) then
        if(pvr_ctl%movie%movie_mode_ctl%iflag .gt. 0) then
          icou_mode(6) = icou_mode(6) + 1
          ipvr_sorted = istack_mode(5) + icou_mode(6)
        else
          icou_mode(5) = icou_mode(5) + 1
          ipvr_sorted = istack_mode(4) + icou_mode(5)
        end if
      else
        if(pvr_ctl%movie%movie_mode_ctl%iflag .gt. 0) then
          call find_quilt_PVRs_by_type                                  &
     &       (pvr_ctl, istack_mode(2), istack_mode(3),                  &
     &        icou_mode(3) , icou_mode(4), ipvr_sorted)
        else
          call find_quilt_PVRs_by_type                                  &
     &       (pvr_ctl, istack_mode(0), istack_mode(1),                  &
     &        icou_mode(1) , icou_mode(2), ipvr_sorted)
        end if
      end if
!
      end subroutine find_anaglyph_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      subroutine find_quilt_PVRs_by_type(pvr_ctl, ist_base, ist_quilt,  &
     &          icou_base, icou_quilt, ipvr_sorted)
!
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl
!
      integer(kind = kint), intent(in) :: ist_base, ist_quilt
      integer(kind = kint), intent(inout) :: icou_base, icou_quilt
      integer(kind = kint), intent(inout) :: ipvr_sorted
!
!
!
      if(yes_flag(pvr_ctl%quilt_ctl%charavalue)) then
        icou_quilt = icou_quilt + 1
        ipvr_sorted = ist_quilt + icou_quilt
      else
        icou_base =  icou_base + 1
        ipvr_sorted = ist_base +  icou_base
      end if
!
      end subroutine find_quilt_PVRs_by_type
!
!  ---------------------------------------------------------------------
!
      end module t_sort_PVRs_by_type
