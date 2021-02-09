!>@file   t_ctl_param_sph_series_util.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Dec., 2020
!!
!
!> @brief Time average spherical harmonics spectrum parameter
!!
!!@verbatim
!!      subroutine set_spec_series_file_param(tave_sph_ctl, spec_evo_p)
!!      subroutine dealloc_spec_series_file_param(spec_evo_p)
!!        type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
!!        type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!!@endverbatim
!
      module t_ctl_param_sph_series_util
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type sph_spectr_file_param
        real(kind = kreal) :: start_time
        real(kind = kreal) :: end_time
!
        integer(kind = kint) :: iflag_old_fmt = 0
!
        integer(kind = kint) :: lst = 0
        integer(kind = kint) :: led = 0
!
        integer(kind = kint) :: nfile_vol_series_file = 0
        character(len = kchara), allocatable :: vol_series_prefix(:)
!
        integer(kind = kint) :: nfile_vol_spectr_file = 0
        character(len = kchara), allocatable :: vol_spectr_prefix(:)
!
        integer(kind = kint) :: nfile_layer_series_file = 0
        character(len = kchara), allocatable :: layer_series_prefix(:)
!
        integer(kind = kint) :: nfile_layer_sprctr_file = 0
        character(len = kchara), allocatable :: layer_spectr_prefix(:)
      end type sph_spectr_file_param
!
      private :: alloc_vol_series_prefix,   dealloc_vol_series_prefix
      private :: alloc_vol_spectr_prefix,   dealloc_vol_spectr_prefix
      private :: alloc_layer_series_prefix, dealloc_layer_series_prefix
      private :: alloc_layer_spectr_prefix, dealloc_layer_spectr_prefix
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_spec_series_file_param(tave_sph_ctl, spec_evo_p)
!
      use t_ctl_data_tave_sph_monitor
      use skip_comment_f
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      integer(kind = kint) :: i
!
!
      if(tave_sph_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set start time'
        stop
      end if
      spec_evo_p%start_time = tave_sph_ctl%start_time_ctl%realvalue
!
      if(tave_sph_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Set end time'
        stop
      end if
      spec_evo_p%end_time = tave_sph_ctl%end_time_ctl%realvalue
!
      spec_evo_p%iflag_old_fmt = 0
      if(tave_sph_ctl%old_format_ctl%iflag .gt. 0                       &
     &    .and. yes_flag(tave_sph_ctl%old_format_ctl%charavalue)) then
        spec_evo_p%iflag_old_fmt = 1
      end if
!
      if(tave_sph_ctl%degree_range_ctl%iflag .gt. 0) then
        spec_evo_p%lst = tave_sph_ctl%degree_range_ctl%intvalue(1)
        spec_evo_p%led = tave_sph_ctl%degree_range_ctl%intvalue(2)
      end if
!
      if(tave_sph_ctl%volume_series_file_ctl%num .gt. 0) then
        call alloc_vol_series_prefix                                    &
     &     (tave_sph_ctl%volume_series_file_ctl%num, spec_evo_p)
!
        do i = 1, spec_evo_p%nfile_vol_series_file
          spec_evo_p%vol_series_prefix(i)                               &
     &       = tave_sph_ctl%volume_series_file_ctl%c_tbl(i)
        end do
      end if
!
      if(tave_sph_ctl%volume_spec_file_ctl%num .gt. 0) then
        call alloc_vol_spectr_prefix                                    &
     &     (tave_sph_ctl%volume_spec_file_ctl%num, spec_evo_p)
!
        do i = 1, spec_evo_p%nfile_vol_spectr_file
          spec_evo_p%vol_spectr_prefix(i)                               &
     &       = tave_sph_ctl%volume_spec_file_ctl%c_tbl(i)
        end do
      end if
!
      if(tave_sph_ctl%layered_series_file_ctl%num .gt. 0) then
        call alloc_layer_series_prefix                                  &
     &     (tave_sph_ctl%layered_series_file_ctl%num, spec_evo_p)
!
        do i = 1, spec_evo_p%nfile_layer_series_file
          spec_evo_p%layer_series_prefix(i)                             &
     &       = tave_sph_ctl%layered_series_file_ctl%c_tbl(i)
        end do
      end if
!
      if(tave_sph_ctl%layered_spec_file_ctl%num .gt. 0) then
        call alloc_layer_spectr_prefix                                  &
     &     (tave_sph_ctl%layered_spec_file_ctl%num, spec_evo_p)
!
        do i = 1, spec_evo_p%nfile_layer_sprctr_file
          spec_evo_p%layer_spectr_prefix(i)                             &
     &       = tave_sph_ctl%layered_spec_file_ctl%c_tbl(i)
        end do
      end if
!
      end subroutine set_spec_series_file_param
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_spec_series_file_param(spec_evo_p)
!
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      call dealloc_vol_series_prefix(spec_evo_p)
      call dealloc_vol_spectr_prefix(spec_evo_p)
      call dealloc_layer_series_prefix(spec_evo_p)
      call dealloc_layer_spectr_prefix(spec_evo_p)
!
      end subroutine dealloc_spec_series_file_param
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine alloc_vol_series_prefix(num, spec_evo_p)
!
      integer(kind = kint), intent(in) :: num
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%vol_series_prefix)) return
      spec_evo_p%nfile_vol_series_file = num
      allocate(spec_evo_p%vol_series_prefix(num))
!
      end subroutine alloc_vol_series_prefix
!
!   --------------------------------------------------------------------
!
      subroutine alloc_vol_spectr_prefix(num, spec_evo_p)
!
      integer(kind = kint), intent(in) :: num
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%vol_spectr_prefix)) return
      spec_evo_p%nfile_vol_spectr_file = num
      allocate(spec_evo_p%vol_spectr_prefix(num))
!
      end subroutine alloc_vol_spectr_prefix
!
!   --------------------------------------------------------------------
!
      subroutine alloc_layer_series_prefix(num, spec_evo_p)
!
      integer(kind = kint), intent(in) :: num
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%layer_series_prefix)) return
      spec_evo_p%nfile_layer_series_file = num
      allocate(spec_evo_p%layer_series_prefix(num))
!
      end subroutine alloc_layer_series_prefix
!
!   --------------------------------------------------------------------
!
      subroutine alloc_layer_spectr_prefix(num, spec_evo_p)
!
      integer(kind = kint), intent(in) :: num
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%layer_spectr_prefix)) return
      spec_evo_p%nfile_layer_sprctr_file = num
      allocate(spec_evo_p%layer_spectr_prefix(num))
!
      end subroutine alloc_layer_spectr_prefix
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_vol_series_prefix(spec_evo_p)
!
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%vol_series_prefix) .eqv. .FALSE.) return
      deallocate(spec_evo_p%vol_series_prefix)
!
      end subroutine dealloc_vol_series_prefix
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_vol_spectr_prefix(spec_evo_p)
!
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%vol_spectr_prefix) .eqv. .FALSE.) return
      deallocate(spec_evo_p%vol_spectr_prefix)
!
      end subroutine dealloc_vol_spectr_prefix
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_layer_series_prefix(spec_evo_p)
!
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%layer_series_prefix).eqv. .FALSE.) return
      deallocate(spec_evo_p%layer_series_prefix)
!
      end subroutine dealloc_layer_series_prefix
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_layer_spectr_prefix(spec_evo_p)
!
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      if(allocated(spec_evo_p%layer_spectr_prefix).eqv. .FALSE.) return
      deallocate(spec_evo_p%layer_spectr_prefix)
!
      end subroutine dealloc_layer_spectr_prefix
!
!   --------------------------------------------------------------------
!
      end module t_ctl_param_sph_series_util
