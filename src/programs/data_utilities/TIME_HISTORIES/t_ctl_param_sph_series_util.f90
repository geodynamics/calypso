!>@file   t_ctl_param_sph_series_util.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Dec., 2020
!!
!
!> @brief Time average spherical harmonics spectrum parameter
!!
!!@verbatim
!!      subroutine set_spec_series_file_and_time(tave_sph_ctl,          &
!!     &                                         spec_evo_p)
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
      type sph_spectr_file_list
        integer(kind = kint) :: num_file = 0
        character(len = kchara), allocatable :: evo_file_name(:)
      end type sph_spectr_file_list
!
      type sph_spectr_file_param
        real(kind = kreal) :: start_time = 0.0d0
        real(kind = kreal) :: end_time =   0.0d0
!
        logical :: flag_old_fmt = .FALSE.
!
        integer(kind = kint) :: lst = 0
        integer(kind = kint) :: led = 0
!
        type(sph_spectr_file_list) :: vol_series
        type(sph_spectr_file_list) :: vol_spec_series
        type(sph_spectr_file_list) :: layer_series
        type(sph_spectr_file_list) :: layer_spec_series
        type(sph_spectr_file_list) :: pick_spec_series
      end type sph_spectr_file_param
!
      private :: set_sph_series_file_list
      private :: alloc_series_prefix_list, dealloc_series_prefix_list
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_spec_series_file_and_time(tave_sph_ctl,            &
     &                                         spec_evo_p)
!
      use t_ctl_data_tave_sph_monitor
      use skip_comment_f
      use set_parallel_file_name

!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      character(len = kchara) :: file_prefix
      integer(kind = kint) :: i
!
!
      if(tave_sph_ctl%start_time_ctl%iflag .eq. 0) then
        write(*,*) 'Error: Set start time'
        stop
      end if
      spec_evo_p%start_time = tave_sph_ctl%start_time_ctl%realvalue
!
      if(tave_sph_ctl%end_time_ctl%iflag .eq. 0) then
        write(*,*) 'Error: Set end time'
        stop
      end if
      spec_evo_p%end_time = tave_sph_ctl%end_time_ctl%realvalue
!
      spec_evo_p%flag_old_fmt = .FALSE.
      if(tave_sph_ctl%old_format_ctl%iflag .gt. 0) then
        spec_evo_p%flag_old_fmt                                         &
     &     = yes_flag(tave_sph_ctl%old_format_ctl%charavalue)
      end if
!
      if(tave_sph_ctl%degree_range_ctl%iflag .gt. 0) then
        spec_evo_p%lst = tave_sph_ctl%degree_range_ctl%intvalue(1)
        spec_evo_p%led = tave_sph_ctl%degree_range_ctl%intvalue(2)
      end if
!
      call set_spec_series_file_param(tave_sph_ctl, spec_evo_p)
!
      end subroutine set_spec_series_file_and_time
!
!   --------------------------------------------------------------------
!
      subroutine set_spec_series_file_param(tave_sph_ctl, spec_evo_p)
!
      use t_ctl_data_tave_sph_monitor
!
      type(tave_sph_monitor_ctl), intent(in) :: tave_sph_ctl
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
!
      call set_sph_series_file_list                                     &
     &   (tave_sph_ctl%volume_series_file_ctl,                          &
     &    spec_evo_p%vol_series)
      call set_sph_series_file_list(tave_sph_ctl%volume_spec_file_ctl,  &
     &                              spec_evo_p%vol_spec_series)
      call set_sph_series_file_list                                     &
     &   (tave_sph_ctl%layered_series_file_ctl,                         &
     &    spec_evo_p%layer_series)
      call set_sph_series_file_list(tave_sph_ctl%layered_spec_file_ctl, &
     &                              spec_evo_p%layer_spec_series)
      call set_sph_series_file_list(tave_sph_ctl%picked_mode_file_ctl,  &
     &                              spec_evo_p%pick_spec_series)
!
      end subroutine set_spec_series_file_param
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_spec_series_file_param(spec_evo_p)
!
      type(sph_spectr_file_param), intent(inout) :: spec_evo_p
!
      call dealloc_series_prefix_list(spec_evo_p%vol_series)
      call dealloc_series_prefix_list(spec_evo_p%vol_spec_series)
      call dealloc_series_prefix_list(spec_evo_p%layer_series)
      call dealloc_series_prefix_list(spec_evo_p%layer_spec_series)
      call dealloc_series_prefix_list(spec_evo_p%pick_spec_series)
!
      end subroutine dealloc_spec_series_file_param
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_sph_series_file_list(file_list_ctl, f_list)
!
      use t_control_array_character
      use set_parallel_file_name
!
      type(ctl_array_chara), intent(in) :: file_list_ctl
      type(sph_spectr_file_list), intent(inout) :: f_list
!
      character(len = kchara) :: file_prefix
      integer(kind = kint) :: i
!
!
      if(file_list_ctl%num .le. 0) return
      call alloc_series_prefix_list(file_list_ctl%num, f_list)
!
      do i = 1, f_list%num_file
        file_prefix = file_list_ctl%c_tbl(i)
        f_list%evo_file_name(i) = add_dat_extension(file_prefix)
      end do
!
      end subroutine set_sph_series_file_list
!
!   --------------------------------------------------------------------
!
      subroutine alloc_series_prefix_list(num, f_list)
!
      integer(kind = kint), intent(in) :: num
      type(sph_spectr_file_list), intent(inout) :: f_list
!
      if(allocated(f_list%evo_file_name)) return
      f_list%num_file = num
      allocate(f_list%evo_file_name(num))
!
      end subroutine alloc_series_prefix_list
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_series_prefix_list(f_list)
!
      type(sph_spectr_file_list), intent(inout) :: f_list
!
      if(allocated(f_list%evo_file_name) .eqv. .FALSE.) return
      deallocate(f_list%evo_file_name)
!
      end subroutine dealloc_series_prefix_list
!
!   --------------------------------------------------------------------
!
      end module t_ctl_param_sph_series_util
