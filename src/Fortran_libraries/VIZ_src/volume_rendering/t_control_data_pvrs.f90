!>@file   t_control_data_pvrs.f90
!!@brief  module t_control_data_pvrs
!!
!!@author  H. Matsui
!!@date Programmed in July, 2006
!
!>@brief structure of control data for multiple PVRs
!!
!!@verbatim
!!      subroutine alloc_pvr_ctl_struct(pvr_ctls)
!!      subroutine read_files_4_pvr_ctl                                 &
!!     &         (id_control, hd_pvr_ctl, pvr_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_pvr_ctl
!!        type(volume_rendering_controls), intent(inout) :: pvr_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_files_4_pvr_ctl                                &
!!     &         (id_control, hd_pvr_ctl, pvr_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_pvr_ctl
!!        type(volume_rendering_controls), intent(in) :: pvr_ctls
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine add_fields_4_pvrs_to_fld_ctl(pvr_ctl, field_ctl)
!!        type(volume_rendering_controls), intent(in) :: pvr_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  volume_rendering  1
!!      file  volume_rendering  'ctl_pvr_temp'
!!    end array volume_rendering
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvrs
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_pvr
!
      implicit  none
!
      type volume_rendering_controls
        integer(kind = kint) :: num_pvr_ctl = 0
        character(len = kchara), allocatable :: fname_pvr_ctl(:)
        type(pvr_parameter_ctl), allocatable :: pvr_ctl_type(:)
      end type volume_rendering_controls
!
      private :: append_new_pvr_ctl_struct, dup_pvr_ctl_struct
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_ctl_struct(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
!
      allocate(pvr_ctls%fname_pvr_ctl(pvr_ctls%num_pvr_ctl))
      allocate(pvr_ctls%pvr_ctl_type(pvr_ctls%num_pvr_ctl))
!
      end subroutine alloc_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_ctl_struct(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
      integer(kind = kint) :: i
!
!
      if(allocated(pvr_ctls%fname_pvr_ctl)) then
        do i = 1, pvr_ctls%num_pvr_ctl
          call deallocate_cont_dat_pvr(pvr_ctls%pvr_ctl_type(i))
        end do
!
        deallocate(pvr_ctls%pvr_ctl_type, pvr_ctls%fname_pvr_ctl)
      end if
      pvr_ctls%num_pvr_ctl = 0
!
      end subroutine dealloc_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_pvr_ctl                                   &
     &         (id_control, hd_pvr_ctl, pvr_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
      use ctl_file_each_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_pvr_ctl
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_pvr_ctl) .eqv. .FALSE.) return
      if(allocated(pvr_ctls%fname_pvr_ctl)) return
      pvr_ctls%num_pvr_ctl = 0
      call alloc_pvr_ctl_struct(pvr_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_pvr_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_pvr_ctl)) exit
!
        if(check_file_flag(c_buf, hd_pvr_ctl)                           &
     &     .or. check_begin_flag(c_buf, hd_pvr_ctl)) then
          call append_new_pvr_ctl_struct(pvr_ctls)
!
          call write_multi_ctl_file_message                             &
     &       (hd_pvr_ctl, pvr_ctls%num_pvr_ctl, c_buf%level)
          call sel_read_control_pvr(id_control, hd_pvr_ctl,             &
     &        pvr_ctls%fname_pvr_ctl(pvr_ctls%num_pvr_ctl),             &
     &        pvr_ctls%pvr_ctl_type(pvr_ctls%num_pvr_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_pvr_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_files_4_pvr_ctl                                  &
     &         (id_control, hd_pvr_ctl, pvr_ctls, level)
!
      use t_read_control_elements
      use skip_comment_f
      use ctl_file_each_pvr_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_pvr_ctl
!
      type(volume_rendering_controls), intent(in) :: pvr_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      if(pvr_ctls%num_pvr_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_pvr_ctl)
      do i = 1, pvr_ctls%num_pvr_ctl
        write(*,'(3a,i4,a)', ADVANCE='NO') '!  ', trim(hd_pvr_ctl),     &
     &                                     ' No. ', i
        call sel_write_control_pvr(id_control, hd_pvr_ctl,              &
     &      pvr_ctls%fname_pvr_ctl(i), pvr_ctls%pvr_ctl_type(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     hd_pvr_ctl)
!
      end subroutine write_files_4_pvr_ctl
!
!   --------------------------------------------------------------------
!
      subroutine add_fields_4_pvrs_to_fld_ctl(pvr_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(volume_rendering_controls), intent(in) :: pvr_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_pvr
!
!
      do i_pvr = 1, pvr_ctls%num_pvr_ctl
        call add_field_4_pvr_to_fld_ctl                                 &
     &     (pvr_ctls%pvr_ctl_type(i_pvr), field_ctl)
      end do
!
      end subroutine add_fields_4_pvrs_to_fld_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_new_pvr_ctl_struct(pvr_ctls)
!
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
      type(volume_rendering_controls) :: tmp_pvrs_c
!
!
      tmp_pvrs_c%num_pvr_ctl = pvr_ctls%num_pvr_ctl
      call alloc_pvr_ctl_struct(tmp_pvrs_c)
      call dup_pvr_ctl_struct                                           &
     &   (pvr_ctls%num_pvr_ctl, pvr_ctls, tmp_pvrs_c)
      call dealloc_pvr_ctl_struct(pvr_ctls)
!
      pvr_ctls%num_pvr_ctl = tmp_pvrs_c%num_pvr_ctl + 1
      call alloc_pvr_ctl_struct(pvr_ctls)
      call dup_pvr_ctl_struct                                           &
     &   (tmp_pvrs_c%num_pvr_ctl, tmp_pvrs_c, pvr_ctls)
      call dealloc_pvr_ctl_struct(tmp_pvrs_c)
!
      end subroutine append_new_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_ctl_struct(num_pvr, org_pvrs_c, new_pvrs_c)
!
      use bcast_control_data_4_pvr
!
      integer(kind = kint), intent(in) :: num_pvr
      type(volume_rendering_controls), intent(in) :: org_pvrs_c
      type(volume_rendering_controls), intent(inout) :: new_pvrs_c
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_pvr
        new_pvrs_c%fname_pvr_ctl(i) = org_pvrs_c%fname_pvr_ctl(i)
        call dup_pvr_ctl(org_pvrs_c%pvr_ctl_type(i),                    &
     &                   new_pvrs_c%pvr_ctl_type(i))
      end do
!
      end subroutine dup_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvrs
