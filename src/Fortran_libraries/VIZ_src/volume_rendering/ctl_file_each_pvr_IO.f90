!>@file   ctl_file_each_pvr_IO.f90
!!@brief  module ctl_file_each_pvr_IO
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Set PVR parameters from control files
!!
!!@verbatim
!!      subroutine sel_read_control_pvr(id_control, hd_pvr_ctl,         &
!!     &          fname_pvr_ctl, pvr_ctl_type, c_buf)
!!      subroutine read_control_pvr_file(id_control, fname_pvr_ctl,     &
!!     &          hd_pvr_ctl, pvr_ctl_type, c_buf)
!!      subroutine read_control_pvr_update(id_control, fname_pvr_ctl,   &
!!     &                                   hd_pvr_ctl, pvr_ctl_type)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_pvr_ctl
!!        character(len = kchara), intent(inout) :: fname_pvr_ctl
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine sel_write_control_pvr(id_control, hd_pvr_ctl,        &
!!     &          fname_pvr_ctl, pvr_ctl_type, level)
!!      subroutine write_control_pvr_file(id_control, fname_pvr_ctl,    &
!!     &          hd_pvr_ctl, pvr_ctl_type)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: fname_pvr_ctl
!!        character(len = kchara), intent(in) :: hd_pvr_ctl
!!        type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
!!        integer(kind = kint), intent(inout) :: level
!!@endverbatim
!
      module ctl_file_each_pvr_IO
!
      use m_precision
!
      use t_control_data_4_pvr
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_read_control_pvr(id_control, hd_pvr_ctl,           &
     &          fname_pvr_ctl, pvr_ctl_type, c_buf)
!
      use ctl_data_each_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_pvr_ctl
      character(len = kchara), intent(inout) :: fname_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_pvr_ctl)) then
        fname_pvr_ctl = third_word(c_buf)
!
        write(*,'(2a)') ' is read from ', trim(fname_pvr_ctl)
        call read_control_pvr_file(id_control+2, fname_pvr_ctl,         &
     &                             hd_pvr_ctl, pvr_ctl_type, c_buf)
      else if(check_begin_flag(c_buf, hd_pvr_ctl)) then
        fname_pvr_ctl = 'NO_FILE'
!
        write(*,*) 'is included.'
        call read_pvr_ctl(id_control, hd_pvr_ctl, pvr_ctl_type, c_buf)
      end if
!
      end subroutine sel_read_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_file(id_control, fname_pvr_ctl,       &
     &          hd_pvr_ctl, pvr_ctl_type, c_buf)
!
      use ctl_data_each_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(id_control, file=fname_pvr_ctl, status='old')
      do
        call load_one_line_from_control(id_control, hd_pvr_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_pvr_ctl(id_control, hd_pvr_ctl,                       &
     &                    pvr_ctl_type, c_buf)
        if(pvr_ctl_type%i_pvr_ctl .gt. 0) exit
      end do
      close(id_control)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_pvr_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_update(id_control, fname_pvr_ctl,     &
     &                                   hd_pvr_ctl, pvr_ctl_type)
!
      use ctl_data_each_pvr_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in)  :: fname_pvr_ctl
      character(len = kchara), intent(in)  :: hd_pvr_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
!
      type(buffer_for_control) :: c_buf1
!
      if(no_file_flag(fname_pvr_ctl)) return
!
      c_buf1%level = 0
      open(id_control, file=fname_pvr_ctl, status='old')
      pvr_ctl_type%i_pvr_ctl = 0
!
      do
        call load_one_line_from_control(id_control, hd_pvr_ctl, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_pvr_update_flag                                       &
     &     (id_control, hd_pvr_ctl, pvr_ctl_type, c_buf1)
        if(pvr_ctl_type%i_pvr_ctl .gt. 0) exit
      end do
      close(id_control)
      if(c_buf1%iend .gt. 0) pvr_ctl_type%i_pvr_ctl = - c_buf1%iend
!
      end subroutine read_control_pvr_update
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_control_pvr(id_control, hd_pvr_ctl,          &
     &          fname_pvr_ctl, pvr_ctl_type, level)
!
      use ctl_data_each_pvr_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
      integer(kind = kint), intent(inout) :: level
!
!
      if(no_file_flag(fname_pvr_ctl)) then
        write(*,'(a)') ' is included.'
        call write_pvr_ctl(id_control, hd_pvr_ctl,                      &
     &                     pvr_ctl_type, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(2a)') ' should be written to file ... ',              &
     &                trim(fname_pvr_ctl)
        call write_pvr_ctl(id_control, hd_pvr_ctl,                      &
     &                     pvr_ctl_type, level)
      else
        write(*,'(2a)') ' is written to file...', trim(fname_pvr_ctl)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_pvr_ctl, fname_pvr_ctl)
        call write_control_pvr_file(id_control+2, fname_pvr_ctl,        &
     &                              hd_pvr_ctl,  pvr_ctl_type)
      end if
!
      end subroutine sel_write_control_pvr
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_pvr_file(id_control, fname_pvr_ctl,      &
     &          hd_pvr_ctl, pvr_ctl_type)
!
      use ctl_data_each_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_pvr_ctl
      character(len = kchara), intent(in) :: hd_pvr_ctl
      type(pvr_parameter_ctl), intent(in) :: pvr_ctl_type
!
       integer(kind = kint) :: level
!
!
      level = 0
      open(id_control, file=fname_pvr_ctl)
      call write_pvr_ctl(id_control, hd_pvr_ctl, pvr_ctl_type, level)
      close(id_control)
!
      end subroutine write_control_pvr_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_each_pvr_IO
