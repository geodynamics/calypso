!>@file   t_control_data_flines.f90
!!@brief  module t_control_data_flines
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine dealloc_fline_ctl_struct(fline_ctls)
!!      subroutine alloc_fline_ctl_struct(fline_ctls)
!!      subroutine init_fline_ctl_struct(hd_block, fline_ctls)
!!
!!      subroutine add_fields_4_flines_to_fld_ctl(fline_ctls, field_ctl)
!!        type(fieldline_controls), intent(in) :: fline_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine append_fline_control(idx_in, hd_block, fline_ctls)
!!      subroutine delete_fline_control(idx_in, fline_ctls)
!!        integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline  1
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_flines
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use t_ctl_data_field_line
!
      implicit  none
!
      type fieldline_controls
!>        Control block name
        character(len = kchara) :: block_name = 'fieldline'
!
        integer(kind = kint) :: num_fline_ctl = 0
        character(len = kchara), allocatable :: fname_fline_ctl(:)
        type(fline_ctl), allocatable :: fline_ctl_struct(:)
      end type fieldline_controls
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_ctl_struct(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      integer(kind = kint) :: i
!
      if(allocated(fline_ctls%fline_ctl_struct) .eqv. .FALSE.) return
!
      do i = 1, fline_ctls%num_fline_ctl
        call deallocate_cont_dat_fline(fline_ctls%fline_ctl_struct(i))
      end do
      deallocate(fline_ctls%fline_ctl_struct)
      deallocate(fline_ctls%fname_fline_ctl)
      fline_ctls%num_fline_ctl = 0
!
      end subroutine dealloc_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_ctl_struct(fline_ctls)
!
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      allocate(fline_ctls%fline_ctl_struct(fline_ctls%num_fline_ctl))
      allocate(fline_ctls%fname_fline_ctl(fline_ctls%num_fline_ctl))
!
      end subroutine alloc_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine init_fline_ctl_struct(hd_block, fline_ctls)
!
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      fline_ctls%block_name = hd_block
      fline_ctls%num_fline_ctl = 0
!
      end subroutine init_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_fline_control(idx_in, hd_block, fline_ctls)
!
      use ctl_data_field_line_IO
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      type(fieldline_controls) :: tmp_fline_c
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.fline_ctls%num_fline_ctl) return
!
      tmp_fline_c%num_fline_ctl = fline_ctls%num_fline_ctl
      call alloc_fline_ctl_struct(tmp_fline_c)
!
      do i = 1, tmp_fline_c%num_fline_ctl
        tmp_fline_c%fname_fline_ctl(i)                                  &
     &        = fline_ctls%fname_fline_ctl(i)
        call dup_control_4_fline(fline_ctls%fline_ctl_struct(i),        &
                                 tmp_fline_c%fline_ctl_struct(i))
      end do
!
      call dealloc_fline_ctl_struct(fline_ctls)
      fline_ctls%num_fline_ctl = tmp_fline_c%num_fline_ctl + 1
      call alloc_fline_ctl_struct(fline_ctls)
!
      do i = 1, idx_in
        fline_ctls%fname_fline_ctl(i)                                   &
     &        = tmp_fline_c%fname_fline_ctl(i)
        call dup_control_4_fline(tmp_fline_c%fline_ctl_struct(i),       &
                                 fline_ctls%fline_ctl_struct(i))
      end do
!
      fline_ctls%fname_fline_ctl(idx_in+1) = 'NO_FILE'
      call init_field_line_ctl_label(hd_block,                          &
     &    fline_ctls%fline_ctl_struct(idx_in+1))
!
      do i = idx_in+1, tmp_fline_c%num_fline_ctl
        fline_ctls%fname_fline_ctl(i+1)                                 &
     &        = tmp_fline_c%fname_fline_ctl(i)
        call dup_control_4_fline(tmp_fline_c%fline_ctl_struct(i),       &
                                 fline_ctls%fline_ctl_struct(i+1))
      end do
!
      call dealloc_fline_ctl_struct(tmp_fline_c)
!
      end subroutine append_fline_control
!
! -----------------------------------------------------------------------
!
      subroutine delete_fline_control(idx_in, fline_ctls)
!
      use ctl_data_field_line_IO
!
      integer(kind = kint), intent(in) :: idx_in
      type(fieldline_controls), intent(inout) :: fline_ctls
!
      type(fieldline_controls) :: tmp_fline_c
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.fline_ctls%num_fline_ctl) return
!
      tmp_fline_c%num_fline_ctl = fline_ctls%num_fline_ctl
      call alloc_fline_ctl_struct(tmp_fline_c)
!
      do i = 1, tmp_fline_c%num_fline_ctl
        tmp_fline_c%fname_fline_ctl(i)                                  &
     &        = fline_ctls%fname_fline_ctl(i)
        call dup_control_4_fline(fline_ctls%fline_ctl_struct(i),        &
                                 tmp_fline_c%fline_ctl_struct(i))
      end do
!
      call dealloc_fline_ctl_struct(fline_ctls)
      fline_ctls%num_fline_ctl = tmp_fline_c%num_fline_ctl + 1
      call alloc_fline_ctl_struct(fline_ctls)
!
      do i = 1, idx_in-1
        fline_ctls%fname_fline_ctl(i)                                   &
     &        = tmp_fline_c%fname_fline_ctl(i)
        call dup_control_4_fline(tmp_fline_c%fline_ctl_struct(i),       &
                                 fline_ctls%fline_ctl_struct(i))
      end do
      do i = idx_in, fline_ctls%num_fline_ctl
        fline_ctls%fname_fline_ctl(i)                                   &
     &        = tmp_fline_c%fname_fline_ctl(i+1)
        call dup_control_4_fline(tmp_fline_c%fline_ctl_struct(i+1),     &
                                 fline_ctls%fline_ctl_struct(i))
      end do
!
      call dealloc_fline_ctl_struct(tmp_fline_c)
!
      end subroutine delete_fline_control
!
! -----------------------------------------------------------------------
!
      subroutine add_fields_4_flines_to_fld_ctl(fline_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(fieldline_controls), intent(in) :: fline_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_fline
!
!
      do i_fline = 1, fline_ctls%num_fline_ctl
        call add_field_4_fline_to_fld_ctl                               &
     &     (fline_ctls%fline_ctl_struct(i_fline), field_ctl)
      end do
!
      end subroutine add_fields_4_flines_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_flines
