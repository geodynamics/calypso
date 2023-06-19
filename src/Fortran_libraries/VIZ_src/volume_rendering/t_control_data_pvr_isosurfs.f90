!>@file   t_control_data_pvr_isosurfs.f90
!!@brief  module t_control_data_pvr_isosurfs
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_isosurfs_ctl                                &
!!     &         (id_control, hd_block, pvr_isos_c, c_buf)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_isosurfs_ctl                               &
!!     &         (id_control, hd_block, pvr_isos_c, level)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(pvr_isosurfs_ctl), intent(in) :: pvr_isos_c
!!         integer(kind = kint), intent(inout) :: level
!!      subroutine alloc_pvr_isosurfs_ctl(pvr_isos_c)
!!      subroutine dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine dup_pvr_isosurfs_ctl(org_pvr_iso_c, new_pvr_isos_c)
!!        type(pvr_isosurfs_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_isosurfs_ctl), intent(inout) :: new_pvr_isos_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array isosurface_ctl  2
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_isosurfs
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_ctl_data_pvr_isosurface
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_isosurfs_ctl
        integer(kind = kint) :: num_pvr_iso_ctl = 0
        type(pvr_isosurf_ctl), allocatable :: pvr_iso_ctl(:)
      end type pvr_isosurfs_ctl
!
      private :: append_new_pvr_isosurf_ctl
      private :: copy_pvr_isosurfs_ctl, reset_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurfs_ctl                                  &
     &         (id_control, hd_block, pvr_isos_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(pvr_isos_c%pvr_iso_ctl)) return
      pvr_isos_c%num_pvr_iso_ctl = 0
      call alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_pvr_isosurf_ctl(pvr_isos_c)
          call read_pvr_isosurface_ctl(id_control, hd_block,            &
     &        pvr_isos_c%pvr_iso_ctl(pvr_isos_c%num_pvr_iso_ctl),       &
     &        c_buf)
        end if
      end do
!
      end subroutine read_pvr_isosurfs_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_pvr_isosurfs_ctl                                 &
     &         (id_control, hd_block, pvr_isos_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurfs_ctl), intent(in) :: pvr_isos_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(pvr_isos_c%num_pvr_iso_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, pvr_isos_c%num_pvr_iso_ctl
        call write_pvr_isosurface_ctl(id_control, hd_block,             &
     &                                pvr_isos_c%pvr_iso_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     hd_block)
!
      end subroutine write_pvr_isosurfs_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
!
      if(allocated(pvr_isos_c%pvr_iso_ctl)) then
        call reset_pvr_isosurfs_ctl(pvr_isos_c)
        deallocate(pvr_isos_c%pvr_iso_ctl)
      end if
!
      pvr_isos_c%num_pvr_iso_ctl = 0
!
      end subroutine dealloc_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
!
      allocate(pvr_isos_c%pvr_iso_ctl(pvr_isos_c%num_pvr_iso_ctl))
      call reset_pvr_isosurfs_ctl(pvr_isos_c)
!
      end subroutine alloc_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_new_pvr_isosurf_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
      type(pvr_isosurfs_ctl) :: tmp_pvr_isos
!
!
      tmp_pvr_isos%num_pvr_iso_ctl = pvr_isos_c%num_pvr_iso_ctl
      call alloc_pvr_isosurfs_ctl(tmp_pvr_isos)
      call copy_pvr_isosurfs_ctl(tmp_pvr_isos%num_pvr_iso_ctl,          &
     &                           pvr_isos_c, tmp_pvr_isos)
!
      call dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      pvr_isos_c%num_pvr_iso_ctl = tmp_pvr_isos%num_pvr_iso_ctl + 1
      call alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      call copy_pvr_isosurfs_ctl(tmp_pvr_isos%num_pvr_iso_ctl,          &
     &                           tmp_pvr_isos, pvr_isos_c)
!
      call dealloc_pvr_isosurfs_ctl(tmp_pvr_isos)
!
      end subroutine append_new_pvr_isosurf_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_isosurfs_ctl(org_pvr_iso_c, new_pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_isosurfs_ctl), intent(inout) :: new_pvr_isos_c
!
!
      new_pvr_isos_c%num_pvr_iso_ctl = org_pvr_iso_c%num_pvr_iso_ctl
      call alloc_pvr_isosurfs_ctl(new_pvr_isos_c)
      call copy_pvr_isosurfs_ctl(org_pvr_iso_c%num_pvr_iso_ctl,         &
     &                           org_pvr_iso_c, new_pvr_isos_c)
!
      end subroutine dup_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_isosurfs_ctl                                  &
     &         (num_pvr_iso, org_pvr_isos_c, new_pvr_isos_c)
!
      integer(kind = kint), intent(in) :: num_pvr_iso
      type(pvr_isosurfs_ctl), intent(in) :: org_pvr_isos_c
      type(pvr_isosurfs_ctl), intent(inout) :: new_pvr_isos_c
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_iso
        call dup_pvr_isosurface_ctl(org_pvr_isos_c%pvr_iso_ctl(i),      &
     &                              new_pvr_isos_c%pvr_iso_ctl(i))
      end do
!
      end subroutine copy_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurfs_ctl(pvr_isos_ctl)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_ctl
!
      integer(kind = kint) :: i
!
      do i = 1, pvr_isos_ctl%num_pvr_iso_ctl
        call reset_pvr_isosurface_ctl(pvr_isos_ctl%pvr_iso_ctl(i))
      end do
!
      end subroutine reset_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_isosurfs
