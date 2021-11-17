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
!!      subroutine bcast_pvr_isosurfs_ctl(pvr_isos_c)
!!
!!      subroutine alloc_pvr_isosurfs_ctl(pvr_isos_c)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dup_pvr_isosurfs_ctl(org_pvr_iso_c, new_pvr_isos_c)
!!        type(pvr_isosurfs_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_isosurfs_ctl), intent(inout) :: new_pvr_isos_c
!!
!!      integer(kind = kint) function num_label_pvr_isosurface()
!!      subroutine set_label_pvr_isosurface(names)
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
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_isosurf_ctl
        type(read_character_item) :: isosurf_type_ctl
        type(read_real_item) :: iso_value_ctl
        type(read_real_item) :: opacity_ctl
      end type pvr_isosurf_ctl
!
      type pvr_isosurfs_ctl
        integer(kind = kint) :: num_pvr_iso_ctl = 0
        type(pvr_isosurf_ctl), allocatable :: pvr_iso_ctl(:)
      end type pvr_isosurfs_ctl
!
!     3rd level for isosurface
!
      character(len=kchara) :: hd_isosurf_value = 'isosurf_value'
      character(len=kchara) :: hd_pvr_opacity =   'opacity_ctl'
      character(len=kchara) :: hd_iso_direction = 'surface_direction'
!
      integer(kind = kint), parameter :: n_label_pvr_isosurface =   3
!
      private :: hd_isosurf_value, hd_pvr_opacity, hd_iso_direction
      private :: n_label_pvr_isosurface
!
      private :: alloc_pvr_isosurfs_ctl, read_pvr_isosurface_ctl
      private :: append_new_pvr_isosurf_ctl
      private :: copy_pvr_isosurfs_ctl, reset_pvr_isosurfs_ctl
      private :: dup_pvr_isosurface_ctl, reset_pvr_isosurface_ctl
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
      type(pvr_isosurf_ctl) :: pvr_iso_tmp
      integer(kind = kint) :: inum
      integer :: i
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(pvr_isos_c%pvr_iso_ctl)) return
      pvr_isos_c%num_pvr_iso_ctl = 0
      call alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_pvr_isosurf_ctl(pvr_isos_c)
          inum = pvr_isos_c%num_pvr_iso_ctl
          pvr_iso_tmp%iso_value_ctl%iflag =     0
          pvr_iso_tmp%isosurf_type_ctl%iflag =  0
          pvr_iso_tmp%opacity_ctl%iflag =       0

          call read_pvr_isosurface_ctl(id_control, hd_block,            &
     &        pvr_iso_tmp, c_buf)
!
          pvr_isos_c%pvr_iso_ctl(inum)%iso_value_ctl%iflag              &
     &       = pvr_iso_tmp%iso_value_ctl%iflag
          pvr_isos_c%pvr_iso_ctl(inum)%iso_value_ctl%realvalue          &
     &       = pvr_iso_tmp%iso_value_ctl%realvalue
          pvr_isos_c%pvr_iso_ctl(inum)%opacity_ctl%iflag                &
     &       = pvr_iso_tmp%opacity_ctl%iflag
          pvr_isos_c%pvr_iso_ctl(inum)%opacity_ctl%realvalue            &
     &       = pvr_iso_tmp%opacity_ctl%realvalue
          pvr_isos_c%pvr_iso_ctl(inum)%isosurf_type_ctl%iflag           &
     &       = pvr_iso_tmp%isosurf_type_ctl%iflag
          pvr_isos_c%pvr_iso_ctl(inum)%isosurf_type_ctl%charavalue      &
     &       = pvr_iso_tmp%isosurf_type_ctl%charavalue
!
        end if
      end do
!
!      write(*,*) my_rank, 'pvr_isos_c%num_pvr_iso_ctl',                &
!     &                   pvr_isos_c%num_pvr_iso_ctl
!      do i = 1, pvr_isos_c%num_pvr_iso_ctl
!       write(*,*)                                                      &
!     &        'pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue',     &
!     &         pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%iflag,          &
!     &         pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue
!        write(*,*) 'pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue',  &
!     &         pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%iflag,            &
!     &         pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue
!        write(*,*)                                                     &
!     &        'pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%realvalue',  &
!     &         pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%iflag,       &
!     &         pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%charavalue
!      end do
!
      end subroutine read_pvr_isosurfs_ctl
!
! -----------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurfs_ctl(pvr_isos_c)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
      integer(kind = kint) :: i
!
!
      call calypso_mpi_bcast_one_int(pvr_isos_c%num_pvr_iso_ctl, 0)
      if(pvr_isos_c%num_pvr_iso_ctl .gt. 0 .and. my_rank .gt. 0) then
        call alloc_pvr_isosurfs_ctl(pvr_isos_c)
      end if
      call calypso_mpi_barrier
!
      do i = 1, pvr_isos_c%num_pvr_iso_ctl
        call bcast_ctl_type_c1                                          &
     &     (pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl)
        call bcast_ctl_type_r1                                          &
     &     (pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl)
        call bcast_ctl_type_r1(pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl)
      end do
!
!      write(*,*) my_rank, 'pvr_isos_c%num_pvr_iso_ctl',                &
!     &                     pvr_isos_c%num_pvr_iso_ctl
!      do i = 1, pvr_isos_c%num_pvr_iso_ctl
!        write(*,*) my_rank,                                            &
!    &         'pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue',     &
!    &       i, pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%iflag,          &
!    &          pvr_isos_c%pvr_iso_ctl(i)%iso_value_ctl%realvalue
!        write(*,*) my_rank,                                            &
!    &         'pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue',       &
!    &       i, pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%iflag,            &
!    &          pvr_isos_c%pvr_iso_ctl(i)%opacity_ctl%realvalue
!        write(*,*) my_rank,                                            &
!    &         'pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%realvalue',  &
!    &       i, pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%iflag,       &
!    &          pvr_isos_c%pvr_iso_ctl(i)%isosurf_type_ctl%charavalue
!     end do
!
      end subroutine bcast_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      type(pvr_isosurfs_ctl), intent(inout) :: pvr_isos_c
!
!
      if(allocated(pvr_isos_c%pvr_iso_ctl)) then
        call reset_pvr_isosurfs_ctl                                     &
     &    (pvr_isos_c%num_pvr_iso_ctl, pvr_isos_c%pvr_iso_ctl)
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
      call reset_pvr_isosurfs_ctl                                       &
     &    (pvr_isos_c%num_pvr_iso_ctl, pvr_isos_c%pvr_iso_ctl)
!
      end subroutine alloc_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurface_ctl                                &
     &         (id_control, hd_block, pvr_iso_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_iso_direction,               &
     &      pvr_iso_ctl%isosurf_type_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_isosurf_value, pvr_iso_ctl%iso_value_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_iso_ctl%opacity_ctl)
      end do
!
      end subroutine read_pvr_isosurface_ctl
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
     &    pvr_isos_c%pvr_iso_ctl, tmp_pvr_isos%pvr_iso_ctl)
!
      call dealloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      pvr_isos_c%num_pvr_iso_ctl = tmp_pvr_isos%num_pvr_iso_ctl + 1
      call alloc_pvr_isosurfs_ctl(pvr_isos_c)
!
      call copy_pvr_isosurfs_ctl(tmp_pvr_isos%num_pvr_iso_ctl,          &
     &    tmp_pvr_isos%pvr_iso_ctl, pvr_isos_c%pvr_iso_ctl(1))
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
     &    org_pvr_iso_c%pvr_iso_ctl, new_pvr_isos_c%pvr_iso_ctl)
!
      end subroutine dup_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_isosurfs_ctl                                  &
     &         (num_pvr_iso, org_pvr_iso_c, new_pvr_iso_c)
!
      integer(kind = kint), intent(in) :: num_pvr_iso
      type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c(num_pvr_iso)
      type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c(num_pvr_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_iso
        call dup_pvr_isosurface_ctl(org_pvr_iso_c(i), new_pvr_iso_c(i))
      end do
!
      end subroutine copy_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurfs_ctl(num_pvr_iso, pvr_iso_ctl)
!
      integer(kind = kint), intent(in) :: num_pvr_iso
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl(num_pvr_iso)
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_iso
        call reset_pvr_isosurface_ctl(pvr_iso_ctl(i))
      end do
!
      end subroutine reset_pvr_isosurfs_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_isosurface_ctl(org_pvr_iso_c, new_pvr_iso_c)
!
      type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c
!
!
      call copy_chara_ctl(org_pvr_iso_c%isosurf_type_ctl,               &
     &                    new_pvr_iso_c%isosurf_type_ctl)
      call copy_real_ctl(org_pvr_iso_c%iso_value_ctl,                   &
     &                   new_pvr_iso_c%iso_value_ctl)
      call copy_real_ctl(org_pvr_iso_c%opacity_ctl,                     &
     &                   new_pvr_iso_c%opacity_ctl)
!
      end subroutine dup_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurface_ctl(pvr_iso_ctl)
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      pvr_iso_ctl%isosurf_type_ctl%iflag =  0
      pvr_iso_ctl%iso_value_ctl%iflag =     0
      pvr_iso_ctl%opacity_ctl%iflag =       0
!
      end subroutine reset_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_isosurface()
      num_label_pvr_isosurface = n_label_pvr_isosurface
      return
      end function num_label_pvr_isosurface
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_isosurface(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_isosurface)
!
!
      call set_control_labels(hd_isosurf_value, names( 1))
      call set_control_labels(hd_pvr_opacity,   names( 2))
      call set_control_labels(hd_iso_direction, names( 3))
!
      end subroutine set_label_pvr_isosurface
!
! ----------------------------------------------------------------------
!
      end module t_control_data_pvr_isosurfs
