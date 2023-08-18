!>@file   t_control_data_isosurfaces.f90
!!@brief  module t_control_data_isosurfaces
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for isosurfaces
!!
!!@verbatim
!!      subroutine alloc_iso_ctl_stract(iso_ctls)
!!      subroutine dealloc_iso_ctl_stract(iso_ctls)
!!      subroutine init_iso_ctls_labels(hd_block, iso_ctls)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!
!!       subroutine add_fields_4_isos_to_fld_ctl(iso_ctls, field_ctl)
!!        type(isosurf_controls), intent(in) :: iso_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine append_isosurface_control(idx_in, hd_block, iso_ctls)
!!      subroutine delete_isosurface_control(idx_in, iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_isosurfaces
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_iso
!
      implicit  none
!
!
      type isosurf_controls
!>        Control block name
        character(len = kchara) :: block_name = 'isosurface_ctl'
!>        # of Structure for isosurface control
        integer(kind = kint) :: num_iso_ctl = 0
!>        file name for isosurface control
        character(len = kchara), allocatable :: fname_iso_ctl(:)
!>        Structure for isosurface control
        type(iso_ctl), allocatable :: iso_ctl_struct(:)
      end type isosurf_controls
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      allocate(iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl))
      allocate(iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl))
!
      end subroutine alloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_ctl_stract(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      integer(kind = kint) :: i
!
      if(allocated(iso_ctls%iso_ctl_struct) .eqv. .FALSE.) return
!
      do i = 1, iso_ctls%num_iso_ctl
        call dealloc_cont_dat_4_iso(iso_ctls%iso_ctl_struct(i))
      end do
      deallocate(iso_ctls%iso_ctl_struct, iso_ctls%fname_iso_ctl)
      iso_ctls%num_iso_ctl = 0
!
      end subroutine dealloc_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine init_iso_ctls_labels(hd_block, iso_ctls)
!
      character(len=kchara), intent(in) :: hd_block
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      iso_ctls%num_iso_ctl = 0
      iso_ctls%block_name = hd_block
!
      end subroutine init_iso_ctls_labels
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_isos_to_fld_ctl(iso_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(isosurf_controls), intent(in) :: iso_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
      integer(kind = kint) :: i_iso
!
!
      do i_iso = 1, iso_ctls%num_iso_ctl
        call add_fields_4_iso_to_fld_ctl                                &
     &     (iso_ctls%iso_ctl_struct(i_iso), field_ctl)
      end do
!
      end subroutine add_fields_4_isos_to_fld_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_isosurface_control(idx_in, hd_block, iso_ctls)
!
      use ctl_data_isosurface_IO
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      type(isosurf_controls) :: tmp_iso_c
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.iso_ctls%num_iso_ctl) return
!
      tmp_iso_c%num_iso_ctl = iso_ctls%num_iso_ctl
      call alloc_iso_ctl_stract(tmp_iso_c)
      do i = 1, iso_ctls%num_iso_ctl
        call dup_control_4_iso(iso_ctls%iso_ctl_struct(i),              &
     &                         tmp_iso_c%iso_ctl_struct(i))
        tmp_iso_c%fname_iso_ctl(i) = iso_ctls%fname_iso_ctl(i)
      end do
!
      call dealloc_iso_ctl_stract(iso_ctls)
      iso_ctls%num_iso_ctl = tmp_iso_c%num_iso_ctl + 1
      call alloc_iso_ctl_stract(iso_ctls)
!
      do i = 1, idx_in
        call dup_control_4_iso(tmp_iso_c%iso_ctl_struct(i),             &
                               iso_ctls%iso_ctl_struct(i))
        iso_ctls%fname_iso_ctl(i) = tmp_iso_c%fname_iso_ctl(i)
      end do
      call init_iso_ctl_stract(hd_block,                                &
     &                         iso_ctls%iso_ctl_struct(idx_in+1))
      iso_ctls%fname_iso_ctl(idx_in+1) = 'NO_FILE'
      do i = idx_in+1, tmp_iso_c%num_iso_ctl
        call dup_control_4_iso(tmp_iso_c%iso_ctl_struct(i),             &
     &                         iso_ctls%iso_ctl_struct(i+1))
        iso_ctls%fname_iso_ctl(i+1) = tmp_iso_c%fname_iso_ctl(i)
      end do
!
      call dealloc_iso_ctl_stract(tmp_iso_c)
!
      end subroutine append_isosurface_control
!
! -----------------------------------------------------------------------
!
      subroutine delete_isosurface_control(idx_in, iso_ctls)
!
      integer(kind = kint), intent(in) :: idx_in
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      type(isosurf_controls) :: tmp_iso_c
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.iso_ctls%num_iso_ctl) return
!
      tmp_iso_c%num_iso_ctl = iso_ctls%num_iso_ctl
      call alloc_iso_ctl_stract(tmp_iso_c)
      do i = 1, iso_ctls%num_iso_ctl
        call dup_control_4_iso(iso_ctls%iso_ctl_struct(i),              &
     &                         tmp_iso_c%iso_ctl_struct(i))
        tmp_iso_c%fname_iso_ctl(i) = iso_ctls%fname_iso_ctl(i)
      end do
!
      call dealloc_iso_ctl_stract(iso_ctls)
      iso_ctls%num_iso_ctl = tmp_iso_c%num_iso_ctl - 1
      call alloc_iso_ctl_stract(iso_ctls)
!
      do i = 1, idx_in-1
        call dup_control_4_iso(tmp_iso_c%iso_ctl_struct(i),             &
     &                         iso_ctls%iso_ctl_struct(i))
        iso_ctls%fname_iso_ctl(i) = tmp_iso_c%fname_iso_ctl(i)
      end do
      do i = idx_in, iso_ctls%num_iso_ctl
        call dup_control_4_iso(tmp_iso_c%iso_ctl_struct(i+1),           &
     &                         iso_ctls%iso_ctl_struct(i))
        iso_ctls%fname_iso_ctl(i+1) = tmp_iso_c%fname_iso_ctl(i)
      end do
!
      call dealloc_iso_ctl_stract(tmp_iso_c)
!
      end subroutine delete_isosurface_control
!
! -----------------------------------------------------------------------
!
      end module t_control_data_isosurfaces
