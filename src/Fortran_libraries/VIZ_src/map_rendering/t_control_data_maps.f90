!>@file   t_control_data_maps.f90
!!@brief  module t_control_data_maps
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine alloc_map_ctl_stract(map_ctls)
!!      subroutine dealloc_map_ctl_stract(map_ctls)
!!      subroutine init_map_ctls_labels(hd_block, map_ctls)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!
!!      subroutine append_map_render_control(idx_in, hd_block, map_ctls)
!!      subroutine delete_map_render_control(idx_in, map_ctls)
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!
!!      subroutine add_fields_4_maps_to_fld_ctl(map_ctls, field_ctl)
!!        type(map_rendering_controls), intent(in) :: map_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array map_rendering_ctl
!!      file   map_rendering_ctl   'ctl_map_cmb'
!!    end array map_rendering_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_maps
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_map
!
      implicit  none
!
!
      type map_rendering_controls
!>        Control block name
        character(len = kchara) :: block_name = 'map_rendering_ctl'
!>        # of structure of sections control
        integer(kind = kint) :: num_map_ctl = 0
!>        External section control file names
        character(len = kchara), allocatable :: fname_map_ctl(:)
!>        Structure of sections control
        type(map_ctl), allocatable :: map_ctl_struct(:)
      end type map_rendering_controls
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_map_ctl_stract(map_ctls)
!
      use ctl_data_map_rendering_IO
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      integer(kind = kint) :: i
!
!
      allocate(map_ctls%map_ctl_struct(map_ctls%num_map_ctl))
      allocate(map_ctls%fname_map_ctl(map_ctls%num_map_ctl))
!
      end subroutine alloc_map_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_map_ctl_stract(map_ctls)
!
      type(map_rendering_controls), intent(inout) :: map_ctls
!
      integer(kind = kint) :: i
!
      if(allocated(map_ctls%map_ctl_struct) .eqv. .FALSE.) return
!
      do i = 1, map_ctls%num_map_ctl
        call dealloc_cont_dat_4_map(map_ctls%map_ctl_struct(i))
      end do
!
      deallocate(map_ctls%map_ctl_struct, map_ctls%fname_map_ctl)
      map_ctls%num_map_ctl = 0
!
      end subroutine dealloc_map_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine init_map_ctls_labels(hd_block, map_ctls)
!
      character(len=kchara), intent(in) :: hd_block
      type(map_rendering_controls), intent(inout) :: map_ctls
!
      map_ctls%num_map_ctl = 0
      map_ctls%block_name = hd_block
!
      end subroutine init_map_ctls_labels
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_4_maps_to_fld_ctl(map_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(map_rendering_controls), intent(in) :: map_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, map_ctls%num_map_ctl
        call add_fields_4_map_to_fld_ctl                                &
     &     (map_ctls%map_ctl_struct(i_psf), field_ctl)
      end do
!
      end subroutine add_fields_4_maps_to_fld_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_map_render_control(idx_in, hd_block, map_ctls)
!
      use ctl_data_map_rendering_IO
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(map_rendering_controls), intent(inout) :: map_ctls
!
      type(map_rendering_controls) :: tmp_map_c
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.map_ctls%num_map_ctl) return
!
      tmp_map_c%num_map_ctl = map_ctls%num_map_ctl
      call alloc_map_ctl_stract(tmp_map_c)
      do i = 1, tmp_map_c%num_map_ctl
        call dup_control_4_map(map_ctls%map_ctl_struct(i),              &
     &                         tmp_map_c%map_ctl_struct(i))
        tmp_map_c%fname_map_ctl(i) = map_ctls%fname_map_ctl(i)
      end do
!
      call dealloc_map_ctl_stract(map_ctls)
      map_ctls%num_map_ctl = tmp_map_c%num_map_ctl + 1
      call alloc_map_ctl_stract(map_ctls)
!
      do i = 1, idx_in
        call dup_control_4_map(tmp_map_c%map_ctl_struct(i),             &
     &                         map_ctls%map_ctl_struct(i))
        map_ctls%fname_map_ctl(i) = tmp_map_c%fname_map_ctl(i)
      end do
      call init_map_control_label(hd_block,                             &
     &                            map_ctls%map_ctl_struct(idx_in+1))
      map_ctls%fname_map_ctl(idx_in+1) = 'NO_FILE'
      do i = idx_in+1, tmp_map_c%num_map_ctl
        call dup_control_4_map(tmp_map_c%map_ctl_struct(i),             &
     &                         map_ctls%map_ctl_struct(i+1))
        map_ctls%fname_map_ctl(i+1) = tmp_map_c%fname_map_ctl(i)
      end do
!
      call dealloc_map_ctl_stract(tmp_map_c)
!
      end subroutine append_map_render_control
!
! -----------------------------------------------------------------------
!
      subroutine delete_map_render_control(idx_in, map_ctls)
!
      use ctl_data_map_rendering_IO
!
      integer(kind = kint), intent(in) :: idx_in
      type(map_rendering_controls), intent(inout) :: map_ctls
!
      type(map_rendering_controls) :: tmp_map_c
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.map_ctls%num_map_ctl) return
!
      tmp_map_c%num_map_ctl = map_ctls%num_map_ctl
      call alloc_map_ctl_stract(tmp_map_c)
      do i = 1, tmp_map_c%num_map_ctl
        call dup_control_4_map(map_ctls%map_ctl_struct(i),              &
     &                         tmp_map_c%map_ctl_struct(i))
        tmp_map_c%fname_map_ctl(i) = map_ctls%fname_map_ctl(i)
      end do
!
      call dealloc_map_ctl_stract(map_ctls)
      map_ctls%num_map_ctl = tmp_map_c%num_map_ctl + 1
      call alloc_map_ctl_stract(map_ctls)
!
      do i = 1, idx_in-1
        call dup_control_4_map(tmp_map_c%map_ctl_struct(i),             &
     &                         map_ctls%map_ctl_struct(i))
        map_ctls%fname_map_ctl(i) = tmp_map_c%fname_map_ctl(i)
      end do
      do i = idx_in, map_ctls%num_map_ctl
        call dup_control_4_map(tmp_map_c%map_ctl_struct(i+1),           &
     &                         map_ctls%map_ctl_struct(i))
        map_ctls%fname_map_ctl(i) = tmp_map_c%fname_map_ctl(i+1)
      end do
!
      call dealloc_map_ctl_stract(tmp_map_c)
!
      end subroutine delete_map_render_control
!
! -----------------------------------------------------------------------
!
      end module t_control_data_maps
