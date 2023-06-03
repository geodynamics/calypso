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
!!
!!      subroutine append_new_map_render_control(map_ctls)
!!        type(map_rendering_controls), intent(inout) :: map_ctls
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
        integer(kind = kint) :: num_map_ctl = 0
!>        External section control file names
        character(len = kchara), allocatable :: fname_map_ctl(:)
!>        Structure of sections control
        type(map_ctl), allocatable :: map_ctl_struct(:)
      end type map_rendering_controls
!
      private :: dup_control_4_maps
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_map_ctl_stract(map_ctls)
!
      type(map_rendering_controls), intent(inout) :: map_ctls
      integer(kind = kint) :: i
!
!
      allocate(map_ctls%map_ctl_struct(map_ctls%num_map_ctl))
      allocate(map_ctls%fname_map_ctl(map_ctls%num_map_ctl))
!
      do i = 1, map_ctls%num_map_ctl
        call init_map_ctl_stract(map_ctls%map_ctl_struct(i))
      end do
!
      end subroutine alloc_map_ctl_stract
!
!  ---------------------------------------------------------------------
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
!   --------------------------------------------------------------------
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
      subroutine append_new_map_render_control(map_ctls)
!
      type(map_rendering_controls), intent(inout) :: map_ctls
!
      type(map_rendering_controls) :: tmp_psf_c
!
!
      tmp_psf_c%num_map_ctl = map_ctls%num_map_ctl
      call alloc_map_ctl_stract(tmp_psf_c)
      call dup_control_4_maps                                           &
     &    (tmp_psf_c%num_map_ctl, map_ctls, tmp_psf_c)
!
      call dealloc_map_ctl_stract(map_ctls)
!
      map_ctls%num_map_ctl = tmp_psf_c%num_map_ctl + 1
      call alloc_map_ctl_stract(map_ctls)
!
      call dup_control_4_maps                                           &
     &   (tmp_psf_c%num_map_ctl, tmp_psf_c, map_ctls)
!
      call dealloc_map_ctl_stract(tmp_psf_c)
!
      end subroutine append_new_map_render_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_maps                                     &
     &         (num_psf, org_psf_ctls, new_psf_ctls)
!
      integer(kind = kint), intent(in) :: num_psf
      type(map_rendering_controls), intent(in) :: org_psf_ctls
      type(map_rendering_controls), intent(inout) :: new_psf_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dup_control_4_map(org_psf_ctls%map_ctl_struct(i),          &
            new_psf_ctls%map_ctl_struct(i))
      end do
      new_psf_ctls%fname_map_ctl(1:num_psf)                             &
     &      = org_psf_ctls%fname_map_ctl(1:num_psf)
!
      end subroutine dup_control_4_maps
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_maps
