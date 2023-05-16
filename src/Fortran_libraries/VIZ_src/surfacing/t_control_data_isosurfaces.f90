!>@file   t_control_data_isosurfaces.f90
!!@brief  module t_control_data_isosurfaces
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for isosurfaces
!!
!!@verbatim
!!      subroutine dealloc_iso_ctl_stract(iso_ctls)
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!
!!       subroutine add_fields_4_isos_to_fld_ctl(iso_ctls, field_ctl)
!!        type(isosurf_controls), intent(in) :: iso_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
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
        integer(kind = kint) :: num_iso_ctl = 0
!>        file name for isosurface control
        character(len = kchara), allocatable :: fname_iso_ctl(:)
!>        Structure for isosurface control
        type(iso_ctl), allocatable :: iso_ctl_struct(:)
      end type isosurf_controls
!
!
      private :: dup_control_4_isos
!
!   --------------------------------------------------------------------
!
      contains
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
      subroutine append_new_isosurface_control(iso_ctls)
!
      type(isosurf_controls), intent(inout) :: iso_ctls
!
      type(isosurf_controls) :: tmp_iso_c
!
!
      tmp_iso_c%num_iso_ctl = iso_ctls%num_iso_ctl
      call alloc_iso_ctl_stract(tmp_iso_c)
      call dup_control_4_isos                                           &
     &    (tmp_iso_c%num_iso_ctl, iso_ctls, tmp_iso_c)
!
      call dealloc_iso_ctl_stract(iso_ctls)
!
      iso_ctls%num_iso_ctl = tmp_iso_c%num_iso_ctl + 1
      call alloc_iso_ctl_stract(iso_ctls)
!
      call dup_control_4_isos                                           &
     &   (tmp_iso_c%num_iso_ctl, tmp_iso_c, iso_ctls)
!
      call dealloc_iso_ctl_stract(tmp_iso_c)
!
      end subroutine append_new_isosurface_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_isos                                     &
     &         (num_iso, org_iso_ctls, new_iso_ctls)
!
      integer(kind = kint), intent(in) :: num_iso
      type(isosurf_controls), intent(in) :: org_iso_ctls
      type(isosurf_controls), intent(inout) :: new_iso_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_iso
        call dup_control_4_iso(org_iso_ctls%iso_ctl_struct(i),          &
            new_iso_ctls%iso_ctl_struct(i))
      end do
      new_iso_ctls%fname_iso_ctl(1:num_iso)                             &
     &      = org_iso_ctls%fname_iso_ctl(1:num_iso)
!
      end subroutine dup_control_4_isos
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
      end module t_control_data_isosurfaces
