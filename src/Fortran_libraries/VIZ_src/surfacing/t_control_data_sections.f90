!>@file   t_control_data_sections.f90
!!@brief  module t_control_data_sections
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine alloc_psf_ctl_stract(psf_ctls)
!!      subroutine dealloc_psf_ctl_stract(psf_ctls)
!!
!!      subroutine append_new_section_control(psf_ctls)
!!        type(section_controls), intent(inout) :: psf_ctls
!!      subroutine add_fields_4_psfs_to_fld_ctl(psf_ctls, field_ctl)
!!        type(section_controls), intent(in) :: psf_ctls
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl  1
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_psf
!
      implicit  none
!
!
      type section_controls
        integer(kind = kint) :: num_psf_ctl = 0
!>        External section control file names
        character(len = kchara), allocatable :: fname_psf_ctl(:)
!>        Structure of sections control
        type(psf_ctl), allocatable :: psf_ctl_struct(:)
      end type section_controls
!
      private :: dup_control_4_psfs
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
      integer(kind = kint) :: i
!
!
      allocate(psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl))
      allocate(psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl))
!
      do i = 1, psf_ctls%num_psf_ctl
        call init_psf_ctl_stract(psf_ctls%psf_ctl_struct(i))
      end do
!
      end subroutine alloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_ctl_stract(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      integer(kind = kint) :: i
!
      if(allocated(psf_ctls%psf_ctl_struct) .eqv. .FALSE.) return
!
      do i = 1, psf_ctls%num_psf_ctl
        call dealloc_cont_dat_4_psf(psf_ctls%psf_ctl_struct(i))
      end do
!
      deallocate(psf_ctls%psf_ctl_struct, psf_ctls%fname_psf_ctl)
      psf_ctls%num_psf_ctl = 0
!
      end subroutine dealloc_psf_ctl_stract
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine add_fields_4_psfs_to_fld_ctl(psf_ctls, field_ctl)
!
      use t_control_array_character3
!
      type(section_controls), intent(in) :: psf_ctls
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, psf_ctls%num_psf_ctl
        call add_fields_4_psf_to_fld_ctl                                &
     &     (psf_ctls%psf_ctl_struct(i_psf), field_ctl)
      end do
!
      end subroutine add_fields_4_psfs_to_fld_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_new_section_control(psf_ctls)
!
      type(section_controls), intent(inout) :: psf_ctls
!
      type(section_controls) :: tmp_psf_c
!
!
      tmp_psf_c%num_psf_ctl = psf_ctls%num_psf_ctl
      call alloc_psf_ctl_stract(tmp_psf_c)
      call dup_control_4_psfs                                           &
     &    (tmp_psf_c%num_psf_ctl, psf_ctls, tmp_psf_c)
!
      call dealloc_psf_ctl_stract(psf_ctls)
!
      psf_ctls%num_psf_ctl = tmp_psf_c%num_psf_ctl + 1
      call alloc_psf_ctl_stract(psf_ctls)
!
      call dup_control_4_psfs                                           &
     &   (tmp_psf_c%num_psf_ctl, tmp_psf_c, psf_ctls)
!
      call dealloc_psf_ctl_stract(tmp_psf_c)
!
      end subroutine append_new_section_control
!
! -----------------------------------------------------------------------
!
      subroutine dup_control_4_psfs                                     &
     &         (num_psf, org_psf_ctls, new_psf_ctls)
!
      integer(kind = kint), intent(in) :: num_psf
      type(section_controls), intent(in) :: org_psf_ctls
      type(section_controls), intent(inout) :: new_psf_ctls
!
      integer(kind = kint) :: i
!
      do i = 1, num_psf
        call dup_control_4_psf(org_psf_ctls%psf_ctl_struct(i),          &
            new_psf_ctls%psf_ctl_struct(i))
      end do
      new_psf_ctls%fname_psf_ctl(1:num_psf)                             &
     &      = org_psf_ctls%fname_psf_ctl(1:num_psf)
!
      end subroutine dup_control_4_psfs
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_sections
