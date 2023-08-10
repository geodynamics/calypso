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
!!      subroutine init_psf_ctls_labels(hd_block, psf_ctls)
!!        character(len=kchara), intent(in) :: hd_block
!!        type(section_controls), intent(inout) :: psf_ctls
!!
!!      subroutine append_section_control(idx_in, hd_block, psf_ctls)
!!      subroutine delete_section_control(idx_in, psf_ctls)
!!        type(section_controls), intent(inout) :: psf_ctls
!!
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
!>        Control block name
        character(len = kchara) :: block_name = 'cross_section_ctl'
!>        # of Structure for isosurface control
        integer(kind = kint) :: num_psf_ctl = 0
!>        External section control file names
        character(len = kchara), allocatable :: fname_psf_ctl(:)
!>        Structure of sections control
        type(psf_ctl), allocatable :: psf_ctl_struct(:)
      end type section_controls
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
!
!
      allocate(psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl))
      allocate(psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl))
!
      end subroutine alloc_psf_ctl_stract
!
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
!
      subroutine init_psf_ctls_labels(hd_block, psf_ctls)
!
      character(len=kchara), intent(in) :: hd_block
      type(section_controls), intent(inout) :: psf_ctls
!
      psf_ctls%num_psf_ctl = 0
      psf_ctls%block_name = hd_block
!
      end subroutine init_psf_ctls_labels
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
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
      subroutine append_section_control(idx_in, hd_block, psf_ctls)
!
      use ctl_data_section_IO
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(section_controls), intent(inout) :: psf_ctls
!
      type(section_controls) :: tmp_psf_c
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.psf_ctls%num_psf_ctl) return
!
      tmp_psf_c%num_psf_ctl = psf_ctls%num_psf_ctl
      call alloc_psf_ctl_stract(tmp_psf_c)
      do i = 1, tmp_psf_c%num_psf_ctl
        call dup_control_4_psf(psf_ctls%psf_ctl_struct(i),              &
                               tmp_psf_c%psf_ctl_struct(i))
        tmp_psf_c%fname_psf_ctl(i) = psf_ctls%fname_psf_ctl(i)
      end do
!
      call dealloc_psf_ctl_stract(psf_ctls)
      psf_ctls%num_psf_ctl = tmp_psf_c%num_psf_ctl + 1
      call alloc_psf_ctl_stract(psf_ctls)
!
      do i = 1, idx_in
        call dup_control_4_psf(tmp_psf_c%psf_ctl_struct(i),             &
                               psf_ctls%psf_ctl_struct(i))
        psf_ctls%fname_psf_ctl(i) = tmp_psf_c%fname_psf_ctl(i)
      end do
      call init_psf_ctl_stract(hd_block,                                &
     &                         psf_ctls%psf_ctl_struct(idx_in+1))
      psf_ctls%fname_psf_ctl(idx_in+1) = 'NO_FILE'
      do i = idx_in+1, tmp_psf_c%num_psf_ctl
        call dup_control_4_psf(tmp_psf_c%psf_ctl_struct(i),             &
     &                         psf_ctls%psf_ctl_struct(i+1))
        psf_ctls%fname_psf_ctl(i+1) = tmp_psf_c%fname_psf_ctl(i)
      end do
!
      call dealloc_psf_ctl_stract(tmp_psf_c)
!
      end subroutine append_section_control
!
! -----------------------------------------------------------------------
!
      subroutine delete_section_control(idx_in, psf_ctls)
!
      integer(kind = kint), intent(in) :: idx_in
      type(section_controls), intent(inout) :: psf_ctls
!
      type(section_controls) :: tmp_psf_c
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.psf_ctls%num_psf_ctl) return
!
      tmp_psf_c%num_psf_ctl = psf_ctls%num_psf_ctl
      call alloc_psf_ctl_stract(tmp_psf_c)
      do i = 1, tmp_psf_c%num_psf_ctl
        call dup_control_4_psf(psf_ctls%psf_ctl_struct(i),              &
                               tmp_psf_c%psf_ctl_struct(i))
        tmp_psf_c%fname_psf_ctl(i) = psf_ctls%fname_psf_ctl(i)
      end do
!
      call dealloc_psf_ctl_stract(psf_ctls)
      psf_ctls%num_psf_ctl = tmp_psf_c%num_psf_ctl - 1
      call alloc_psf_ctl_stract(psf_ctls)
!
      do i = 1, idx_in-1
        call dup_control_4_psf(tmp_psf_c%psf_ctl_struct(i),             &
                               psf_ctls%psf_ctl_struct(i))
        psf_ctls%fname_psf_ctl(i) = tmp_psf_c%fname_psf_ctl(i)
      end do
      do i = idx_in, psf_ctls%num_psf_ctl
        call dup_control_4_psf(tmp_psf_c%psf_ctl_struct(i+1),           &
     &                         psf_ctls%psf_ctl_struct(i))
        psf_ctls%fname_psf_ctl(i) = tmp_psf_c%fname_psf_ctl(i+1)
      end do
!
      call dealloc_psf_ctl_stract(tmp_psf_c)
!
      end subroutine delete_section_control
!
! -----------------------------------------------------------------------
!
      end module t_control_data_sections
