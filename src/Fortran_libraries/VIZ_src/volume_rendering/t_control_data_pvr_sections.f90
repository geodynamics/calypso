!>@file   t_control_data_pvr_sections.f90
!!@brief  module t_control_data_pvr_sections
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine alloc_pvr_sections_ctl(pvr_scts_c)
!!      subroutine dealloc_pvr_sections_ctl(pvr_scts_c)
!!      subroutine init_pvr_sections_ctl(hd_block, pvr_scts_c)
!!        type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!!
!!      subroutine read_pvr_sections_ctl                                &
!!     &         (id_control, hd_block, pvr_scts_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_sections_ctl                               &
!!     &         (id_control, hd_block, pvr_scts_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_sections_ctl), intent(in) :: pvr_scts_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine append_pvr_section_ctl(idx_in, hd_block,             &
!!     &                                  pvr_scts_c)
!!      subroutine delete_pvr_section_ctl(idx_in, pvr_scts_c)
!!        type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!!      subroutine dup_pvr_sections_ctl(org_pvr_scts_c, new_pvr_scts_c)
!!        type(pvr_section_ctl), intent(in)                             &
!!     &                       :: org_pvr_sect_c(num_pvr_sect)
!!        type(pvr_section_ctl), intent(inout)                          &
!!     &                       :: new_pvr_sect_c(num_pvr_sect)
!!
!!      subroutine dealloc_pvr_section_ctl(pvr_sect_ctl)
!!        type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array section_ctl
!!    file surface_define     ctl_psf_eq
!!    begin surface_define
!!      ...
!!    end surface_define
!!
!!    opacity_ctl       0.9
!!  end array section_ctl
!!!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_sections
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf_def
      use t_control_array_real
      use t_control_array_character
      use t_control_array_chara2real
      use t_ctl_data_pvr_section
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_sections_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'section_ctl'
!
        integer(kind = kint) :: num_pvr_sect_ctl = 0
        type(pvr_section_ctl), allocatable :: pvr_sect_ctl(:)
      end type pvr_sections_ctl
!
      private :: dup_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_sections_ctl(pvr_scts_c)
!
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
      integer(kind = kint) :: i
!
     if(allocated(pvr_scts_c%pvr_sect_ctl) .eqv. .FALSE.) return
!
      do i = 1, pvr_scts_c%num_pvr_sect_ctl
        call dealloc_pvr_section_ctl(pvr_scts_c%pvr_sect_ctl(i))
      end do
      deallocate(pvr_scts_c%pvr_sect_ctl)
!
      pvr_scts_c%num_pvr_sect_ctl = 0
!
      end subroutine dealloc_pvr_sections_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_sections_ctl(pvr_scts_c)
!
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
!
      allocate(pvr_scts_c%pvr_sect_ctl(pvr_scts_c%num_pvr_sect_ctl))
!
      end subroutine alloc_pvr_sections_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_pvr_sections_ctl(hd_block, pvr_scts_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
!
      pvr_scts_c%block_name = hd_block
      pvr_scts_c%num_pvr_sect_ctl = 0
!
      end subroutine init_pvr_sections_ctl
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_sections_ctl                                  &
     &         (id_control, hd_block, pvr_scts_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(pvr_scts_c%pvr_sect_ctl)) return
      call alloc_pvr_sections_ctl(pvr_scts_c)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          n_append = pvr_scts_c%num_pvr_sect_ctl
          call append_pvr_section_ctl(n_append, hd_block, pvr_scts_c)
!
          call read_pvr_section_ctl                                     &
     &       (id_control, hd_block, pvr_scts_c%num_pvr_sect_ctl,        &
     &        pvr_scts_c%pvr_sect_ctl(pvr_scts_c%num_pvr_sect_ctl),     &
     &        c_buf)
        end if
      end do
!
      end subroutine read_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_sections_ctl                                 &
     &         (id_control, hd_block, pvr_scts_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_sections_ctl), intent(in) :: pvr_scts_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(pvr_scts_c%num_pvr_sect_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, pvr_scts_c%num_pvr_sect_ctl
        write(*,'(3a,i4)') '!  ', trim(hd_block), ' No. ', i
        call write_pvr_section_ctl(id_control, hd_block,                &
     &      pvr_scts_c%pvr_sect_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     hd_block)
!
      end subroutine write_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_pvr_section_ctl(idx_in, hd_block, pvr_scts_c)
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
      type(pvr_sections_ctl) :: tmp_pvr_scts
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.pvr_scts_c%num_pvr_sect_ctl) return
!
      tmp_pvr_scts%num_pvr_sect_ctl = pvr_scts_c%num_pvr_sect_ctl
      call alloc_pvr_sections_ctl(tmp_pvr_scts)
      do i = 1, tmp_pvr_scts%num_pvr_sect_ctl
        call dup_pvr_section_ctl(pvr_scts_c%pvr_sect_ctl(i),            &
     &                           tmp_pvr_scts%pvr_sect_ctl(i))
      end do
!
      call dealloc_pvr_sections_ctl(pvr_scts_c)
      pvr_scts_c%num_pvr_sect_ctl = tmp_pvr_scts%num_pvr_sect_ctl + 1
      call alloc_pvr_sections_ctl(pvr_scts_c)
!
      do i = 1, idx_in
        call dup_pvr_section_ctl(tmp_pvr_scts%pvr_sect_ctl(i),          &
     &                           pvr_scts_c%pvr_sect_ctl(i))
      end do
      call init_pvr_section_ctl_label(hd_block,                         &
     &                                pvr_scts_c%pvr_sect_ctl(idx_in+1))
      do i = idx_in+1, tmp_pvr_scts%num_pvr_sect_ctl
        call dup_pvr_section_ctl(tmp_pvr_scts%pvr_sect_ctl(i),          &
     &                           pvr_scts_c%pvr_sect_ctl(i+1))
      end do
!
      call dealloc_pvr_sections_ctl(tmp_pvr_scts)
!
      end subroutine append_pvr_section_ctl
!
! -----------------------------------------------------------------------
!
      subroutine delete_pvr_section_ctl(idx_in, pvr_scts_c)
!
      integer(kind = kint), intent(in) :: idx_in
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
      type(pvr_sections_ctl) :: tmp_pvr_scts
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.pvr_scts_c%num_pvr_sect_ctl) return
!
      tmp_pvr_scts%num_pvr_sect_ctl = pvr_scts_c%num_pvr_sect_ctl
      call alloc_pvr_sections_ctl(tmp_pvr_scts)
      do i = 1, tmp_pvr_scts%num_pvr_sect_ctl
        call dup_pvr_section_ctl(pvr_scts_c%pvr_sect_ctl(i),            &
     &                           tmp_pvr_scts%pvr_sect_ctl(i))
      end do
!
      call dealloc_pvr_sections_ctl(pvr_scts_c)
      pvr_scts_c%num_pvr_sect_ctl = tmp_pvr_scts%num_pvr_sect_ctl - 1
      call alloc_pvr_sections_ctl(pvr_scts_c)
!
      do i = 1, idx_in-1
        call dup_pvr_section_ctl(tmp_pvr_scts%pvr_sect_ctl(i),          &
     &                           pvr_scts_c%pvr_sect_ctl(i))
      end do
      do i = idx_in, pvr_scts_c%num_pvr_sect_ctl
        call dup_pvr_section_ctl(tmp_pvr_scts%pvr_sect_ctl(i+1),        &
     &                           pvr_scts_c%pvr_sect_ctl(i))
      end do
!
      call dealloc_pvr_sections_ctl(tmp_pvr_scts)
!
      end subroutine delete_pvr_section_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_sections_ctl(org_pvr_scts_c, new_pvr_scts_c)
!
      type(pvr_sections_ctl), intent(in) :: org_pvr_scts_c
      type(pvr_sections_ctl), intent(inout) :: new_pvr_scts_c
!
      integer(kind = kint) :: i
!
      new_pvr_scts_c%block_name =       org_pvr_scts_c%block_name
      new_pvr_scts_c%num_pvr_sect_ctl = org_pvr_scts_c%num_pvr_sect_ctl
      call alloc_pvr_sections_ctl(new_pvr_scts_c)
!
      do i = 1, org_pvr_scts_c%num_pvr_sect_ctl
        call dup_pvr_section_ctl(org_pvr_scts_c%pvr_sect_ctl(i),        &
     &                           new_pvr_scts_c%pvr_sect_ctl(i))
      end do
!
      end subroutine dup_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_sections
