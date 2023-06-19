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
!!      subroutine append_new_pvr_section_ctl(pvr_scts_c)
!!        type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!!      subroutine dup_pvr_sections_ctl(org_pvr_scts_c, new_pvr_scts_c)
!!      subroutine copy_pvr_sections_ctl                                &
!!     &         (num_pvr_sect, org_pvr_sect_c, new_pvr_sect_c)
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
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_sections_ctl                                  &
     &         (id_control, hd_block, pvr_scts_c, c_buf)
!
      use ctl_data_pvr_section_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(pvr_scts_c%pvr_sect_ctl)) return
      pvr_scts_c%num_pvr_sect_ctl = 0
      call alloc_pvr_sections_ctl(pvr_scts_c)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_pvr_section_ctl(pvr_scts_c)
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
      use ctl_data_pvr_section_IO
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
      subroutine copy_pvr_sections_ctl                                  &
     &         (num_pvr_sect, org_pvr_scts_c, new_pvr_scts_c)
!
      integer(kind = kint), intent(in) :: num_pvr_sect
      type(pvr_sections_ctl), intent(in) :: org_pvr_scts_c
      type(pvr_sections_ctl), intent(inout) :: new_pvr_scts_c
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_sect
        call dup_pvr_section_ctl(org_pvr_scts_c%pvr_sect_ctl(i),        &
     &                           new_pvr_scts_c%pvr_sect_ctl(i))
      end do
!
      end subroutine copy_pvr_sections_ctl
!
! -----------------------------------------------------------------------
!
      subroutine append_new_pvr_section_ctl(pvr_scts_c)
!
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
      type(pvr_sections_ctl) :: tmp_pvr_scts
!
!
      tmp_pvr_scts%num_pvr_sect_ctl = pvr_scts_c%num_pvr_sect_ctl
      call alloc_pvr_sections_ctl(tmp_pvr_scts)
      call copy_pvr_sections_ctl(tmp_pvr_scts%num_pvr_sect_ctl,         &
     &                           pvr_scts_c, tmp_pvr_scts)
!
      call dealloc_pvr_sections_ctl(pvr_scts_c)
!
      pvr_scts_c%num_pvr_sect_ctl = tmp_pvr_scts%num_pvr_sect_ctl + 1
      call alloc_pvr_sections_ctl(pvr_scts_c)
!
      call copy_pvr_sections_ctl(tmp_pvr_scts%num_pvr_sect_ctl,         &
     &                           tmp_pvr_scts, pvr_scts_c)
!
      call dealloc_pvr_sections_ctl(tmp_pvr_scts)
!
      end subroutine append_new_pvr_section_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_sections_ctl(org_pvr_scts_c, new_pvr_scts_c)
!
      type(pvr_sections_ctl), intent(in) :: org_pvr_scts_c
      type(pvr_sections_ctl), intent(inout) :: new_pvr_scts_c
!
!
      new_pvr_scts_c%num_pvr_sect_ctl = org_pvr_scts_c%num_pvr_sect_ctl
      call alloc_pvr_sections_ctl(new_pvr_scts_c)
      call copy_pvr_sections_ctl(org_pvr_scts_c%num_pvr_sect_ctl,       &
     &                           org_pvr_scts_c, new_pvr_scts_c)
!
      end subroutine dup_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_sections
