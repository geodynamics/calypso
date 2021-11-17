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
!!        type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine read_pvr_section_ctl                                 &
!!     &         (id_control, hd_block, pvr_sect_ctl, c_buf)
!!        type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!!
!!      subroutine bcast_pvr_sections_ctl(pvr_scts_c)
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
!!
!!      integer(kind = kint) function num_label_pvr_section()
!!      subroutine set_label_pvr_section(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array section_ctl
!!    file section_ctl     ctl_psf_eq
!!    begin section_ctl
!!      ...
!!    end section_ctl
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
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf_def
      use t_control_array_real
      use t_control_array_character
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_section_ctl
        character(len = kchara) :: fname_sect_ctl
        type(psf_define_ctl) :: psf_def_c
        type(read_real_item) :: opacity_ctl
      end type pvr_section_ctl
!
!
      type pvr_sections_ctl
        integer(kind = kint) :: num_pvr_sect_ctl = 0
        type(pvr_section_ctl), allocatable :: pvr_sect_ctl(:)
      end type pvr_sections_ctl
!
!     4th level for area group
!
      integer(kind = kint), parameter :: n_label_pvr_section =   2
!
      character(len=kchara), parameter                                  &
     &                  :: hd_surface_define =  'surface_define'
      character(len=kchara), parameter                                  &
     &                  :: hd_pvr_opacity =   'opacity_ctl'
!
      private :: hd_pvr_opacity, hd_surface_define, n_label_pvr_section
!
      private :: read_control_pvr_section_def
      private :: read_pvr_section_ctl, dup_pvr_section_ctl
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
!
     if(allocated(pvr_scts_c%pvr_sect_ctl)) then
        call dealloc_pvr_sects_ctl                                      &
     &     (pvr_scts_c%num_pvr_sect_ctl, pvr_scts_c%pvr_sect_ctl)
        deallocate(pvr_scts_c%pvr_sect_ctl)
      end if
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
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          call append_new_pvr_section_ctl(pvr_scts_c)
!
          write(*,'(2a,i4)', ADVANCE='NO') trim(hd_block),              &
     &                           ' No. ', pvr_scts_c%num_pvr_sect_ctl
          call read_pvr_section_ctl(id_control, hd_block,               &
     &        pvr_scts_c%pvr_sect_ctl(pvr_scts_c%num_pvr_sect_ctl),     &
     &        c_buf)
        end if
      end do
!
      end subroutine read_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_section_def                           &
     &         (id_control, fname_sect_ctl, psf_def_c)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: fname_sect_ctl
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) trim(fname_sect_ctl), ' for surface definition'
      open(id_control, file = fname_sect_ctl, status='old')
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        if(check_begin_flag(c_buf1, hd_surface_define)) then
          call read_section_def_control(id_control, hd_surface_define,  &
     &                                  psf_def_c, c_buf1)
          exit
        end if
      end do
!
      close(id_control)
!
      end subroutine read_control_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_section_ctl                                   &
     &         (id_control, hd_block, pvr_sect_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      pvr_sect_ctl%psf_def_c%i_surface_define = 0
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_surface_define)) then
          write(*,'(a)', ADVANCE='NO') ' is read from file... '
          pvr_sect_ctl%fname_sect_ctl = third_word(c_buf)
          call read_control_pvr_section_def(id_control+2,               &
     &        pvr_sect_ctl%fname_sect_ctl, pvr_sect_ctl%psf_def_c)
        end if
        if(check_begin_flag(c_buf, hd_surface_define)) then
          write(*,*) ' is included'
          pvr_sect_ctl%fname_sect_ctl = 'NO_FILE'
          call read_section_def_control(id_control, hd_surface_define,  &
     &                                  pvr_sect_ctl%psf_def_c, c_buf)
        end if
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
      end do
      pvr_sect_ctl%psf_def_c%i_surface_define = 1
!
      end subroutine read_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_sections_ctl(pvr_scts_c)
!
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
!
      type(pvr_sections_ctl), intent(inout) :: pvr_scts_c
!
      integer(kind = kint) :: i
!
!
      call calypso_mpi_bcast_one_int(pvr_scts_c%num_pvr_sect_ctl, 0)
      if(pvr_scts_c%num_pvr_sect_ctl .gt. 0 .and. my_rank .gt. 0) then
        allocate(pvr_scts_c%pvr_sect_ctl(pvr_scts_c%num_pvr_sect_ctl))
      end if
!
      do i = 1, pvr_scts_c%num_pvr_sect_ctl
        call calypso_mpi_bcast_character                                &
     &     (pvr_scts_c%pvr_sect_ctl(i)%fname_sect_ctl,                  &
     &      cast_long(kchara), 0)
!
        call bcast_section_def_control                                  &
     &     (pvr_scts_c%pvr_sect_ctl(i)%psf_def_c)
        call bcast_ctl_type_r1(pvr_scts_c%pvr_sect_ctl(i)%opacity_ctl)
      end do
!
      end subroutine bcast_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
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
     &    pvr_scts_c%pvr_sect_ctl, tmp_pvr_scts%pvr_sect_ctl)
!
      call dealloc_pvr_sections_ctl(pvr_scts_c)
!
      pvr_scts_c%num_pvr_sect_ctl = tmp_pvr_scts%num_pvr_sect_ctl + 1
      call alloc_pvr_sections_ctl(pvr_scts_c)
!
      call copy_pvr_sections_ctl(tmp_pvr_scts%num_pvr_sect_ctl,         &
     &    tmp_pvr_scts%pvr_sect_ctl, pvr_scts_c%pvr_sect_ctl(1))
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
     &    org_pvr_scts_c%pvr_sect_ctl, new_pvr_scts_c%pvr_sect_ctl)
!
      end subroutine dup_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_pvr_sections_ctl                                  &
     &         (num_pvr_sect, org_pvr_sect_c, new_pvr_sect_c)
!
      integer(kind = kint), intent(in) :: num_pvr_sect
      type(pvr_section_ctl), intent(in) :: org_pvr_sect_c(num_pvr_sect)
      type(pvr_section_ctl), intent(inout)                              &
     &                       :: new_pvr_sect_c(num_pvr_sect)
!
      integer(kind = kint) :: i
!
      do i = 1, num_pvr_sect
        call dup_pvr_section_ctl(org_pvr_sect_c(i), new_pvr_sect_c(i))
      end do
!
      end subroutine copy_pvr_sections_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_sects_ctl(num_pvr_sect, pvr_sect_ctl)
!
      integer(kind = kint), intent(in) :: num_pvr_sect
      type(pvr_section_ctl), intent(inout)                              &
     &                       :: pvr_sect_ctl(num_pvr_sect)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_pvr_sect
        call dealloc_pvr_section_ctl(pvr_sect_ctl(i))
      end do
!
      end subroutine dealloc_pvr_sects_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_section_ctl(org_pvr_sect_c, new_pvr_sect_c)
!
      type(pvr_section_ctl), intent(in) :: org_pvr_sect_c
      type(pvr_section_ctl), intent(inout) :: new_pvr_sect_c
!
!
      call dup_control_4_psf_def                                        &
     &   (org_pvr_sect_c%psf_def_c, new_pvr_sect_c%psf_def_c)
      new_pvr_sect_c%fname_sect_ctl = org_pvr_sect_c%fname_sect_ctl
!
      call copy_real_ctl(org_pvr_sect_c%opacity_ctl,                    &
     &                   new_pvr_sect_c%opacity_ctl)
!
      end subroutine dup_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_section_ctl(pvr_sect_ctl)
!
      type(pvr_section_ctl), intent(inout) :: pvr_sect_ctl
!
!
      call dealloc_cont_dat_4_psf_def(pvr_sect_ctl%psf_def_c)
      pvr_sect_ctl%opacity_ctl%iflag =    0
!
      end subroutine dealloc_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_section()
      num_label_pvr_section = n_label_pvr_section
      return
      end function num_label_pvr_section
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_section(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_section)
!
!
      call set_control_labels(hd_surface_define, names( 1))
      call set_control_labels(hd_pvr_opacity,    names( 2))
!
      end subroutine set_label_pvr_section
!
! ----------------------------------------------------------------------
!
      end module t_control_data_pvr_sections
