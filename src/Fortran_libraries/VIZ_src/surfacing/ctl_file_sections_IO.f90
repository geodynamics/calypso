!>@file   ctl_file_sections_IO.f90
!!@brief  module ctl_file_sections_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine read_files_4_psf_ctl                                 &
!!     &         (id_control, hd_block, psf_ctls, c_buf)
!!      subroutine sel_read_control_4_psf_file(id_control, hd_block,    &
!!     &          file_name, psf_ctl_struct, c_buf)
!!      subroutine read_control_4_psf_file(id_control, file_name,       &
!!     &                                   hd_block, psf_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(section_controls), intent(inout) :: psf_ctls
!!        type(psf_ctl), intent(inout) :: psf_ctl_struct
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine write_files_4_psf_ctl                                &
!!     &         (id_control, hd_block, psf_ctls, level)
!!      subroutine sel_write_control_4_psf_file(id_control, hd_block,   &
!!     &          file_name, psf_ctl_struct, level)
!!      subroutine write_control_4_psf_file(id_control, file_name,      &
!!     &                                    hd_block, psf_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(section_controls), intent(in) :: psf_ctls
!!        type(psf_ctl), intent(in) :: psf_ctl_struct
!!        integer(kind = kint), intent(inout) :: level
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array cross_section_ctl
!!      file   cross_section_ctl   'ctl_psf_eq'
!!    end array cross_section_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_sections_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_psf
      use t_control_data_sections
!
      implicit  none
!
!
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      private :: hd_section_ctl, hd_psf_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_psf_ctl                                   &
     &         (id_control, hd_block, psf_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_data_section_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(section_controls), intent(inout) :: psf_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(psf_ctls%psf_ctl_struct)) return
      psf_ctls%num_psf_ctl = 0
      call alloc_psf_ctl_stract(psf_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)                             &
     &      .or. check_begin_flag(c_buf, hd_block)) then
          call append_new_section_control(psf_ctls)
!
          call write_multi_ctl_file_message                             &
     &       (hd_block, psf_ctls%num_psf_ctl, c_buf%level)
          call sel_read_control_4_psf_file(id_control, hd_block,        &
     &        psf_ctls%fname_psf_ctl(psf_ctls%num_psf_ctl),             &
     &        psf_ctls%psf_ctl_struct(psf_ctls%num_psf_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_control_4_psf_file(id_control, hd_block,      &
     &          file_name, psf_ctl_struct, c_buf)
!
      use t_read_control_elements
      use ctl_data_section_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(psf_ctl), intent(inout) :: psf_ctl_struct
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        write(*,'(a)', ADVANCE='NO') ' is read from file ... '
        call read_control_4_psf_file((id_control+2), file_name,         &
     &                               hd_block, psf_ctl_struct)
        if(psf_ctl_struct%i_psf_ctl .ne. 1)                             &
     &                         c_buf%iend = psf_ctl_struct%i_psf_ctl
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        write(*,*) ' is included'
        call s_read_psf_control_data(id_control, hd_block,              &
     &                               psf_ctl_struct, c_buf)
      end if
!
      end subroutine sel_read_control_4_psf_file
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_psf_file(id_control, file_name,         &
     &                                   hd_block, psf_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_psf
      use ctl_data_section_IO
!
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(psf_ctl), intent(inout) :: psf_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      write(*,'(a)') trim(file_name)
      open(id_control, file=file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call s_read_psf_control_data(id_control, hd_block,              &
     &      psf_ctl_struct, c_buf1)
        call s_read_psf_control_data(id_control, hd_section_ctl,        &
     &      psf_ctl_struct, c_buf1)
        call s_read_psf_control_data(id_control, hd_psf_ctl,            &
     &      psf_ctl_struct, c_buf1)
        if(psf_ctl_struct%i_psf_ctl .gt. 0) exit
      end do
      close(id_control)
      if(c_buf1%iend .gt. 0) psf_ctl_struct%i_psf_ctl = c_buf1%iend
!
      end subroutine read_control_4_psf_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_files_4_psf_ctl                                  &
     &         (id_control, hd_block, psf_ctls, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(section_controls), intent(in) :: psf_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, psf_ctls%num_psf_ctl
        write(*,'(3a,i4)', ADVANCE='NO') '!  ', trim(hd_block),         &
     &                                   ' No. ', i
        call sel_write_control_4_psf_file(id_control, hd_block,         &
     &      psf_ctls%fname_psf_ctl(i), psf_ctls%psf_ctl_struct(i),      &
     &      level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_files_4_psf_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_write_control_4_psf_file(id_control, hd_block,     &
     &          file_name, psf_ctl_struct, level)
!
      use t_read_control_elements
      use write_control_elements
      use ctl_data_section_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(psf_ctl), intent(in) :: psf_ctl_struct
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        write(*,'(a)') ' is included.'
        call write_psf_control_data(id_control, hd_block,               &
     &                              psf_ctl_struct, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(2a)') ' should be written to file ... ',              &
     &                trim(file_name)
        call write_psf_control_data(id_control, hd_block,               &
     &                              psf_ctl_struct, level)
      else
        write(*,'(2a)') ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_control_4_psf_file((id_control+2), file_name,        &
     &                                hd_block, psf_ctl_struct)
      end if
!
      end subroutine sel_write_control_4_psf_file
!
!   --------------------------------------------------------------------
!
      subroutine write_control_4_psf_file(id_control, file_name,        &
     &                                    hd_block, psf_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_psf
      use ctl_data_section_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(psf_ctl), intent(in) :: psf_ctl_struct
!
      integer(kind = kint) :: level
!
!
      level = 0
      open(id_control, file=file_name)
      call write_psf_control_data(id_control, hd_block,                 &
     &                            psf_ctl_struct, level)
      close(id_control)
!
      end subroutine write_control_4_psf_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_sections_IO
