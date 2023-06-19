!>@file   ctl_file_isosurfaces_IO.f90
!!@brief  module ctl_file_isosurfaces_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for isosurfaces
!!
!!@verbatim
!!      subroutine read_files_4_iso_ctl                                 &
!!     &         (id_control, hd_block, iso_ctls, c_buf)
!!      subroutine sel_read_control_4_iso_file(id_control, hd_block,    &
!!     &          file_name, iso_ctl_struct, c_buf)
!!      subroutine read_control_4_iso_file                              &
!!     &         (id_control, file_name, hd_block, iso_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(isosurf_controls), intent(inout) :: iso_ctls
!!        type(iso_ctl), intent(inout) :: iso_ctl_struct
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine write_files_4_iso_ctl                                &
!!     &         (id_control, hd_block, iso_ctls, level)
!!      subroutine sel_write_control_4_iso_file(id_control, hd_block,   &
!!     &          file_name, iso_ctl_struct, level)
!!      subroutine write_control_4_iso_file                             &
!!     &         (id_control, file_name, hd_block, iso_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(isosurf_controls), intent(in) :: iso_ctls
!!        type(iso_ctl), intent(in) :: iso_ctl_struct
!!        integer(kind = kint), intent(inout) :: level
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array isosurface_ctl     2
!!      file   isosurface_ctl   'ctl_iso_p_n1e4'
!!      file   isosurface_ctl   'ctl_iso_p_p1e4'
!!    end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_isosurfaces_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_iso
      use t_control_data_isosurfaces
!
      implicit  none
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
      private :: hd_isosurf_ctl, hd_iso_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_iso_ctl                                   &
     &         (id_control, hd_block, iso_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_data_isosurface_IO
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(isosurf_controls), intent(inout) :: iso_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      iso_ctls%num_iso_ctl = 0
      call alloc_iso_ctl_stract(iso_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)                             &
     &      .or. check_begin_flag(c_buf, hd_block)) then
          call append_new_isosurface_control(iso_ctls)
!
          call write_multi_ctl_file_message                             &
     &       (hd_block, iso_ctls%num_iso_ctl, c_buf%level)
          call sel_read_control_4_iso_file(id_control, hd_block,        &
     &        iso_ctls%fname_iso_ctl(iso_ctls%num_iso_ctl),             &
     &        iso_ctls%iso_ctl_struct(iso_ctls%num_iso_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_control_4_iso_file(id_control, hd_block,      &
     &          file_name, iso_ctl_struct, c_buf)
!
      use t_read_control_elements
      use ctl_data_isosurface_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(iso_ctl), intent(inout) :: iso_ctl_struct
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        write(*,'(a)', ADVANCE='NO') ' is read from file... '
        call read_control_4_iso_file((id_control+2), file_name,         &
     &                               hd_block, iso_ctl_struct)
        if(iso_ctl_struct%i_iso_ctl .ne. 1)                             &
     &                         c_buf%iend = iso_ctl_struct%i_iso_ctl
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        write(*,'(a)') ' is included'
        call s_read_iso_control_data(id_control, hd_block,              &
     &                               iso_ctl_struct, c_buf)
      end if
!
      end subroutine sel_read_control_4_iso_file
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_iso_file                                &
     &         (id_control, file_name, hd_block, iso_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_iso
      use ctl_data_isosurface_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(inout) :: iso_ctl_struct
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      write(*,*) 'Isosurface control file: ', trim(file_name)
      open(id_control, file=file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call s_read_iso_control_data                                    &
     &     (id_control, hd_block, iso_ctl_struct, c_buf1)
        call s_read_iso_control_data                                    &
     &     (id_control, hd_isosurf_ctl, iso_ctl_struct, c_buf1)
        call s_read_iso_control_data                                    &
     &     (id_control, hd_iso_ctl, iso_ctl_struct, c_buf1)
        if(iso_ctl_struct%i_iso_ctl .gt. 0) exit
      end do
      close(id_control)
      if(c_buf1%iend .gt. 0) iso_ctl_struct%i_iso_ctl = c_buf1%iend
!
      end subroutine read_control_4_iso_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_files_4_iso_ctl                                  &
     &         (id_control, hd_block, iso_ctls, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(isosurf_controls), intent(in) :: iso_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, iso_ctls%num_iso_ctl
        write(*,'(2a,i4)', ADVANCE='NO') trim(hd_block), ' No. ', i
        call sel_write_control_4_iso_file                               &
     &     (id_control, hd_block, iso_ctls%fname_iso_ctl(i),            &
     &      iso_ctls%iso_ctl_struct(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_files_4_iso_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_write_control_4_iso_file(id_control, hd_block,     &
     &          file_name, iso_ctl_struct, level)
!
      use t_read_control_elements
      use write_control_elements
      use ctl_data_isosurface_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(in) :: iso_ctl_struct
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        write(*,'(a)') ' is included.'
        call write_iso_control_data(id_control, hd_block,               &
     &                              iso_ctl_struct, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(2a)', ADVANCE='NO')                                   &
     &            ' should be written to file ... ', trim(file_name)
        call write_iso_control_data(id_control, hd_block,               &
     &                              iso_ctl_struct, level)
      else
        write(*,'(3a)', ADVANCE='NO')  trim(hd_block),                  &
     &            ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_control_4_iso_file((id_control+2), file_name,        &
     &                                hd_block, iso_ctl_struct)
      end if
!
      end subroutine sel_write_control_4_iso_file
!
!   --------------------------------------------------------------------
!
      subroutine write_control_4_iso_file                               &
     &         (id_control, file_name, hd_block, iso_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_iso
      use ctl_data_isosurface_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(in) :: iso_ctl_struct
!
      integer(kind = kint) :: level
!
!
      level = 0
      open(id_control, file=file_name)
      call write_iso_control_data                                       &
     &   (id_control, hd_block, iso_ctl_struct, level)
      close(id_control)
!
      end subroutine write_control_4_iso_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_isosurfaces_IO
