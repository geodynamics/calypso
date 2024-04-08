!>@file   ctl_file_map_renderings_IO.f90
!!@brief  module ctl_file_map_renderings_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for cross sections
!!
!!@verbatim
!!      subroutine read_files_4_map_ctl                                 &
!!     &         (id_control, hd_block, map_ctls, c_buf)
!!      subroutine sel_read_control_4_map_file(id_control, hd_block,    &
!!     &          file_name, map_ctl_struct, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(map_rendering_controls), intent(inout) :: map_ctls
!!        type(map_ctl), intent(inout) :: map_ctl_struct
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine write_files_4_map_ctl                                &
!!     &         (id_control, hd_block, map_ctls, level)
!!      subroutine sel_write_control_4_map_file(id_control, hd_block,   &
!!     &          file_name, map_ctl_struct, level)
!!      subroutine write_control_4_map_file(id_control, file_name,      &
!!     &                                    hd_block, map_ctl_struct)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len=kchara), intent(in) :: hd_block
!!        type(map_rendering_controls), intent(in) :: map_ctls
!!        type(map_ctl), intent(in) :: map_ctl_struct
!!        integer(kind = kint), intent(inout) :: level
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array map_rendering_ctl
!!      file   map_rendering_ctl   'ctl_map_cmb'
!!    end array map_rendering_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_file_map_renderings_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_control_data_4_map
      use t_control_data_maps
!
      implicit  none
!
!
      character(len=kchara), parameter, private                         &
     &             :: hd_map_rendering = 'map_rendering_ctl'
!
      private :: read_control_4_map_file
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_4_map_ctl                                   &
     &         (id_control, hd_block, map_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_data_section_IO
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(map_rendering_controls), intent(inout) :: map_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(map_ctls%map_ctl_struct)) return
      map_ctls%num_map_ctl = 0
      call alloc_map_ctl_stract(map_ctls)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)                             &
     &      .or. check_begin_flag(c_buf, hd_block)) then
          n_append = map_ctls%num_map_ctl
          call append_map_render_control(n_append, hd_block, map_ctls)
!
          call write_multi_ctl_file_message                             &
     &       (hd_block, map_ctls%num_map_ctl, c_buf%level)
          call sel_read_control_4_map_file(id_control, hd_block,        &
     &        map_ctls%fname_map_ctl(map_ctls%num_map_ctl),             &
     &        map_ctls%map_ctl_struct(map_ctls%num_map_ctl), c_buf)
        end if
      end do
!
      end subroutine read_files_4_map_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_control_4_map_file(id_control, hd_block,      &
     &          file_name, map_ctl_struct, c_buf)
!
      use t_read_control_elements
      use ctl_data_map_rendering_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      character(len = kchara), intent(inout) :: file_name
      type(map_ctl), intent(inout) :: map_ctl_struct
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        write(*,'(2a)') ' is read file from ... ',  trim(file_name)
        call read_control_4_map_file((id_control+2), file_name,         &
     &                               hd_block, map_ctl_struct, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        write(*,'(a)') ' is included.'
        call s_read_map_control_data(id_control, hd_block,              &
     &                               map_ctl_struct, c_buf)
      end if
!
      end subroutine sel_read_control_4_map_file
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_map_file(id_control, file_name,         &
     &          hd_block, map_ctl_struct, c_buf)
!
      use t_read_control_elements
      use t_control_data_4_map
      use ctl_data_map_rendering_IO
!
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(map_ctl), intent(inout) :: map_ctl_struct
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(id_control, file=file_name, status='old')
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call s_read_map_control_data(id_control, hd_block,              &
     &      map_ctl_struct, c_buf)
        call s_read_map_control_data(id_control, hd_map_rendering,      &
     &      map_ctl_struct, c_buf)
        if(map_ctl_struct%i_map_ctl .gt. 0) exit
      end do
      close(id_control)
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_4_map_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_files_4_map_ctl                                  &
     &         (id_control, hd_block, map_ctls, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(map_rendering_controls), intent(in) :: map_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(map_ctls%num_map_ctl .le. 0) return
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, map_ctls%num_map_ctl
        write(*,'(3a,i4)', ADVANCE='NO') '!  ', trim(hd_block),         &
     &                                     ' No. ', i
        call sel_write_control_4_map_file(id_control, hd_block,         &
     &      map_ctls%fname_map_ctl(i), map_ctls%map_ctl_struct(i),      &
     &      level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_files_4_map_ctl
!
!   --------------------------------------------------------------------
!
      subroutine sel_write_control_4_map_file(id_control, hd_block,     &
     &          file_name, map_ctl_struct, level)
!
      use t_read_control_elements
      use write_control_elements
      use ctl_data_map_rendering_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(map_ctl), intent(in) :: map_ctl_struct
      integer(kind = kint), intent(inout) :: level
!
!
      if(no_file_flag(file_name)) then
        call write_map_control_data(id_control, hd_block,               &
     &                              map_ctl_struct, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(2a)') ' should be written to ... ', trim(file_name)
        call write_map_control_data(id_control, hd_block,               &
     &                              map_ctl_struct, level)
      else
        write(*,'(2a)') ' is written to ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_control_4_map_file((id_control+2), file_name,        &
     &                                hd_block, map_ctl_struct)
      end if
!
      end subroutine sel_write_control_4_map_file
!
!   --------------------------------------------------------------------
!
      subroutine write_control_4_map_file(id_control, file_name,        &
     &                                    hd_block, map_ctl_struct)
!
      use t_read_control_elements
      use t_control_data_4_map
      use ctl_data_map_rendering_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(map_ctl), intent(in) :: map_ctl_struct
!
      integer(kind = kint) :: level
!
!
      level = 0
      open(id_control, file=file_name)
      call write_map_control_data(id_control, hd_block,                 &
     &                            map_ctl_struct, level)
      close(id_control)
!
      end subroutine write_control_4_map_file
!
!  ---------------------------------------------------------------------
!
      end module ctl_file_map_renderings_IO
