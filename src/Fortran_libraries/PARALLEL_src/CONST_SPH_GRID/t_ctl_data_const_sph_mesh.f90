!>@file   t_ctl_data_const_sph_mesh.f90
!!@brief  module t_ctl_data_const_sph_mesh
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_control_4_const_shell(file_name,                &
!!     &                                      gen_SPH_ctl, c_buf)
!!        character(len=kchara), intent(in) :: file_name
!!        type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!!      subroutine write_control_4_const_shell(file_name, gen_SPH_ctl)
!!        character(len=kchara), intent(in) :: file_name
!!        type(sph_mesh_generation_ctl), intent(in) :: gen_SPH_ctl
!!@endverbatim
!
      module t_ctl_data_const_sph_mesh
!
      use m_precision
!
      use m_machine_parameter
      use skip_comment_f
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_gen_sph_shell
!
      implicit none
!
!
      integer(kind=kint), parameter, private :: control_file_code = 11
!
      type sph_mesh_generation_ctl
!>        Block name
        character(len=kchara) :: hd_mesh_generation = 'MHD_control'
!
!>        Structure for file settings
        type(platform_data_control) :: plt
!
!>        file name for parallel spherical shell control
        character(len = kchara) :: fname_psph = 'NO_FILE'
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
        integer(kind=kint) :: i_sph_mesh_ctl = 0
      end type sph_mesh_generation_ctl
!
!   2nd level for MHD
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
!
      private :: read_sph_shell_define_ctl
      private :: write_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_const_shell(file_name,                  &
     &                                      gen_SPH_ctl, c_buf)
!
      character(len=kchara), intent(in) :: file_name
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(control_file_code, file = file_name, status='old')
!
      do
        call load_one_line_from_control                                 &
     &     (control_file_code, gen_SPH_ctl%hd_mesh_generation, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_sph_shell_define_ctl                                  &
     &     (control_file_code, gen_SPH_ctl%hd_mesh_generation,          &
     &      gen_SPH_ctl, c_buf)
        if(gen_SPH_ctl%i_sph_mesh_ctl .gt. 0) exit
      end do
!
      close(control_file_code)
!
      c_buf%level = c_buf%level - 1
!
      end subroutine read_control_4_const_shell
!
! ----------------------------------------------------------------------
!
      subroutine write_control_4_const_shell(file_name, gen_SPH_ctl)
!
      use delete_data_files
!
      character(len=kchara), intent(in) :: file_name
      type(sph_mesh_generation_ctl), intent(in) :: gen_SPH_ctl
!
      integer(kind = kint) :: level1
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      write(*,*) 'Write control file: ', trim(file_name)
      open(control_file_code, file = file_name, status='old' )
      call write_sph_shell_define_ctl                                   &
     &   (control_file_code, gen_SPH_ctl%hd_mesh_generation,            &
     &    gen_SPH_ctl, level1)
      close(control_file_code)
!
      end subroutine write_control_4_const_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_shell_define_ctl                              &
     &         (id_control, hd_block, gen_SPH_ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_file_gen_sph_shell_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(gen_SPH_ctl%i_sph_mesh_ctl .gt. 0) return
      call init_platforms_labels(hd_platform, gen_SPH_ctl%plt)
      call init_parallel_shell_ctl_label(hd_sph_shell,                  &
     &                                   gen_SPH_ctl%psph_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, gen_SPH_ctl%plt, c_buf)
        call sel_read_ctl_gen_shell_grids(id_control, hd_sph_shell,     &
     &      gen_SPH_ctl%fname_psph, gen_SPH_ctl%psph_ctl, c_buf)
      end do
      gen_SPH_ctl%i_sph_mesh_ctl = 1
!
      end subroutine read_sph_shell_define_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_shell_define_ctl                             &
     &         (id_control, hd_block, gen_SPH_ctl, level)
!
      use ctl_data_platforms_IO
      use ctl_file_gen_sph_shell_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_mesh_generation_ctl), intent(in) :: gen_SPH_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(gen_SPH_ctl%i_sph_mesh_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_platforms                                      &
     &   (id_control, hd_platform, gen_SPH_ctl%plt, level)
      call sel_write_ctl_gen_shell_grids(id_control,                    &
     &    gen_SPH_ctl%fname_psph, gen_SPH_ctl%psph_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_shell_define_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_const_sph_mesh
