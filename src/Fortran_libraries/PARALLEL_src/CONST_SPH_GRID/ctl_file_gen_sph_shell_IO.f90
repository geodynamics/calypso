!>@file   ctl_file_gen_sph_shell_IO.f90
!!@brief  module ctl_file_gen_sph_shell_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine sel_read_ctl_gen_shell_grids                         &
!!     &         (id_control, hd_block, file_name, psph_ctl, c_buf)
!!      subroutine read_ctl_file_gen_shell_grids(id_control, file_name, &
!!     &          hd_block, psph_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine sel_write_ctl_gen_shell_grids                        &
!!     &         (id_control, hd_block, file_name, psph_ctl, level)
!!      subroutine write_ctl_file_gen_shell_grids(id_control, file_name,&
!!     &                                          hd_block, psph_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        character(len = kchara), intent(in) :: hd_block
!!        type(parallel_sph_shell_control), intent(in) :: psph_ctl
!!        integer(kind = kint), intent(inout) :: level
!! =======================================================
!!    example of control section
!!
!!    begin FEM_mesh_ctl
!!      ...
!!    end FEM_mesh_ctl
!!
!!    begin num_domain_ctl
!!      num_radial_domain_ctl         2
!!      num_horizontal_domain_ctl     2
!!      ...
!!    end num_domain_ctl
!!
!!    begin num_grid_sph
!!      sph_coef_type_ctl       no_pole
!!      sph_grid_type_ctl       no_pole
!!      truncation_level_ctl     4
!!      longitude_symmetry_ctl   2
!!      ngrid_meridonal_ctl     12
!!      ngrid_zonal_ctl         24
!!      ...
!!    end num_grid_sph
!!
!! =======================================================
!!@endverbatim
!
      module ctl_file_gen_sph_shell_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
!
      use t_read_control_elements
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
      use t_ctl_data_gen_sph_shell
!
      implicit none
!
      character(len=kchara), parameter, private                         &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
!
      private :: read_ctl_file_gen_shell_grids
      private :: write_ctl_file_gen_shell_grids
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine sel_read_ctl_gen_shell_grids                           &
     &         (id_control, hd_block, file_name, psph_ctl, c_buf)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      character(len = kchara), intent(inout) :: file_name
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(psph_ctl%iflag_sph_shell .gt. 0) return
      if(check_file_flag(c_buf, hd_block)) then
        file_name = third_word(c_buf)
!
        call write_one_ctl_file_message                                 &
     &     (hd_block, c_buf%level, file_name)
        call read_ctl_file_gen_shell_grids(id_control+2, file_name,     &
     &                                     hd_block, psph_ctl, c_buf)
      else if(check_begin_flag(c_buf, hd_block)) then
        file_name = 'NO_FILE'
!
        call write_included_message(hd_block, c_buf%level)
        call read_parallel_shell_ctl                                    &
     &     (id_control, hd_block, psph_ctl, c_buf)
      end if
!
      end subroutine sel_read_ctl_gen_shell_grids
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_file_gen_shell_grids(id_control, file_name,   &
     &          hd_block, psph_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(id_control, file = file_name)
!
      do
        if(psph_ctl%iflag_sph_shell .gt. 0) exit
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_parallel_shell_ctl(id_control, hd_block,              &
     &                               psph_ctl, c_buf)
        call read_parallel_shell_ctl(id_control, hd_sph_shell,          &
     &                               psph_ctl, c_buf)
        if(psph_ctl%iflag_sph_shell .gt. 0) exit
      end do
!
      close(id_control)
      c_buf%level = c_buf%level - 1
!
      end subroutine read_ctl_file_gen_shell_grids
!
! -----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine sel_write_ctl_gen_shell_grids                          &
     &         (id_control, hd_block, file_name, psph_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len = kchara), intent(in) :: hd_block
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(cmp_no_case(file_name, 'NO_FILE')) then
        call write_parallel_shell_ctl(id_control, hd_block,             &
     &                                psph_ctl, level)
      else if(id_control .eq. id_monitor) then
        write(*,'(4a)') '!  ', trim(hd_block),                          &
     &           ' should be written to file ... ', trim(file_name)
        call write_parallel_shell_ctl(id_control, hd_block,             &
     &                                psph_ctl, level)
      else
        write(*,'(3a)') trim(hd_block),                                 &
     &           ' is written to file ... ', trim(file_name)
        call write_file_name_for_ctl_line(id_control, level,            &
     &                                    hd_block, file_name)
        call write_ctl_file_gen_shell_grids((id_control+2), file_name,  &
     &                                     hd_block, psph_ctl)
      end if
!
      end subroutine sel_write_ctl_gen_shell_grids
!
!   --------------------------------------------------------------------
!
      subroutine write_ctl_file_gen_shell_grids(id_control, file_name,  &
     &                                          hd_block, psph_ctl)
!
      use delete_data_files
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: file_name
      character(len=kchara), intent(in) :: hd_block
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
!
      integer(kind = kint) :: level
!
!
      if(check_file_exist(file_name)) then
        write(*,*) 'File ', trim(file_name), ' exist. Continue?'
        read(*,*)
      end if
!
      level = 0
      open(id_control, file = file_name)
      call write_parallel_shell_ctl(id_control, hd_block,               &
     &                              psph_ctl, level)
      close(id_control)
!
      end subroutine write_ctl_file_gen_shell_grids
!
! -----------------------------------------------------------------------
!
      end module ctl_file_gen_sph_shell_IO
