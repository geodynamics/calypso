!>@file   t_ctl_data_gen_sph_shell.f90
!!@brief  module t_ctl_data_gen_sph_shell
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine init_parallel_shell_ctl_label(hd_block, psph_ctl)
!!      subroutine read_parallel_shell_ctl                              &
!!     &         (id_control, hd_block, psph_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        character(len = kchara), intent(inout) :: file_name
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine write_parallel_shell_ctl(id_control, psph_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: file_name
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_parallel_shell_ctl(psph_ctl)
!!        type(platform_data_control), intent(inout) :: plt
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!
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
      module t_ctl_data_gen_sph_shell
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
!
      implicit none
!
!>      structure of parallel spherical shell data
      type parallel_sph_shell_control
!>        Block name
        character(len=kchara) :: block_name = 'spherical_shell_ctl'
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!>        Structure of spherical shell configuration
        type(sphere_data_control) :: spctl
!>        Structure of spherical shell domain decomposition
        type(sphere_domain_control) :: sdctl
!
!>        Integer flag to defined spherical shell
        integer (kind=kint) :: iflag_sph_shell = 0
      end type parallel_sph_shell_control
!
!   Labels
!
      character(len=kchara), parameter, private                         &
     &                     :: hd_FEM_mesh = 'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                     :: hd_sph_def = 'shell_define_ctl'
      character(len=kchara), parameter, private                         &
     &                     :: hd_domains_sph = 'num_domain_ctl'
!
!  Deprecated
      character(len=kchara), parameter, private                         &
     &                      :: hd_shell_def = 'num_grid_sph'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_parallel_shell_ctl                                &
     &         (id_control, hd_block, psph_ctl, c_buf)
!
      use ctl_data_sphere_model_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(psph_ctl%iflag_sph_shell .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, psph_ctl%Fmesh_ctl, c_buf)
        call read_control_shell_define                                  &
     &     (id_control, hd_sph_def, psph_ctl%spctl, c_buf)
        call read_control_shell_define                                  &
     &     (id_control, hd_shell_def, psph_ctl%spctl, c_buf)
!
        call read_control_shell_domain                                  &
     &     (id_control, hd_domains_sph, psph_ctl%sdctl, c_buf)
      end do
      psph_ctl%iflag_sph_shell = 1
!
      end subroutine read_parallel_shell_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_parallel_shell_ctl(id_control, psph_ctl, level)
!
      use ctl_data_sphere_model_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(psph_ctl%iflag_sph_shell .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 psph_ctl%block_name)
      call write_FEM_mesh_control                                       &
     &   (id_control, psph_ctl%Fmesh_ctl, level)
      call write_control_shell_domain                                   &
     &   (id_control, psph_ctl%sdctl, level)
      call write_control_shell_define                                   &
     &   (id_control, psph_ctl%spctl, level)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                psph_ctl%block_name)
!
      end subroutine write_parallel_shell_ctl
!
!   --------------------------------------------------------------------
!
      subroutine init_parallel_shell_ctl_label(hd_block, psph_ctl)
!
      use ctl_data_sphere_model_IO
!
      character(len=kchara), intent(in) :: hd_block
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
      psph_ctl%block_name = trim(hd_block)
      call init_ctl_shell_define_label(hd_sph_def, psph_ctl%spctl)
      call init_ctl_shell_domain_label(hd_domains_sph, psph_ctl%sdctl)
      call init_FEM_mesh_ctl_label(hd_FEM_mesh, psph_ctl%Fmesh_ctl)
!
      end subroutine init_parallel_shell_ctl_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_parallel_shell_ctl(psph_ctl)
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      psph_ctl%iflag_sph_shell = 0
!
      call reset_FEM_mesh_control(psph_ctl%Fmesh_ctl)
      call dealloc_control_shell_define(psph_ctl%spctl)
      call dealloc_ndomain_rtp_ctl(psph_ctl%sdctl)
!
      end subroutine dealloc_parallel_shell_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_gen_sph_shell
