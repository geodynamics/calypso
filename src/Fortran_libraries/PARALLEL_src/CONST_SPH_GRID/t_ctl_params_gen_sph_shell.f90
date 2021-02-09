!>@file   t_ctl_params_gen_sph_shell.f90
!!@brief  module t_ctl_params_gen_sph_shell
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine set_control_4_gen_shell_grids                        &
!!     &         (id_rank, plt, psph_ctl, sph, sph_files, gen_sph, ierr)
!!        type(platform_data_control), intent(in) :: plt
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(sph_grids), intent(inout) :: sph
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module t_ctl_params_gen_sph_shell
!
      use m_precision
!
      use t_spheric_parameter
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_gen_sph_shell
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
      use t_spheric_global_ranks
      use t_const_spherical_grid
      use t_sph_grid_maker_in_sim
!
      implicit  none
!
!>      Structure of file name and format for MHD
      type gen_sph_file_IO_params
!>        FEM mesh IO flags
        type(FEM_file_IO_flags) :: FEM_mesh_flags
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: sph_file_param
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
!>        Structure of file name and format for spectr data file
        type(field_IO_params) :: sph_file_IO
      end type gen_sph_file_IO_params
!
!
      character(len=kchara), parameter :: cflag_SGS_r = 'SGS_r'
      character(len=kchara), parameter :: cflag_SGS_t = 'SGS_theta'
!
      private :: cflag_SGS_r, cflag_SGS_t
      private :: set_control_4_shell_files
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_gen_shell_grids                          &
     &         (id_rank, plt, psph_ctl, sph_files, sph_maker, ierr)
!
      use m_file_format_switch
!
      integer, intent(in) :: id_rank
      type(platform_data_control), intent(in) :: plt
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(gen_sph_file_IO_params), intent(inout) ::  sph_files
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
      integer(kind = kint), intent(inout) :: ierr
!
      integer :: nprocs_check
!
!
      call set_control_4_shell_files                                    &
     &   (id_rank, plt, psph_ctl%Fmesh_ctl, nprocs_check, sph_files)
!
      call set_ctl_4_sph_grid_maker(nprocs_check, psph_ctl,             &
     &    plt%sph_file_prefix, sph_files%sph_file_param,                &
     &    sph_maker, ierr)
      call dealloc_parallel_shell_ctl(psph_ctl)
      call reset_control_shell_define(psph_ctl%spctl)
!
      end subroutine set_control_4_gen_shell_grids
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_control_4_shell_files                              &
     &         (id_rank, plt, Fmesh_ctl, nprocs_check, sph_files)
!
      use set_control_platform_item
      use set_control_platform_data
      use gen_sph_grids_modes
!
      integer, intent(in) :: id_rank
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      type(gen_sph_file_IO_params), intent(inout) ::  sph_files
      integer, intent(inout) :: nprocs_check
!
!
      nprocs_check = 1
      if(plt%ndomain_ctl%iflag .gt. 0) then
        nprocs_check = int(plt%ndomain_ctl%intvalue)
      end if
!
      call turn_off_debug_flag_by_ctl(id_rank, plt)
      call set_control_sph_mesh(plt, Fmesh_ctl,                         &
     &    sph_files%sph_file_param, sph_files%mesh_file_IO,             &
     &    sph_files%sph_file_IO, sph_files%FEM_mesh_flags)
!
      end subroutine set_control_4_shell_files
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_params_gen_sph_shell
