!>@file   t_ctl_data_gen_sph_shell.f90
!!@brief  module t_ctl_data_gen_sph_shell
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine read_ctl_file_gen_shell_grids(psph_ctl)
!!      subroutine read_parallel_shell_in_MHD_ctl(hd_block, psph_ctl)
!!
!!      subroutine bcast_parallel_shell_ctl(psph_ctl)
!!        type(platform_data_control), intent(inout) :: plt
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!
!! =======================================================
!!    example of control section
!!
!!    begin FEM_mesh_ctl
!!      memory_conservation_ctl        'YES'
!!      FEM_mesh_output_switch         'NO'
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
!!
!!      sleeve_level_ctl            2
!!      element_overlap_ctl        ON
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
      use m_read_control_elements
      use skip_comment_f
!
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
!
      implicit none
!
      integer(kind = kint), private :: control_file_code = 13
!
!>      structure of parallel spherical shell data
      type parallel_sph_shell_control
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!>        Structure of spherical shell configuration
        type(sphere_data_control) :: spctl
!>        Structure of spherical shell domain decomposition
        type(sphere_domain_control) :: sdctl
!
!>        File name to read spherical shell control file
        character (len = kchara)                                        &
     &         :: control_sph_file = 'control_sph_shell'
!>        Tnteger flag to defined spherical shell
        integer (kind=kint) :: iflag_sph_shell = 0
!>        Tnteger flag to read spherical shell control file
        integer (kind=kint) :: ifile_sph_shell = 0
      end type parallel_sph_shell_control
!
!   Top level
!
      character(len=kchara), parameter, private                         &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
!
!   Second level
!
      character(len=kchara), parameter, private                         &
     &                     :: hd_FEM_mesh = 'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                     :: hd_sph_def = 'shell_define_ctl'
      character(len=kchara), parameter, private                         &
     &                     :: hd_domains_sph = 'num_domain_ctl'
      integer(kind=kint), private :: i_FEM_mesh =    0
      integer(kind = kint), private :: i_shell_def =   0
      integer(kind = kint), private :: i_domains_sph = 0
!
!  Deprecated
      character(len=kchara), parameter, private                         &
     &                      :: hd_shell_def = 'num_grid_sph'
!
      private :: read_parallel_shell_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_file_gen_shell_grids(psph_ctl)
!
      use calypso_mpi
      use bcast_4_sphere_ctl
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open(ctl_file_code, file = psph_ctl%control_sph_file)
!
        call load_ctl_label_and_line
        call read_parallel_shell_ctl(hd_sph_shell, psph_ctl)
!
        close(ctl_file_code)
      end if
!
      call bcast_parallel_shell_ctl(psph_ctl)
!
      end subroutine read_ctl_file_gen_shell_grids
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_parallel_shell_in_MHD_ctl(hd_block, psph_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      if((psph_ctl%iflag_sph_shell + psph_ctl%ifile_sph_shell) .gt. 0)  &
     &      return
      if(right_file_flag(hd_block) .gt. 0) then
        call read_file_name_from_ctl_line                               &
     &     (psph_ctl%ifile_sph_shell, psph_ctl%control_sph_file)
        return
      end if
!
      call read_parallel_shell_ctl(hd_block, psph_ctl)
!
      end subroutine read_parallel_shell_in_MHD_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_parallel_shell_ctl(hd_block, psph_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      do
        call load_ctl_label_and_line
!
        psph_ctl%iflag_sph_shell = find_control_end_flag(hd_block)
        if(psph_ctl%iflag_sph_shell .gt. 0) exit
!
        call read_FEM_mesh_control                                      &
     &     (hd_FEM_mesh, i_FEM_mesh, psph_ctl%Fmesh_ctl)
        call read_control_shell_define                                  &
     &     (hd_sph_def, i_shell_def, psph_ctl%spctl)
        call read_control_shell_define                                  &
     &     (hd_shell_def, i_shell_def, psph_ctl%spctl)
!
        call read_control_shell_domain                                  &
     &     (hd_domains_sph, i_domains_sph, psph_ctl%sdctl)
      end do
!
      end subroutine read_parallel_shell_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_parallel_shell_ctl(psph_ctl)
!
      use calypso_mpi
      use bcast_4_sphere_ctl
      use bcast_4_platform_ctl
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      call MPI_Bcast(psph_ctl%iflag_sph_shell, 1,                       &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(psph_ctl%ifile_sph_shell, 1,                       &
     &    CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(psph_ctl%control_sph_file , kchara,                &
     &    CALYPSO_CHARACTER, 0, CALYPSO_COMM, ierr_MPI)
!
      if(psph_ctl%iflag_sph_shell .eq. 0) return
!
      call bcast_FEM_mesh_control(psph_ctl%Fmesh_ctl)
      call bcast_ctl_4_shell_define(psph_ctl%spctl)
      call bcast_ctl_ndomain_4_shell(psph_ctl%sdctl)
!
      end subroutine bcast_parallel_shell_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_parallel_shell_ctl(psph_ctl)
!
      use calypso_mpi
      use bcast_4_sphere_ctl
      use bcast_4_platform_ctl
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      psph_ctl%iflag_sph_shell = 0
      psph_ctl%ifile_sph_shell = 0
!
      call dealloc_control_shell_define(psph_ctl%spctl)
      call dealloc_ndomain_rtp_ctl(psph_ctl%sdctl)
!
      end subroutine dealloc_parallel_shell_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_gen_sph_shell
