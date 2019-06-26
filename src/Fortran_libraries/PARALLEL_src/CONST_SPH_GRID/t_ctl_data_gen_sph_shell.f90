!>@file   t_ctl_data_gen_sph_shell.f90
!!@brief  module t_ctl_data_gen_sph_shell
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine read_parallel_shell_in_MHD_ctl                       &
!!     &         (id_control, hd_block, psph_ctl, c_buf)
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
!
!  Deprecated
      character(len=kchara), parameter, private                         &
     &                      :: hd_shell_def = 'num_grid_sph'
!
      private :: read_ctl_file_gen_shell_grids
      private :: read_parallel_shell_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_ctl_file_gen_shell_grids(id_control, psph_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      write(*,*) 'Spherical shell resolution file: ',                   &
     &          trim(psph_ctl%control_sph_file)
      open(id_control, file = psph_ctl%control_sph_file)
!
      do
        call load_one_line_from_control(id_control, c_buf1)
        if(check_begin_flag(c_buf1, hd_sph_shell)) then
          call read_parallel_shell_ctl                                  &
     &       (id_control, hd_sph_shell, psph_ctl, c_buf1)
        end if
        if(psph_ctl%iflag_sph_shell .gt. 0) exit
      end do
!
      close(id_control)
      write(*,*) 'Spherical shell resolution file end'
!
      end subroutine read_ctl_file_gen_shell_grids
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_parallel_shell_in_MHD_ctl                         &
     &         (id_control, hd_block, psph_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if((psph_ctl%iflag_sph_shell + psph_ctl%ifile_sph_shell) .gt. 0)  &
     &      return
      if(check_file_flag(c_buf, hd_block)) then
        psph_ctl%control_sph_file = third_word(c_buf)
        psph_ctl%ifile_sph_shell = 1
        call read_ctl_file_gen_shell_grids(id_control+2, psph_ctl)
        return
      end if
!
      if(check_begin_flag(c_buf, hd_block)) then
        write(*,*) 'resolution data is included'
        call read_parallel_shell_ctl                                    &
     &     (id_control, hd_block, psph_ctl, c_buf)
      end if
!
      end subroutine read_parallel_shell_in_MHD_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_parallel_shell_ctl                                &
     &         (id_control, hd_block, psph_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(psph_ctl%iflag_sph_shell .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
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
