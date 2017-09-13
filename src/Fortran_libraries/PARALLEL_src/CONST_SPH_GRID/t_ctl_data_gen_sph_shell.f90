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
!!  begin num_domain_ctl
!!    num_radial_domain_ctl         2
!!    num_horizontal_domain_ctl     2
!!
!!    array  num_domain_sph_grid   2
!!      num_domain_sph_grid    radial       2   end
!!      num_domain_sph_grid   meridional    3   end
!!    end array num_domain_sph_grid
!!
!!    array num_domain_legendre   2
!!      num_domain_legendre   radial        2   end
!!      num_domain_legendre   zonal         3   end
!!    end array num_domain_legendre
!!
!!    array num_domain_spectr     1
!!      num_domain_spectr     modes         6   end
!!    end array num_domain_spectr
!!  end num_domain_ctl
!!
!!  begin num_grid_sph
!!! ----------------------------------------------------------------
!!!   sph_coef_type_ctl:  grid type for spherical harmonics data
!!!         no_pole:      Coefficients on spherical shell only
!!!         with_center:  Add center
!!!   sph_grid_type_ctl:  grid type for mesh data
!!!         no_pole:      Gaussian points only
!!!         with_pole:    Add pole grids
!!!         with_center:  Add center
!!! ----------------------------------------------------------------
!!
!!    sph_coef_type_ctl       no_pole
!!    sph_grid_type_ctl       no_pole
!!    truncation_level_ctl     4
!!    longitude_symmetry_ctl   2
!!    ngrid_meridonal_ctl     12
!!    ngrid_zonal_ctl         24
!!
!!   radial_grid_type_ctl:  Definition for radial grid   
!!         explicit:        Set each radial grid explicitly
!!         Chebyshev:       Set Chebyshev collocation points
!!         equi_distance:   Set equi-diatance grid
!!
!!    radial_grid_type_ctl   explicit
!!    array r_layer       4
!!      r_layer    1  0.3584615384615
!!      r_layer    2  0.5384615384615     ICB
!!      r_layer    3  1.038461538462      Mid
!!      r_layer    4  1.538461538462      CMB
!!    end array r_layer
!!
!!    radial_grid_type_ctl   Chebyshev
!!     num_fluid_grid_ctl  5
!!     fluid_core_size_ctl   0.35
!!     ICB_to_CMB_ratio_ctl  1.0
!!     Min_radius_ctl      0.0
!!     ICB_radius_ctl      0.5384615384615
!!     CMB_radius_ctl      1.538461538462
!!     Max_radius_ctl      2.0
!!
!!    array boundaries_ctl   3
!!      boundaries_ctl  to_Center   1
!!      boundaries_ctl  ICB         2
!!      boundaries_ctl  CMB         4
!!    end array  boundaries_ctl
!!
!!
!!    num_radial_layering_ctl        10
!!    num_meridional_layering_ctl    10
!!
!!    array radial_layering_ctl        2
!!      radial_layering_ctl          1   8
!!      radial_layering_ctl          8  15
!!    end array radial_layering_ctl
!!
!!    array meridional_layering_ctl        2
!!      meridional_layering_ctl          1   6
!!      meridional_layering_ctl          6  13
!!    end array meridional_layering_ctl
!!  end num_grid_sph
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
      use t_ctl_data_4_sphere_model
      use t_ctl_data_4_divide_sphere
!
      implicit none
!
      integer(kind = kint), private :: control_file_code = 13
!
!>      structure of parallel spherical shell data
      type parallel_sph_shell_control
!>        structure of spherical shell configuration
        type(sphere_data_control) :: spctl
!>        structure of spherical shell domain decomposition
        type(sphere_domain_control) :: sdctl
!
!>        file name to read spherical shell control file
        character (len = kchara)                                        &
     &         :: control_sph_file = 'control_sph_shell'
!>        integer flag to defined spherical shell
        integer (kind=kint) :: iflag_sph_shell = 0
!>        integer flag to read spherical shell control file
        integer (kind=kint) :: ifile_sph_shell = 0
      end type parallel_sph_shell_control
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &      :: hd_sph_shell = 'spherical_shell_ctl'
!
!   Second level
!
      character(len=kchara), parameter                                  &
     &                     :: hd_sph_def = 'shell_define_ctl'
      character(len=kchara), parameter                                  &
     &                     :: hd_domains_sph = 'num_domain_ctl'
      integer(kind = kint) :: i_shell_def =   0
      integer(kind = kint) :: i_domains_sph = 0
!
!  Deprecated
      character(len=kchara), parameter :: hd_shell_def = 'num_grid_sph'
!
      private :: hd_sph_shell
      private :: hd_sph_def, hd_shell_def, i_shell_def
      private :: hd_domains_sph, i_domains_sph
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
        call find_control_end_flag(hd_block, psph_ctl%iflag_sph_shell)
        if(psph_ctl%iflag_sph_shell .gt. 0) exit
!
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
!
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!
!
      call MPI_Bcast(psph_ctl%iflag_sph_shell, ione,                    &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(psph_ctl%ifile_sph_shell, ione,                    &
     &    CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(psph_ctl%control_sph_file , kchara,                &
     &    CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
!
      if(psph_ctl%iflag_sph_shell .eq. 0) return
!
      call bcast_ctl_4_shell_define(psph_ctl%spctl)
      call bcast_ctl_ndomain_4_shell(psph_ctl%sdctl)
!
      end subroutine bcast_parallel_shell_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_gen_sph_shell
