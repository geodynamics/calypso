!>@file   m_ctl_data_4_sphere_model.f90
!!@brief  module m_ctl_data_4_sphere_model
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine read_ctl_4_shell_define
!!
!! =======================================================
!!    example of control section
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
!!  end num_grid_sph
!!
!! =======================================================
!!@endverbatim
!
      module m_ctl_data_4_sphere_model
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!     resolution of spherical harmonics
!
!>      Truncation lavel of spherical harmonics
      integer(kind = kint) :: ltr_ctl
!>      Type of spherical grids
      character(len = kchara) :: sph_grid_type_ctl
!>      Type of spherical coefficients
      character(len = kchara) :: sph_coef_type_ctl
!
!>      Number of grids in meridional direction
      integer(kind = kint) :: ngrid_elevation_ctl
!>      Number of grids in longitudinal direction
      integer(kind = kint) :: ngrid_azimuth_ctl
!
!>      Structure for radial point data
!!@n      light_position_ctl%ivec:  radial ID
!!@n      light_position_ctl%vect:  Radius
      type(ctl_array_ir), save :: radius_ctl
!
!>      Structure for radial grouping data for boundaries
!!@n      light_position_ctl%c_tble:  Group name
!!@n      light_position_ctl%ivec:    radial ID
      type(ctl_array_ci), save :: radial_grp_ctl
!
!>      Grid spacing type
      character(len = kchara) :: radial_grid_type_ctl
!>      Minimum radius of the simulation domain @f$ R_{c} @f$
      integer(kind = kint) :: num_fluid_grid_ctl
!>      ICB radius     @f$ R_{i} @f$
      real(kind = kreal) :: Min_radius_ctl
!>      ICB radius     @f$ R_{i} @f$
      real(kind = kreal) :: ICB_radius_ctl
!>      CMB radius     @f$ R_{o} @f$
      real(kind = kreal) :: CMB_radius_ctl
!>      Maximum radius of the simulation domain @f$ R_{m} @f$
      real(kind = kreal) :: Max_radius_ctl
!>      Size of the fluid shell
!!       @f$ L = R_{o} - R_{i} @f$
      real(kind = kreal) :: fluid_core_size_ctl
!>      Ratio of inner core radius to the outer core radius
!!       @f$ R_{i} / R_{o} @f$
      real(kind = kreal) :: ICB_to_CMB_ratio_ctl
!
!
!   labels of data field
!
      character(len=kchara), parameter :: hd_shell_def = 'num_grid_sph'
      integer(kind = kint) :: i_shell_def =   0
!
!   labels of shell define
!
      character(len=kchara), parameter                                  &
     &      ::  hd_numlayer_shell = 'r_layer'
!
      character(len=kchara), parameter                                  &
     &      ::  hd_ntheta_shell = 'ngrid_meridonal_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_nphi_shell =   'ngrid_zonal_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_sph_truncate = 'truncation_level_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_sph_c_type =   'sph_coef_type_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_sph_g_type =   'sph_grid_type_ctl'
!
      character(len=kchara), parameter                                  &
     &      ::  hd_r_grid_type = 'radial_grid_type_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_n_fluid_grid = 'num_fluid_grid_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_Min_radius =  'Min_radius_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_ICB_radius =  'ICB_radius_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_CMB_radius =  'CMB_radius_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_Max_radius =  'Max_radius_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_shell_size =  'fluid_core_size_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_shell_ratio = 'ICB_to_CMB_ratio_ctl'
!
      character(len=kchara), parameter                                  &
     &      ::  hd_bc_sph = 'boundaries_ctl'
!
      integer (kind=kint) :: i_ntheta_shell = 0
      integer (kind=kint) :: i_nphi_shell =   0
      integer (kind=kint) :: i_sph_truncate = 0
      integer (kind=kint) :: i_sph_c_type =   0
      integer (kind=kint) :: i_sph_g_type =   0
!
      integer (kind=kint) :: i_r_grid_type =  0
      integer (kind=kint) :: i_n_fluid_grid = 0
      integer (kind=kint) :: i_Min_radius =   0
      integer (kind=kint) :: i_ICB_radius =   0
      integer (kind=kint) :: i_CMB_radius =   0
      integer (kind=kint) :: i_Max_radius =   0
      integer (kind=kint) :: i_shell_size =   0
      integer (kind=kint) :: i_shell_ratio =  0
!
!   3rd level for boundary define
!
      private :: hd_shell_def, i_shell_def
      private :: hd_numlayer_shell, hd_sph_c_type
      private :: hd_ntheta_shell, hd_nphi_shell, hd_sph_truncate
      private :: hd_r_grid_type, hd_n_fluid_grid, hd_Min_radius
      private :: hd_ICB_radius, hd_CMB_radius, hd_Max_radius
      private :: hd_shell_size, hd_shell_ratio, hd_bc_sph
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_4_shell_define
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_shell_def) .eq. 0) return
      if (i_shell_def .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_shell_def, i_shell_def)
        if(i_shell_def .gt. 0) exit
!
!
        call read_control_array_i_r(hd_numlayer_shell, radius_ctl)
!
        call read_control_array_c_i(hd_bc_sph, radial_grp_ctl)
!
!
        call read_character_ctl_item(hd_sph_c_type,                     &
     &        i_sph_c_type, sph_coef_type_ctl)
        call read_character_ctl_item(hd_sph_g_type,                     &
     &        i_sph_g_type, sph_grid_type_ctl)
        call read_character_ctl_item(hd_r_grid_type,                    &
     &        i_r_grid_type, radial_grid_type_ctl)
!
        call read_integer_ctl_item(hd_sph_truncate,                     &
     &          i_sph_truncate, ltr_ctl)
        call read_integer_ctl_item(hd_ntheta_shell,                     &
     &        i_nphi_shell, ngrid_elevation_ctl)
        call read_integer_ctl_item(hd_nphi_shell,                       &
     &        i_ntheta_shell, ngrid_azimuth_ctl)
!
        call read_integer_ctl_item(hd_n_fluid_grid,                     &
     &         i_n_fluid_grid, num_fluid_grid_ctl)
!
!
        call read_real_ctl_item(hd_Min_radius,                          &
     &         i_Min_radius, Min_radius_ctl)
        call read_real_ctl_item(hd_ICB_radius,                          &
     &         i_ICB_radius, ICB_radius_ctl)
        call read_real_ctl_item(hd_CMB_radius,                          &
     &         i_CMB_radius, CMB_radius_ctl)
        call read_real_ctl_item(hd_Max_radius,                          &
     &         i_Max_radius, Max_radius_ctl)
!
        call read_real_ctl_item(hd_shell_size,                          &
     &         i_shell_size, fluid_core_size_ctl)
        call read_real_ctl_item(hd_shell_ratio,                         &
     &         i_shell_ratio, ICB_to_CMB_ratio_ctl)
      end do
!
      end subroutine read_ctl_4_shell_define
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_sphere_model
