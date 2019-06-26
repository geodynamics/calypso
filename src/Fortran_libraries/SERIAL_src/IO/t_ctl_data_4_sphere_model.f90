!>@file   t_ctl_data_4_sphere_model.f90
!!@brief  module t_ctl_data_4_sphere_model
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine read_control_shell_define                            &
!!     &         (id_control, hd_block, spctl, c_buf)
!!      subroutine dealloc_control_shell_define(spctl)
!!      subroutine reset_control_shell_define(spctl)
!!        type(sphere_data_control), intent(inout) :: spctl
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
      module t_ctl_data_4_sphere_model
!
      use m_precision
      use t_control_elements
      use t_control_array_charaint
      use t_control_array_integer2
      use t_control_array_intreal
!
      implicit  none
!
!     resolution of spherical harmonics
!
!>      control data structure for spherical shell
      type sphere_data_control
!>        Truncation lavel of spherical harmonics
        type(read_integer_item) :: ltr_ctl
!>        longitudinal symmetry
        type(read_integer_item) :: phi_symmetry_ctl
!
!>        Type of spherical grids
        type(read_character_item) :: sph_grid_type_ctl
!>        Type of spherical coefficients
        type(read_character_item) :: sph_coef_type_ctl
!
!>        Number of grids in meridional direction
        type(read_integer_item) :: ngrid_elevation_ctl
!>        Number of grids in longitudinal direction
        type(read_integer_item) :: ngrid_azimuth_ctl
!
!>        Structure for radial point data
!!@n        light_position_ctl%ivec:  radial ID
!!@n        light_position_ctl%vect:  Radius
        type(ctl_array_ir) :: radius_ctl
!
!>        Structure for radial grouping data for boundaries
!!@n        light_position_ctl%c_tble:  Group name
!!@n        light_position_ctl%ivec:    radial ID
        type(ctl_array_ci) :: radial_grp_ctl
!
!>        Grid spacing type
        type(read_character_item) :: radial_grid_type_ctl
!>        Minimum radius of the simulation domain @f$ R_{c} @f$
        type(read_integer_item) :: num_fluid_grid_ctl
!>        ICB radius     @f$ R_{i} @f$
        type(read_real_item) :: Min_radius_ctl
!>        ICB radius     @f$ R_{i} @f$
        type(read_real_item) :: ICB_radius_ctl
!>        CMB radius     @f$ R_{o} @f$
        type(read_real_item) :: CMB_radius_ctl
!>        Maximum radius of the simulation domain @f$ R_{m} @f$
        type(read_real_item) :: Max_radius_ctl
!>        Size of the fluid shell
!!         @f$ L = R_{o} - R_{i} @f$
        type(read_real_item) :: fluid_core_size_ctl
!>        Ratio of inner core radius to the outer core radius
!!         @f$ R_{i} / R_{o} @f$
        type(read_real_item) :: ICB_to_CMB_ratio_ctl
!
!
!        Parametes for SGS model
!>        Number of radial layering for outer core
        type(read_integer_item) :: num_radial_layer_ctl
!>        Number of moridional layering for outer core
        type(read_integer_item) :: num_med_layer_ctl
!
!>        Number of radial layering for outer core
        type(ctl_array_i2) :: radial_layer_list_ctl
!>        Number of moridional layering for outer core
        type(ctl_array_i2) :: med_layer_list_ctl
!
        integer(kind = kint) :: i_shell_def =   0
      end type sphere_data_control
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
     &      ::  hd_phi_symmetry = 'longitude_symmetry_ctl'
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
      character(len=kchara), parameter                                  &
     &      ::  hd_num_radial_grp = 'num_radial_layering_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_num_med_grp =    'num_meridional_layering_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_list_radial_grp = 'radial_layering_ctl'
      character(len=kchara), parameter                                  &
     &      ::  hd_list_med_grp = 'meridional_layering_ctl'
!
      private :: hd_numlayer_shell, hd_sph_c_type, hd_phi_symmetry
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
      subroutine read_control_shell_define                              &
     &         (id_control, hd_block, spctl, c_buf)
!
      use m_machine_parameter
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sphere_data_control), intent(inout) :: spctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(spctl%i_shell_def .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_i_r(id_control,                         &
     &      hd_numlayer_shell, spctl%radius_ctl, c_buf)
!
        call read_control_array_c_i(id_control,                         &
     &      hd_bc_sph, spctl%radial_grp_ctl, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_sph_c_type, spctl%sph_coef_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_sph_g_type, spctl%sph_grid_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_r_grid_type, spctl%radial_grid_type_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_phi_symmetry, spctl%phi_symmetry_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sph_truncate, spctl%ltr_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_ntheta_shell, spctl%ngrid_elevation_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_nphi_shell, spctl%ngrid_azimuth_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_n_fluid_grid, spctl%num_fluid_grid_ctl)
!
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_Min_radius, spctl%Min_radius_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_ICB_radius, spctl%ICB_radius_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_CMB_radius, spctl%CMB_radius_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_Max_radius, spctl%Max_radius_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_shell_size, spctl%fluid_core_size_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_shell_ratio, spctl%ICB_to_CMB_ratio_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_radial_grp, spctl%num_radial_layer_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_med_grp, spctl%num_med_layer_ctl)
!
        call read_control_array_i2(id_control,                          &
     &      hd_list_radial_grp, spctl%radial_layer_list_ctl, c_buf)
        call read_control_array_i2(id_control,                          &
     &      hd_list_med_grp, spctl%med_layer_list_ctl, c_buf)
      end do
      spctl%i_shell_def = 1
!
      end subroutine read_control_shell_define
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_shell_define(spctl)
!
      type(sphere_data_control), intent(inout) :: spctl
!
!
      call dealloc_control_array_i2(spctl%radial_layer_list_ctl)
      call dealloc_control_array_i2(spctl%med_layer_list_ctl)
      call dealloc_control_array_c_i(spctl%radial_grp_ctl)
      call dealloc_control_array_i_r(spctl%radius_ctl)
!
      end subroutine dealloc_control_shell_define
!
!   --------------------------------------------------------------------
!
      subroutine reset_control_shell_define(spctl)
!
      type(sphere_data_control), intent(inout) :: spctl
!
!
      spctl%ltr_ctl%iflag =          0
      spctl%phi_symmetry_ctl%iflag = 0
!
      spctl%sph_grid_type_ctl%iflag = 0
      spctl%sph_coef_type_ctl%iflag = 0
!
      spctl%ngrid_elevation_ctl%iflag = 0
      spctl%ngrid_azimuth_ctl%iflag =   0
!
      spctl%radial_grid_type_ctl%iflag = 0
      spctl%num_fluid_grid_ctl%iflag =   0
      spctl%Min_radius_ctl%iflag =       0
      spctl%ICB_radius_ctl%iflag =       0
      spctl%CMB_radius_ctl%iflag =       0
      spctl%Max_radius_ctl%iflag =       0
      spctl%fluid_core_size_ctl%iflag =  0
      spctl%ICB_to_CMB_ratio_ctl%iflag = 0
!
      spctl%num_radial_layer_ctl%iflag = 0
      spctl%num_med_layer_ctl%iflag =    0
!
      end subroutine reset_control_shell_define
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_4_sphere_model
