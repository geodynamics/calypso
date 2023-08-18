!>@file   t_ctl_data_4_sphere_model.f90
!!        module t_ctl_data_4_sphere_model
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
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
!!    num_fluid_grid_ctl      10
!!    increment_of_chebyshev   2
!!
!!    fluid_core_size_ctl   0.35
!!    ICB_to_CMB_ratio_ctl  1.0
!!    Min_radius_ctl      0.0
!!    ICB_radius_ctl      0.5384615384615
!!    CMB_radius_ctl      1.538461538462
!!    Max_radius_ctl      2.0
!!
!!    array add_external_layer
!!      add_external_layer      2.815
!!      add_external_layer      2.825
!!    end array add_external_layer
!!
!!    array boundaries_ctl
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
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
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
!>        Block name
        character(len=kchara) :: block_name = 'shell_define_ctl'
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
!!@n        spctl%ivec:  radial ID
!!@n        spctl%vect:  Radius
        type(ctl_array_ir) :: radius_ctl
!
!>        Structure for radial grouping data for boundaries
!!@n        spctl%ivec:  radial ID
!!@n        spctl%vect:  Radius
        type(ctl_array_ci) :: radial_grp_ctl
!
!>        Structure for additional radial point data at external
!!@n        spctl%vect:  Radius
        type(ctl_array_real) :: add_ext_layer_ctl
!
!>        Grid spacing type
        type(read_character_item) :: radial_grid_type_ctl
!>        Number of element in fluid (number of grid - 1)
        type(read_integer_item) :: num_fluid_grid_ctl
!>        Increment between chebyshev points
        type(read_integer_item) :: increment_cheby_ctl
!>        Minimum radius of the simulation domain @f$ R_{c} @f$
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_control_shell_define(spctl)
!
      type(sphere_data_control), intent(inout) :: spctl
!
!
      call dealloc_control_array_i2(spctl%radial_layer_list_ctl)
      call dealloc_control_array_i2(spctl%med_layer_list_ctl)
      call dealloc_control_array_c_i(spctl%radial_grp_ctl)
      call dealloc_control_array_real(spctl%add_ext_layer_ctl)
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
      spctl%increment_cheby_ctl%iflag =  0
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
