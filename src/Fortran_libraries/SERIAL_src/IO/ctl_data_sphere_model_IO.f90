!>@file   ctl_data_sphere_model_IO.f90
!!        module ctl_data_sphere_model_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!>@brief  control data for resolutions of spherical shell
!!
!!@verbatim
!!      subroutine init_ctl_shell_define_label(hd_block, spctl)
!!      subroutine read_control_shell_define                            &
!!     &         (id_control, hd_block, spctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sphere_data_control), intent(inout) :: spctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_control_shell_define(id_control, spctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(sphere_data_control), intent(in) :: spctl
!!        integer(kind = kint), intent(inout) :: level
!!
!! =======================================================
!!    example of control section
!!
!!  begin num_grid_sph
!!! ----------------------------------------------------------------
!!!   sph_center_coef_ctl:  grid type for spherical harmonics data
!!!         no_pole:      Coefficients on spherical shell only
!!!         with_center:  Add center
!!!   sph_grid_type_ctl:  grid type for mesh data
!!!         no_pole:      Gaussian points only
!!!         with_pole:    Add pole grids
!!!         with_center:  Add center
!!! ----------------------------------------------------------------
!!
!!    sph_center_coef_ctl       no_pole
!!    sph_grid_type_ctl         no_pole
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
!!    fluid_core_size_ctl   1.0
!!    ICB_to_CMB_ratio_ctl  0.35
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
      module ctl_data_sphere_model_IO
!
      use m_precision
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_charaint
      use t_control_array_integer2
      use t_control_array_intreal
      use t_ctl_data_4_sphere_model
!
      implicit  none
!
!   labels of shell define
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_numlayer_shell =     'r_layer'
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_ntheta_shell = 'ngrid_meridonal_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_nphi_shell =   'ngrid_zonal_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_sph_truncate = 'truncation_level_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_phi_symmetry = 'longitude_symmetry_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_sph_c_type =   'sph_center_coef_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_sph_g_type =   'sph_grid_type_ctl'
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_r_grid_type = 'radial_grid_type_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_n_fluid_grid = 'num_fluid_grid_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_cheby_increment = 'increment_of_chebyshev'
      character(len=kchara), parameter, private                         &
     &      ::  hd_Min_radius =  'Min_radius_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_ICB_radius =  'ICB_radius_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_CMB_radius =  'CMB_radius_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_Max_radius =  'Max_radius_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_shell_size =  'fluid_core_size_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_shell_ratio = 'ICB_to_CMB_ratio_ctl'
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_add_external_layer = 'add_external_layer'
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_bc_sph = 'boundaries_ctl'
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_num_radial_grp = 'num_radial_layering_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_num_med_grp =    'num_meridional_layering_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_list_radial_grp = 'radial_layering_ctl'
      character(len=kchara), parameter, private                         &
     &      ::  hd_list_med_grp = 'meridional_layering_ctl'
!
!!    Deprecated flags
!
      character(len=kchara), parameter, private                         &
     &      ::  hd_sph_ctl_type =   'sph_coef_type_ctl'
!
!
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
      if(spctl%i_shell_def .gt. 0) return
      spctl%block_name = hd_block
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_i_r(id_control,                         &
     &      hd_numlayer_shell, spctl%radius_ctl, c_buf)
        call read_control_array_c_i(id_control,                         &
     &      hd_bc_sph, spctl%radial_grp_ctl, c_buf)
        call read_control_array_r1(id_control,                          &
     &      hd_add_external_layer, spctl%add_ext_layer_ctl, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_sph_c_type, spctl%sph_coef_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_sph_ctl_type, spctl%sph_coef_type_ctl)
!
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
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_cheby_increment, spctl%increment_cheby_ctl)
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
      subroutine write_control_shell_define(id_control, spctl, level)
!
      use m_machine_parameter
      use t_read_control_elements
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(sphere_data_control), intent(in) :: spctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(spctl%i_shell_def .le. 0) return
!
      maxlen = len_trim(hd_sph_c_type)
      maxlen = max(maxlen, len_trim(hd_sph_c_type))
      maxlen = max(maxlen, len_trim(hd_sph_g_type))
      maxlen = max(maxlen, len_trim(hd_r_grid_type))
!
      maxlen = max(maxlen, len_trim(hd_phi_symmetry))
      maxlen = max(maxlen, len_trim(hd_sph_truncate))
      maxlen = max(maxlen, len_trim(hd_ntheta_shell))
      maxlen = max(maxlen, len_trim(hd_nphi_shell))
!
      maxlen = max(maxlen, len_trim(hd_n_fluid_grid))
      maxlen = max(maxlen, len_trim(hd_cheby_increment))
!
      maxlen = max(maxlen, len_trim(hd_Min_radius))
      maxlen = max(maxlen, len_trim(hd_ICB_radius))
      maxlen = max(maxlen, len_trim(hd_CMB_radius))
      maxlen = max(maxlen, len_trim(hd_Max_radius))
!
      maxlen = max(maxlen, len_trim(hd_shell_size))
      maxlen = max(maxlen, len_trim(hd_shell_ratio))
!
      maxlen = max(maxlen, len_trim(hd_num_radial_grp))
      maxlen = max(maxlen, len_trim(hd_num_med_grp))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 spctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    spctl%sph_coef_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    spctl%sph_grid_type_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%ltr_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%phi_symmetry_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%ngrid_elevation_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%ngrid_azimuth_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    spctl%radial_grid_type_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%num_fluid_grid_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%increment_cheby_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    spctl%fluid_core_size_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    spctl%ICB_to_CMB_ratio_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    spctl%Min_radius_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    spctl%ICB_radius_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    spctl%CMB_radius_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    spctl%Max_radius_ctl)
!
      call write_control_array_i_r(id_control, level,                   &
     &    spctl%radius_ctl)
      call write_control_array_r1(id_control, level,                    &
     &    spctl%add_ext_layer_ctl)
!
      call write_control_array_c_i(id_control, level,                   &
     &    spctl%radial_grp_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%num_radial_layer_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    spctl%num_med_layer_ctl)
!
      call write_control_array_i2(id_control, level,                    &
     &    spctl%radial_layer_list_ctl)
      call write_control_array_i2(id_control, level,                    &
     &    spctl%med_layer_list_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                spctl%block_name)
!
      end subroutine write_control_shell_define
!
!   --------------------------------------------------------------------
!
      subroutine init_ctl_shell_define_label(hd_block, spctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(sphere_data_control), intent(inout) :: spctl
!
!
      spctl%block_name = hd_block
!
        call init_i_r_array_label                                       &
     &     (hd_numlayer_shell, spctl%radius_ctl)
        call init_c_i_array_label                                       &
     &     (hd_bc_sph, spctl%radial_grp_ctl)
        call init_real_ctl_array_label                                  &
     &     (hd_add_external_layer, spctl%add_ext_layer_ctl)
!
!
        call init_chara_ctl_item_label                                  &
     &     (hd_sph_c_type, spctl%sph_coef_type_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_sph_g_type, spctl%sph_grid_type_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_r_grid_type, spctl%radial_grid_type_ctl)
!
        call init_int_ctl_item_label                                    &
     &     (hd_phi_symmetry, spctl%phi_symmetry_ctl)
        call init_int_ctl_item_label                                    &
     &     (hd_sph_truncate, spctl%ltr_ctl)
        call init_int_ctl_item_label                                    &
     &     (hd_ntheta_shell, spctl%ngrid_elevation_ctl)
        call init_int_ctl_item_label                                    &
     &     (hd_nphi_shell, spctl%ngrid_azimuth_ctl)
!
        call init_int_ctl_item_label                                    &
     &     (hd_n_fluid_grid, spctl%num_fluid_grid_ctl)
        call init_int_ctl_item_label                                    &
     &     (hd_cheby_increment, spctl%increment_cheby_ctl)
!
!
        call init_real_ctl_item_label                                   &
     &     (hd_Min_radius, spctl%Min_radius_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_ICB_radius, spctl%ICB_radius_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_CMB_radius, spctl%CMB_radius_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_Max_radius, spctl%Max_radius_ctl)
!
        call init_real_ctl_item_label                                   &
     &     (hd_shell_size, spctl%fluid_core_size_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_shell_ratio, spctl%ICB_to_CMB_ratio_ctl)
!
        call init_int_ctl_item_label                                    &
     &     (hd_num_radial_grp, spctl%num_radial_layer_ctl)
        call init_int_ctl_item_label                                    &
     &     (hd_num_med_grp, spctl%num_med_layer_ctl)
!
        call init_int2_ctl_array_label                                  &
     &     (hd_list_radial_grp, spctl%radial_layer_list_ctl)
        call init_int2_ctl_array_label                                  &
     &     (hd_list_med_grp, spctl%med_layer_list_ctl)
!
      end subroutine init_ctl_shell_define_label
!
!   --------------------------------------------------------------------
!
      end module ctl_data_sphere_model_IO
