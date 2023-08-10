!>@file   t_ctl_data_4_platforms.f90
!!        module t_ctl_data_4_platforms
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine reset_control_platforms(plt)
!!        type(platform_data_control), intent(inout) :: plt
!!      subroutine copy_ctl_data_4_platform(org_plt, new_plt)
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(platform_data_control), intent(inout) :: new_plt
!!
!! ------------------------------------------------------------------
!!      Example of control parameters
!!
!!    begin data_files_def
!!      debug_flag_ctl            'ON'
!!
!!      num_subdomain_ctl           2
!!      num_smp_ctl                 4
!!
!!      mesh_file_prefix            'mesh/in'
!!
!!      sph_file_prefix             'sph_shell/in'
!!
!!      coriolis_int_file_name      'sph_shell/rot_int.dat'
!!      boundary_data_file_name     'bc_spec.dat'
!!      radial_field_file_name      'radial_data.dat'
!!
!!      interpolate_sph_to_fem_ctl  'sph_shell/sph_to_fem'
!!      interpolate_fem_to_sph_ctl  'sph_shell/fem_to_sph'
!!
!!      field_file_prefix           'field/out'
!!      restart_file_prefix         'restart/rst'
!!
!!      spectr_field_file_prefix    'sph_spectr/spectr'
!!
!!      rayleigh_spectr_dir            'Checkpoints/'
!!      rayleigh_field_dir             'Spherical_3D/'
!!
!!      mesh_file_fmt_ctl           'ascii'
!!      restart_file_fmt_ctl        'ascii'
!!      field_file_fmt_ctl          'ucd_ascii'
!!      sph_file_fmt_ctl            'ascii'
!!      spectr_field_fmt_ctl        'ascii'
!!      itp_file_fmt_ctl            'ascii'
!!      coriolis_file_fmt_ctl       'ascii'
!!
!!      delete_original_data_flag       'YES'
!!    end data_files_def
!! ------------------------------------------------------------------
!!@endverbatim
!!
!>@n@param      debug_flag_ctl             Debug flag
!!                 ('Full', 'On', or 'Off')
!>@n
!>@n@param      num_subdomain_ctl
!>                Number of subdomain (MPI processes)
!>@n@param      num_smp_ctl                Number of SMP threads
!>@n
!>@n@param      mesh_file_prefix         File prefix for FEM mesh
!>@n
!>@n@param      sph_file_prefix
!>               File prefix for spherical harmonics mode files
!>@n
!>@n@param      field_file_prefix          File prefix for field data
!>@n@param      restart_file_prefix        File prefix for restart data
!>@n
!>@n@param      coriolis_int_file_name
!>                File name for hermonic integration for Coriolis term
!>@n@param      boundary_data_file_name
!>                File name for boundary conditions
!>@n@param      radial_field_file_name
!>                File name for reference radial data
!>@n@param      interpolate_sph_to_fem 
!>               File header for interpolation table
!>               from spherical grid to FEM grid
!>@n@param      interpolate_fem_to_sph  
!>               File header for interpolation table
!>               from FEM grid to spherical grid
!>@n
!>@n@param      mesh_file_fmt_ctl        mesh data  file format
!>@n@param      restart_file_fmt_ctl     restart data  file format
!>@n@param      field_file_fmt_ctl       field data  file format
!>@n@param      sph_file_fmt_ctl         spectr data  file format
!>@n@param      itp_file_fmt_ctl         interpolation data file format
!>@n@param      spectr_field_fmt_ctl     Spectr data file format
!>@n@param      coriolis_file_fmt_ctl    integration data  file format
!
      module t_ctl_data_4_platforms
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!
!>      Structure of parallel and file information
      type platform_data_control
!>        Block name
        character(len=kchara) :: block_name = 'data_files_def'
!
!>        Structure of number of subdomain control
        type(read_integer_item) :: ndomain_ctl
!>        Structure of number of OpenMP threads
        type(read_integer_item) :: num_smp_ctl
!
!>        Structure of debug flag
        type(read_character_item) :: debug_flag_ctl
!
!>        Structure of spherical harmonics index file prefix
        type(read_character_item) :: sph_file_prefix
!>        Structure of FEM mesh prefix
        type(read_character_item) :: mesh_file_prefix
!
!>        Structure of restart file prefix
        type(read_character_item) :: restart_file_prefix
!>        Structure of field file prefix
        type(read_character_item) :: field_file_prefix
!>        Structure of spectr field data file prefix
        type(read_character_item) :: spectr_field_file_prefix
!
!>        Structure of Gaunt integral data file prefix
        type(read_character_item) :: coriolis_int_file_name
!>        Structure of boundary condition data file prefix
        type(read_character_item) :: bc_data_file_name_ctl
!>        Structure of radial data file prefix
        type(read_character_item) :: radial_data_file_name_ctl
!
!>        Structure of interpolation table file prefix
!!        from spherical shell to FEM
        type(read_character_item) :: interpolate_sph_to_fem
!>        Structure of interpolation table file prefix
!!        from FEM to spherical shell
        type(read_character_item) :: interpolate_fem_to_sph
!
!>        Structure of Rayleigh spectr data directory name
        type(read_character_item) :: rayleigh_spectr_dir
!>        Structure of Rayleigh field data directory name
        type(read_character_item) :: rayleigh_field_dir
!
!>        Structure of spherical harmonics index file format
        type(read_character_item) :: sph_file_fmt_ctl
!>        Structure of FEM mesh format
        type(read_character_item) :: mesh_file_fmt_ctl
!>        Structure of restart file format
        type(read_character_item) :: restart_file_fmt_ctl
!>        Structure of spectr field data file format
        type(read_character_item) :: field_file_fmt_ctl
!>        Structure of spectr field data file format
        type(read_character_item) :: spectr_field_fmt_ctl
!>        Structure of interpolation table file format
        type(read_character_item) :: itp_file_fmt_ctl
!>        Structure of Gaunt integral data file format
        type(read_character_item) :: coriolis_file_fmt_ctl
!
!>        Structure of delete original data flag
        type(read_character_item) :: del_org_data_ctl
!
        integer(kind = kint) :: i_platform =     0
      end type platform_data_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine reset_control_platforms(plt)
!
      type(platform_data_control), intent(inout) :: plt
!
!
      plt%ndomain_ctl%iflag = 0
      plt%num_smp_ctl%iflag = 0
!
      plt%mesh_file_prefix%iflag =         0
      plt%field_file_prefix%iflag =        0
      plt%restart_file_prefix%iflag =      0
      plt%spectr_field_file_prefix%iflag = 0
!
      plt%sph_file_prefix%iflag =            0
      plt%rayleigh_spectr_dir%iflag =        0
      plt%rayleigh_field_dir%iflag =         0
!
      plt%coriolis_int_file_name%iflag =     0
      plt%bc_data_file_name_ctl%iflag =      0
      plt%radial_data_file_name_ctl%iflag =  0
      plt%interpolate_sph_to_fem%iflag = 0
      plt%interpolate_fem_to_sph%iflag = 0
!
      plt%mesh_file_fmt_ctl%iflag =     0
      plt%sph_file_fmt_ctl%iflag =      0
      plt%restart_file_fmt_ctl%iflag =  0
      plt%field_file_fmt_ctl%iflag =    0
      plt%itp_file_fmt_ctl%iflag =      0
      plt%spectr_field_fmt_ctl%iflag =  0
      plt%coriolis_file_fmt_ctl%iflag = 0
!
      plt%del_org_data_ctl%iflag = 0
!
      plt%i_platform = 0
!
      end subroutine reset_control_platforms
!
!  ---------------------------------------------------------------------
!
      subroutine copy_ctl_data_4_platform(org_plt, new_plt)
!
      type(platform_data_control), intent(in) :: org_plt
      type(platform_data_control), intent(inout) :: new_plt
!
!
      call copy_integer_ctl(org_plt%ndomain_ctl, new_plt%ndomain_ctl)
      call copy_integer_ctl(org_plt%num_smp_ctl, new_plt%num_smp_ctl)
!
      call copy_chara_ctl(org_plt%mesh_file_prefix,                     &
     &                    new_plt%mesh_file_prefix)
!
      call copy_chara_ctl(org_plt%field_file_prefix,                    &
     &                    new_plt%field_file_prefix)
      call copy_chara_ctl(org_plt%restart_file_prefix,                  &
     &                    new_plt%restart_file_prefix)
      call copy_chara_ctl(org_plt%spectr_field_file_prefix,             &
     &                    new_plt%spectr_field_file_prefix)
!
      call copy_chara_ctl(org_plt%sph_file_prefix,                      &
     &                    new_plt%sph_file_prefix)
!
      call copy_chara_ctl(org_plt%coriolis_int_file_name,               &
     &                     new_plt%coriolis_int_file_name)
      call copy_chara_ctl(org_plt%bc_data_file_name_ctl,                &
     &                    new_plt%bc_data_file_name_ctl)
      call copy_chara_ctl(org_plt%radial_data_file_name_ctl,            &
     &                    new_plt%radial_data_file_name_ctl)
!
      call copy_chara_ctl(org_plt%rayleigh_spectr_dir,                  &
     &                    new_plt%rayleigh_spectr_dir)
      call copy_chara_ctl(org_plt%rayleigh_field_dir,                   &
     &                    new_plt%rayleigh_field_dir)
!
      call copy_chara_ctl(org_plt%interpolate_sph_to_fem,               &
     &                    new_plt%interpolate_sph_to_fem)
      call copy_chara_ctl(org_plt%interpolate_fem_to_sph,               &
     &                    new_plt%interpolate_fem_to_sph)
!
      call copy_chara_ctl(org_plt%mesh_file_fmt_ctl,                    &
     &                    new_plt%mesh_file_fmt_ctl)
      call copy_chara_ctl(org_plt%restart_file_fmt_ctl,                 &
     &                    new_plt%restart_file_fmt_ctl)
      call copy_chara_ctl(org_plt%field_file_fmt_ctl,                   &
     &                    new_plt%field_file_fmt_ctl)
      call copy_chara_ctl(org_plt%sph_file_fmt_ctl,                     &
     &                    new_plt%sph_file_fmt_ctl)
      call copy_chara_ctl(org_plt%itp_file_fmt_ctl,                     &
     &                    new_plt%itp_file_fmt_ctl)
      call copy_chara_ctl(org_plt%spectr_field_fmt_ctl,                 &
     &                    new_plt%spectr_field_fmt_ctl)
      call copy_chara_ctl(org_plt%coriolis_file_fmt_ctl,                &
     &                    new_plt%coriolis_file_fmt_ctl)
!
      call copy_chara_ctl(org_plt%debug_flag_ctl,                       &
     &                    new_plt%debug_flag_ctl)
!
      call copy_chara_ctl(org_plt%del_org_data_ctl,                     &
     &                    new_plt%del_org_data_ctl)
!
      new_plt%i_platform = org_plt%i_platform
      new_plt%block_name = org_plt%block_name
!
      end subroutine copy_ctl_data_4_platform
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_platforms
