!>@file   t_ctl_data_4_platforms.f90
!!        module t_ctl_data_4_platforms
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_control_platforms                               &
!!     &         (id_control, hd_block, plt, c_buf)
!!        type(platform_data_control), intent(inout) :: plt
!!      subroutine write_control_platforms                              &
!!     &         (id_file, hd_block, plt, level)
!!        type(platform_data_control), intent(in) :: plt
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
!!      radial_data_file_name       'radial_data.dat'
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
!>@n@param      radial_data_file_name
!>                File name for reference radial data
!>@n@param      interpolate_sph_to_fem_ctl 
!>               File header for interpolation table
!>               from spherical grid to FEM grid
!>@n@param      interpolate_fem_to_sph_ctl  
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
        type(read_character_item) :: interpolate_sph_to_fem_ctl
!>        Structure of interpolation table file prefix
!!        from FEM to spherical shell
        type(read_character_item) :: interpolate_fem_to_sph_ctl
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
!   file and domain controls
!
      character(len=kchara), parameter, private                         &
     &       :: hd_num_subdomain = 'num_subdomain_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_num_smp =       'num_smp_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_mesh_header =   'mesh_file_prefix'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_udt_header =    'field_file_prefix'
      character(len=kchara), parameter, private                         &
     &       :: hd_rst_header =    'restart_file_prefix'
      character(len=kchara), parameter, private                         &
     &       :: hd_spectr_header = 'spectr_field_file_prefix'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_sph_files_header =    'sph_file_prefix'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_rayleigh_spectr_dir = 'rayleigh_spectr_dir'
      character(len=kchara), parameter, private                         &
     &       :: hd_rayleigh_field_dir =  'rayleigh_field_dir'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_coriolis_tri_int_name = 'coriolis_int_file_name'
      character(len=kchara), parameter, private                         &
     &       :: hd_bc_data_file_name =     'boundary_data_file_name'
      character(len=kchara), parameter, private                         &
     &       :: hd_radial_data_file_name = 'radial_data_file_name'
      character(len=kchara), parameter, private                         &
     &       :: hd_itp_sph_to_fem =        'interpolate_sph_to_fem_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_itp_fem_to_sph =        'interpolate_fem_to_sph_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_mesh_file_fmt =      'mesh_file_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_rst_files_fmt =      'restart_file_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_udt_files_fmt =      'field_file_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_sph_files_fmt =      'sph_file_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_itp_files_fmt =      'itp_file_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_spect_field_fmt =    'spectr_field_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_coriolis_file_fmt =  'coriolis_file_fmt_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_debug_flag_ctl =     'debug_flag_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_del_org_data =       'delete_original_data_flag'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_platforms                                 &
     &         (id_control, hd_block, plt, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(platform_data_control), intent(inout) :: plt
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(plt%i_platform .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_subdomain, plt%ndomain_ctl)
        call read_integer_ctl_type(c_buf, hd_num_smp, plt%num_smp_ctl)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_mesh_header, plt%mesh_file_prefix)
!
        call read_chara_ctl_type(c_buf, hd_udt_header,                  &
     &      plt%field_file_prefix)
        call read_chara_ctl_type(c_buf, hd_rst_header,                  &
     &      plt%restart_file_prefix)
        call read_chara_ctl_type(c_buf, hd_spectr_header,               &
     &      plt%spectr_field_file_prefix)
!
        call read_chara_ctl_type(c_buf, hd_sph_files_header,            &
     &       plt%sph_file_prefix)
!
        call read_chara_ctl_type(c_buf, hd_coriolis_tri_int_name,       &
     &      plt%coriolis_int_file_name)
        call read_chara_ctl_type(c_buf, hd_bc_data_file_name,           &
     &      plt%bc_data_file_name_ctl)
        call read_chara_ctl_type(c_buf, hd_radial_data_file_name,       &
     &      plt%radial_data_file_name_ctl)
!
        call read_chara_ctl_type(c_buf, hd_itp_sph_to_fem,              &
     &      plt%interpolate_sph_to_fem_ctl)
        call read_chara_ctl_type(c_buf, hd_itp_fem_to_sph,              &
     &      plt%interpolate_fem_to_sph_ctl)
!
        call read_chara_ctl_type(c_buf, hd_rayleigh_spectr_dir,         &
     &       plt%rayleigh_spectr_dir)
        call read_chara_ctl_type(c_buf, hd_rayleigh_field_dir,          &
     &       plt%rayleigh_field_dir)
!
        call read_chara_ctl_type(c_buf, hd_mesh_file_fmt,               &
     &      plt%mesh_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_rst_files_fmt,               &
     &      plt%restart_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_udt_files_fmt,               &
     &      plt%field_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_sph_files_fmt,               &
     &      plt%sph_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_itp_files_fmt,               &
     &      plt%itp_file_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_spect_field_fmt,             &
     &      plt%spectr_field_fmt_ctl)
        call read_chara_ctl_type(c_buf, hd_coriolis_file_fmt,           &
     &      plt%coriolis_file_fmt_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_debug_flag_ctl, plt%debug_flag_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_del_org_data, plt%del_org_data_ctl)
       end do
       plt%i_platform = 1
!
      end subroutine read_control_platforms
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_platforms                                &
     &         (id_control, hd_block, plt, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(platform_data_control), intent(in) :: plt
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
      maxlen = max(maxlen, len_trim(hd_debug_flag_ctl))
      maxlen = max(maxlen, len_trim(hd_num_subdomain))
      maxlen = max(maxlen, len_trim(hd_num_smp))
      maxlen = max(maxlen, len_trim(hd_mesh_header))
      maxlen = max(maxlen, len_trim(hd_udt_header))
      maxlen = max(maxlen, len_trim(hd_rst_header))
      maxlen = max(maxlen, len_trim(hd_spectr_header))
      maxlen = max(maxlen, len_trim(hd_sph_files_header))
      maxlen = max(maxlen, len_trim(hd_coriolis_tri_int_name))
      maxlen = max(maxlen, len_trim(hd_bc_data_file_name))
      maxlen = max(maxlen, len_trim(hd_radial_data_file_name))
      maxlen = max(maxlen, len_trim(hd_itp_sph_to_fem))
      maxlen = max(maxlen, len_trim(hd_mesh_file_fmt))
      maxlen = max(maxlen, len_trim(hd_rst_files_fmt))
      maxlen = max(maxlen, len_trim(hd_udt_files_fmt))
      maxlen = max(maxlen, len_trim(hd_sph_files_fmt))
      maxlen = max(maxlen, len_trim(hd_itp_files_fmt))
      maxlen = max(maxlen, len_trim(hd_spect_field_fmt))
      maxlen = max(maxlen, len_trim(hd_itp_fem_to_sph))
      maxlen = max(maxlen, len_trim(hd_coriolis_file_fmt))
      maxlen = max(maxlen, len_trim(hd_del_org_data))
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_debug_flag_ctl, plt%debug_flag_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_num_subdomain, plt%ndomain_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_num_smp, plt%num_smp_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_mesh_header, plt%mesh_file_prefix)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_udt_header, plt%field_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rst_header, plt%restart_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_spectr_header, plt%spectr_field_file_prefix)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_sph_files_header, plt%sph_file_prefix)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rayleigh_spectr_dir, plt%rayleigh_spectr_dir)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rayleigh_field_dir, plt%rayleigh_field_dir)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_coriolis_tri_int_name, plt%coriolis_int_file_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_bc_data_file_name, plt%bc_data_file_name_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_radial_data_file_name, plt%radial_data_file_name_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_itp_sph_to_fem, plt%interpolate_sph_to_fem_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_itp_fem_to_sph, plt%interpolate_fem_to_sph_ctl)
!
      write(id_control,'(a)') '!'
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_mesh_file_fmt, plt%mesh_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rst_files_fmt, plt%restart_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_udt_files_fmt, plt%field_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_sph_files_fmt, plt%sph_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_itp_files_fmt, plt%itp_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_spect_field_fmt, plt%spectr_field_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_coriolis_file_fmt, plt%coriolis_file_fmt_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_del_org_data, plt%del_org_data_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_control_platforms
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
      plt%interpolate_sph_to_fem_ctl%iflag = 0
      plt%interpolate_fem_to_sph_ctl%iflag = 0
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
      call copy_chara_ctl(org_plt%interpolate_sph_to_fem_ctl,           &
     &                    new_plt%interpolate_sph_to_fem_ctl)
      call copy_chara_ctl(org_plt%interpolate_fem_to_sph_ctl,           &
     &                    new_plt%interpolate_fem_to_sph_ctl)
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
!
      end subroutine copy_ctl_data_4_platform
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_platforms
