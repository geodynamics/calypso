!>@file   ctl_data_platforms_IO.f90
!!        module ctl_data_platforms_IO
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
      module ctl_data_platforms_IO
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_integer
      use t_ctl_data_4_platforms
!
      implicit  none
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
     &       :: hd_radial_data_file_name = 'radial_field_file_name'
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
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
!
      if(plt%i_platform .le. 0) return
!
      maxlen = len_trim(hd_debug_flag_ctl)
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
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
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
     &    hd_sph_files_header, plt%sph_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rst_header, plt%restart_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_udt_header, plt%field_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_spectr_header, plt%spectr_field_file_prefix)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_mesh_file_fmt, plt%mesh_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_sph_files_fmt, plt%sph_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rst_files_fmt, plt%restart_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_udt_files_fmt, plt%field_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_spect_field_fmt, plt%spectr_field_fmt_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_bc_data_file_name, plt%bc_data_file_name_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_radial_data_file_name, plt%radial_data_file_name_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rayleigh_spectr_dir, plt%rayleigh_spectr_dir)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_rayleigh_field_dir, plt%rayleigh_field_dir)
!
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_coriolis_tri_int_name, plt%coriolis_int_file_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_itp_sph_to_fem, plt%interpolate_sph_to_fem_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_itp_fem_to_sph, plt%interpolate_fem_to_sph_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_coriolis_file_fmt, plt%coriolis_file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_itp_files_fmt, plt%itp_file_fmt_ctl)
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
      end module  ctl_data_platforms_IO
