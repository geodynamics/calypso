!>@file   t_ctl_data_4_platforms.f90
!!@brief  module t_ctl_data_4_platforms
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_control_platforms(hd_block, iflag, plt)
!!        type(platform_data_control), intent(inout) :: plt
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
!!
!!      interpolate_sph_to_fem_ctl  'sph_shell/sph_to_fem'
!!      interpolate_fem_to_sph_ctl  'sph_shell/fem_to_sph'
!!
!!      field_file_prefix           'field/out'
!!      restart_file_prefix         'restart/rst'
!!
!!      spectr_field_file_prefix    'sph_spectr/spectr'
!!
!!
!!      mesh_file_fmt_ctl           'ascii'
!!      restart_file_fmt_ctl        'ascii'
!!      field_file_fmt_ctl          'ucd_ascii'
!!      sph_file_fmt_ctl            'ascii'
!!      spectr_field_fmt_ctl        'ascii'
!!      itp_file_fmt_ctl            'ascii'
!!      coriolis_file_fmt_ctl       'ascii'
!!
!!      memory_conservation_ctl     'YES'
!!      FEM_mesh_output_switch      'NO'
!!      FEM_surface_output_switch   'NO'
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
!>
!>
!>@n@param      memory_conservation_ctl    memory conservation flag
!>              File format for spherical shell grid data
!
      module t_ctl_data_4_platforms
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
      type platform_data_control
        type(read_integer_item) :: ndomain_ctl
        type(read_integer_item) :: num_smp_ctl
!
        type(read_character_item) :: mesh_file_prefix
!
        type(read_character_item) :: field_file_prefix
        type(read_character_item) :: restart_file_prefix
        type(read_character_item) :: spectr_field_file_prefix
!
        type(read_character_item) :: sph_file_prefix
!
        type(read_character_item) :: coriolis_int_file_name
        type(read_character_item) :: bc_data_file_name_ctl
        type(read_character_item) :: interpolate_sph_to_fem_ctl
        type(read_character_item) :: interpolate_fem_to_sph_ctl
!
        type(read_character_item) :: mesh_file_fmt_ctl
        type(read_character_item) :: sph_file_fmt_ctl
        type(read_character_item) :: restart_file_fmt_ctl
        type(read_character_item) :: field_file_fmt_ctl
        type(read_character_item) :: itp_file_fmt_ctl
        type(read_character_item) :: spectr_field_fmt_ctl
        type(read_character_item) :: coriolis_file_fmt_ctl
!
        type(read_character_item) :: debug_flag_ctl
!
        type(read_character_item) :: memory_conservation_ctl
        type(read_character_item) :: FEM_mesh_output_switch
        type(read_character_item) :: FEM_surface_output_switch
!
        type(read_character_item) :: del_org_data_ctl
!
!         Deprecated
        type(read_character_item) :: excluding_FEM_mesh_ctl
      end type platform_data_control
!
!   file and domain controls
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_subdomain = 'num_subdomain_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_num_smp =   'num_smp_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mesh_header = 'mesh_file_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_udt_header =   'field_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_rst_header =   'restart_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_spectr_header =   'spectr_field_file_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_sph_files_header = 'sph_file_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_coriolis_tri_int_name = 'coriolis_int_file_name'
      character(len=kchara), parameter                                  &
     &       :: hd_bc_data_file_name = 'boundary_data_file_name'
      character(len=kchara), parameter                                  &
     &       :: hd_itp_sph_to_fem =  'interpolate_sph_to_fem_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_itp_fem_to_sph =  'interpolate_fem_to_sph_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mesh_file_fmt =  'mesh_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_rst_files_fmt =  'restart_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_udt_files_fmt =  'field_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_sph_files_fmt =  'sph_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_itp_files_fmt =  'itp_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_spect_field_fmt =  'spectr_field_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_coriolis_file_fmt =  'coriolis_file_fmt_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_debug_flag_ctl =  'debug_flag_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mem_conserve =   'memory_conservation_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_FEM_mesh_output = 'FEM_mesh_output_switch'
      character(len=kchara), parameter                                  &
     &       :: hd_FEM_surf_output = 'FEM_surface_output_switch'
!
      character(len=kchara), parameter                                  &
     &       :: hd_del_org_data = 'delete_original_data_flag'
!
!       Deprecated
!
      character(len=kchara), parameter                                  &
     &       :: hd_exclude_FEM_mesh = 'excluding_FEM_mesh_ctl'
!
!
      private :: hd_num_subdomain, hd_num_smp, hd_sph_files_header
      private :: hd_mesh_header, hd_udt_header, hd_rst_header
      private :: hd_spectr_header, hd_bc_data_file_name
      private :: hd_mesh_file_fmt, hd_rst_files_fmt
      private :: hd_udt_files_fmt, hd_sph_files_fmt
      private :: hd_debug_flag_ctl, hd_mem_conserve
      private :: hd_coriolis_tri_int_name
      private :: hd_itp_sph_to_fem, hd_itp_fem_to_sph
      private :: hd_itp_files_fmt, hd_coriolis_file_fmt
      private :: hd_spect_field_fmt
      private :: hd_FEM_mesh_output, hd_FEM_surf_output
      private :: hd_exclude_FEM_mesh, hd_del_org_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_platforms(hd_block, iflag, plt)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(platform_data_control), intent(inout) :: plt
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if(iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iflag)
        if(iflag .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_num_subdomain, plt%ndomain_ctl)
        call read_integer_ctl_type(hd_num_smp, plt%num_smp_ctl)
!
!
        call read_chara_ctl_type(hd_mesh_header, plt%mesh_file_prefix)
!
        call read_chara_ctl_type(hd_udt_header, plt%field_file_prefix)
        call read_chara_ctl_type(hd_rst_header,                         &
     &      plt%restart_file_prefix)
        call read_chara_ctl_type(hd_spectr_header,                      &
     &      plt%spectr_field_file_prefix)
!
        call read_chara_ctl_type(hd_sph_files_header,                   &
     &       plt%sph_file_prefix)
!
        call read_chara_ctl_type(hd_coriolis_tri_int_name,              &
     &      plt%coriolis_int_file_name)
        call read_chara_ctl_type(hd_bc_data_file_name,                  &
     &      plt%bc_data_file_name_ctl)
!
        call read_chara_ctl_type(hd_itp_sph_to_fem,                     &
     &      plt%interpolate_sph_to_fem_ctl)
        call read_chara_ctl_type(hd_itp_fem_to_sph,                     &
     &      plt%interpolate_fem_to_sph_ctl)
!
        call read_chara_ctl_type(hd_mesh_file_fmt,                      &
     &      plt%mesh_file_fmt_ctl)
        call read_chara_ctl_type(hd_rst_files_fmt,                      &
     &      plt%restart_file_fmt_ctl)
        call read_chara_ctl_type(hd_udt_files_fmt,                      &
     &      plt%field_file_fmt_ctl)
        call read_chara_ctl_type(hd_sph_files_fmt,                      &
     &      plt%sph_file_fmt_ctl)
        call read_chara_ctl_type(hd_itp_files_fmt,                      &
     &      plt%itp_file_fmt_ctl)
        call read_chara_ctl_type(hd_spect_field_fmt,                    &
     &      plt%spectr_field_fmt_ctl)
        call read_chara_ctl_type(hd_coriolis_file_fmt,                  &
     &      plt%coriolis_file_fmt_ctl)
!
        call read_chara_ctl_type(hd_debug_flag_ctl, plt%debug_flag_ctl)
        call read_chara_ctl_type(hd_mem_conserve,                       &
     &      plt%memory_conservation_ctl)
        call read_chara_ctl_type(hd_FEM_mesh_output,                    &
     &      plt%FEM_mesh_output_switch)
        call read_chara_ctl_type(hd_FEM_surf_output,                    &
     &      plt%FEM_surface_output_switch)
!
        call read_chara_ctl_type(hd_exclude_FEM_mesh,                   &
     &      plt%excluding_FEM_mesh_ctl)
!
        call read_chara_ctl_type(hd_del_org_data, plt%del_org_data_ctl)
       end do
!
      end subroutine read_control_platforms
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_platforms
