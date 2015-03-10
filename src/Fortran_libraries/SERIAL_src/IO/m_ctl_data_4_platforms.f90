!>@file   m_ctl_data_4_platforms.f90
!!@brief  module m_ctl_data_4_platforms
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!> @brief Control input routine for data file headers
!!
!!@verbatim
!!      subroutine read_ctl_data_4_platform
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
!!      elem_file_prefix            'mesh/in_element'
!!      surf_file_prefix            'mesh/in_surface'
!!      edge_file_prefix            'mesh/in_edge'
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
!!      spectr_file_head_ctl        'sph_spectr/spectr'
!!
!!
!!      mesh_file_fmt_ctl           'ascii'
!!      restart_file_fmt_ctl        'ascii'
!!      field_file_fmt_ctl          'ucd_ascii'
!!      sph_file_fmt_ctl            'ascii'
!!      spectr_file_fmt_ctl         'ascii'
!!      itp_file_fmt_ctl            'ascii'
!!      coriolis_file_fmt_ctl       'ascii'
!!
!!      mesh_extension_flags_ctl    'ON'
!!      memory_conservation_ctl     'YES'
!!      excluding_FEM_mesh_ctl      'NO'
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
!>@n@param      elem_file_prefix
!>                File prefix for FEM element comm. table
!>@n@param      surf_file_prefix           File prefix for surface data
!>@n@param      edge_file_prefix           File prefix for edge data
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
!>@n@param      spectr_file_fmt_ctl      Spectr data file format
!>@n@param      coriolis_file_fmt_ctl    integration data  file format
!>
!>
!>@n@param      memory_conservation_ctl    memory conservation flag
!>              File format for spherical shell grid data
!
      module m_ctl_data_4_platforms
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
      type(read_integer_item), save :: ndomain_ctl
      type(read_integer_item), save :: num_smp_ctl
!
      type(read_character_item), save :: mesh_file_prefix
!
      type(read_character_item), save :: elem_file_prefix
      type(read_character_item), save :: surf_file_prefix
      type(read_character_item), save :: edge_file_prefix
!
!
      type(read_character_item), save :: udt_file_head_ctl
      type(read_character_item), save :: restart_file_prefix
      type(read_character_item), save :: spectr_file_head_ctl
!
      type(read_character_item), save :: sph_file_prefix
!
      type(read_character_item), save :: coriolis_int_file_name
      type(read_character_item), save :: bc_data_file_name_ctl
      type(read_character_item), save :: interpolate_sph_to_fem_ctl
      type(read_character_item), save :: interpolate_fem_to_sph_ctl
!
      type(read_character_item), save :: mesh_file_fmt_ctl
      type(read_character_item), save :: sph_file_fmt_ctl
      type(read_character_item), save :: restart_file_fmt_ctl
      type(read_character_item), save :: udt_file_fmt_ctl
      type(read_character_item), save :: itp_file_fmt_ctl
      type(read_character_item), save :: spectr_file_fmt_ctl
      type(read_character_item), save :: coriolis_file_fmt_ctl
!
      type(read_character_item), save :: debug_flag_ctl
!
      type(read_character_item), save :: memory_conservation_ctl
      type(read_character_item), save :: mesh_extension_ctl
      type(read_character_item), save :: excluding_FEM_mesh_ctl
!
!     Label for the entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      integer (kind=kint) :: i_platform =   0
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
      character(len=kchara), parameter                                  &
     &       :: hd_elem_header = 'elem_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_surf_header = 'surf_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_edge_header = 'edge_file_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_udt_header =   'field_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_rst_header =   'restart_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_spectr_header =   'spectr_file_head_ctl'
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
     &       :: hd_spect_files_fmt =  'spectr_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_coriolis_file_fmt =  'coriolis_file_fmt_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_debug_flag_ctl =  'debug_flag_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_mem_conserve =   'memory_conservation_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_mesh_extension = 'mesh_extension_flags_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_exclude_FEM_mesh = 'excluding_FEM_mesh_ctl'
!
!
      private :: hd_platform, i_platform
      private :: hd_num_subdomain, hd_num_smp, hd_sph_files_header
      private :: hd_mesh_header, hd_elem_header, hd_surf_header
      private :: hd_udt_header, hd_rst_header
      private :: hd_spectr_header, hd_bc_data_file_name
      private :: hd_edge_header
      private :: hd_mesh_file_fmt, hd_rst_files_fmt
      private :: hd_udt_files_fmt, hd_sph_files_fmt
      private :: hd_debug_flag_ctl, hd_mem_conserve
      private :: hd_coriolis_tri_int_name, hd_mesh_extension
      private :: hd_itp_sph_to_fem, hd_itp_fem_to_sph
      private :: hd_itp_files_fmt, hd_coriolis_file_fmt
      private :: hd_spect_files_fmt
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_platform
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_platform) .eq. 0) return
      if (i_platform .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_platform, i_platform)
        if(i_platform .gt. 0) exit
!
!
        call read_integer_ctl_type(hd_num_subdomain, ndomain_ctl)
        call read_integer_ctl_type(hd_num_smp, num_smp_ctl)
!
!
        call read_chara_ctl_type(hd_mesh_header, mesh_file_prefix)
!
        call read_chara_ctl_type(hd_elem_header, elem_file_prefix)
        call read_chara_ctl_type(hd_surf_header, surf_file_prefix)
        call read_chara_ctl_type(hd_edge_header, edge_file_prefix)
!
        call read_chara_ctl_type(hd_udt_header, udt_file_head_ctl)
        call read_chara_ctl_type(hd_rst_header, restart_file_prefix)
        call read_chara_ctl_type(hd_spectr_header,                      &
     &      spectr_file_head_ctl)
!
        call read_chara_ctl_type(hd_sph_files_header, sph_file_prefix)
!
        call read_chara_ctl_type(hd_coriolis_tri_int_name,              &
     &      coriolis_int_file_name)
        call read_chara_ctl_type(hd_bc_data_file_name,                  &
     &      bc_data_file_name_ctl)
!
        call read_chara_ctl_type(hd_itp_sph_to_fem,                     &
     &      interpolate_sph_to_fem_ctl)
        call read_chara_ctl_type(hd_itp_fem_to_sph,                     &
     &      interpolate_fem_to_sph_ctl)
!
        call read_chara_ctl_type(hd_mesh_file_fmt, mesh_file_fmt_ctl)
        call read_chara_ctl_type(hd_rst_files_fmt,                      &
     &      restart_file_fmt_ctl)
        call read_chara_ctl_type(hd_udt_files_fmt, udt_file_fmt_ctl)
        call read_chara_ctl_type(hd_sph_files_fmt, sph_file_fmt_ctl)
        call read_chara_ctl_type(hd_itp_files_fmt, itp_file_fmt_ctl)
        call read_chara_ctl_type(hd_spect_files_fmt,                    &
     &      spectr_file_fmt_ctl)
        call read_chara_ctl_type(hd_coriolis_file_fmt,                  &
     &      coriolis_file_fmt_ctl)
!
        call read_chara_ctl_type(hd_debug_flag_ctl, debug_flag_ctl)
        call read_chara_ctl_type(hd_mem_conserve,                       &
     &      memory_conservation_ctl)
        call read_chara_ctl_type(hd_mesh_extension, mesh_extension_ctl)
        call read_chara_ctl_type(hd_exclude_FEM_mesh,                   &
     &      excluding_FEM_mesh_ctl)
       end do
!
      end subroutine read_ctl_data_4_platform
!
!  ---------------------------------------------------------------------
!
      end module  m_ctl_data_4_platforms
