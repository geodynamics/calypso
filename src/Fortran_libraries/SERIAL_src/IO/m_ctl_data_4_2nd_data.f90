!
!      module m_ctl_data_4_2nd_data
!
!        programmed by H.Matsui on Oct., 2008
!
!
!      subroutine read_ctl_data_4_new_data
!
!  ---------------------------------------------------------------------
!
!  begin new_data_files_def
!    num_new_domain_ctl           2
!
!    new_mesh_prefix            'mesh_target/in'
!    new_sph_mode_prefix        'sph_new/in_rj'
!
!    new_restart_prefix         'rst_new/rst'
!
!    new_field_file_prefix      'field_new/out'
!    new_spectr_file_prefix     'spectr_new/spectr'
!
!    new_vtk_prefix             'outall'
!
!    new_dx_head_prefix         'outall'
!    new_dx_data_prefix         'field_new/field'
!
!    new_mesh_file_fmt_ctl           'ascii'
!    new_rst_files_fmt_ctl           'ascii'
!    new_field_files_fmt_ctl         'ascii'
!    new_sph_files_fmt_ctl           'ascii'
!
!    delete_original_data_flag       'YES'
!  end new_data_files_def
!
!  ---------------------------------------------------------------------
!
      module m_ctl_data_4_2nd_data
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: num_new_domain_ctl
!
      character(len=kchara) :: new_mesh_prefix
      character(len=kchara) :: new_sph_mode_prefix
!
      character(len=kchara) :: new_restart_prefix
!
      character(len=kchara) :: new_field_file_prefix
      character(len=kchara) :: new_spectr_file_prefix
      character(len=kchara) :: new_vtk_prefix
!
      character(len=kchara) :: new_dx_head_prefix
      character(len=kchara) :: new_dx_data_prefix
!
      character(len=kchara) :: new_mesh_file_fmt_ctl = 'ascii'
      character(len=kchara) :: new_sph_file_fmt_ctl =  'ascii'
      character(len=kchara) :: new_rst_files_fmt_ctl = 'ascii'
      character(len=kchara) :: new_udt_file_fmt_ctl =  'ascii'
!
      character(len=kchara) :: del_org_data_ctl = 'NO'
!
!  label for group entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_new_data = 'new_data_files_def'
      integer (kind=kint) :: i_new_data =      0
!
!
!   file and domain controls
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_new_domain =    'num_new_domain_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_new_mesh_head =     'new_mesh_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_new_sph_mode_head = 'new_sph_mode_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_new_rst_head =     'new_restart_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_new_udt_head =     'new_field_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_new_spectr_head =  'new_spectr_file_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_new_vtk_head =     'new_vtk_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_new_dx_head =      'new_dx_head_prefix'
      character(len=kchara), parameter                                  &
     &       :: hd_new_dx_data_head = 'new_dx_data_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_new_mesh_file_fmt = 'new_mesh_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_new_rst_files_fmt = 'new_rst_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_new_udt_files_fmt = 'new_field_files_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_new_sph_files_fmt = 'new_sph_files_fmt_ctl'
!
      character(len=kchara), parameter                                  &
&            :: hd_del_org_data = 'delete_original_data_flag'
!
      integer(kind = kint) :: i_num_new_domain = 0
      integer(kind = kint) :: i_new_mesh_head =  0
      integer(kind = kint) :: i_new_sph_mode_head = 0
!
      integer(kind = kint) :: i_new_rst_head =    0
      integer(kind = kint) :: i_new_udt_head =    0
      integer(kind = kint) :: i_new_spectr_head = 0
      integer(kind = kint) :: i_new_vtk_head =    0
!
      integer(kind = kint) :: i_new_dx_head =      0
      integer(kind = kint) :: i_new_dx_data_head = 0
!
      integer(kind = kint) :: i_new_mesh_file_fmt =   0
      integer(kind = kint) :: i_new_rst_files_fmt =   0
      integer(kind = kint) :: i_new_udt_files_fmt =   0
      integer(kind = kint) :: i_new_sph_files_fmt =   0
!
      integer(kind = kint) :: i_del_org_data =   0
!
!
      private :: hd_new_data, i_new_data
      private :: hd_num_new_domain
      private :: hd_new_mesh_head, hd_new_sph_mode_head
      private :: hd_new_rst_head, hd_new_spectr_head
      private :: hd_new_udt_head, hd_new_vtk_head
      private :: hd_new_dx_head, hd_new_dx_data_head
      private :: hd_new_mesh_file_fmt, hd_new_rst_files_fmt
      private :: hd_new_udt_files_fmt, hd_new_sph_files_fmt
      private :: hd_del_org_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_new_data
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_new_data) .eq. 0) return
      if (i_new_data .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_new_data, i_new_data)
        if(i_new_data .gt. 0) exit
!
!
        call read_integer_ctl_item(hd_num_new_domain,                   &
     &        i_num_new_domain, num_new_domain_ctl)
!
!
        call read_character_ctl_item(hd_new_mesh_head,                  &
     &        i_new_mesh_head, new_mesh_prefix)
        call read_character_ctl_item(hd_new_sph_mode_head,              &
     &        i_new_sph_mode_head, new_sph_mode_prefix)
!
        call read_character_ctl_item(hd_new_rst_head,                   &
     &          i_new_rst_head, new_restart_prefix)
!
        call read_character_ctl_item(hd_new_udt_head,                   &
     &          i_new_udt_head, new_field_file_prefix)
        call read_character_ctl_item(hd_new_spectr_head,                &
     &          i_new_spectr_head, new_spectr_file_prefix)
        call read_character_ctl_item(hd_new_vtk_head,                   &
     &          i_new_vtk_head, new_vtk_prefix)
!
        call read_character_ctl_item(hd_new_dx_head,                    &
     &        i_new_dx_head, new_dx_head_prefix)
        call read_character_ctl_item(hd_new_dx_data_head,               &
     &        i_new_dx_data_head, new_dx_data_prefix)
!
        call read_character_ctl_item(hd_new_mesh_file_fmt,              &
     &        i_new_mesh_file_fmt, new_mesh_file_fmt_ctl)
        call read_character_ctl_item(hd_new_rst_files_fmt,              &
     &        i_new_rst_files_fmt, new_rst_files_fmt_ctl)
        call read_character_ctl_item(hd_new_udt_files_fmt,              &
     &        i_new_udt_files_fmt, new_udt_file_fmt_ctl)
        call read_character_ctl_item(hd_new_sph_files_fmt,              &
     &        i_new_sph_files_fmt, new_sph_file_fmt_ctl)
!
        call read_character_ctl_item(hd_del_org_data,                   &
     &        i_del_org_data, del_org_data_ctl)
      end do
!
      end subroutine read_ctl_data_4_new_data
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_2nd_data
