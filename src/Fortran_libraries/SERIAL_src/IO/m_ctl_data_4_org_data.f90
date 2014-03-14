!
!      module m_ctl_data_4_org_data
!
!        programmed by H.Matsui on Oct., 2008
!
!
!      subroutine read_ctl_data_4_org_data
!
!  ---------------------------------------------------------------------
!
!  begin org_data_files_def
!    num_org_domain_ctl           2
!    org_mesh_head_ctl           'mesh_target/in'
!    orginal_sph_file_prefix     'sph_org/in_rj'
!
!    orginal_restart_prefix      'rst_org/rst'
!
!    org_field_file_head_ctl     'field_org/out'
!    org_spectr_file_head_ctl    'spectr_org/spectr'
!
!    org_mesh_file_fmt_ctl           'ascii'
!    org_restart_file_fmt_ctl        'ascii'
!    org_field_file_fmt_ctl          'ascii'
!    org_sph_file_fmt_ctl            'ascii'
!  end org_data_files_def
!
!  ---------------------------------------------------------------------
!
      module m_ctl_data_4_org_data
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: num_org_domain_ctl
!
      character(len=kchara) :: org_mesh_head_ctl
      character(len=kchara) :: org_sph_mode_head_ctl
!
      character(len=kchara) :: orginal_restart_prefix
!
      character(len=kchara) :: org_udt_head_ctl
      character(len=kchara) :: org_spectr_file_head_ctl
!
      character(len=kchara) :: org_mesh_file_fmt_ctl =    'ascii'
      character(len=kchara) :: org_sph_file_fmt_ctl =     'ascii'
      character(len=kchara) :: org_restart_file_fmt_ctl = 'ascii'
      character(len=kchara) :: org_udt_file_fmt_ctl =     'ascii'
!
!  label for group entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      integer (kind=kint) :: i_org_data =      0
!
!
!   file and domain controls
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_org_domain =    'num_org_domain_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_org_mesh_head =     'org_mesh_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_org_sph_mode_head = 'orginal_sph_file_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_org_rst_head =     'orginal_restart_prefix'
!
      character(len=kchara), parameter                                  &
     &       :: hd_org_udt_head =     'org_field_file_head_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_org_spectr_head =  'org_spectr_file_head_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_org_mesh_file_fmt = 'org_mesh_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_org_rst_files_fmt = 'org_restart_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_org_udt_files_fmt = 'org_field_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_org_sph_files_fmt = 'org_sph_file_fmt_ctl'
!
      integer(kind = kint) :: i_num_org_domain = 0
      integer(kind = kint) :: i_org_mesh_head =  0
      integer(kind = kint) :: i_org_sph_mode_head = 0
!
      integer(kind = kint) :: i_org_rst_head =    0
      integer(kind = kint) :: i_org_udt_head =    0
      integer(kind = kint) :: i_org_spectr_head = 0
!
      integer(kind = kint) :: i_org_mesh_file_fmt =   0
      integer(kind = kint) :: i_org_rst_files_fmt =   0
      integer(kind = kint) :: i_org_udt_files_fmt =   0
      integer(kind = kint) :: i_org_sph_files_fmt =   0
!
!
      private :: hd_org_data, i_org_data
      private :: hd_num_org_domain
      private :: hd_org_mesh_head, hd_org_sph_mode_head
      private :: hd_org_rst_head, hd_org_udt_head, hd_org_spectr_head
      private :: hd_org_mesh_file_fmt, hd_org_rst_files_fmt
      private :: hd_org_udt_files_fmt, hd_org_sph_files_fmt
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_org_data
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_org_data) .eq. 0) return
      if (i_org_data .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_org_data, i_org_data)
        if(i_org_data .gt. 0) exit
!
!
        call read_integer_ctl_item(hd_num_org_domain,                   &
     &        i_num_org_domain, num_org_domain_ctl)
!
!
        call read_character_ctl_item(hd_org_mesh_head,                  &
     &        i_org_mesh_head, org_mesh_head_ctl)
        call read_character_ctl_item(hd_org_sph_mode_head,              &
     &        i_org_sph_mode_head, org_sph_mode_head_ctl)
!
        call read_character_ctl_item(hd_org_rst_head,                   &
     &        i_org_rst_head, orginal_restart_prefix)
!
        call read_character_ctl_item(hd_org_udt_head,                   &
     &        i_org_udt_head, org_udt_head_ctl)
        call read_character_ctl_item(hd_org_spectr_head,                &
     &        i_org_spectr_head, org_spectr_file_head_ctl)
!
        call read_character_ctl_item(hd_org_mesh_file_fmt,              &
     &        i_org_mesh_file_fmt, org_mesh_file_fmt_ctl)
        call read_character_ctl_item(hd_org_rst_files_fmt,              &
     &        i_org_rst_files_fmt, org_restart_file_fmt_ctl)
        call read_character_ctl_item(hd_org_udt_files_fmt,              &
     &        i_org_udt_files_fmt, org_udt_file_fmt_ctl)
        call read_character_ctl_item(hd_org_sph_files_fmt,              &
     &        i_org_sph_files_fmt, org_sph_file_fmt_ctl)
      end do
!
      end subroutine read_ctl_data_4_org_data
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_org_data
