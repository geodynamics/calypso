!m_control_params_2nd_files.f90
!      module m_control_params_2nd_files
!
!        programmed by H.Matsui on Nov., 2009
!
!      subroutine set_control_org_sph_mesh
!      subroutine set_control_org_fld_file_def
!
      module m_control_params_2nd_files
!
      use m_precision
!
      use m_constants
!
      implicit  none
!
!>      file header for original field data
      character(len=kchara) :: org_ucd_header =  "field_org/out"
!>      file header for original field data
      integer(kind=kint) :: ifmt_org_ucd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_sph_mesh
!
      use m_ctl_data_4_org_data
      use m_read_mesh_data
      use m_node_id_spherical_IO
      use m_field_data_IO
      use m_file_format_switch
!
!
      iflag_org_sph_rj_head = i_org_sph_mode_head
      if(iflag_org_sph_rj_head .gt. 0) then
        org_sph_rj_head =       org_sph_mode_head_ctl
      end if
!
      iflag_org_sph_spec_head = i_org_spectr_head
      if(iflag_org_sph_spec_head .gt. 0) then
        org_sph_spec_head =    org_spectr_file_head_ctl
      end if
!
      end subroutine set_control_org_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_fld_file_def
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_field_data_IO
      use m_field_file_format
      use m_file_format_switch
!
!
      iflag_org_rst_head = i_org_rst_head
      if (iflag_org_rst_head .gt. 0) then
        org_rst_header = orginal_restart_prefix
      end if
!
      if (i_org_udt_head .gt. 0) then
        org_ucd_header = org_udt_head_ctl
      end if
!
      call choose_ucd_file_format(udt_file_fmt_ctl,                     &
     &    i_udt_files_fmt, ifmt_org_ucd)
!
      end subroutine set_control_org_fld_file_def
!
! -----------------------------------------------------------------------
!
      end module m_control_params_2nd_files
