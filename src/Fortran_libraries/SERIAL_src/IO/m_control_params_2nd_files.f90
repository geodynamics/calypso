!>@file   m_control_params_2nd_files.f90
!!@brief  module m_control_params_2nd_files
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for the second mesh data
!!
!!@verbatim
!!      subroutine set_control_org_sph_mesh
!!      subroutine set_control_org_fld_file_def
!!@endverbatim
!
      module m_control_params_2nd_files
!
      use m_precision
!
      use m_constants
!
      implicit  none
!
!>      file header for original spectrum indexing data
      character(len=kchara) :: org_sph_rj_head =      "sph_org/in_rj"
!>      file header for original spectrum indexing data
      integer(kind = kint) :: ifmt_org_sph_rj_head = 0
!>      file header for original spectrum indexing data
      integer(kind = kint) :: iflag_org_sph_rj_head = 0
!
!>      file header for original field data
      character(len=kchara) :: org_ucd_header =  "field_org/out"
!>      file header for original field data
      integer(kind=kint) :: ifmt_org_ucd
!
!>      file header for original restart data
      character(len=kchara) :: org_rst_header =   "rst_org/rst"
!>      file header for original restart data
      integer (kind=kint) :: ifmt_org_rst
!>      file header for original restart data
      integer (kind=kint) :: iflag_org_rst
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
      use m_file_format_switch
!
!
      iflag_org_sph_rj_head = org_sph_mode_head_ctl%iflag
      if(iflag_org_sph_rj_head .gt. 0) then
        org_sph_rj_head = org_sph_mode_head_ctl%charavalue
      end if
!
      call choose_file_format                                           &
     &   (org_sph_file_fmt_ctl, ifmt_org_sph_rj_head)
!
      end subroutine set_control_org_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine set_control_org_fld_file_def
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
      use m_field_file_format
      use m_file_format_switch
!
!
      iflag_org_rst = orginal_restart_prefix%iflag
      if(iflag_org_rst .gt. 0) then
        org_rst_header = orginal_restart_prefix%charavalue
      end if
!
      if (org_udt_head_ctl%iflag .gt. 0) then
        org_ucd_header = org_udt_head_ctl%charavalue
      end if
!
      call choose_ucd_file_format(restart_file_fmt_ctl%charavalue,      &
     &    restart_file_fmt_ctl%iflag, ifmt_org_rst)
      call choose_ucd_file_format(udt_file_fmt_ctl%charavalue,          &
     &    udt_file_fmt_ctl%iflag, ifmt_org_ucd)
!
      end subroutine set_control_org_fld_file_def
!
! -----------------------------------------------------------------------
!
      end module m_control_params_2nd_files
