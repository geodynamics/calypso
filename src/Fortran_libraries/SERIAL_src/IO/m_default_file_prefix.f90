!>@file   m_default_file_prefix.f90
!!@brief  module m_default_file_prefix
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief List of default file prefixes
!!
      module m_default_file_prefix
!
      use m_precision
!
      implicit  none
!
!>      Mesh file
      character(len=kchara), parameter:: def_mesh_file_head = 'mesh/in'
!
!>      Element mesh file
      character(len=kchara), parameter                                  &
                  :: def_ele_mesh_head = 'mesh/element'
!>      Surface mesh file
      character(len=kchara), parameter                                  &
     &            :: def_surf_mesh_head = 'mesh/surface'
!>      Edge mesh file
      character(len=kchara), parameter                                  &
     &            :: def_edge_mesh_head = 'mesh/edge'
!
!>      Node file in spherical coordinate
      character(len=kchara), parameter                                  &
     &                   :: def_sph_mesh_head = 'mesh/node_sph'
!>      Node file in Cylindrical coordinate
      character(len=kchara), parameter                                  &
     &                   :: def_cyl_mesh_head = 'mesh/node_cyl'
!
!>      Mesh file
      character(len=kchara), parameter                                  &
     &                   :: def_org_mesh_head = 'mesh_org/in'
!>      file header for original spectrum indexing data
      character(len=kchara) :: def_org_sph_rj_head =      "sph_org/in_rj"
!>      file header for original field data
      character(len=kchara) :: def_org_ucd_header =  "field_org/out"
!>      file header for original restart data
      character(len=kchara) :: def_org_rst_header =   "rst_org/rst"
!
!>      file header for new mesh data
      character(len=kchara), parameter                                  &
     &           :: def_new_mesh_head = 'mesh_target/in'
!
      end module m_default_file_prefix
