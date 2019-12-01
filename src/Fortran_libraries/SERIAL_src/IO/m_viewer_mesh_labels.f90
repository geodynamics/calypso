!>@file  m_viewer_mesh_labels.f90
!!       module m_viewer_mesh_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2017
!
!> @brief Comments for viewer mesh data
!!
!!@verbatim
!!      character(len=ilen_ndomain_viewer) function hd_ndomain_viewer()
!!      character(len=ilen_node_viewer) function hd_node_viewer()
!!      character(len=ilen_surf_viewer) function hd_surf_viewer()
!!      character(len=ilen_edge_viewer) function hd_edge_viewer()
!!      character(len=ilen_edge_on_sf_viewer)                           &
!!     &                        function hd_edge_on_sf_viewer()
!!
!!      character(len=ilen_domain_nod_grp) function hd_domain_nod_grp()
!!      character(len=ilen_domain_surf_grp)                             &
!!     &             function hd_domain_surf_grp()
!!      character(len=ilen_domain_edge_grp)                             &
!!     &             function hd_domain_edge_grp()
!!
!!      character(len=ilen_ele_surf_grp) function hd_ele_surf_grp()
!!      character(len=ilen_ele_nod_grp) function hd_ele_nod_grp()
!!      character(len=ilen_ele_edge_grp) function hd_ele_edge_grp()
!!
!!      character(len=ilen_surf_surf_grp) function hd_surf_surf_grp()
!!      character(len=ilen_surf_nod_grp) function hd_edge_nod_grp()
!!      character(len=ilen_surf_edge_grp) function hd_surf_edge_grp()
!!@endverbatim
!
      module m_viewer_mesh_labels
!
!
      use m_precision
!
!>      length of ilen_ndomain_viewer
      integer(kind = kint), parameter                                   &
     &      :: ilen_ndomain_viewer = 1+21+31+31+31+1+6
!>      length of hd_node_viewer
      integer(kind = kint), parameter                                   &
     &      :: ilen_node_viewer = 1+21+21+21+1+5
!>      length of hd_surf_viewer
      integer(kind = kint), parameter                                   &
     &      :: ilen_surf_viewer = 1+25+25+25+1+5
!>      length of hd_edge_viewer
      integer(kind = kint), parameter                                   &
     &      :: ilen_edge_viewer = 1+25+25+25+1+5
!>      length of hd_edge_on_sf_viewer
      integer(kind = kint), parameter                                   &
     &      :: ilen_edge_on_sf_viewer = 1+26+1+3
!
!>      length of hd_domain_nod_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_domain_nod_grp = 1+36+1+3
!>      length of hd_domain_surf_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_domain_surf_grp = 1+36+1+3
!>      length of hd_domain_edge_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_domain_edge_grp = 1+36+1+3
!
!>      length of hd_ele_surf_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_ele_surf_grp = 20+1+2
!>      length of hd_ele_nod_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_ele_nod_grp = 20+1+2
!>      length of hd_ele_edge_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_ele_edge_grp = 20+1+2
!
!>      length of hd_surf_surf_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_surf_surf_grp = 20+1+2
!>      length of hd_surf_nod_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_surf_nod_grp = 20+1+2
!>      length of hd_surf_edge_grp
      integer(kind = kint), parameter                                   &
     &      :: ilen_surf_edge_grp = 20+1+2
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_ndomain_viewer) function hd_ndomain_viewer()
!
      hd_ndomain_viewer                                                 &
     &      = '!'  // char(10)                                          &
     &     // '! 0. number of domain' // char(10)                       &
     &     // '!   stack of node for domain   '  // char(10)            &
     &     // '!   stack of surface for domain'  // char(10)            &
     &     // '!   stack of edge for domain   '  // char(10)            &
     &     // '!'  // char(10)
!
      end function hd_ndomain_viewer
!
!------------------------------------------------------------------
!
      character(len=ilen_node_viewer) function hd_node_viewer()
!
      hd_node_viewer                                                    &
     &      = '!'  // char(10)                                          &
     &     // '! 1. node information' // char(10)                       &
     &     // '!      number_of node' // char(10)                       &
     &     // '!  Global ID, x, y, z' // char(10)                       &
     &     // '!'  // char(10)
!
      end function hd_node_viewer
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_viewer) function hd_surf_viewer()
!
      hd_surf_viewer                                                    &
     &      = '!'  // char(10)                                          &
     &     // '! 2. element information ' // char(10)                   &
     &     // '! element type           ' // char(10)                   &
     &     // '! Global ID, connectivity' // char(10)                   &
     &     // '!'  // char(10)
!
      end function hd_surf_viewer
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_viewer) function hd_edge_viewer()
!
      hd_edge_viewer                                                    &
     &      = '!'  // char(10)                                          &
     &     // '! 3. edge information    ' // char(10)                   &
     &     // '!    edge type           ' // char(10)                   &
     &     // '! Global ID, connectivity' // char(10)                   &
     &     // '!'  // char(10)
!
      end function hd_edge_viewer
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_on_sf_viewer)                             &
     &                        function hd_edge_on_sf_viewer()
!
      hd_edge_on_sf_viewer                                              &
     &      = '!'  // char(10)                                          &
     &     // '! 3.2 edge ID for surfaces' // char(10)                  &
     &     // '!'  // char(10)
!
      end function hd_edge_on_sf_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_domain_nod_grp) function hd_domain_nod_grp()
!
      hd_domain_nod_grp                                                 &
     &      = '!'  // char(10)                                          &
     &     // '! 4. node ID for domain boundary    ' // char(10)        &
     &     // '!'  // char(10)
!
      end function hd_domain_nod_grp
!
!------------------------------------------------------------------
!
      character(len=ilen_domain_surf_grp)                               &
     &             function hd_domain_surf_grp()
!
      hd_domain_surf_grp                                                &
     &      = '!'  // char(10)                                          &
     &     // '! 4.1 surface ID for domain boundary' // char(10)        &
     &     // '!'  // char(10)
!
      end function hd_domain_surf_grp
!
!------------------------------------------------------------------
!
      character(len=ilen_domain_edge_grp)                               &
     &             function hd_domain_edge_grp()
!
      hd_domain_edge_grp                                                &
     &      = '!'  // char(10)                                          &
     &     // '! 4.2 edge ID for domain boundary   ' // char(10)        &
     &     // '!'  // char(10)
!
      end function hd_domain_edge_grp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_ele_surf_grp) function hd_ele_surf_grp()
!
      hd_ele_surf_grp                                                   &
     &      = '! 4.2.1 element data' // char(10)                        &
     &     // '!'  // char(10)
!
      end function hd_ele_surf_grp
!
!------------------------------------------------------------------
!
      character(len=ilen_ele_nod_grp) function hd_ele_nod_grp()
!
      hd_ele_nod_grp                                                    &
     &      = '! 4.2.2 node data   ' // char(10)                        &
     &     // '!'  // char(10)
!
      end function hd_ele_nod_grp
!
!------------------------------------------------------------------
!
      character(len=ilen_ele_edge_grp) function hd_ele_edge_grp()
!
      hd_ele_edge_grp                                                   &
     &      = '! 4.2.3 edge data   ' // char(10)                        &
     &     // '!'  // char(10)
!
      end function hd_ele_edge_grp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_surf_surf_grp) function hd_surf_surf_grp()
!
      hd_surf_surf_grp                                                  &
     &      = '! 4.3.1 element data' // char(10)                        &
     &     // '!'  // char(10)
!
      end function hd_surf_surf_grp
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_nod_grp) function hd_surf_nod_grp()
!
      hd_surf_nod_grp                                                   &
     &      = '! 4.3.2 node data   ' // char(10)                        &
     &     // '!'  // char(10)
!
      end function hd_surf_nod_grp
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_edge_grp) function hd_surf_edge_grp()
!
      hd_surf_edge_grp                                                  &
     &      = '! 4.3.3 edge data   ' // char(10)                        &
     &     // '!'  // char(10)
!
      end function hd_surf_edge_grp
!
!------------------------------------------------------------------
!
      end module m_viewer_mesh_labels
