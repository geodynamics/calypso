!>@file  m_fem_mesh_labels.f90
!!       module m_fem_mesh_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Comments for mesh data
!!
!!@verbatim
!!      character(len=ilen_fem_para) function hd_fem_para()
!!      character(len=ilen_fem_node) function hd_fem_node()
!!      character(len=ilen_fem_elem) function hd_fem_elem()
!!
!!      character(len=ilen_fem_import) function hd_fem_import()
!!      character(len=ilen_fem_export) function hd_fem_export()
!!
!!      character(len=ilen_fem_nodgrp) function hd_fem_nodgrp()
!!      character(len=ilen_fem_elegrp) function hd_fem_elegrp()
!!      character(len=ilen_fem_sfgrp) function hd_fem_sfgrp()
!!
!!      character(len=ilen_fem_para_sph) function hd_fem_para_sph()
!!      character(len=ilen_fem_node_sph) function hd_fem_node_sph()
!!
!!      character(len=ilen_fem_para_cyl) function hd_fem_para_cyl()
!!      character(len=ilen_fem_node_cyl) function hd_fem_node_cyl()
!!@endverbatim
!
      module m_fem_mesh_labels
!
      use m_precision
!
!>      length of hd_fem_para
      integer(kind = kint), parameter :: ilen_fem_para = 1+24+1+3
!>      length of hd_fem_node
      integer(kind = kint), parameter :: ilen_fem_node = 1+54+1+22+1+5
!>      length of hd_fem_elem
      integer(kind = kint), parameter :: ilen_fem_elem = 1+27+1+3
!
!>      length of hd_fem_import
      integer(kind = kint), parameter :: ilen_fem_import = 1+32+13+1+4
!>      length of hd_fem_export
      integer(kind = kint), parameter :: ilen_fem_export = 1+13+1+3
!
!>      length of hd_fem_nodgrp
      integer(kind = kint), parameter :: ilen_fem_nodgrp = 1+24+17+1+4
!>      length of hd_fem_elegrp
      integer(kind = kint), parameter :: ilen_fem_elegrp = 1+20+1+3
!>      length of hd_fem_sfgrp
      integer(kind = kint), parameter :: ilen_fem_sfgrp =  1+20+1+3
!
!>      length of hd_fem_nod_sph
      integer(kind = kint), parameter                                   &
     &      :: ilen_fem_para_sph = 1+17+26+1+4
!>      length of hd_fem_node
      integer(kind = kint), parameter                                   &
     &      :: ilen_fem_node_sph = 1+54+1+27+1+5
!
!>      length of hd_fem_nod_sph
      integer(kind = kint), parameter                                   &
     &      :: ilen_fem_para_cyl = 1+17+28+1+4
!>      length of hd_fem_node
      integer(kind = kint), parameter                                   &
     &      :: ilen_fem_node_cyl = 1+54+1+23+1+5
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_para) function hd_fem_para()
!
      hd_fem_para                                                       &
     &           = '!' // char(10)                                      &
     &          // '! 1.parallel information' // char(10)               &
     &          // '!' // char(10)
!
      end function hd_fem_para
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_node) function hd_fem_node()
!
      hd_fem_node                                                       &
     &      = '!' // char(10)                                           &
     &     // '! 2.mesh information (nodes and elements in partition)'  &
     &     // char(10)                                                  &
     &     // '!' // char(10)                                           &
     &     // '! 2.1 node (position) ' // char(10)                      &
     &     // '!' // char(10)
!
      end function hd_fem_node
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_elem) function hd_fem_elem()
!
      hd_fem_elem                                                       &
     &           = '!' // char(10)                                      &
     &          // '! 2.2 element (connection) ' // char(10)            &
     &          // '!' // char(10)
!
      end function hd_fem_elem
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_fem_import) function hd_fem_import()
!
      hd_fem_import                                                     &
     &           = '!' // char(10)                                      &
     &          // '! 3.import / export information ' // char(10)       &
     &          // '! 3.1 import ' // char(10)                          &
     &          // '!' // char(10)
!
      end function hd_fem_import
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_export) function hd_fem_export()
!
      hd_fem_export                                                     &
     &           = '!' // char(10)                                      &
     &          // '! 3.2 export ' // char(10)                          &
     &          // '!' // char(10)
!
      end function hd_fem_export
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_fem_nodgrp) function hd_fem_nodgrp()
!
!
      hd_fem_nodgrp                                                     &
     &           = '!' // char(10)                                      &
     &          // '! 4. group information  ' // char(10)               &
     &          // '! 4.1 node group ' // char(10)                      &
     &          // '!' // char(10)
!
      end function hd_fem_nodgrp
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_elegrp) function hd_fem_elegrp()
!
!
      hd_fem_elegrp                                                     &
     &           = '!' // char(10)                                      &
     &          // '! 4.2 element group ' // char(10)                   &
     &          // '!' // char(10)
!
      end function hd_fem_elegrp
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_sfgrp) function hd_fem_sfgrp()
!
!
      hd_fem_sfgrp                                                      &
     &           = '!' // char(10)                                      &
     &          // '! 4.3 surface group ' // char(10)                   &
     &          // '!' // char(10)
!
      end function hd_fem_sfgrp
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_fem_para_sph) function hd_fem_para_sph()
!
      hd_fem_para_sph                                                   &
     &      = '!' // char(10)                                           &
     &     // '!  node position ' // char(10)                           &
     &     // '!  by spherical coordinate' // char(10)                  &
     &     // '!' // char(10)
!
      end function hd_fem_para_sph
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_node_sph) function hd_fem_node_sph()
!
      hd_fem_node_sph                                                   &
     &      = '!' // char(10)                                           &
     &     // '! 2.mesh information (nodes and elements in partition)'  &
     &     // char(10)                                                  &
     &     // '!' // char(10)                                           &
     &     // '! 2.1 node (r, theta, phi) ' // char(10)                 &
     &     // '!' // char(10)
!
      end function hd_fem_node_sph
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_fem_para_cyl) function hd_fem_para_cyl()
!
      hd_fem_para_cyl                                                   &
     &      = '!' // char(10)                                           &
     &     // '!  node position ' // char(10)                           &
     &     // '!  by cylindrical coordinate' // char(10)                &
     &     // '!' // char(10)
!
      end function hd_fem_para_cyl
!
!------------------------------------------------------------------
!
      character(len=ilen_fem_node_cyl) function hd_fem_node_cyl()
!
      hd_fem_node_cyl                                                   &
     &      = '!' // char(10)                                           &
     &     // '! 2.mesh information (nodes and elements in partition)'  &
     &     // char(10)                                                  &
     &     // '!' // char(10)                                           &
     &     // '! 2.1 node (s, phi, z) ' // char(10)                     &
     &     // '!' // char(10)
!
      end function hd_fem_node_cyl
!
!------------------------------------------------------------------
!
      end module m_fem_mesh_labels
