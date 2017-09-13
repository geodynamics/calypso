!>@file  m_fem_surface_labels.f90
!!       module m_fem_surface_labels
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2017
!
!> @brief Comments for surface mesh data
!!
!!@verbatim
!!      character(len=ilen_ecomm_para) function hd_ecomm_para()
!!      character(len=ilen_ecomm_import) function hd_ecomm_import()
!!      character(len=ilen_ecomm_export) function hd_ecomm_export()
!!
!!      character(len=ilen_ecomm_point) function hd_ecomm_point()
!!      character(len=ilen_ecomm_vol) function hd_ecomm_vol()
!!
!!      character(len=ilen_surf_para) function hd_surf_para()
!!      character(len=ilen_surf_connect) function hd_surf_connect()
!!      character(len=ilen_surf_on_ele) function hd_surf_on_ele()
!!      character(len=ilen_surf_import) function hd_surf_import()
!!      character(len=ilen_surf_export) function hd_surf_export()
!!
!!      character(len=ilen_surf_point) function hd_surf_point()
!!      character(len=ilen_surf_norm) function hd_surf_norm()
!!      character(len=ilen_surf_area) function hd_surf_area()
!!
!!      character(len=ilen_edge_para) function hd_edge_para()
!!      character(len=ilen_edge_connect) function hd_edge_connect()
!!      character(len=ilen_edge_on_surf) function hd_edge_on_surf()
!!      character(len=ilen_edge_on_ele) function hd_edge_on_ele()
!!      character(len=ilen_edge_import) function hd_edge_import()
!!      character(len=ilen_edge_export) function hd_edge_export()
!!
!!      character(len=ilen_edge_point) function hd_edge_point()
!!      character(len=ilen_edge_dir) function hd_edge_dir()
!!      character(len=ilen_edge_length) function hd_edge_length()
!!@endverbatim
!
      module m_fem_surface_labels
!
      use m_precision
!>      length of ilen_ecomm_para
      integer(kind = kint), parameter                                   &
     &      :: ilen_ecomm_para = 1+19+26+1+4
!>      length of hd_ecomm_import
      integer(kind = kint), parameter                                   &
     &      :: ilen_ecomm_import = 1+31+27+1+4
!>      length of hd_ecomm_export
      integer(kind = kint), parameter                                   &
     &      :: ilen_ecomm_export = 1+27+1+3
!
!>      length of hd_ecomm_point
      integer(kind = kint), parameter                                   &
     &      :: ilen_ecomm_point = 1+23+1+34+1+5
!>      length of hd_ecomm_vol
      integer(kind = kint), parameter :: ilen_ecomm_vol = 1+23+1+3
!
!>      length of hd_surf_para
      integer(kind = kint), parameter :: ilen_surf_para = 1+23+26+1+4
!>      length of hd_surf_connect
      integer(kind = kint), parameter                                   &
     &      :: ilen_surf_connect = 1+26+28+28+1+5
!>      length of hd_surf_on_ele
      integer(kind = kint), parameter                                   &
     &      :: ilen_surf_on_ele = 1+34+33+30+1+5
!>      length of hd_surf_import
      integer(kind = kint), parameter :: ilen_surf_import = 1+31+27+1+4
!>      length of hd_surf_export
      integer(kind = kint), parameter :: ilen_surf_export = 1+27+1+3
!
!>      length of hd_surf_point
      integer(kind = kint), parameter :: ilen_surf_point = 1+23+25+1+4
!>      length of hd_surf_norm
      integer(kind = kint), parameter :: ilen_surf_norm = 1+31+1+3
!>      length of hd_surf_area
      integer(kind = kint), parameter :: ilen_surf_area = 1+21+1+3
!
!>      length of hd_edge_para
      integer(kind = kint), parameter :: ilen_edge_para = 1+20+26+1+4
!>      length of hd_edge_connect
      integer(kind = kint), parameter                                   &
     &      :: ilen_edge_connect = 1+23+25+28+1+5
!>      length of hd_edge_on_surf
      integer(kind = kint), parameter :: ilen_edge_on_surf = 1+32+1+3
!>      length of hd_edge_on_ele
      integer(kind = kint), parameter :: ilen_edge_on_ele = 1+33+1+3
!>      length of hd_edge_import
      integer(kind = kint), parameter :: ilen_edge_import = 1+31+25+1+4
!>      length of hd_edge_export
      integer(kind = kint), parameter :: ilen_edge_export = 1+24+1+3
!
!>      length of hd_edge_point
      integer(kind = kint), parameter :: ilen_edge_point = 1+22+23+1+4
!>      length of hd_edge_dir
      integer(kind = kint), parameter :: ilen_edge_dir = 1+25+1+3
!>      length of hd_edge_length
      integer(kind = kint), parameter :: ilen_edge_length = 1+22+1+3
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      character(len=ilen_ecomm_para) function hd_ecomm_para()
!
      hd_ecomm_para                                                     &
     &      = '!'  // char(10)                                          &
     &     // '!  element position' // char(10)                         &
     &     // '!  and communication table' // char(10)                  &
     &     // '!'  // char(10)
!
      end function hd_ecomm_para
!
!------------------------------------------------------------------
!
      character(len=ilen_ecomm_import) function hd_ecomm_import()
!
      hd_ecomm_import                                                   &
     &      = '!' // char(10)                                           &
     &     // '! 2.import / export information' // char(10)             &
     &     // '! 2.1 element ID for import' // char(10)                 &
     &     // '!' // char(10)
!
      end function hd_ecomm_import
!
!------------------------------------------------------------------
!
      character(len=ilen_ecomm_export) function hd_ecomm_export()
!
      hd_ecomm_export                                                   &
     &      = '!' // char(10)                                           &
     &     // '! 2.2 element ID for export' // char(10)                 &
     &     // '!' // char(10)
!
      end function hd_ecomm_export
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_ecomm_point) function hd_ecomm_point()
!
      hd_ecomm_point                                                    &
     &      = '!' // char(10)                                           &
     &     // '! 3.element information' // char(10)                     &
     &     // '!' // char(10)                                           &
     &     // '! 3.1 center of element (position)' // char(10)          &
     &     // '!' // char(10)
!
      end function hd_ecomm_point
!
!------------------------------------------------------------------
!
      character(len=ilen_ecomm_vol) function hd_ecomm_vol()
!
      hd_ecomm_vol                                                      &
     &      = '!' // char(10)                                           &
     &     // '! 3.2 Volume of element' // char(10)                     &
     &     // '!' // char(10)
!
      end function hd_ecomm_vol
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_surf_para) function hd_surf_para()

      hd_surf_para                                                      &
     &      = '!'  // char(10)                                          &
     &     // '!  surface connectivity' // char(10)                     &
     &     // '!  and communication table' // char(10)                  &
     &     // '!'  // char(10)
!
      end function hd_surf_para
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_connect) function hd_surf_connect()
!
      hd_surf_connect                                                   &
     &      = '!' // char(10)                                           &
     &     // '!  2  surface connectivity' // char(10)                  &
     &     // '!  2.1  surface connectivity' // char(10)                &
     &     // '!      (type and connection)' // char(10)                &
     &     // '!' // char(10)
!
      end function hd_surf_connect
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_on_ele) function hd_surf_on_ele()
!
      hd_surf_on_ele                                                    &
     &      = '!' // char(10)                                           &
     &     // '!  2.2 surface id for each element' // char(10)          &
     &     // '!        positive: outward normal' // char(10)           &
     &     // '!        normal: inward normal' // char(10)              &
     &     // '!' // char(10)
!
      end function hd_surf_on_ele
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_import) function hd_surf_import()
!
      hd_surf_import                                                    &
     &      = '!' // char(10)                                           &
     &     // '! 3.import / export information' // char(10)             &
     &     // '! 3.1 surface ID for import' // char(10)                 &
     &     // '!' // char(10)
!
      end function hd_surf_import
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_export) function hd_surf_export()
!
      hd_surf_export                                                    &
     &      = '!' // char(10)                                           &
     &     // '! 3.2 surface ID for export' // char(10)                 &
     &     // '!' // char(10)
!
      end function hd_surf_export
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_surf_point) function hd_surf_point()
!
      hd_surf_point                                                     &
     &      = '!' // char(10)                                           &
     &     // '! 4.  geometry of surface' // char(10)                   &
     &     // '! 4.1 center of surface' // char(10)                     &
     &     // '!' // char(10)
!
      end function hd_surf_point
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_norm) function hd_surf_norm()
!
      hd_surf_norm                                                      &
     &      = '!' // char(10)                                           &
     &     // '!  4.2 normal vector of surface' // char(10)             &
     &     // '!' // char(10)
!
      end function hd_surf_norm
!
!------------------------------------------------------------------
!
      character(len=ilen_surf_area) function hd_surf_area()
!
      hd_surf_area                                                      &
     &      = '!' // char(10)                                           &
     &     // '! 4.3 area of surface' // char(10)                       &
     &     // '!' // char(10)

      end function hd_surf_area
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_edge_para) function hd_edge_para()
!
      hd_edge_para                                                      &
     &      = '!'  // char(10)                                          &
     &     // '!  edge connectivity' // char(10)                        &
     &     // '!  and communication table' // char(10)                  &
     &     // '!'  // char(10)
!
      end function hd_edge_para
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_connect) function hd_edge_connect()
!
      hd_edge_connect                                                   &
     &      = '!' // char(10)                                           &
     &     // '!  2  edge connectivity' // char(10)                     &
     &     // '!  2.1  edge connectivity' // char(10)                   &
     &     // '!      (type and connection)' // char(10)                &
     &     // '!' // char(10)
!
      end function hd_edge_connect
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_on_surf) function hd_edge_on_surf()
!
      hd_edge_on_surf                                                   &
     &      = '!' // char(10)                                           &
     &     // '!  2.2  edge id for each surface' // char(10)            &
     &     // '!' // char(10)
!
      end function hd_edge_on_surf
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_on_ele) function hd_edge_on_ele()
!
      hd_edge_on_ele                                                    &
     &      = '!' // char(10)                                           &
     &     // '!  2.3   edge id for each element' // char(10)           &
     &     // '!' // char(10)
!
      end function hd_edge_on_ele
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_import) function hd_edge_import()
!
      hd_edge_import                                                    &
     &      = '!' // char(10)                                           &
     &     // '! 3.import / export information' // char(10)             &
     &     // '! 3.1 edge ID for import' // char(10)                    &
     &     // '!' // char(10)
!
      end function hd_edge_import
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_export) function hd_edge_export()
!
      hd_edge_export                                                    &
     &      = '!' // char(10)                                           &
     &     // '! 3.2 edge ID for export' // char(10)                    &
     &     // '!' // char(10)
!
      end function hd_edge_export
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      character(len=ilen_edge_point) function hd_edge_point()
!
      hd_edge_point                                                     &
     &      = '!' // char(10)                                           &
     &     // '! 4.   geometry of edge' // char(10)                     &
     &     // '!  4.1. center of edge' // char(10)                      &
     &     // '!' // char(10)
!
      end function hd_edge_point
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_dir) function hd_edge_dir()
!
      hd_edge_dir                                                       &
     &      = '!' // char(10)                                           &
     &     // '!  4.2  direction of edge' // char(10)                   &
     &     // '!' // char(10)
!
      end function hd_edge_dir
!
!------------------------------------------------------------------
!
      character(len=ilen_edge_length) function hd_edge_length()
!
      hd_edge_length                                                    &
     &      = '!' // char(10)                                           &
     &     // '!  4.3  length of edge' // char(10)                      &
     &     // '!' // char(10)
!
      end function hd_edge_length
!
!------------------------------------------------------------------
!
      end module m_fem_surface_labels
