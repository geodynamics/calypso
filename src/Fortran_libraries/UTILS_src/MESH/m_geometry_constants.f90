!>@file   m_geometry_constants.f90
!!@brief  module m_geometry_constants
!!
!!@author H. Matsui
!!@date Programmed in May, 2009
!
!>@brief Constants for geometry data
!!
      module m_geometry_constants
!
      use m_precision
!
      implicit  none
!
!
!>      integer flag for certecian coordinate @f$ (x,y,z) @f$
      integer(kind=kint), parameter :: iflag_certecian =  0
!>      integer flag for spherical coordinate @f$ (r,\theta,\phi) @f$
      integer(kind=kint), parameter :: iflag_spherical =  1
!>      integer flag for Cylindrical coordinate @f$ (s,\phi,z) @f$
      integer(kind=kint), parameter :: iflag_cylindrical = 2
!
!
!>   number of nodes in each Lagrange element
      integer(kind=kint), parameter :: num_t_lag =   27
!>   number of nodes in each quadrature element
      integer(kind=kint), parameter :: num_t_quad =  20
!>   number of nodes in each linear element
      integer(kind=kint), parameter :: num_t_linear = 8
!
!>   number of nodes in each Lagrange surface
      integer(kind=kint), parameter :: num_lag_sf =    9
!>   number of nodes in each quadrature surface
      integer(kind=kint), parameter :: num_quad_sf =   8
!>   number of nodes in each linear surface
      integer(kind=kint), parameter :: num_linear_sf = 4
!>   number of nodes in each triangle
      integer(kind=kint), parameter :: num_triangle =  3
!
!>   number of nodes in quadrature edge
      integer(kind=kint), parameter :: num_quad_edge =   3
!>   number of nodes in linear edge
      integer(kind=kint), parameter :: num_linear_edge = 2
!
!>   number of surface in each element
      integer(kind=kint), parameter :: nsurf_4_ele =  6
!>   number of edge in each element
      integer(kind=kint), parameter :: nedge_4_ele = 12
!>   number of edge in each surface
      integer(kind=kint), parameter :: nedge_4_surf = 4
!>   number of end point for edge
      integer(kind=kint), parameter :: nend_4_edge =  2
!
!
!
!>   local node id for surface in one element for linear element 
!!   (node index,surface index)
      integer (kind=kint), parameter  :: node_on_sf_4(4,6)              &
     &     = reshape( (/  1,  5,  8,  4,    2,  3,  7,  6,              &
     &                    1,  2,  6,  5,    4,  8,  7,  3,              &
     &                    1,  4,  3,  2,    5,  6,  7,  8/),            &
     &        shape=(/4,6/))
!
!
!>   local node id for opposit surface in one element
!!   for linear element (node index,surface index)
      integer (kind=kint), parameter  :: node_on_sf_n_4(4,6)            &
     &     = reshape( (/  2,  6,  7,  3,   1,  4,  8,  5,               &
     &                    4,  3,  7,  8,   1,  5,  6,  2,               &
     &                    5,  8,  7,  6,   1,  2,  3,  4/),             &
     &        shape=(/4,6/))
!
!>   local node id for surface in one element for quadrature element 
!!   (node index,surface index)
      integer (kind=kint), parameter  :: node_on_sf_8(8,6)              &
     &     = reshape( (/  1,  5,  8,  4,   17, 16, 20, 12,              &
     &                    2,  3,  7,  6,   10, 19, 14, 18,              &
     &                    1,  2,  6,  5,    9, 18, 13, 17,              &
     &                    4,  8,  7,  3,   20, 15, 19, 11,              &
     &                    1,  4,  3,  2,   12, 11, 10,  9,              &
     &                    5,  6,  7,  8,   13, 14, 15, 16/),            &
     &        shape=(/8,6/))
!
!
!>   local node id for opposit surface in one element
!!   for quadrature element (node index,surface index)
      integer (kind=kint), parameter  :: node_on_sf_n_8(8,6)            &
     &     = reshape( (/  2,  6,  7,  3,   18, 14, 19, 10,              &
     &                    1,  4,  8,  5,   12, 20, 16, 17,              &
     &                    4,  3,  7,  8,   11, 19, 15, 20,              &
     &                    1,  5,  6,  2,   17, 13, 18,  9,              &
     &                    5,  8,  7,  6,   16, 15, 14, 13,              &
     &                    1,  2,  3,  4,    9, 10, 11, 12/),            &
     &        shape=(/8,6/))
!
!>   local node id for surface in one element for lagrande element 
!!   (node index,surface index)
      integer (kind=kint), parameter  :: node_on_sf_9(9,6)              &
     &     = reshape( (/  1,  5,  8,  4,   17, 16, 20, 12,   21,        &
     &                    2,  3,  7,  6,   10, 19, 14, 18,   22,        &
     &                    1,  2,  6,  5,    9, 18, 13, 17,   23,        &
     &                    4,  8,  7,  3,   20, 15, 19, 11,   24,        &
     &                    1,  4,  3,  2,   12, 11, 10,  9,   25,        &
     &                    5,  6,  7,  8,   13, 14, 15, 16,   26/),      &
     &        shape=(/9,6/))
!
!
!>   local node id for opposit surface in one element
!!   for lagrande element (node index,surface index)
      integer (kind=kint), parameter  :: node_on_sf_n_9(9,6)            &
     &     = reshape( (/  2,  6,  7,  3,   18, 14, 19, 10,   22,        &
     &                    1,  4,  8,  5,   12, 20, 16, 17,   21,        &
     &                    4,  3,  7,  8,   11, 19, 15, 20,   24,        &
     &                    1,  5,  6,  2,   17, 13, 18,  9,   23,        &
     &                    5,  8,  7,  6,   16, 15, 14, 13,   26,        &
     &                    1,  2,  3,  4,    9, 10, 11, 12,   25/),      &
     &        shape=(/9,6/))
!
!
!
!>   local node id for edge in one element for linear element
!!   (node index,edge index)
      integer (kind=kint), parameter  :: node_on_edge_l(2,12)           &
     &     = reshape(                                                   &
     &       (/ 1,  2,         2,  3,         4,  3,        1,  4,      &
     &          5,  6,         6,  7,         8,  7,        5,  8,      &
     &          1,  5,         2,  6,         3,  7,        4,  8/),    &
     &        shape=(/2,12/))
!
!>   local node id for edge in one element for quadrature element
!!   (node index,edge index)
      integer (kind=kint), parameter  :: node_on_edge_q(3,12)           &
     &     = reshape(                                                   &
     &      (/ 1,  9,  2,     2, 10,  3,     4, 11,  3,    1, 12,  4,   &
     &         5, 13,  6,     6, 14,  7,     8, 15,  7,    5, 16,  8,   &
     &         1, 17,  5,     2, 18,  6,     3, 19,  7,    4, 20,  8/), &
     &       shape=(/3,12/))
!
!>    local surface axis in one element (surface index)
      integer (kind = kint), parameter :: ishape_dir_surf(2,6)          &
     &     = reshape(                                                   &
     &         (/ 3, 2,   2, 3,   1, 3,   3, 1,   2, 1,   1, 2/),       &
     &        shape=(/2,6/))
!
!
!
!>   local node id for edge in one surface for linear element
!!   (node index,edge index)
      integer (kind=kint), parameter  :: node_on_edge_sf_l(2,4)         &
     &     = reshape(                                                   &
     &       (/ 1,  2,         2,  3,         4,  3,        1,  4/),    &
     &        shape=(/2,4/))
!
!>   local node id for edge in one surface for quadrature element
!!   (node index,edge index)
      integer (kind=kint), parameter  :: node_on_edge_sf_q(3,4)         &
     &    = reshape(                                                    &
     &      (/ 1,  5,  2,     2,  6,  3,     4,  7,  3,    1,  8,  4/), &
     &        shape=(/3,4/))
!
!
!>    local edge direction in one element (edge index)
      integer (kind = kint), parameter :: ishape_dir_edge(12)           &
     &       = (/ 1, 2, 1, 2,   1, 2, 1, 2,   3, 3, 3, 3/)
!
!
      end module m_geometry_constants
!
