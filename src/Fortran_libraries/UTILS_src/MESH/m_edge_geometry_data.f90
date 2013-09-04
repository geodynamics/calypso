!
!      module m_edge_geometry_data
!
!> @brief edge geometry data
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_edge_geometry
!      subroutine allocate_edge_vectors
!      subroutine allocate_edge_vector_sph
!      subroutine allocate_edge_vector_cyl
!
!      subroutine deallocate_edge_geometry
!      subroutine deallocate_edge_vectors
!      subroutine deallocate_edge_vector_sph
!      subroutine deallocate_edge_vector_cyl
!
      module m_edge_geometry_data
!
      use m_precision
!
      implicit  none
!
!
!>   position of center of edge
      real(kind=kreal)  , allocatable  :: x_edge(:,:)
!>   distance from the center of edge
      real(kind=kreal)  , allocatable  :: r_edge(:)
!>   1/r_edge
      real(kind=kreal)  , allocatable  :: ar_edge(:)
!>   longitude of center of edge
      real(kind=kreal)  , allocatable  :: phi_edge(:)
!>   colatitude of center of edge
      real(kind=kreal)  , allocatable  :: theta_edge(:)
!>   cylindorical radius of center of edge
      real(kind=kreal)  , allocatable  :: s_edge(:)
!>   1 / s_edge
      real(kind=kreal)  , allocatable  :: as_edge(:)
!
!>   length of each edge
      real (kind=kreal), allocatable :: edge_length(:)
!>   1 / edge_length
      real (kind=kreal), allocatable :: a_edge_length(:)
!
!>   edge vector
      real (kind=kreal), allocatable :: edge_vect(:,:)
!>   edge vector (spherical coordinate)
      real (kind=kreal), allocatable :: edge_vect_sph(:,:)
!>   edge vector (cylindrical coordinate)
      real (kind=kreal), allocatable :: edge_vect_cyl(:,:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_geometry
!
      use m_geometry_parameter
!
      allocate( x_edge(numedge,3) )
!
      allocate( r_edge(numedge) )
      allocate( ar_edge(numedge) )
      allocate( phi_edge(numedge) )
      allocate( theta_edge(numedge) )
!
      allocate( s_edge(numedge) )
      allocate( as_edge(numedge) )
!
      x_edge =      0.0d0
!
      r_edge =      0.0d0
      ar_edge =     0.0d0
      phi_edge =    0.0d0
      theta_edge =  0.0d0
!
      s_edge =      0.0d0
      as_edge =     0.0d0
!
      end subroutine allocate_edge_geometry
!
! ------------------------------------------------------
!
      subroutine allocate_edge_vectors
!
      use m_geometry_parameter
!
      allocate( edge_length(numedge) )
      allocate( a_edge_length(numedge) )
      allocate( edge_vect(numedge,3) )
!
      edge_length =   0.0d0
      a_edge_length = 0.0d0
      edge_vect =     0.0d0
!
      end subroutine allocate_edge_vectors
!
! ------------------------------------------------------
!
      subroutine allocate_edge_vector_sph
!
      use m_geometry_parameter
!
      allocate( edge_vect_sph(numedge,3) )
      edge_vect_sph =     0.0d0
!
      end subroutine allocate_edge_vector_sph
!
! ------------------------------------------------------
!
      subroutine allocate_edge_vector_cyl
!
      use m_geometry_parameter
!
      allocate( edge_vect_cyl(numedge,3) )
      edge_vect_cyl =     0.0d0
!
      end subroutine allocate_edge_vector_cyl
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_edge_geometry
!
      deallocate( x_edge )
!
      deallocate( r_edge )
      deallocate( ar_edge )
      deallocate( phi_edge )
      deallocate( theta_edge )
!
      deallocate( s_edge )
      deallocate( as_edge )
!
      end subroutine deallocate_edge_geometry
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_vectors
!
      deallocate( edge_length )
      deallocate( a_edge_length )
      deallocate( edge_vect )
!
      end subroutine deallocate_edge_vectors
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_vector_sph
!
      deallocate( edge_vect_sph )
!
      end subroutine deallocate_edge_vector_sph
!
! ------------------------------------------------------
!
      subroutine deallocate_edge_vector_cyl
!
      deallocate( edge_vect_cyl )
!
      end subroutine deallocate_edge_vector_cyl
!
! ------------------------------------------------------
!
      end module m_edge_geometry_data
