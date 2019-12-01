!
!     module m_node_quad_2_linear_sf
!.......................................................................
!
!      written by H. Matsui on Jan., 2007
!
!!      subroutine allocate_quad4_2_linear(nnod_4_ele)
!!      subroutine deallocate_quad4_2_linear
!
!
      module m_node_quad_2_linear_sf
!
      use m_precision
      use m_geometry_constants
!
      implicit  none
!
!
      integer (kind=kint) :: num_quad_2_tri
      integer (kind=kint), allocatable :: node_quad_2_tri(:,:)
!
      integer(kind = kint), parameter  :: nod_quad4_2_2tris(3,2)        &
     &     = reshape((/1, 2, 3,   3, 4, 1/), shape=(/3,2/))
!
      integer(kind = kint), parameter  :: nod_quad8_2_6tris(3,6)        &
     &     = reshape((/5, 6, 7,   7, 8, 5,   1, 5, 8,   2, 6, 5,        &
     &                 3, 7, 6,   4, 8, 7/), shape=(/3,6/))
!
      integer(kind = kint), parameter  :: nod_quad9_2_8tris(3,8)        &
     &     = reshape((/1, 5, 9,   9, 8, 1,   5, 2, 6,   6, 9, 5,        &
     &                 9, 6, 3,   3, 7, 9,   8, 9, 7,   7, 4, 8/),      &
     &       shape=(/3,8/))
!
      private :: allocate_quad4_2_linear_tri
      private :: allocate_quad8_2_linear_tri
      private :: allocate_quad9_2_linear_tri
      private :: nod_quad4_2_2tris, nod_quad8_2_6tris, nod_quad9_2_8tris
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_quad4_2_linear(nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
!
!
      if (nnod_4_ele .eq. num_t_lag ) then
        call allocate_quad4_2_linear_tri
      else if (nnod_4_ele .eq. num_t_quad ) then
        call allocate_quad8_2_linear_tri
      else if (nnod_4_ele .eq. num_t_linear ) then
        call allocate_quad9_2_linear_tri
      end if
!
       end subroutine allocate_quad4_2_linear
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_quad4_2_linear
!
!
      deallocate ( node_quad_2_tri )
!
      end subroutine deallocate_quad4_2_linear
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine allocate_quad4_2_linear_tri
!
!
       num_quad_2_tri = 2
       allocate ( node_quad_2_tri(num_triangle,num_quad_2_tri) )
       node_quad_2_tri(1:num_triangle,1:num_quad_2_tri)                 &
     &      = nod_quad4_2_2tris(1:num_triangle,1:num_quad_2_tri)
!
       end subroutine allocate_quad4_2_linear_tri
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_quad8_2_linear_tri
!
!
       num_quad_2_tri = 6
       allocate ( node_quad_2_tri(num_triangle,num_quad_2_tri) )
       node_quad_2_tri(1:num_triangle,1:num_quad_2_tri)                 &
     &      = nod_quad8_2_6tris(1:num_triangle,1:num_quad_2_tri)
!
       end subroutine allocate_quad8_2_linear_tri
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_quad9_2_linear_tri
!
!
       num_quad_2_tri = 8
       allocate ( node_quad_2_tri(num_triangle,num_quad_2_tri) )
       node_quad_2_tri(1:num_triangle,1:num_quad_2_tri)                 &
     &      = nod_quad9_2_8tris(1:num_triangle,1:num_quad_2_tri)
!
       end subroutine allocate_quad9_2_linear_tri
!
!  ---------------------------------------------------------------------
!
      end module m_node_quad_2_linear_sf
