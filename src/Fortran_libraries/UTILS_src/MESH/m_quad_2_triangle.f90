!
!     module m_quad_2_triangle
!
!> @brief  Index table to devide one quadrature patch to triangles
!
!      written by H. Matsui on March, 2007
!
!>        table to devide quadrature to two triangles
!>@n@param   ie_quad_2_tri(i,j)   i... local triangle ID (1, 2, 3)
!>@n                                j... devided triangle  (1, 2)
!
!>@code
!!          4---3      4---3
!!          |   |      |  /|
!!          |   |  --> | / |
!!          |   |      |/  |
!!          1---2      1---2
!>@endcode
!
      module m_quad_2_triangle
!
      use m_precision
!
      implicit  none
!
!>   local index to construct triangle from one quadrature patch
!>     ie_quad_2_tri(i,j): i...triangle ID, j...devided ID
      integer (kind=kint), parameter :: ie_quad_2_tri(3,2)              &
     &               = reshape((/1, 2, 3,    1, 3, 4/), shape=(/3,2/))
!
!  ---------------------------------------------------------------------
!
!      contains
!
!  ---------------------------------------------------------------------
!
      end module m_quad_2_triangle
