!set_nnod_for_ele_by_type.f90
!     module set_nnod_for_ele_by_type
!
!> @brief Set number of element from element type ID in mesh data
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine set_num_node_for_ele_by_etype(ierr)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(inout) :: ele
!        integer(kind = kint), intent(inout) :: ierr
!
!>@n List of the element type is the following:
!!
!>@n@code
!! +--------------+
!! | ELEMENT-TYPE |
!! +--------------+
!!
!!   ROD :             111  1-2
!!                     112  1-3:2
!!   2D  : triangle    211  1-2-3
!!                     212  1-2-3:4-5-6
!!   2D  : quad.       221  1-2-3-4
!!                     222  1-2-3-4:5-6-7-8
!!                     223  1-2-3-4:5-6-7-8:9
!!   3D  : tet.        311  1-2-3-4
!!                     312  1-2-3-4:5-6-7:8-9-10
!!   3D  : prism       321  1-2-3-4-5-6
!!                     322  1-2-3-4-5-6:7-8-9:10-11-12:13-14-15
!!   3D  : hexa.       331  1-2-3-4-5-6-7-8
!!                     332  1-2-3-4-5-6-7-8:9-10-11-12:13-14-15-16
!!                         :17-18-19-20
!!                     333  1-2-3-4-5-6-7-8:9-10-11-12:13-14-15-16
!!                         :17-18-19-20:21-22-23-24-25-26:27
!!
!!   master-slave(tri) 411  1:2-3-4 
!!                     412  1:2-3-4:5-6-7
!!   master-slave(quad)421  1:2-3-4-5 
!!                     422  1:2-3-4-6:6-7-8-9
!!
!!   joint (tri)       511 (1-2-3)*(4-5-6)
!!                     512 (1-2-3:4-5-6)*(7-8-9:10-11-12)
!!   joint (quad)      521 (1-2-3-4)*(5-6-7-8)
!!                     522 (1-2-3-4:5-6-7-8)*(9-10-11-12:13-14-15-16)
!!
!!   beam              611  1-2
!!                     612  1-2:3
!!
!!   shell: triangle   711  1-2-3
!!                     712  1-2-3:4-5-6
!!   shell: quad.      721  1-2-3-4
!!                     722  1-2-3-4:5-6-7-8
!!===
!>@endcode
!
      module set_nnod_for_ele_by_type
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_num_node_for_ele_by_etype(node, ele, ierr)
!
      use t_geometry_data
      use set_nnod_for_element
!
      type(node_data), intent(in) :: node
      type(element_data), intent(inout) :: ele
      integer(kind = kint), intent(inout) :: ierr
!
!
      call set_nnod_for_ele_by_eletype(ele%numele, ele%elmtyp,          &
     &    ele%nodelm, ierr)
      call check_wrong_element_list(node%numnod, ele%numele,            &
     &    ele%nnod_4_ele, ele%nodelm, ele%ie, ierr)
!
      end subroutine set_num_node_for_ele_by_etype
!
!   --------------------------------------------------------------------
!
      end module set_nnod_for_ele_by_type
