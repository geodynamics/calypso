!set_ucd_data.f90
!      module set_ucd_data
!
!        programmed by H.Matsui on July, 2006
!
!!      subroutine link_node_data_2_output(numnod, inod_gl, xx, ucd)
!!      subroutine link_ele_data_2_output(numele, nnod_4_ele,           &
!!     &          iele_gl, ie, ucd)
!!      subroutine link_num_field_2_output(numnod, ntot_comp_vis, ucd)
!!      subroutine link_field_data_2_output(numnod, num_phys, ntot_comp,&
!!     &          num_phys_vis, ntot_comp_vis, num_component,           &
!!     &          phy_name, d_nod, ucd)
!
      module set_ucd_data
!
      use m_precision
      use m_constants
!
      use t_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_node_data_2_output(numnod, inod_gl, xx, ucd)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), target, intent(in) :: inod_gl(numnod)
      real(kind = kreal), target, intent(in) :: xx(numnod,3)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod =         numnod
      ucd%inod_global => inod_gl(1:numnod)
      ucd%xx =>          xx(1:numnod,1:3)
!
      end subroutine link_node_data_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_ele_data_2_output(numele, nnod_4_ele,             &
     &          iele_gl, ie, ucd)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), target, intent(in) :: iele_gl(numele)
      integer(kind = kint), target, intent(in) :: ie(numele,nnod_4_ele)
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nele =         numele
      ucd%nnod_4_ele =   nnod_4_ele
      ucd%ie =>          ie(1:numele,1:nnod_4_ele)
      ucd%iele_global => iele_gl(1:numele)
!
      end subroutine link_ele_data_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_num_field_2_output(numnod, ntot_comp_vis, ucd)
!
      integer(kind = kint), intent(in) :: numnod, ntot_comp_vis
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod =      numnod
      ucd%ntot_comp = ntot_comp_vis
!
      end subroutine link_num_field_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_field_data_2_output(numnod, num_phys, ntot_comp,  &
     &          num_phys_vis, ntot_comp_vis, num_component,             &
     &          phy_name, d_nod, ucd)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: num_phys, num_phys_vis
      integer(kind = kint), intent(in) :: ntot_comp, ntot_comp_vis
      integer(kind = kint), target, intent(in)                          &
     &      :: num_component(num_phys)
      character(len = kchara), target, intent(in)                       &
     &      :: phy_name(num_phys)
      real(kind = kreal), target, intent(in)                            &
     &      :: d_nod(numnod,ntot_comp)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod =      numnod
      ucd%num_field = num_phys_vis
      ucd%ntot_comp = ntot_comp_vis
!
      ucd%num_comp =>  num_component(1:num_phys_vis)
      ucd%phys_name => phy_name(1:num_phys_vis)
!
      ucd%d_ucd =>     d_nod(1:numnod,1:ntot_comp_vis)
!
      end subroutine link_field_data_2_output
!
!-----------------------------------------------------------------------
!
      end module set_ucd_data
