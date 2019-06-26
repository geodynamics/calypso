!>@file  set_ucd_data.f90
!!       module set_ucd_data
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Link field data to IO structure for data IO
!!
!!@verbatim
!!      subroutine link_node_data_2_output(numnod, inod_gl, xx, ucd)
!!      subroutine link_ele_data_2_output(numele, nnod_4_ele,           &
!!     &          iele_gl, ie, ucd)
!!      subroutine link_num_field_2_output(numnod, ntot_comp_vis, ucd)
!!
!!      subroutine link_numnod_stacks_2_output                          &
!!     &         (nprocs, istack_numnod, istack_internod, m_ucd)
!!      subroutine link_numele_stacks_2_output                          &
!!     &          (nprocs, istack_interele, m_ucd)
!!        type(merged_ucd_data), intent(inout) :: m_ucd
!!@endverbatim
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
      integer(kind = kint_gl), target, intent(in) :: inod_gl(numnod)
      real(kind = kreal), target, intent(in) :: xx(numnod,3)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod =         numnod
      ucd%inod_global => inod_gl
      ucd%xx =>          xx
!
      end subroutine link_node_data_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_ele_data_2_output(numele, nnod_4_ele,             &
     &          iele_gl, ie, ucd)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint_gl), target, intent(in) :: iele_gl(numele)
      integer(kind = kint), target, intent(in) :: ie(numele,nnod_4_ele)
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nele =         numele
      ucd%nnod_4_ele =   nnod_4_ele
!
      call allocate_ucd_ele(ucd)
!
      if(numele .le. 0) return
!
!$omp parallel workshare
      ucd%ie(1:numele,1:nnod_4_ele) = ie(1:numele,1:nnod_4_ele)
      ucd%iele_global(1:numele) =     iele_gl(1:numele)
!$omp end parallel workshare
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
      ucd%num_comp =>  num_component
      ucd%phys_name => phy_name
!
      ucd%d_ucd =>     d_nod
!
      end subroutine link_field_data_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_numnod_stacks_2_output                            &
     &         (nprocs, istack_numnod, istack_internod, m_ucd)
!
      integer, intent(in) :: nprocs
      integer(kind = kint_gl), intent(in), target                       &
     &                        :: istack_numnod(0:nprocs)
      integer(kind = kint_gl), intent(in), target                       &
     &                        :: istack_internod(0:nprocs)
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      m_ucd%istack_merged_nod  =>   istack_numnod
      m_ucd%istack_merged_intnod => istack_internod
!
      end subroutine link_numnod_stacks_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_numele_stacks_2_output                            &
     &          (nprocs, istack_interele, m_ucd)
!
      integer, intent(in) :: nprocs
      integer(kind = kint_gl), intent(in), target                       &
     &                        :: istack_interele(0:nprocs)
!
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      m_ucd%istack_merged_ele =>    istack_interele
!
      end subroutine link_numele_stacks_2_output
!
!-----------------------------------------------------------------------
!
      end module set_ucd_data
