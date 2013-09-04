!
!      module find_grp_ele_id_4_node
!
!        programmed by H.Matsui on Oct., 2006 (ver 1.1)
!
!      subroutine count_grp_iele_4_node(numnod, numele, nnod_4_ele, ie, &
!     &          nele_grp, iele_grp, nele_4_node)
!      subroutine set_grp_iele_4_node(numnod, numele, nnod_4_ele, ie,   &
!     &          nele_grp, iele_grp, ntot_ele_4_node, iele_stack_4_node,&
!     &          nele_4_node, iele_4_node, iconn_4_node)
!
      module find_grp_ele_id_4_node
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_grp_iele_4_node(numnod, numele, nnod_4_ele, ie,  &
     &          nele_grp, iele_grp, nele_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
      integer (kind = kint) :: inod, inum, iele, k
!
!
      nele_4_node = 0
!
      do inum = 1, nele_grp
        iele = iele_grp(inum)
        do k=1, nnod_4_ele
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
        end do
      end do
!
      end  subroutine count_grp_iele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine set_grp_iele_4_node(numnod, numele, nnod_4_ele, ie,    &
     &          nele_grp, iele_grp, ntot_ele_4_node, iele_stack_4_node, &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: nele_grp
      integer (kind=kint), intent(in) :: iele_grp(nele_grp)
      integer (kind=kint), intent(in) :: ntot_ele_4_node
      integer (kind=kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
      integer (kind=kint), intent(inout)                                &
     &                    :: iele_4_node(ntot_ele_4_node)
      integer (kind=kint), intent(inout)                                &
     &                    :: iconn_4_node(ntot_ele_4_node)
!
      integer (kind = kint) :: inod, inum, iele, icou, k
!
!
      nele_4_node(1:numnod) = 0
      do inum = 1, nele_grp
        iele = iele_grp(inum)
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
          icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
          iele_4_node(icou) = iele
          iconn_4_node(icou) =  k
        end do
      end do
!
      end  subroutine set_grp_iele_4_node
!
! -----------------------------------------------------------------------
!
      end module find_grp_ele_id_4_node
