!
!     module set_node_4_group
!
!     Writteg by H.Matsui on Aug., 2006
!
!
!      subroutine count_nod_4_ele_grp(numnod, numele, nnod_4_ele, ie,   &
!     &          num_grp, num_item, istack_grp, item_grp,               &
!     &          ntot_nod_grp, nnod_grp, inod_stack_grp, inod_2_nod_grp)
!      subroutine count_nod_each_grp(numnod, numele, nnod_4_ele, ie,    &
!     &          nele_grp, iele_grp, nnod_grp, inod_2_nod_grp)
!
!      subroutine set_nod_each_grp(numnod, numele, nnod_4_ele, ie,      &
!     &          nele_grp, iele_grp, nnod_grp, inod_2_nod_grp, inod_grp)
!
!      subroutine set_new_connect_4_grp(numnod, numele, nnod_4_ele, ie, &
!     &          nele_grp, ist_grp, ied_grp, iele_grp, nnod_grp,        &
!     &          inod_2_nod_grp, inod_grp, ie_grp)
!
!
      module set_node_4_group
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_nod_4_ele_grp(numnod, numele, nnod_4_ele, ie,    &
     &          num_grp, num_item, istack_grp, item_grp,                &
     &          ntot_nod_grp, nnod_grp, inod_stack_grp, inod_2_nod_grp)
!
      integer(kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind=kint), intent(in) :: num_grp, num_item
      integer(kind=kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind=kint), intent(in) :: item_grp(num_item)
!
      integer(kind=kint), intent(inout) :: nnod_grp(num_grp)
      integer(kind=kint), intent(inout) :: inod_stack_grp(0:num_grp)
      integer(kind=kint), intent(inout) :: ntot_nod_grp
      integer(kind=kint), intent(inout) :: inod_2_nod_grp(numnod)
!
      integer(kind=kint) :: i, ist_grp, nele_grp
!
!
      inod_stack_grp(0) = 0
      do i = 1, num_grp
!
        ist_grp = istack_grp(i-1) + 1
        nele_grp = istack_grp(i) - istack_grp(i-1)
        call count_nod_each_grp(numnod, numele, nnod_4_ele, ie,         &
     &      nele_grp, item_grp(ist_grp), nnod_grp(i),                   &
     &      inod_2_nod_grp)
!
        inod_stack_grp(i) = inod_stack_grp(i-1) + nnod_grp(i)
      end do
      ntot_nod_grp = inod_stack_grp(num_grp)
!
      end subroutine count_nod_4_ele_grp
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_4_ele_grp(numnod, numele, nnod_4_ele, ie,      &
     &          num_grp, num_item, istack_grp, item_grp,                &
     &          ntot_nod_grp, nnod_grp, inod_stack_grp, inod_ele_grp,   &
     &          inod_2_nod_grp)
!
      integer(kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind=kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind=kint), intent(in) :: num_grp, num_item
      integer(kind=kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind=kint), intent(in) :: item_grp(num_item)
!
      integer(kind=kint), intent(in) :: nnod_grp(num_grp)
      integer(kind=kint), intent(in) :: inod_stack_grp(0:num_grp)
      integer(kind=kint), intent(in) :: ntot_nod_grp
!
      integer(kind=kint), intent(inout) :: inod_ele_grp(ntot_nod_grp)
      integer(kind=kint), intent(inout) :: inod_2_nod_grp(numnod)
!
      integer(kind=kint) :: i, ist_grp, nele_grp, ist_nod
!
!
      do i = 1, num_grp
        ist_grp = istack_grp(i-1) + 1
        nele_grp = istack_grp(i) - istack_grp(i-1)
        ist_nod = inod_stack_grp(i-1) + 1
        call set_nod_each_grp(numnod, numele, nnod_4_ele, ie,           &
     &      nele_grp, item_grp(ist_grp), nnod_grp(i),                   &
     &      inod_2_nod_grp, inod_ele_grp(ist_nod) )
      end do
!
      end subroutine set_nod_4_ele_grp
!
!-----------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_nod_each_grp(numnod, numele, nnod_4_ele, ie,     &
     &          nele_grp, iele_grp, nnod_grp, inod_2_nod_grp)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(inout) :: inod_2_nod_grp(numnod)
      integer(kind = kint), intent(inout) :: nnod_grp
!
      integer(kind = kint) :: inum, iele, inod, inod_a, i
!
!
      nnod_grp = 0
      inod_2_nod_grp(1:numnod) = 0
      do inum = 1, nele_grp
        iele = abs(iele_grp(inum))
        if (iele.ge.1 .and. iele.le.numele) then
!
          do i = 1, nnod_4_ele
            inod = ie(iele,i)
            inod_a = abs(inod)
!            if ( inod_a .gt. numnod ) then
!              write(*,*) 'node id is wrong!!', inod, numnod
!              return
!            end if 
            if (inod_2_nod_grp(inod_a) .eq. 0) then
              nnod_grp = nnod_grp + 1
              inod_2_nod_grp(inod_a) = nnod_grp
            end if
          end do
!
        end if
      end do
!
      end subroutine count_nod_each_grp
!
!------------------------------------------------------------------
!
      subroutine set_nod_each_grp(numnod, numele, nnod_4_ele, ie,       &
     &          nele_grp, iele_grp, nnod_grp, inod_2_nod_grp, inod_grp)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      integer(kind = kint), intent(in) :: nnod_grp
!
      integer(kind = kint), intent(inout) :: inod_2_nod_grp(numnod)
      integer(kind = kint), intent(inout) :: inod_grp(nnod_grp)
!
      integer(kind = kint) :: icou, inum, iele, inod, inod_a, i
!
!
      icou = 0
      inod_2_nod_grp(1:numnod) = 0
      do inum = 1, nele_grp
        iele = abs(iele_grp(inum))
        if (iele.ge.1 .and. iele.le.numele) then
!
          do i = 1, nnod_4_ele
            inod = ie(iele,i)
            inod_a = abs(inod)
            if (inod_2_nod_grp(inod_a) .eq. 0) then
              icou = icou + 1
              inod_grp(icou) = inod
              inod_2_nod_grp(inod_a) = icou
            end if
          end do
!
        end if
      end do
!
      end subroutine set_nod_each_grp
!
!------------------------------------------------------------------
!
      subroutine set_new_connect_4_grp(numnod, numele, nnod_4_ele, ie,  &
     &          nele_grp, ist_grp, ied_grp, iele_grp, nnod_grp,         &
     &          inod_2_nod_grp, inod_grp, ie_grp)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp, ist_grp, ied_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      integer(kind = kint), intent(in) :: nnod_grp
      integer(kind = kint), intent(in) :: inod_grp(nnod_grp)
!
      integer(kind = kint), intent(inout) :: inod_2_nod_grp(numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: ie_grp(nele_grp,nnod_4_ele)
!
      integer(kind = kint) :: inum, iele, inod, inod_a, i
!
!
      inod_2_nod_grp(1:numnod) = 0
      do inum = 1, nnod_grp
        inod_a = abs( inod_grp(inum) )
        inod_2_nod_grp(inod_a) = inum
      end do
!
      do i = 1, nnod_4_ele
        do inum = ist_grp, ied_grp
          iele = abs( iele_grp(inum) )
          inod = ie(iele,i)
          inod_a = abs(inod)
          ie_grp(inum,i) = inod_2_nod_grp(inod_a)
        end do
!
      end do
!
      end subroutine set_new_connect_4_grp
!
!------------------------------------------------------------------
!
      end module set_node_4_group
