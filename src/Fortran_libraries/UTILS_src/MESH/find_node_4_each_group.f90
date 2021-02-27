!>@file   find_node_4_each_group.f90
!!@brief  module find_node_4_each_group
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief Set belonged node list for each group item
!!
!!@verbatim
!!      subroutine count_each_nod_4_grp_smp                             &
!!     &         (numele, nnod_4_ele, ie,  nele_grp, iele_grp,          &
!!     &          nmax_list, ilist_4_node, nnod_grp)
!!      subroutine set_each_nod_4_grp_smp(numele, nnod_4_ele, ie,       &
!!     &          nele_grp, iele_grp, nmax_list, ilist_4_node,          &
!!     &          nnod_grp, inod_grp, iweight_grp)
!!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!!        integer(kind = kint), intent(in) :: nele_grp
!!        integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!!        integer(kind = kint), intent(in) :: nmax_list
!!        integer(kind = kint), intent(inout) :: ilist_4_node(nmax_list)
!!        integer(kind = kint), intent(inout) :: nnod_grp
!!        integer(kind = kint), intent(inout) :: inod_grp(nnod_grp)
!!        integer(kind = kint), intent(inout) :: iweight_grp(nnod_grp)
!!
!!      subroutine count_each_nod_4_grp_smp_old                         &
!!     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp,           &
!!     &          numnod, imark_4_node, nnod_grp)
!!      subroutine set_each_nod_4_grp_smp_old                           &
!!     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp,           &
!!     &          numnod, imark_4_node, nnod_grp, inod_grp, iweight_grp)
!!        integer(kind = kint), intent(in) :: numele, nnod_4_ele
!!        integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!!        integer(kind = kint), intent(in) :: nele_grp
!!        integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!!        integer(kind = kint), intent(in) :: numnod
!!        integer(kind = kint), intent(inout) :: imark_4_node(numnod)
!!        integer(kind = kint), intent(inout) :: nnod_grp
!!        integer(kind = kint), intent(inout) :: inod_grp(nnod_grp)
!!        integer(kind = kint), intent(inout) :: iweight_grp(nnod_grp)
!!@endverbatim
      module find_node_4_each_group
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_each_nod_4_grp_smp                               &
     &         (numele, nnod_4_ele, ie,  nele_grp, iele_grp,            &
     &          nmax_list, ilist_4_node, nnod_grp)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(in) :: nmax_list
      integer(kind = kint), intent(inout) :: ilist_4_node(0:nmax_list)
!
      integer(kind = kint), intent(inout) :: nnod_grp
!
      integer(kind = kint) :: iele, k, n_item, jnum
!
!
      n_item = 0
      do jnum = 1, nele_grp
        iele = abs(iele_grp(jnum))
        do k = 1, nnod_4_ele
          n_item = n_item + 1
          ilist_4_node(n_item) = ie(iele,k)
        end do
      end do
!
      if(n_item .gt. ione) then
        call quicksort_int(n_item, ilist_4_node(1), ione, n_item)
      end if
!
      nnod_grp = 0
      do jnum = 1, n_item
        if(ilist_4_node(jnum) .ne. ilist_4_node(jnum-1)) then
          nnod_grp = nnod_grp + 1
        end if
      end do
!
      end subroutine count_each_nod_4_grp_smp
!
!-----------------------------------------------------------------------
!
      subroutine set_each_nod_4_grp_smp(numele, nnod_4_ele, ie,         &
     &          nele_grp, iele_grp, nmax_list, ilist_4_node,            &
     &          nnod_grp, inod_grp, iweight_grp)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(in) :: nmax_list
      integer(kind = kint), intent(inout) :: ilist_4_node(0:nmax_list)
!
      integer(kind = kint), intent(in) :: nnod_grp
      integer(kind = kint), intent(inout) :: inod_grp(nnod_grp)
      integer(kind = kint), intent(inout) :: iweight_grp(nnod_grp)
!
      integer(kind = kint) :: iele, k, icou, n_item, jnum
!
!
      n_item = 0
      do jnum = 1, nele_grp
        iele = abs(iele_grp(jnum))
        do k = 1, nnod_4_ele
          n_item = n_item + 1
          ilist_4_node(n_item) = ie(iele,k)
        end do
      end do
!
      if(n_item .gt. ione) then
        call quicksort_int(n_item, ilist_4_node(1), ione, n_item)
      end if
!
      icou = 0
      do jnum = 1, n_item
        if(ilist_4_node(jnum) .eq. ilist_4_node(jnum-1)) then
          iweight_grp(icou) = iweight_grp(icou) + 1
        else
          icou = icou + 1
          inod_grp(icou) = ilist_4_node(jnum)
          iweight_grp(icou) = 1
        end if
      end do
!
      end subroutine set_each_nod_4_grp_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_each_nod_4_grp_smp_old                           &
     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp,             &
     &          numnod, imark_4_node, nnod_grp)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(inout) :: imark_4_node(numnod)
      integer(kind = kint), intent(inout) :: nnod_grp
!
      integer(kind = kint) :: iele, inod, k, jnum
!
!
      imark_4_node(1:numnod) = 0
      nnod_grp = 0
      do jnum = 1, nele_grp
        iele = abs(iele_grp(jnum))
!
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          if (imark_4_node(inod) .eq. 0) then
            nnod_grp = nnod_grp + 1
            imark_4_node(inod) = imark_4_node(inod) + 1
          end if
        end do
      end do
!
      end subroutine count_each_nod_4_grp_smp_old
!
!-----------------------------------------------------------------------
!
      subroutine set_each_nod_4_grp_smp_old                             &
     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp,             &
     &          numnod, imark_4_node, nnod_grp, inod_grp, iweight_grp)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(inout) :: imark_4_node(numnod)
!
      integer(kind = kint), intent(in) :: nnod_grp
      integer(kind = kint), intent(inout) :: inod_grp(nnod_grp)
      integer(kind = kint), intent(inout) :: iweight_grp(nnod_grp)
!
      integer(kind = kint) :: iele, inod, k, icou, jnum
!
!
      imark_4_node(1:numnod) = 0
!
      icou = 0
      do jnum = 1, nele_grp
        iele = abs(iele_grp(jnum))
!
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          if (imark_4_node(inod) .eq. 0) then
            icou = icou + 1
            inod_grp(icou) = inod
          end if
          imark_4_node(inod) = imark_4_node(inod) + 1
        end do
      end do
!
      do jnum = 1, nnod_grp
        inod = inod_grp(jnum)
        iweight_grp(jnum) = imark_4_node(inod)
      end do
!
      end subroutine set_each_nod_4_grp_smp_old
!
!-----------------------------------------------------------------------
!
      end module find_node_4_each_group
