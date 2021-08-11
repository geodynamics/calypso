!>@file   find_element_id_4_node.f90
!!@brief  module find_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!> @brief Search belonged element list for each node
!!
!!@verbatim
!!      subroutine count_iele_4_node(numnod, numele, nnod_4_ele, ie,    &
!!     &          iele_st, iele_ed, nele_4_node)
!!      subroutine set_iele_4_node(numnod, numele, nnod_4_ele, ie,      &
!!     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node, &
!!     &          nele_4_node, iele_4_node, iconn_4_node)
!!      subroutine set_iele_4_node_sum_order                            &
!!     &         (numnod, inod_dbl, numele, nnod_4_ele, ie,             &
!!     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node, &
!!     &          nele_4_node, iele_4_node, iconn_4_node, isum_ele)
!!@endverbatim
!
      module find_element_id_4_node
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine count_iele_4_node(numnod, numele, nnod_4_ele, ie,      &
     &          iele_st, iele_ed, nele_4_node)
!
!      use degraded_node_in_ele
!
      integer (kind=kint), intent(in) :: numnod, numele, nnod_4_ele
      integer (kind=kint), intent(in) :: ie(numele,nnod_4_ele)
      integer (kind=kint), intent(in) :: iele_st, iele_ed
!
      integer (kind=kint), intent(inout) :: nele_4_node(numnod)
!
      integer (kind = kint) :: inod, iele, k
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
        end do
      end do
!
      end  subroutine count_iele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine set_iele_4_node(numnod, numele, nnod_4_ele, ie,        &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: iele_st, iele_ed
      integer(kind = kint), intent(in) :: ntot_ele_4_node
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer(kind = kint), intent(inout) :: nele_4_node(numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_4_node(ntot_ele_4_node)
      integer(kind = kint), intent(inout)                               &
     &                     :: iconn_4_node(ntot_ele_4_node)
!
      integer(kind = kint) :: inod, iele, icou, k, ist
!
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
          icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
          iele_4_node(icou) = iele
          iconn_4_node(icou) =  k
        end do
      end do
!
!$omp parallel do private(inod,ist)
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1) + 1
        if(nele_4_node(inod) .gt. 1) then
          call quicksort_w_index(nele_4_node(inod), iele_4_node(ist),   &
     &        ione, nele_4_node(inod), iconn_4_node(ist))
        end if
      end do
!$omp end parallel do
!
      end  subroutine set_iele_4_node
!
! -----------------------------------------------------------------------
!
      subroutine set_iele_4_node_sum_order                              &
     &         (numnod, inod_dbl, numele, nnod_4_ele, ie,               &
     &          iele_st, iele_ed, ntot_ele_4_node, iele_stack_4_node,   &
     &          nele_4_node, iele_4_node, iconn_4_node,                 &
     &          isum_neib, isum_ele)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: inod_dbl(numnod)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: iele_st, iele_ed
      integer(kind = kint), intent(in) :: ntot_ele_4_node
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
!
      integer(kind = kint), intent(inout) :: nele_4_node(numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_4_node(ntot_ele_4_node)
      integer(kind = kint), intent(inout)                               &
     &                     :: iconn_4_node(ntot_ele_4_node)
      integer(kind = kint), intent(inout)                               &
     &                     :: isum_neib(ntot_ele_4_node)
      integer(kind = kint), intent(inout) :: isum_ele(numele)
!
      integer(kind = kint), allocatable :: idx(:)
      integer(kind = kint), allocatable :: iele_4_sort(:,:)
!
      integer(kind = kint) :: inod, iele, icou, k, k1
      integer(kind = kint) :: ist, num, inum
!
!
!$omp parallel workshare
      isum_ele(1:numele) = 0
!$omp end parallel workshare
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp do private(iele,inod)
        do iele = 1, numele
          inod = ie(iele,k1)
          isum_ele(iele) = isum_ele(iele) + inod_dbl(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      allocate(idx(ntot_ele_4_node))
      allocate(iele_4_sort(ntot_ele_4_node,2))
!
!$omp parallel do private(inum)
      do inum = 1, ntot_ele_4_node
        idx(inum) = inum
      end do
!$omp end parallel do
!
!$omp parallel workshare
      nele_4_node(1:numnod) = 0
!$omp end parallel workshare
!
      do iele = iele_st, iele_ed
        do k = 1, nnod_4_ele
          inod = ie(iele,k)
          nele_4_node(inod) = nele_4_node(inod) + 1
          icou = iele_stack_4_node(inod-1) + nele_4_node(inod)
          iele_4_sort(icou,1) = iele
          iele_4_sort(icou,2) =  k
          isum_neib(icou) = isum_ele(iele)
        end do
      end do
!
!$omp parallel do private(inod,ist,num,inum,icou)
      do inod = 1, numnod
        ist = iele_stack_4_node(inod-1)
        num = iele_stack_4_node(inod  ) - ist
        if(num .gt. 1) then
          call quicksort_w_index(num, isum_neib(ist+1),                 &
     &                           ione, num, idx(ist+1))
!
          do inum = 1, num
            icou = idx(ist+inum)
            iele_4_node(ist+inum) =  iele_4_sort(icou,1)
            iconn_4_node(ist+inum) = iele_4_sort(icou,2)
          end do
        end if
      end do
!$omp end parallel do
!
      deallocate(idx, iele_4_sort)
!
      end  subroutine set_iele_4_node_sum_order
!
! -----------------------------------------------------------------------
!
      end module find_element_id_4_node
