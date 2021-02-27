!>@file   find_node_4_group.f90
!!@brief  module find_node_4_group
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief Set belonged element list for each node
!!
!!@verbatim
!!      subroutine alloc_work_next_node(np_smp, i_smp_stack, numnod,    &
!!     &          nnod_4_ele, num_grp, istack_grp, find_WK)
!!      subroutine dealloc_work_next_node(find_WK)
!!
!!      subroutine count_nod_4_grp_smp(np_smp, numnod, nnod_4_ele, ie,  &
!!     &                               i_smp_stack, num_grp, ntot_grp,  &
!!     &                               istack_grp, iele_grp, nnod_grp)
!!      subroutine set_nod_4_grp_smp(np_smp, numele, nnod_4_ele, ie,    &
!!     &          i_smp_stack, num_grp, ntot_grp, istack_grp, iele_grp, &
!!     &          ntot_nod_grp, inod_stack_grp, inod_grp, iweight_grp)
!!
!!      subroutine move_myself_2_first_smp(np_smp, numnod, ntot_next,   &
!!     &          inod_smp_stack, inod_next_stack,                      &
!!     &          inod_next, iweight_next)
!!      subroutine sort_next_node_list_by_weight(np_smp, numnod,        &
!!     &          ntot_next, inod_smp_stack, inod_next_stack,           &
!!     &          inod_next, iweight_next)
!!@endverbatim
      module find_node_4_group
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: LARGE_SORT = 8192
!
      type work_to_find_next_node
        integer(kind = kint) :: nmax_list
        integer(kind = kint), allocatable :: ilist_4_node(:,:)
!
!        integer(kind = kint) :: nnod_mark
!        integer(kind = kint), allocatable :: imark_4_node(:,:)
      end type work_to_find_next_node
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_work_next_node(np_smp, i_smp_stack, numnod,      &
     &          nnod_4_ele, num_grp, istack_grp, find_WK)
!
      integer(kind = kint), intent(in) :: np_smp, numnod, nnod_4_ele
      integer(kind = kint), intent(in) :: num_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      type(work_to_find_next_node), intent(inout) :: find_WK
!
      integer(kind = kint) :: ip, ist, ied, inum, num
      integer(kind = kint) :: nmax_smp(np_smp)
!
!$omp parallel do private(ip,ist,ied,inum,num)
      do ip = 1, np_smp
        nmax_smp(ip) = 0
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
          num = istack_grp(inum) - istack_grp(inum-1)
          nmax_smp(ip) = max(num,nmax_smp(ip))
        end do
      end do
!$omp end parallel do
!
      find_WK%nmax_list = nnod_4_ele * maxval(nmax_smp)
      allocate(find_WK%ilist_4_node(0:find_WK%nmax_list,np_smp))
      if(find_WK%nmax_list .gt. 0) then
!$omp parallel workshare
        find_WK%ilist_4_node(0:find_WK%nmax_list,1:np_smp) = 0
!$omp end parallel workshare
      end if
!
!      find_WK%nnod_mark = numnod
!      allocate(find_WK%imark_4_node(find_WK%nnod_mark,np_smp))
!      if(find_WK%nmax_list .gt. 0) then
!!$omp parallel workshare
!        find_WK%imark_4_node(1:find_WK%nmax_list,1:np_smp) = 0
!!$omp end parallel workshare
!      end if
!
      end subroutine alloc_work_next_node
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_work_next_node(find_WK)
!
      type(work_to_find_next_node), intent(inout) :: find_WK
!
!
      deallocate(find_WK%ilist_4_node)
!      deallocate(find_WK%imark_4_node)
!
      end subroutine dealloc_work_next_node
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_nod_4_grp_smp(np_smp, numele, nnod_4_ele, ie,    &
     &          i_smp_stack, num_grp, ntot_grp, istack_grp, iele_grp,   &
     &          nnod_grp, find_WK)
!
      use calypso_mpi
      use find_node_4_each_group
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp, ntot_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: iele_grp(ntot_grp)
!
      integer(kind = kint), intent(inout) :: nnod_grp(num_grp)
      type(work_to_find_next_node), intent(inout) :: find_WK
!
      integer(kind = kint) :: ip, ist, ied, inum, ist_grp, nele_grp
!
!
!$omp parallel do private(ip,ist,ied,inum,ist_grp,nele_grp)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
          nele_grp = istack_grp(inum) - istack_grp(inum-1)
          ist_grp = istack_grp(inum-1)
!          if(nele_grp .le. LARGE_SORT) then
            call count_each_nod_4_grp_smp                               &
     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp(ist_grp+1),  &
     &          find_WK%nmax_list, find_WK%ilist_4_node(0,ip),          &
     &          nnod_grp(inum))
!          else
!            call count_each_nod_4_grp_smp_old                          &
!     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp(ist_grp+1), &
!     &          find_WK%nnod_mark, find_WK%imark_4_node(1,ip),         &
!     &          nnod_grp(inum))
!          end if
        end do
      end do
!$omp end parallel do
      call calypso_mpi_barrier
!
      end subroutine count_nod_4_grp_smp
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_4_grp_smp(np_smp, numele, nnod_4_ele, ie,      &
     &          i_smp_stack, num_grp, ntot_grp, istack_grp, iele_grp,   &
     &          ntot_nod_grp, inod_stack_grp, nnod_grp,                 &
     &          inod_grp, iweight_grp, find_WK)
!
      use find_node_4_each_group
!
      integer(kind = kint), intent(in) :: np_smp, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp, ntot_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: iele_grp(ntot_grp)
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: inod_stack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: nnod_grp(num_grp)
!
      integer(kind = kint), intent(inout) :: inod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iweight_grp(ntot_nod_grp)
      type(work_to_find_next_node), intent(inout) :: find_WK
!
      integer(kind = kint) :: ip, ist_grp, nele_grp
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst
!
!
!$omp parallel do private(ist,ied,inum,jst,ist_grp,nele_grp)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
          nele_grp = istack_grp(inum) - istack_grp(inum-1)
          ist_grp = istack_grp(inum-1)
          jst = inod_stack_grp(inum-1)
!          if(nele_grp .le. LARGE_SORT) then
            call set_each_nod_4_grp_smp                                 &
     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp(ist_grp+1),  &
     &          find_WK%nmax_list, find_WK%ilist_4_node(0,ip),          &
     &          nnod_grp(inum), inod_grp(jst+1), iweight_grp(jst+1))
!          else
!            call set_each_nod_4_grp_smp_old                            &
!     &         (numele, nnod_4_ele, ie, nele_grp, iele_grp(ist_grp+1), &
!     &          find_WK%nnod_mark, find_WK%imark_4_node(1,ip),         &
!     &          nnod_grp(inum), inod_grp(jst+1), iweight_grp(jst+1))
!          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_nod_4_grp_smp
!
!-----------------------------------------------------------------------
!
      subroutine move_myself_2_first_smp(np_smp, numnod, ntot_next,     &
     &          inod_smp_stack, inod_next_stack,                        &
     &          inod_next, iweight_next)
!
      integer (kind=kint), intent(in)  :: np_smp
      integer (kind=kint), intent(in)  :: inod_smp_stack(0:np_smp)
      integer (kind=kint), intent(in)  :: numnod
      integer (kind=kint), intent(in)  :: inod_next_stack(0:numnod)
      integer (kind=kint), intent(in)  :: ntot_next
!
      integer (kind=kint), intent(inout) :: inod_next(ntot_next)
      integer (kind=kint), intent(inout) :: iweight_next(ntot_next)
!
      integer (kind=kint) :: ip, ist_nod, ied_nod
      integer (kind=kint) :: inod, ist, ied, i, lw, mw
!
!
!$omp parallel do private(ip,ist_nod,ied_nod,inod,ist,ied,i,lw,mw)
      do ip = 1, np_smp
        ist_nod = inod_smp_stack(ip-1) + 1
        ied_nod = inod_smp_stack(ip)
        do inod = ist_nod, ied_nod
          ist = inod_next_stack(inod-1) + 1
          ied = inod_next_stack(inod)
          if( inod_next(ist) .ne. inod) then
            do i = ist+1, ied
              if (inod_next(i) .eq. inod) then
                lw = inod_next(i)
                mw = iweight_next(i)
!
                inod_next(i) =    inod_next(ist)
                iweight_next(i) = iweight_next(ist)
!
                inod_next(ist) =    lw
                iweight_next(ist) = mw
                exit
              end if
            end do
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine move_myself_2_first_smp
!
!-----------------------------------------------------------------------
!
      subroutine sort_next_node_list_by_weight(np_smp, numnod,          &
     &          ntot_next, inod_smp_stack, inod_next_stack,             &
     &          inod_next, iweight_next)
!
      use quicksort
!
      integer (kind=kint), intent(in)  :: np_smp
      integer (kind=kint), intent(in)  :: inod_smp_stack(0:np_smp)
      integer (kind=kint), intent(in)  :: numnod
      integer (kind=kint), intent(in)  :: inod_next_stack(0:numnod)
      integer (kind=kint), intent(in)  :: ntot_next
!
      integer (kind=kint), intent(inout) :: inod_next(ntot_next)
      integer (kind=kint), intent(inout) :: iweight_next(ntot_next)
!
      integer (kind=kint) :: ip, ist_nod, ied_nod
      integer (kind=kint) :: inod, ist, num
!
!
!$omp parallel do private(ip,ist_nod,ied_nod,inod,ist,num)
      do ip = 1, np_smp
        ist_nod = inod_smp_stack(ip-1) + 1
        ied_nod = inod_smp_stack(ip)
        do inod = ist_nod, ied_nod
          ist = inod_next_stack(inod-1) + 2
          num = inod_next_stack(inod) - inod_next_stack(inod-1) - 1
          if(num .gt. 1) then
            call quicksort_w_index(num, iweight_next(ist),             &
     &                             ione, num, inod_next(ist))
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine sort_next_node_list_by_weight
!
!-----------------------------------------------------------------------
!
      end module find_node_4_group
