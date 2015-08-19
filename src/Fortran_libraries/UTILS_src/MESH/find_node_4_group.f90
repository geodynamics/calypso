!
!     module find_node_4_group
!
!     Writteg by H.Matsui on Oct., 2006
!
!
!
!      subroutine allocate_work_next_node(np_smp, numnod)
!      subroutine deallocate_work_next_node
!
!
!      subroutine count_nod_4_grp_smp(np_smp, numnod, numele,           &
!     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,        &
!     &          istack_grp, iele_grp, nnod_grp)
!
!      subroutine set_nod_4_grp_smp(np_smp, numnod, numele,             &
!     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,        &
!     &          istack_grp, iele_grp, ntot_nod_grp, inod_stack_grp,    &
!     &          nnod_grp, inod_grp)
!
!      subroutine move_myself_2_first_smp(np_smp, numnod, ntot_next,    &
!     &          inod_smp_stack, inod_next_stack,                       &
!     &          inod_next, iweight_next)
!      subroutine sort_next_node_list_by_weight(np_smp, numnod,         &
!     &          ntot_next, inod_smp_stack, inod_next_stack,            &
!     &          inod_next, iweight_next)
!
      module find_node_4_group
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer (kind=kint), allocatable :: imark_4_node(:,:)
      private :: imark_4_node
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_work_next_node(np_smp, numnod)
!
      integer(kind = kint), intent(in) :: np_smp, numnod
!
      allocate( imark_4_node(numnod,np_smp) )
      imark_4_node = 0
!
      end subroutine allocate_work_next_node
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_work_next_node
!
      deallocate( imark_4_node )
!
      end subroutine deallocate_work_next_node
!
!-----------------------------------------------------------------------
!
      subroutine count_nod_4_grp_smp(np_smp, numnod, numele,            &
     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,         &
     &          istack_grp, iele_grp, nnod_grp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp, ntot_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: iele_grp(ntot_grp)
!
      integer(kind = kint), intent(inout) :: nnod_grp(num_grp)
!
      integer(kind = kint) :: ip, iele, inod, k
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum
!
!
      nnod_grp(1:num_grp) = 0
!
!$omp parallel do private(ist,ied,inum,jst,jed,jnum,iele,inod,k)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
!
          imark_4_node(1:numnod,ip) = 0
          jst = istack_grp(inum-1) + 1
          jed = istack_grp(inum)
          do jnum = jst, jed
            iele = abs(iele_grp(jnum))
!
            do k = 1, nnod_4_ele
              inod = ie(iele,k)
              if (imark_4_node(inod,ip) .eq. 0) then
                nnod_grp(inum) = nnod_grp(inum) + 1
                imark_4_node(inod,ip) = imark_4_node(inod,ip) + 1
              end if
            end do
!
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine count_nod_4_grp_smp
!
!-----------------------------------------------------------------------
!
      subroutine set_nod_4_grp_smp(np_smp, numnod, numele,              &
     &          nnod_4_ele, ie, i_smp_stack, num_grp, ntot_grp,         &
     &          istack_grp, iele_grp, ntot_nod_grp, inod_stack_grp,     &
     &          nnod_grp, inod_grp, iweight_grp)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_grp, ntot_grp
      integer(kind = kint), intent(in) :: i_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: istack_grp(0:num_grp)
      integer(kind = kint), intent(in) :: iele_grp(ntot_grp)
      integer(kind = kint), intent(in) :: ntot_nod_grp
      integer(kind = kint), intent(in) :: inod_stack_grp(0:num_grp)
!
      integer(kind = kint), intent(inout) :: nnod_grp(num_grp)
      integer(kind = kint), intent(inout) :: inod_grp(ntot_nod_grp)
      integer(kind = kint), intent(inout) :: iweight_grp(ntot_nod_grp)
!
      integer(kind = kint) :: ip, iele, inod, k, icou
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum
!
!
      nnod_grp(1:num_grp) = 0
!
!$omp parallel do private(ist,ied,inum,jst,jed,jnum,iele,inod,k,icou)
      do ip = 1, np_smp
        ist = i_smp_stack(ip-1) + 1
        ied = i_smp_stack(ip)
        do inum = ist, ied
!
          jst = istack_grp(inum-1) + 1
          jed = istack_grp(inum)
!
          imark_4_node(1:numnod,ip) = 0
!
          do jnum = jst, jed
            iele = abs(iele_grp(jnum))
!
            do k = 1, nnod_4_ele
              inod = ie(iele,k)
              if (imark_4_node(inod,ip) .eq. 0) then
                nnod_grp(inum) = nnod_grp(inum) + 1
                icou = inod_stack_grp(inum-1) + nnod_grp(inum)
                inod_grp(icou) = inod
              end if
              imark_4_node(inod,ip) = imark_4_node(inod,ip) + 1
            end do
!
          end do
!
          jst = inod_stack_grp(inum-1) + 1
          jed = inod_stack_grp(inum)
          do jnum = jst, jed
            inod = inod_grp(jnum)
            iweight_grp(jnum) = imark_4_node(inod,ip)
          end do
!
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
          call quicksort_w_index(num, iweight_next(ist), ione, num,     &
     &        inod_next(ist) )
        end do
      end do
!$omp end parallel do
!
      end subroutine sort_next_node_list_by_weight
!
!-----------------------------------------------------------------------
!
      end module find_node_4_group
