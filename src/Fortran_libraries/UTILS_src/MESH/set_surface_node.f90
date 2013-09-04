!
!     module set_surface_node
!
!        programmed by H.Matsui and H.Okuda
!        modified by H. Matsui on Aug., 2006
!        modified by H. Matsui on Dec., 2008
!        modified by H. Matsui on Jan., 2012
!
!      subroutine allocate_make_4_surf_nod_grp(nnod)
!      subroutine deallocate_make_4_surf_nod_grp
!
!      subroutine count_surf_nod_grp_stack(np_smp, inod_smp_stack,      &
!     &          numele, nnod_4_ele, ie, nnod_4_surf, node_on_sf,       &
!     &          num_surf, num_surf_bc, surf_istack, surf_item,         &
!     &          ntot_node_sf_grp, inod_stack_sf_grp)
!      subroutine set_surf_nod_grp_item                                 &
!     &          (numnod, numele, nnod_4_ele, ie, nnod_4_surf,          &
!     &           node_on_sf, node_on_sf_n, num_surf, num_surf_bc,      &
!     &           surf_istack, surf_item, ntot_node_sf_grp,             &
!     &           inod_stack_sf_grp, inod_surf_grp, surf_node_n,        &
!     &           num_sf_4_nod)
!
      module set_surface_node
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), allocatable :: imark_nod(:)
      integer(kind = kint), allocatable :: icount_smp(:)
      private :: imark_nod, icount_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_make_4_surf_nod_grp(nnod)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: nnod
!
!
      allocate( imark_nod(nnod) )
      allocate( icount_smp(np_smp) )
      imark_nod =  0
      icount_smp = 0
!
      end subroutine allocate_make_4_surf_nod_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_make_4_surf_nod_grp
!
      deallocate( imark_nod )
      deallocate( icount_smp )
!
      end subroutine deallocate_make_4_surf_nod_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_surf_nod_grp_stack(np_smp, inod_smp_stack,       &
     &          numele, nnod_4_ele, ie, nnod_4_surf, node_on_sf,        &
     &          num_surf, num_surf_bc, surf_istack, surf_item,          &
     &          ntot_node_sf_grp, nnod_sf_grp, inod_stack_sf_grp)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in)                                  &
     &               :: node_on_sf(nnod_4_surf,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(inout) :: ntot_node_sf_grp
      integer(kind = kint), intent(inout) :: nnod_sf_grp(num_surf)
      integer(kind = kint), intent(inout)                               &
     &              :: inod_stack_sf_grp(0:num_surf)
!
      integer (kind = kint) :: inod, iele, isurf, isf, k1, i
      integer (kind = kint) :: iq, ip, ist, ied
!
!
      nnod_sf_grp(1:num_surf) = 0
      do iq = 1, num_surf
!
        imark_nod = 0
!
        ist = surf_istack(iq-1)+1
        ied =   surf_istack(iq)
!
        do inod = 1, nnod_4_surf
!$omp parallel do private(iele,isf,i,k1) 
          do isurf = ist, ied
!
            iele = surf_item(1,isurf)
            isf =  surf_item(2,isurf)
            k1 = node_on_sf(inod,isf)
            i = ie(iele,k1)
            imark_nod(i) = 1
          end do
!$omp end parallel do
        end do
!
        icount_smp = 0
!$omp parallel do private(iele,isf,i) 
        do ip = 1, np_smp
          ist = inod_smp_stack(ip-1)+1
          ied = inod_smp_stack(ip)
          do inod = ist, ied
            icount_smp(ip) = icount_smp(ip) + imark_nod(inod)
          end do
        end do
!$omp end parallel do
!
        do ip = 1, np_smp
          nnod_sf_grp(iq) = nnod_sf_grp(iq) + icount_smp(ip)
        end do
        inod_stack_sf_grp(iq) = inod_stack_sf_grp(iq-1)                 &
     &                         + nnod_sf_grp(iq)
!
      end do
!
      ntot_node_sf_grp = inod_stack_sf_grp(num_surf)
!
      end subroutine count_surf_nod_grp_stack
!
!-----------------------------------------------------------------------
!
      subroutine set_surf_nod_grp_item                                  &
     &          (numnod, numele, nnod_4_ele, ie, nnod_4_surf,           &
     &           node_on_sf, node_on_sf_n, num_surf, num_surf_bc,       &
     &           surf_istack, surf_item, ntot_node_sf_grp,              &
     &           inod_stack_sf_grp, inod_surf_grp, surf_node_n,         &
     &           num_sf_4_nod)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in)                                  &
     &               :: node_on_sf(nnod_4_surf,nsurf_4_ele)
      integer(kind = kint), intent(in)                                  &
     &               :: node_on_sf_n(nnod_4_surf,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
!
      integer(kind = kint), intent(inout)                               &
     &              :: inod_surf_grp(ntot_node_sf_grp)
      integer(kind = kint), intent(inout)                               &
     &              :: surf_node_n(ntot_node_sf_grp)
      integer(kind = kint), intent(inout)                               &
     &              :: num_sf_4_nod(ntot_node_sf_grp)
!
      integer (kind = kint) :: ist, ied
      integer (kind = kint) :: iq, icou, i, jnum
      integer (kind = kint) :: inum, inod, iele, isf, isurf, k, k1, k2
!
!
      icou = 0
      do iq = 1, num_surf
        imark_nod = 0
!
        ist = surf_istack(iq-1)+1
        ied = surf_istack(iq)
!
        do inod = 1, nnod_4_surf
          do isurf = ist, ied
!
            iele = surf_item(1,isurf)
            isf =  surf_item(2,isurf)
            k1 = node_on_sf(inod,isf)
            i = ie(iele,k1)
            imark_nod(i) = 1
          end do
        end do
!
        do inod = 1, numnod
          if (imark_nod(inod) .gt. 0) then
            icou = icou + 1
            inod_surf_grp(icou) = inod
          end if
        end do
!
      end do
!
!
      do iq = 1, num_surf
        imark_nod = 0
        ist = inod_stack_sf_grp(iq-1)+1
        ied = inod_stack_sf_grp(iq)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = inod_surf_grp(inum)
          imark_nod(inod) = inum
        end do
!$omp end parallel do
!
        ist = surf_istack(iq-1)+1
        ied =   surf_istack(iq)
        do k = 1, nnod_4_surf
!$omp parallel do private(inum,iele,isf,inod,jnum,k1,k2)
          do inum = ist, ied
            iele = surf_item(1,inum)
            isf = surf_item(2,inum)
            k1 = node_on_sf(k,isf)
            k2 = node_on_sf_n(k,isf)
            inod = ie(iele,k1)
            jnum = imark_nod(inod)
            surf_node_n(jnum) = ie(iele,k2)
          end do
!$omp end parallel do
        end do
      end do
!
      do iq = 1, num_surf
        imark_nod = 0
        ist = inod_stack_sf_grp(iq-1)+1
        ied = inod_stack_sf_grp(iq)
!$omp parallel do private(inum,inod)
        do inum = ist, ied
          inod = inod_surf_grp(inum)
          imark_nod(inod) = inum
        end do
!$omp end parallel do
!
        ist = surf_istack(iq-1)+1
        ied =   surf_istack(iq)
        do k = 1, nnod_4_surf
          do inum = ist, ied
            iele = surf_item(1,inum)
            isf = surf_item(2,inum)
            k1 = node_on_sf(k,isf)
            inod = ie(iele,k1)
            jnum = imark_nod(inod)
            num_sf_4_nod(jnum) = num_sf_4_nod(jnum) + 1
          end do
        end do
      end do
!
      end subroutine set_surf_nod_grp_item
!
!-----------------------------------------------------------------------
!
      end module set_surface_node
