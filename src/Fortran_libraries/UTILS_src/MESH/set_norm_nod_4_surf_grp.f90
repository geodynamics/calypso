!set_norm_nod_4_surf_grp.f90
!      module set_norm_nod_4_surf_grp
!
!        programmed by H.Matsui
!     Modified by H. Matsui on Sep, 2007
!
!      subroutine allocate_work_norm_nod(numnod)
!      subroutine deallocate_work_norm_nod
!
!      subroutine cal_surf_grp_norm_node(numele, nnod_4_ele,            &
!     &          nnod_4_surf, node_on_sf, ie, num_surf, num_surf_bc,    &
!     &          surf_istack, surf_item, vnorm_sf_grp, a_area_sf_grp,   &
!     &          ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,    &
!     &          surf_norm_nod, coef_sf_nod)
!
      module set_norm_nod_4_surf_grp
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
      integer(kind = kint), allocatable, private :: imark_nod(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_work_norm_nod(numnod)
!
      integer(kind = kint), intent(in) :: numnod
!
      allocate( imark_nod(numnod) )
      if (numnod .gt. 0) imark_nod = 0
!
      end subroutine allocate_work_norm_nod
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_work_norm_nod
!
      deallocate( imark_nod )
!
      end subroutine deallocate_work_norm_nod
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_surf_grp_norm_node(numele, nnod_4_ele,             &
     &          nnod_4_surf, node_on_sf, ie, num_surf, num_surf_bc,     &
     &          surf_istack, surf_item, vnorm_sf_grp, a_area_sf_grp,    &
     &          ntot_node_sf_grp, inod_stack_sf_grp, inod_surf_grp,     &
     &          surf_norm_nod, coef_sf_nod)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      real(kind = kreal), intent(in) :: vnorm_sf_grp(num_surf_bc,3)
      real(kind = kreal), intent(in) :: a_area_sf_grp(num_surf_bc)
!
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &              :: inod_surf_grp(ntot_node_sf_grp)
!
      real(kind=kreal), intent(inout)                                   &
     &           :: surf_norm_nod(ntot_node_sf_grp,3)
      real(kind=kreal), intent(inout) :: coef_sf_nod(ntot_node_sf_grp)
!
      real(kind = kreal) :: amp_norm
      integer (kind = kint) :: inod, iele, isf, inum, ist, ied
      integer (kind = kint) :: iq, k, k1, jnum
!
!
      do iq = 1, num_surf
!
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
        ied = surf_istack(iq)
        do inum = ist, ied
!
          iele = surf_item(1,inum)
          isf = surf_item(2,inum)
!
          do k = 1, nnod_4_surf
            k1 = node_on_sf(k,isf)
            inod = ie(iele,k1)
            jnum = imark_nod(inod)
!
            coef_sf_nod(jnum) = coef_sf_nod(jnum)                       &
     &                          + a_area_sf_grp(inum)
            surf_norm_nod(jnum,1) = surf_norm_nod(jnum,1)               &
     &           + vnorm_sf_grp(inum,1) * a_area_sf_grp(inum)
            surf_norm_nod(jnum,2) = surf_norm_nod(jnum,2)               &
     &           + vnorm_sf_grp(inum,2) * a_area_sf_grp(inum)
            surf_norm_nod(jnum,3) = surf_norm_nod(jnum,3)               &
     &           + vnorm_sf_grp(inum,3) * a_area_sf_grp(inum)
!
          end do
        end do
      end do
!
!$omp parallel do
      do inod = 1, ntot_node_sf_grp
        amp_norm = sqrt( surf_norm_nod(inod,1) **2                      &
     &                +  surf_norm_nod(inod,2) **2                      &
     &                +  surf_norm_nod(inod,3) **2 )
        surf_norm_nod(inod,1) = surf_norm_nod(inod,1) / amp_norm
        surf_norm_nod(inod,2) = surf_norm_nod(inod,2) / amp_norm
        surf_norm_nod(inod,3) = surf_norm_nod(inod,3) / amp_norm
      end do
!$omp end parallel do
!
!
      end subroutine cal_surf_grp_norm_node
!
! -----------------------------------------------------------------------
!
      end module set_norm_nod_4_surf_grp
