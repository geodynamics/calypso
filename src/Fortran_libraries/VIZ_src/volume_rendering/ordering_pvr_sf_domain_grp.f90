!ordering_pvr_sf_domain_grp.f90
!      module ordering_pvr_sf_domain_grp
!
!        programmed by H.Matsui on Aug., 2011
!
!      subroutine s_ordering_pvr_sf_domain_grp(pvr_bound)
!      subroutine dealloc_ordering_pvr_domain_grp
!
      module ordering_pvr_sf_domain_grp
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      integer(kind = kint), allocatable :: i_org(:)
      integer(kind = kint), allocatable :: itmp(:)
      real(kind = kreal), allocatable :: ztmp(:)
!
      private :: i_org, itmp, ztmp
      private :: alloc_ordering_pvr_domain_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_ordering_pvr_sf_domain_grp(pvr_bound)
!
      use t_surf_grp_4_pvr_domain
      use quicksort
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
      integer(kind = kint) :: inum
!
!
      call alloc_ordering_pvr_domain_grp(pvr_bound%num_pvr_surf)
!
!$omp parallel do
      do inum = 1, pvr_bound%num_pvr_surf
        ztmp(inum) = pvr_bound%screen_posi(3,inum)
        i_org(inum) = inum
      end do
!$omp end parallel do
!
      if(pvr_bound%num_pvr_surf .gt. 1) then
        call quicksort_real_w_index(pvr_bound%num_pvr_surf, ztmp,       &
     &      ione, pvr_bound%num_pvr_surf, i_org)
      end if
!
!
      call swap_int_items_sf_grp(itwo, pvr_bound%num_pvr_surf,          &
     &    pvr_bound%item_pvr_surf)
!
      call swap_int_items_sf_grp(itwo, pvr_bound%num_pvr_surf,          &
     &    pvr_bound%isurf_xrng)
      call swap_int_items_sf_grp(itwo, pvr_bound%num_pvr_surf,          &
     &    pvr_bound%jsurf_yrng)
!
!
      call swap_real_items_sf_grp(ithree, pvr_bound%num_pvr_surf,       &
     &    pvr_bound%screen_posi)
      call swap_real_items_sf_grp(ithree, pvr_bound%num_pvr_surf,       &
     &    pvr_bound%screen_norm)
!
      call swap_real_items_sf_grp(ione, pvr_bound%num_pvr_surf,         &
     &    pvr_bound%screen_w)
!
      call swap_real_items_sf_grp(itwo, pvr_bound%num_pvr_surf,         &
     &    pvr_bound%screen_xrng)
      call swap_real_items_sf_grp(itwo, pvr_bound%num_pvr_surf,         &
     &    pvr_bound%screen_yrng)
      call swap_real_items_sf_grp(itwo, pvr_bound%num_pvr_surf,         &
     &    pvr_bound%screen_zrng)
!
      call dealloc_ordering_pvr_domain_grp
!
      end subroutine s_ordering_pvr_sf_domain_grp
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ordering_pvr_domain_grp(num_pvr_surf)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
!
      if(allocated(itmp)) return
!
      allocate(itmp(num_pvr_surf))
      allocate(ztmp(num_pvr_surf))
      allocate(i_org(num_pvr_surf))
!
      end subroutine alloc_ordering_pvr_domain_grp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ordering_pvr_domain_grp
!
      if(allocated(itmp)) deallocate(itmp, ztmp, i_org)
!
      end subroutine dealloc_ordering_pvr_domain_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine swap_int_items_sf_grp(nd, num, item)
!
      integer(kind = kint), intent(in) :: nd, num
      integer(kind = kint), intent(inout) :: item(nd,num)
!
      integer(kind = kint) :: id
!
      do id = 1, nd
        call swap_int_4_sf_grp(nd, num, id, item)
      end do
!
      end subroutine swap_int_items_sf_grp
!
! -----------------------------------------------------------------------
!
      subroutine swap_real_items_sf_grp(nd, num, ritem)
!
      integer(kind = kint), intent(in) :: nd, num
      real(kind = kreal), intent(inout) :: ritem(nd,num)
!
      integer(kind = kint) :: id
!
      do id = 1, nd
        call swap_real_4_sf_grp(nd, num, id, ritem)
      end do
!
      end subroutine swap_real_items_sf_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine swap_int_4_sf_grp(nd, num, id, item)
!
      integer(kind = kint), intent(in) :: nd, num, id
      integer(kind = kint), intent(inout) :: item(nd,num)
!
      integer(kind = kint) :: inum, i
!
!
!
      itmp(1:num) = item(id,1:num)
      do inum = 1, num
        i = i_org(inum)
        item(id,inum) = itmp(i)
      end do
!
      end subroutine swap_int_4_sf_grp
!
! -----------------------------------------------------------------------
!
      subroutine swap_real_4_sf_grp(nd, num, id, ritem)
!
      integer(kind = kint), intent(in) :: nd, num, id
      real(kind = kreal), intent(inout) :: ritem(nd,num)
!
      integer(kind = kint) :: inum, i
!
!
      ztmp(1:num) = ritem(id,1:num)
      do inum = 1, num
        i = i_org(inum)
        ritem(id,inum) = ztmp(i)
      end do
!
      end subroutine swap_real_4_sf_grp
!
! -----------------------------------------------------------------------
!
      end module ordering_pvr_sf_domain_grp
