!>@file  t_surf_grp_4_pvr_domain.f90
!!       module t_surf_grp_4_pvr_domain
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for surface group data for volume rendering
!!
!!@verbatim
!!      subroutine alloc_pvr_surf_domain_item(num_surf_in, pvr_bound)
!!      subroutine dealloc_pvr_surf_domain_item(pvr_bound)
!!      subroutine copy_pvr_surf_domain_item(pvr_bd_org, pvr_bound)
!!@endverbatim
!
      module t_surf_grp_4_pvr_domain
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      type pvr_domain_outline
!>    Center of domain
        real(kind = kreal) :: center_g(3) = (/zero,zero,zero/)
!>    Maximum distance from center of domain
        real(kind = kreal) :: rmax_g = zero
!
!>    MAximum and mimimum position of domain
!!@n   minimum value: xx_minmax(1,ndir)
!!@n   maximum value: xx_minmax(2,ndir)
        real(kind = kreal) :: xx_minmax_g(2,3)
!!    Range of field data
        real(kind = kreal) :: d_minmax_pvr(2)
      end type pvr_domain_outline
!
      type pvr_bounds_surf_ctl
!>        Number of Surface address for surface group
        integer(kind = kint) :: num_pvr_surf
!>        Surface address for surface group
        integer(kind = kint), allocatable :: item_pvr_surf(:,:)
!
!>       Average position in screen coordinate
        real(kind = kreal), allocatable :: screen_posi(:,:)
!>       Average normal vector in screen coordinate
        real(kind = kreal), allocatable :: screen_norm(:,:)
!>       Average w in screen coordinate
        real(kind = kreal), allocatable :: screen_w(:)
!
!>    Start and end position in horizontal screen
        real(kind = kreal), allocatable :: screen_xrng(:,:)
!>    Start and end position in horizontal screen
        real(kind = kreal), allocatable :: screen_yrng(:,:)
!>    Start and end depth in horizontal screen
        real(kind = kreal), allocatable :: screen_zrng(:,:)
!>    Start and end pixel in horizontal screen
        integer(kind = kint), allocatable :: isurf_xrng(:,:)
!>    Start and end pixel in vetical screen
        integer(kind = kint), allocatable :: jsurf_yrng(:,:)
      end type pvr_bounds_surf_ctl
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pvr_surf_domain_item(num_surf_in, pvr_bound)
!
      integer(kind = kint), intent(in) :: num_surf_in
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
      pvr_bound%num_pvr_surf = num_surf_in
!
      allocate(pvr_bound%item_pvr_surf(2,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%screen_norm(3,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%screen_posi(3,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%screen_w(pvr_bound%num_pvr_surf))
!
      allocate(pvr_bound%screen_xrng(2,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%screen_yrng(2,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%screen_zrng(2,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%isurf_xrng(2,pvr_bound%num_pvr_surf))
      allocate(pvr_bound%jsurf_yrng(2,pvr_bound%num_pvr_surf))
!
      if(pvr_bound%num_pvr_surf .le. 0) return
      pvr_bound%item_pvr_surf = 0
      pvr_bound%screen_norm = 0.0d0
      pvr_bound%screen_posi = 0.0d0
      pvr_bound%screen_w =    0.0d0
!
      pvr_bound%screen_xrng = 0.0d0
      pvr_bound%screen_yrng = 0.0d0
      pvr_bound%screen_zrng = 0.0d0
      pvr_bound%isurf_xrng = 0
      pvr_bound%jsurf_yrng = 0
!
      end subroutine alloc_pvr_surf_domain_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_surf_domain_item(pvr_bound)
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
      deallocate(pvr_bound%item_pvr_surf, pvr_bound%screen_w)
      deallocate(pvr_bound%screen_posi, pvr_bound%screen_norm)
      deallocate(pvr_bound%screen_xrng, pvr_bound%screen_yrng)
      deallocate(pvr_bound%screen_zrng)
      deallocate(pvr_bound%isurf_xrng,  pvr_bound%jsurf_yrng)
!
      end subroutine dealloc_pvr_surf_domain_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_pvr_surf_domain_item(pvr_bd_org, pvr_bound)
!
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bd_org
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
!$omp parallel workshare
      pvr_bound%item_pvr_surf(:,:) = pvr_bd_org%item_pvr_surf(:,:)
      pvr_bound%screen_norm(:,:) = pvr_bd_org%screen_norm(:,:)
      pvr_bound%screen_posi(:,:) = pvr_bd_org%screen_posi(:,:)
      pvr_bound%screen_w(:) =    pvr_bd_org%screen_w(:)
!
      pvr_bound%screen_xrng(:,:) = pvr_bd_org%screen_xrng(:,:)
      pvr_bound%screen_yrng(:,:) = pvr_bd_org%screen_yrng(:,:)
      pvr_bound%screen_zrng(:,:) = pvr_bd_org%screen_zrng(:,:)
      pvr_bound%isurf_xrng(:,:) = pvr_bd_org%isurf_xrng(:,:)
      pvr_bound%jsurf_yrng(:,:) = pvr_bd_org%jsurf_yrng(:,:)
!$omp end parallel workshare
!
      end subroutine copy_pvr_surf_domain_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_sf_grp_4_pvr_domain(id_rank, num_pvr, pvr_bound)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound(num_pvr)
!
      integer(kind = kint) :: i_pvr, num
!
!
      do i_pvr = 1, num_pvr
        num = pvr_bound(i_pvr)%num_pvr_surf
        write(50+id_rank,*) 'num_pvr_surf',                             &
     &                     i_pvr, pvr_bound(i_pvr)%num_pvr_surf
        write(50+id_rank,'(8i16)')                                      &
    &      pvr_bound(i_pvr)%item_pvr_surf(1,1:num)
        write(50+id_rank,'(8i16)')                                      &
    &      pvr_bound(i_pvr)%item_pvr_surf(2,1:num)
      end do
!
      end subroutine check_sf_grp_4_pvr_domain
!
! -----------------------------------------------------------------------
!
      subroutine check_sf_posi_pvr_domain(id_rank, num_pvr, pvr_bound)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound(num_pvr)
!
      integer(kind = kint) :: i_pvr, inum
!
!
      do i_pvr = 1, num_pvr
        write(50+id_rank,*) 'screen_posi',                              &
     &                     i_pvr, pvr_bound(i_pvr)%num_pvr_surf
        do inum = 1, pvr_bound(i_pvr)%num_pvr_surf
          write(50+id_rank,*) inum,                                     &
     &      pvr_bound(i_pvr)%screen_posi(1:3,inum),                     &
     &      pvr_bound(i_pvr)%screen_w(inum)
        end do
      end do
!
      end subroutine check_sf_posi_pvr_domain
!
! -----------------------------------------------------------------------
!
      subroutine check_sf_norm_pvr_domain(id_rank, num_pvr, pvr_bound)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound(num_pvr)
!
      integer(kind = kint) :: i_pvr, inum
!
!
      do i_pvr = 1, num_pvr
        write(50+id_rank,*) 'screen_norm',                              &
     &                     i_pvr, pvr_bound(i_pvr)%num_pvr_surf
        do inum = 1, pvr_bound(i_pvr)%num_pvr_surf
          write(50+id_rank,*) inum,                                     &
     &           pvr_bound(i_pvr)%screen_norm(1:3,inum)
        end do
      end do
!
      end subroutine check_sf_norm_pvr_domain
!
! -----------------------------------------------------------------------
!
      subroutine check_surf_rng_pvr_domain(id_rank, num_pvr, pvr_bound)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: num_pvr
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound(num_pvr)
!
      integer(kind = kint) :: i_pvr, inum
!
!
      do i_pvr = 1, num_pvr
        write(50+id_rank,*) 'isurf_xrng',                               &
     &                     i_pvr, pvr_bound(i_pvr)%num_pvr_surf
        do inum = 1, pvr_bound(i_pvr)%num_pvr_surf
          write(50+id_rank,'(i16,4i5,1p4e16.7)')                        &
     &          inum, pvr_bound(i_pvr)%isurf_xrng(1:2,inum),            &
     &                pvr_bound(i_pvr)%jsurf_yrng(1:2,inum),            &
     &                pvr_bound(i_pvr)%screen_xrng(1:2,inum),           &
     &                pvr_bound(i_pvr)%screen_yrng(1:2,inum)
        end do
      end do
!
      end subroutine check_surf_rng_pvr_domain
!
! -----------------------------------------------------------------------
!
      end module t_surf_grp_4_pvr_domain
