!>@file   t_surf_grp_list_each_surf.f90
!!@brief  module t_surf_grp_list_each_surf
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Structure of connectivity data for surface group items
!!
!!@verbatim
!!      subroutine init_sf_grp_list_each_surf(surf, surf_grp,           &
!!     &                                      sf_grp_4_sf)
!!      subroutine dealloc_num_sf_grp_each_surf(sf_grp_4_sf)
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(sf_grp_list_each_surf), intent(inout) :: sf_grp_4_sf
!!@endverbatim
!
      module t_surf_grp_list_each_surf
!
      use m_precision
      use t_surface_data
      use t_group_data
!
      implicit  none
!
      type sf_grp_list_each_surf
!>        Total number of surfaces with surface group
        integer(kind = kint) :: ntot_grp_4_surf
!>        Number stack of surface group for each surface
        integer(kind = kint), allocatable :: istack_grp_surf(:)
!>        Ssurface group ID for each surface
        integer(kind = kint), allocatable :: igrp_4_surf(:)
      end type sf_grp_list_each_surf
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sf_grp_list_each_surf(surf, surf_grp,             &
     &                                      sf_grp_4_sf)
!
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
!
      type(sf_grp_list_each_surf), intent(inout) :: sf_grp_4_sf
!
      integer(kind = kint), allocatable :: icou_surf(:)
!
      allocate(icou_surf(surf%numsurf))
!$omp parallel workshare
      icou_surf(1:surf%numsurf) = 0
!$omp end parallel workshare
!
      call alloc_num_sf_grp_each_surf(surf, sf_grp_4_sf)
!
      call count_sf_grp_list_each_surf(surf, surf_grp,                  &
     &    sf_grp_4_sf%ntot_grp_4_surf, sf_grp_4_sf%istack_grp_surf,     &
     &    icou_surf)
!
      call alloc_sf_grp_list_each_surf(sf_grp_4_sf)
      call set_sf_grp_list_each_surf(surf, surf_grp,                    &
     &    sf_grp_4_sf%ntot_grp_4_surf, sf_grp_4_sf%istack_grp_surf,     &
     &    icou_surf, sf_grp_4_sf%igrp_4_surf)
      deallocate(icou_surf)
!
      end subroutine init_sf_grp_list_each_surf
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_sf_grp_each_surf(sf_grp_4_sf)
!
      type(sf_grp_list_each_surf), intent(inout) :: sf_grp_4_sf
!
!
      if(allocated(sf_grp_4_sf%igrp_4_surf) .eqv. .FALSE.) return
      deallocate(sf_grp_4_sf%igrp_4_surf, sf_grp_4_sf%istack_grp_surf)
      sf_grp_4_sf%ntot_grp_4_surf = 0
!
      end subroutine dealloc_num_sf_grp_each_surf
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_num_sf_grp_each_surf(surf, sf_grp_4_sf)
!
      type(surface_data), intent(in) :: surf
      type(sf_grp_list_each_surf), intent(inout) :: sf_grp_4_sf
!
!
      allocate(sf_grp_4_sf%istack_grp_surf(0:surf%numsurf))
!$omp parallel workshare
      sf_grp_4_sf%istack_grp_surf(0:surf%numsurf) = 0
!$omp end parallel workshare
!
      end subroutine alloc_num_sf_grp_each_surf
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sf_grp_list_each_surf(sf_grp_4_sf)
!
      type(sf_grp_list_each_surf), intent(inout) :: sf_grp_4_sf
!
!
      allocate(sf_grp_4_sf%igrp_4_surf(sf_grp_4_sf%ntot_grp_4_surf))
!
      if(sf_grp_4_sf%ntot_grp_4_surf .le. 0) return
!$omp parallel workshare
      sf_grp_4_sf%igrp_4_surf(1:sf_grp_4_sf%ntot_grp_4_surf) = 0
!$omp end parallel workshare
!
      end subroutine alloc_sf_grp_list_each_surf
!
! -----------------------------------------------------------------------
!
      subroutine count_sf_grp_list_each_surf(surf, surf_grp,            &
     &          ntot_grp_4_each_surf, istack_grp_4_each_surf,           &
     &          icou_surf)
!
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
!
      integer(kind = kint), intent(inout) :: ntot_grp_4_each_surf
      integer(kind = kint), intent(inout) :: icou_surf(surf%numsurf)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_grp_4_each_surf(0:surf%numsurf)
!
      integer(kind = kint) :: igrp, ist, ied, inum
      integer(kind = kint) :: iele, k1, isurf
!
!
!$omp parallel workshare
      icou_surf(1:surf%numsurf) = 0
!$omp end parallel workshare
!
      do igrp = 1, surf_grp%num_grp
        ist = surf_grp%istack_grp(igrp-1)+1
        ied = surf_grp%istack_grp(igrp)
        do inum = ist, ied
          iele = surf_grp%item_sf_grp(1,inum)
          k1 =   surf_grp%item_sf_grp(2,inum)
          isurf = abs(surf%isf_4_ele(iele,k1))
!
          icou_surf(isurf) = icou_surf(isurf) + 1
        end do
      end do
!
      istack_grp_4_each_surf(0) = 0
      do isurf = 1, surf%numsurf
        istack_grp_4_each_surf(isurf)                                   &
     &         = istack_grp_4_each_surf(isurf-1) + icou_surf(isurf)
      end do
      ntot_grp_4_each_surf = istack_grp_4_each_surf(surf%numsurf)
!
      end subroutine count_sf_grp_list_each_surf
!
! -----------------------------------------------------------------------
!
      subroutine set_sf_grp_list_each_surf(surf, surf_grp,              &
     &          ntot_grp_4_each_surf, istack_grp_4_each_surf,           &
     &          icou_surf, igrp_4_each_surf)
!
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      integer(kind = kint), intent(in) :: ntot_grp_4_each_surf
!
      integer(kind = kint), intent(inout) :: icou_surf(surf%numsurf)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_grp_4_each_surf(0:surf%numsurf)
      integer(kind = kint), intent(inout)                               &
     &              :: igrp_4_each_surf(ntot_grp_4_each_surf)
!
      integer(kind = kint) :: igrp, ist, ied, inum
      integer(kind = kint) :: iele, k1, isurf, icou
!
!
!$omp parallel workshare
      icou_surf(1:surf%numsurf)                                         &
     &      = istack_grp_4_each_surf(0:surf%numsurf-1)
!$omp end parallel workshare
!
      do igrp = 1, surf_grp%num_grp
        ist = surf_grp%istack_grp(igrp-1)+1
        ied = surf_grp%istack_grp(igrp)
        do inum = ist, ied
          iele = surf_grp%item_sf_grp(1,inum)
          k1 =   surf_grp%item_sf_grp(2,inum)
          isurf = abs(surf%isf_4_ele(iele,k1))
!
          icou_surf(isurf) = icou_surf(isurf) + 1
          icou = icou_surf(isurf)
          igrp_4_each_surf(icou) = igrp
        end do
      end do
!
      end subroutine set_sf_grp_list_each_surf
!
! -----------------------------------------------------------------------
!
      end module t_surf_grp_list_each_surf
