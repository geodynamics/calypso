!>@file   t_fline_seeds_surf_group.f90
!!@brief  module t_fline_seeds_surf_group
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Surface group list to set seed points
!!
!!@verbatim
!!      subroutine init_flux_on_seed_surface(ele, surf, sf_grp, nod_fld,&
!!     &                                     fln_prm, seed_sf_grp)
!!      subroutine dealloc_flux_on_seed_surface(seed_sf_grp)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(fieldline_seeds_surf_group), intent(inout) :: seed_sf_grp
!!@endverbatim
!
      module t_fline_seeds_surf_group
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
!
      implicit  none
!
      type fieldline_seeds_surf_group
        integer(kind = kint) :: nsurf_seed = 0
        integer(kind = kint), allocatable :: isf_grp_seed_item(:,:)
        real(kind = kreal),   allocatable :: flux_start(:)
      end type fieldline_seeds_surf_group
!
      private :: alloc_local_start_grp_item
      private :: cal_flux_for_1sgrp, cal_area_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_flux_on_seed_surface(ele, surf, sf_grp, nod_fld,  &
     &                                     fln_prm, seed_sf_grp)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(fieldline_seeds_surf_group), intent(inout) :: seed_sf_grp
!
      integer(kind = kint) :: num_sf
!
      num_sf = count_nsurf_for_starting(ele, sf_grp,                    &
     &                               fln_prm%igrp_start_fline_surf_grp)
      call alloc_local_start_grp_item(num_sf, seed_sf_grp)
!
      call set_isurf_for_starting                                       &
     &   (ele, sf_grp, fln_prm%igrp_start_fline_surf_grp,               &
     &    seed_sf_grp%nsurf_seed, seed_sf_grp%isf_grp_seed_item)
!
      if(     fln_prm%id_seed_distribution .eq. iflag_random_by_area    &
     &   .or. fln_prm%id_seed_distribution .eq. iflag_no_random) then
        if(iflag_debug .gt. 0) write(*,*) 'cal_area_for_1sgrp'
        call cal_area_for_1sgrp(ele, surf,                              &
     &      seed_sf_grp%nsurf_seed, seed_sf_grp%isf_grp_seed_item,      &
     &      seed_sf_grp%flux_start)
      else
        if(iflag_debug .gt. 0) write(*,*) 'cal_flux_for_1sgrp'
        call cal_flux_for_1sgrp(ele, surf,                              &
     &      seed_sf_grp%nsurf_seed, seed_sf_grp%isf_grp_seed_item,      &
     &      nod_fld%n_point, nod_fld%d_fld(1,fln_prm%iphys_4_fline),    &
     &      seed_sf_grp%flux_start)
      end if
!
      end subroutine init_flux_on_seed_surface
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_flux_on_seed_surface(seed_sf_grp)
      type(fieldline_seeds_surf_group), intent(inout) :: seed_sf_grp
!
      deallocate(seed_sf_grp%isf_grp_seed_item, seed_sf_grp%flux_start)
!
      end subroutine dealloc_flux_on_seed_surface
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_item(num, seed_sf_grp)
!
      integer(kind = kint), intent(in) :: num
      type(fieldline_seeds_surf_group), intent(inout) :: seed_sf_grp
!
!
      seed_sf_grp%nsurf_seed = num
      allocate(seed_sf_grp%isf_grp_seed_item(2,num))
      allocate(seed_sf_grp%flux_start(num))
      if(num .gt. 0) seed_sf_grp%isf_grp_seed_item = 0
      if(num .gt. 0) seed_sf_grp%flux_start = 0.0d0
!
      end subroutine alloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_nsurf_for_starting            &
     &                            (ele, sf_grp, igrp_seed)
!
      integer(kind = kint), intent(in) :: igrp_seed
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint) :: isurf, iele, icou, ist, ied
!
!
      icou = 0
      ist = sf_grp%istack_grp(igrp_seed-1) + 1
      ied = sf_grp%istack_grp(igrp_seed)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) icou = icou + 1
      end do
!
      count_nsurf_for_starting = icou
!
      end function count_nsurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine set_isurf_for_starting(ele, sf_grp, igrp_seed,         &
     &                                  nsurf_seed, isf_grp_seed_item)
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: igrp_seed
!
      integer(kind = kint), intent(in) :: nsurf_seed
      integer(kind = kint), intent(inout)                               &
     &                     :: isf_grp_seed_item(2,nsurf_seed)
!
      integer(kind = kint) :: isurf, inum, iele, ist, ied
!
!
      inum = 0
      ist = sf_grp%istack_grp(igrp_seed-1) + 1
      ied = sf_grp%istack_grp(igrp_seed)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) then
          inum = inum + 1
          isf_grp_seed_item(1:2,inum) = sf_grp%item_sf_grp(1:2,isurf)
        end if
      end do
!
      end subroutine set_isurf_for_starting
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_flux_for_1sgrp(ele, surf, num_sgrp, isurf_grp,     &
     &                              n_point, d_nod, flux)
!
      use m_geometry_constants
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_nod(n_point,3)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: i1,  i2,  i3,  i4
      real(kind = kreal) :: sign_surf, d_surf(3)
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp  parallel do                                                      &
!$omp& private(inum,iele,isf,isurf,sign_surf,i1,i2,i3,i4,d_surf)
!$cdir nodep
      do inum = 1, num_sgrp
        iele = isurf_grp(1,inum)
        isf =  isurf_grp(2,inum)
        isurf = abs(surf%isf_4_ele(iele,isf))
        sign_surf = dble(surf%isf_4_ele(iele,isf) / isurf)
!
        i1 =  surf%ie_surf(isurf, 1)
        i2 =  surf%ie_surf(isurf, 2)
        i3 =  surf%ie_surf(isurf, 3)
        i4 =  surf%ie_surf(isurf, 4)
!
        d_surf(1) = quad * (d_nod(i1,1) + d_nod(i2,1)                   &
     &                    + d_nod(i3,1) + d_nod(i4,1))
        d_surf(2) = quad * (d_nod(i1,2) + d_nod(i2,2)                   &
     &                    + d_nod(i3,2) + d_nod(i4,2))
        d_surf(3) = quad * (d_nod(i1,3) + d_nod(i2,3)                   &
     &                    + d_nod(i3,3) + d_nod(i4,3))
!
        flux(inum) = flux(inum) + (surf%vnorm_surf(isurf,1) * d_surf(1) &
     &                          + surf%vnorm_surf(isurf,2) * d_surf(2)  &
     &                          + surf%vnorm_surf(isurf,3) * d_surf(3)) &
     &                           * surf%area_surf(isurf) * sign_surf    &
     &                           * dble(ele%interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_flux_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      subroutine cal_area_for_1sgrp(ele, surf,    &
     &          num_sgrp, isurf_grp, flux)
!
      use m_geometry_constants
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: num_sgrp
      integer(kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
!
      real(kind = kreal), intent(inout) :: flux(num_sgrp)
!
      integer (kind = kint) :: iele, isf, isurf, inum
!
!
      flux(1:num_sgrp) = 0.0d0
!
!$omp  parallel do private(inum,iele,isf,isurf)
      do inum = 1, num_sgrp
        iele = isurf_grp(1,inum)
        isf =  isurf_grp(2,inum)
        isurf = abs(surf%isf_4_ele(iele,isf))
!
        flux(inum) = flux(inum) + surf%area_surf(isurf)                 &
     &                           * dble(ele%interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_area_for_1sgrp
!
!  ---------------------------------------------------------------------
!
      end module t_fline_seeds_surf_group
