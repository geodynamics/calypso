!>@file   int_area_normal_4_surface.f90
!!@brief  module int_area_normal_4_surface
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2006
!!
!>@brief Integration on surfaces
!!
!!@verbatim
!!      subroutine int_normal_all_surf(surf, g_FEM, jac_2d, n_int,      &
!!     &          area_surf, a_area_surf, vnorm_surf)
!!        type(surface_data), intent(inout) :: surf
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_2d
!!      subroutine int_normal_surf_groups(sf_grp, g_FEM, jac_sf_grp,    &
!!     &           n_int, area_surf, a_area_surf, vnorm_surf)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!      subroutine int_surf_area_1_surf_grp(ele, surf, g_FEM, jac_2d,   &
!!     &          num_int, num_sgrp, isurf_grp, area)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_2d), intent(in) :: jac_2d
!!@endverbatim
!
      module int_area_normal_4_surface
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_fem_gauss_int_coefs
      use t_jacobian_2d
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_normal_all_surf(surf, g_FEM, jac_2d, n_int,        &
     &          area_surf, a_area_surf, vnorm_surf)
!
      type(surface_data), intent(inout) :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: n_int
!
      real(kind = kreal), intent(inout) :: area_surf(surf%numsurf)
      real(kind = kreal), intent(inout) :: a_area_surf(surf%numsurf)
      real(kind = kreal), intent(inout) :: vnorm_surf(surf%numsurf,3)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: isurf, ix, ii
!
!
!$omp parallel workshare
      vnorm_surf(1:surf%numsurf,1) =  0.0d0
      vnorm_surf(1:surf%numsurf,2) =  0.0d0
      vnorm_surf(1:surf%numsurf,3) =  0.0d0
      area_surf(1:surf%numsurf) =     0.0d0
      a_area_surf(1:surf%numsurf) =   0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(ist,ied,ii,ix,isurf)
      do ip = 1, np_smp
        ist = surf%istack_surf_smp(ip-1) + 1
        ied = surf%istack_surf_smp(ip)
!
        do ii = 1, n_int * n_int
          ix = g_FEM%int_start2(n_int) + ii
!
!cdir noloopchg
          do isurf = ist, ied
            area_surf(isurf) = area_surf(isurf)                         &
     &                    + jac_2d%xj_sf(isurf,ix) * g_FEM%owe2d(ix)
!
            vnorm_surf(isurf,1) = vnorm_surf(isurf,1)                   &
     &                    + jac_2d%xsf_sf(isurf,ix,1) * g_FEM%owe2d(ix)
            vnorm_surf(isurf,2) = vnorm_surf(isurf,2)                   &
     &                    + jac_2d%xsf_sf(isurf,ix,2) * g_FEM%owe2d(ix)
            vnorm_surf(isurf,3) = vnorm_surf(isurf,3)                   &
     &                    + jac_2d%xsf_sf(isurf,ix,3) * g_FEM%owe2d(ix)
          end do
        end do
!
!cdir noloopchg
        do isurf = ist, ied
          if (area_surf(isurf) .eq. 0.0d0) then
            a_area_surf(isurf) = 1.0d60
          else
            a_area_surf(isurf) = 1.0d0 / area_surf(isurf)
          end if
        end do
!
!cdir noloopchg
        do isurf = ist, ied
          vnorm_surf(isurf,1) = vnorm_surf(isurf,1)*a_area_surf(isurf)
          vnorm_surf(isurf,2) = vnorm_surf(isurf,2)*a_area_surf(isurf)
          vnorm_surf(isurf,3) = vnorm_surf(isurf,3)*a_area_surf(isurf)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine int_normal_all_surf
!
! ----------------------------------------------------------------------
!
      subroutine int_normal_surf_groups(sf_grp, g_FEM, jac_sf_grp,      &
     &           n_int, area_surf, a_area_surf, vnorm_surf)
!
      type(surface_group_data), intent(in) :: sf_grp
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_sf_grp
      integer (kind = kint), intent(in) :: n_int
!
      real(kind = kreal), intent(inout) :: area_surf(sf_grp%num_item)
      real(kind = kreal), intent(inout) :: a_area_surf(sf_grp%num_item)
      real(kind = kreal), intent(inout) :: vnorm_surf(sf_grp%num_item,3)
!
      integer (kind = kint) :: ip, ist, ied
      integer (kind = kint) :: i_grp, inum, ix, ii, i
!
!
!$omp parallel workshare
      vnorm_surf(1:sf_grp%num_item,1) =  0.0d0
      vnorm_surf(1:sf_grp%num_item,2) =  0.0d0
      vnorm_surf(1:sf_grp%num_item,3) =  0.0d0
      area_surf(1:sf_grp%num_item) =     0.0d0
      a_area_surf(1:sf_grp%num_item) =   0.0d0
!$omp end parallel workshare
!
      do i_grp = 1, sf_grp%num_grp
!$omp parallel do private(i,ist,ied,inum)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = sf_grp%istack_grp_smp(i-1) + 1
          ied = sf_grp%istack_grp_smp(i)
!
          do ii = 1, n_int * n_int
            ix = g_FEM%int_start2(n_int) + ii
!
            do inum = ist, ied
              area_surf(inum) = area_surf(inum)                         &
     &                 + jac_sf_grp%xj_sf(inum,ix) * g_FEM%owe2d(ix)
!
              vnorm_surf(inum,1) = vnorm_surf(inum,1)                   &
     &                 + jac_sf_grp%xsf_sf(inum,ix,1) * g_FEM%owe2d(ix)
              vnorm_surf(inum,2) = vnorm_surf(inum,2)                   &
     &                 + jac_sf_grp%xsf_sf(inum,ix,2) * g_FEM%owe2d(ix)
              vnorm_surf(inum,3) = vnorm_surf(inum,3)                   &
     &                 + jac_sf_grp%xsf_sf(inum,ix,3) * g_FEM%owe2d(ix)
            end do
          end do
!
          do inum = ist, ied
            if(area_surf(inum) .eq. 0.0d0) then
              a_area_surf(inum) = 1.0d60
            else
              a_area_surf(inum) = 1.0d0 / area_surf(inum)
            end if
!
            vnorm_surf(inum,1) = vnorm_surf(inum,1)*a_area_surf(inum)
            vnorm_surf(inum,2) = vnorm_surf(inum,2)*a_area_surf(inum)
            vnorm_surf(inum,3) = vnorm_surf(inum,3)*a_area_surf(inum)
          end do
        end do
!$omp end parallel do
      end do
!
      end subroutine int_normal_surf_groups
!
!  ---------------------------------------------------------------------
!
      subroutine int_surf_area_1_surf_grp(ele, surf, g_FEM, jac_2d,     &
     &          num_int, num_sgrp, isurf_grp, area)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_2d), intent(in) :: jac_2d
      integer (kind = kint), intent(in) :: num_int
      integer (kind = kint), intent(in) :: num_sgrp
      integer (kind = kint), intent(in) :: isurf_grp(2,num_sgrp)
!
      real(kind = kreal), intent(inout) :: area
!
      integer (kind = kint) :: iele, isf, isurf, inum
      integer (kind = kint) :: ii, ix
!
!
      area = 0.0d0
      do ii= 1, num_int * num_int
        ix = g_FEM%int_start2(num_int) + ii
!
!$cdir nodep
        do inum = 1, num_sgrp
          iele = isurf_grp(1,inum)
          isf =  isurf_grp(2,inum)
          isurf = abs(surf%isf_4_ele(iele,isf))
!
          area = area + dble(ele%interior_ele(iele))                    &
     &                 * jac_2d%xj_sf(isurf,ix) * g_FEM%owe2d(ix)
        end do
      end do
!
      end subroutine int_surf_area_1_surf_grp
!
!  ---------------------------------------------------------------------
!
      end module int_area_normal_4_surface
