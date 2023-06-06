!set_position_pvr_screen.f90
!>@file  set_position_pvr_screen.f90
!!       module set_position_pvr_screen
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!>@brief Convert position by modelview or projection matrix
!!
!!@verbatim
!!      subroutine copy_node_position_pvr_domain(numnod, numele,        &
!!     &          numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele,         &
!!     &          num_pvr_surf, item_pvr_surf_domain, xx_pvr_domain)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(in) :: xx(numnod,3)
!!        integer(kind = kint), intent(in) :: numele
!!        integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: ie_surf(numsurf,nnod_4_surf)
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: isf_4_ele(numele,nsurf_4_ele)
!!        integer(kind = kint), intent(in) :: num_pvr_surf
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: xx_pvr_domain(4*num_pvr_surf,4)
!!
!!      subroutine overwte_to_modelview_each_ele                        &
!!     &         (model_mat, numnod, x4_each_model)
!!        real(kind = kreal), intent(in) :: model_mat(4,4)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(inout) :: x4_each_model(4,numnod)
!!      subroutine project_once_each_element                            &
!!     &         (model_mat, project_mat, numnod, x4_projected)
!!      subroutine project_once_each_ele_w_smp                          &
!!     &         (model_mat, project_mat, numnod, x4_projected)
!!        real(kind = kreal), intent(in) :: model_mat(4,4)
!!        real(kind = kreal), intent(in) :: project_mat(4,4)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(inout) :: x4_projected(4,numnod)
!!
!!      subroutine cal_position_pvr_modelview                           &
!!     &         (model_mat, numnod, xx, x_nod_model)
!!        real(kind = kreal), intent(in) :: model_mat(4,4)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(in) :: xx(numnod,3)
!!        real(kind = kreal), intent(inout) :: x_nod_model(numnod,4)
!!      subroutine chenge_direction_pvr_modelview                       &
!!     &         (model_mat, numnod, xx, x_nod_model)
!!        real(kind = kreal), intent(in) :: model_mat(4,4)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(in) :: xx(numnod,3)
!!        real(kind = kreal), intent(inout) :: x_nod_model(numnod,4)
!!
!!      subroutine overwte_position_pvr_screen(project_mat, numnod,     &
!!     &                                       x_nod_screen)
!!        real(kind = kreal), intent(in) :: project_mat(4,4)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(inout) :: x_nod_screen(numnod,4)
!!      subroutine overwte_pvr_domain_on_screen(model_mat, project_mat, &
!!     &          num_pvr_surf, xx_pvr_domain)
!!        real(kind = kreal), intent(in) :: model_mat(4,4)
!!        real(kind = kreal), intent(in) :: project_mat(4,4)
!!        integer(kind = kint), intent(in) :: num_pvr_surf
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: xx_pvr_domain(4*num_pvr_surf,4)
!!@endverbatim
!
      module set_position_pvr_screen
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_node_position_pvr_domain(numnod, numele,          &
     &          numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele,           &
     &          num_pvr_surf, item_pvr_surf_domain, xx_pvr_domain)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in)                                  &
     &                    :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: xx_pvr_domain(4*num_pvr_surf,4)
!
      integer(kind = kint) :: inum, iele, k1, isurf
      integer(kind = kint) :: i1, i2, i3, i4
!
!
!$omp parallel do private (inum,iele,k1,isurf,i1,i2,i3,i4)
      do inum = 1, num_pvr_surf
        iele = item_pvr_surf_domain(1,inum)
        k1 =   item_pvr_surf_domain(2,inum)
        isurf = abs(isf_4_ele(iele,k1))
!
        i1 = ie_surf(isurf,1)
        i2 = ie_surf(isurf,2)
        i3 = ie_surf(isurf,3)
        i4 = ie_surf(isurf,4)
!
        xx_pvr_domain(4*inum-3,1:3) = xx(i1,1:3)
        xx_pvr_domain(4*inum-2,1:3) = xx(i2,1:3)
        xx_pvr_domain(4*inum-1,1:3) = xx(i3,1:3)
        xx_pvr_domain(4*inum,  1:3) = xx(i4,1:3)
        xx_pvr_domain(4*inum-3:4*inum,4) = one
      end do
!$omp end parallel do
!
      end subroutine copy_node_position_pvr_domain
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwte_to_modelview_each_ele                          &
     &         (model_mat, numnod, x4_each_model)
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: x4_each_model(4,numnod)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: x4_tmp(4)
!
!
      do inod = 1, numnod
        x4_tmp(1) =  model_mat(1,1) * x4_each_model(1,inod)             &
     &             + model_mat(1,2) * x4_each_model(2,inod)             &
     &             + model_mat(1,3) * x4_each_model(3,inod)             &
     &             + model_mat(1,4) * x4_each_model(4,inod)
        x4_tmp(2) =  model_mat(2,1) * x4_each_model(1,inod)             &
     &             + model_mat(2,2) * x4_each_model(2,inod)             &
     &             + model_mat(2,3) * x4_each_model(3,inod)             &
     &             + model_mat(2,4) * x4_each_model(4,inod)
        x4_tmp(3) =  model_mat(3,1) * x4_each_model(1,inod)             &
     &             + model_mat(3,2) * x4_each_model(2,inod)             &
     &             + model_mat(3,3) * x4_each_model(3,inod)             &
     &             + model_mat(3,4) * x4_each_model(4,inod)
        x4_tmp(4) =  model_mat(4,1) * x4_each_model(1,inod)             &
     &             + model_mat(4,2) * x4_each_model(2,inod)             &
     &             + model_mat(4,3) * x4_each_model(3,inod)             &
     &             + model_mat(4,4) * x4_each_model(4,inod)
!
        x4_each_model(1:4,inod) = x4_tmp(1:4)
      end do
!
      end subroutine overwte_to_modelview_each_ele
!
! -----------------------------------------------------------------------
!
      subroutine project_once_each_element                              &
     &         (model_mat, project_mat, numnod, x4_projected)
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
      real(kind = kreal), intent(in) :: project_mat(4,4)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: x4_projected(4,numnod)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: coef
      real(kind = kreal) :: x4_tmp(4)
!
!
      do inod = 1, numnod
        x4_tmp(1) =  model_mat(1,1) * x4_projected(1,inod)              &
     &             + model_mat(1,2) * x4_projected(2,inod)              &
     &             + model_mat(1,3) * x4_projected(3,inod)              &
     &             + model_mat(1,4) * x4_projected(4,inod)
        x4_tmp(2) =  model_mat(2,1) * x4_projected(1,inod)              &
     &             + model_mat(2,2) * x4_projected(2,inod)              &
     &             + model_mat(2,3) * x4_projected(3,inod)              &
     &             + model_mat(2,4) * x4_projected(4,inod)
        x4_tmp(3) =  model_mat(3,1) * x4_projected(1,inod)              &
     &             + model_mat(3,2) * x4_projected(2,inod)              &
     &             + model_mat(3,3) * x4_projected(3,inod)              &
     &             + model_mat(3,4) * x4_projected(4,inod)
        x4_tmp(4) =  model_mat(4,1) * x4_projected(1,inod)              &
     &             + model_mat(4,2) * x4_projected(2,inod)              &
     &             + model_mat(4,3) * x4_projected(3,inod)              &
     &             + model_mat(4,4) * x4_projected(4,inod)
!
        x4_projected(1,inod) =  project_mat(1,1)*x4_tmp(1)              &
     &                        + project_mat(1,2)*x4_tmp(2)              &
     &                        + project_mat(1,3)*x4_tmp(3)              &
     &                        + project_mat(1,4)*x4_tmp(4)
        x4_projected(2,inod) =  project_mat(2,1)*x4_tmp(1)              &
     &                        + project_mat(2,2)*x4_tmp(2)              &
     &                        + project_mat(2,3)*x4_tmp(3)              &
     &                        + project_mat(2,4)*x4_tmp(4)
        x4_projected(3,inod) =  project_mat(3,1)*x4_tmp(1)              &
     &                        + project_mat(3,2)*x4_tmp(2)              &
     &                        + project_mat(3,3)*x4_tmp(3)              &
     &                        + project_mat(3,4)*x4_tmp(4)
        x4_projected(4,inod) =  project_mat(4,1)*x4_tmp(1)              &
     &                        + project_mat(4,2)*x4_tmp(2)              &
     &                        + project_mat(4,3)*x4_tmp(3)              &
     &                        + project_mat(4,4)*x4_tmp(4)
!
!
        coef = one / x4_projected(4,inod)
        x4_projected(1,inod) = x4_projected(1,inod) * coef
        x4_projected(2,inod) = x4_projected(2,inod) * coef
        x4_projected(3,inod) = x4_projected(3,inod) * coef
        x4_projected(4,inod) = one
      end do
!
      end subroutine project_once_each_element
!
! -----------------------------------------------------------------------
!
      subroutine project_once_each_ele_w_smp                            &
     &         (model_mat, project_mat, numnod, x4_projected)
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
      real(kind = kreal), intent(in) :: project_mat(4,4)
      integer(kind = kint), intent(in) :: numnod
!
      real(kind = kreal), intent(inout) :: x4_projected(4,numnod)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: coef
      real(kind = kreal) :: x4_tmp(4)
!
!
!$omp parallel do private(inod,x4_tmp,coef)
      do inod = 1, numnod
        x4_tmp(1) =  model_mat(1,1) * x4_projected(1,inod)              &
     &             + model_mat(1,2) * x4_projected(2,inod)              &
     &             + model_mat(1,3) * x4_projected(3,inod)              &
     &             + model_mat(1,4) * x4_projected(4,inod)
        x4_tmp(2) =  model_mat(2,1) * x4_projected(1,inod)              &
     &             + model_mat(2,2) * x4_projected(2,inod)              &
     &             + model_mat(2,3) * x4_projected(3,inod)              &
     &             + model_mat(2,4) * x4_projected(4,inod)
        x4_tmp(3) =  model_mat(3,1) * x4_projected(1,inod)              &
     &             + model_mat(3,2) * x4_projected(2,inod)              &
     &             + model_mat(3,3) * x4_projected(3,inod)              &
     &             + model_mat(3,4) * x4_projected(4,inod)
        x4_tmp(4) =  model_mat(4,1) * x4_projected(1,inod)              &
     &             + model_mat(4,2) * x4_projected(2,inod)              &
     &             + model_mat(4,3) * x4_projected(3,inod)              &
     &             + model_mat(4,4) * x4_projected(4,inod)
!
        x4_projected(1,inod) =  project_mat(1,1)*x4_tmp(1)              &
     &                        + project_mat(1,2)*x4_tmp(2)              &
     &                        + project_mat(1,3)*x4_tmp(3)              &
     &                        + project_mat(1,4)*x4_tmp(4)
        x4_projected(2,inod) =  project_mat(2,1)*x4_tmp(1)              &
     &                        + project_mat(2,2)*x4_tmp(2)              &
     &                        + project_mat(2,3)*x4_tmp(3)              &
     &                        + project_mat(2,4)*x4_tmp(4)
        x4_projected(3,inod) =  project_mat(3,1)*x4_tmp(1)              &
     &                        + project_mat(3,2)*x4_tmp(2)              &
     &                        + project_mat(3,3)*x4_tmp(3)              &
     &                        + project_mat(3,4)*x4_tmp(4)
        x4_projected(4,inod) =  project_mat(4,1)*x4_tmp(1)              &
     &                        + project_mat(4,2)*x4_tmp(2)              &
     &                        + project_mat(4,3)*x4_tmp(3)              &
     &                        + project_mat(4,4)*x4_tmp(4)
!
!
        coef = one / x4_projected(4,inod)
        x4_projected(1,inod) = x4_projected(1,inod) * coef
        x4_projected(2,inod) = x4_projected(2,inod) * coef
        x4_projected(3,inod) = x4_projected(3,inod) * coef
        x4_projected(4,inod) = one
      end do
!$omp end parallel do
!
      end subroutine project_once_each_ele_w_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_position_pvr_modelview                             &
     &         (model_mat, numnod, xx, x_nod_model)
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: x_nod_model(numnod,4)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private(inod)
      do inod = 1, numnod
          x_nod_model(inod,1) =  model_mat(1,1) * xx(inod,1)            &
     &                         + model_mat(1,2) * xx(inod,2)            &
     &                         + model_mat(1,3) * xx(inod,3)            &
     &                         + model_mat(1,4) * one
          x_nod_model(inod,2)  = model_mat(2,1) * xx(inod,1)            &
     &                         + model_mat(2,2) * xx(inod,2)            &
     &                         + model_mat(2,3) * xx(inod,3)            &
     &                         + model_mat(2,4) * one
          x_nod_model(inod,3) =  model_mat(3,1) * xx(inod,1)            &
     &                         + model_mat(3,2) * xx(inod,2)            &
     &                         + model_mat(3,3) * xx(inod,3)            &
     &                         + model_mat(3,4) * one
          x_nod_model(inod,4) =  model_mat(4,1) * xx(inod,1)            &
     &                         + model_mat(4,2) * xx(inod,2)            &
     &                         + model_mat(4,3) * xx(inod,3)            &
     &                         + model_mat(4,4) * one
      end do
!$omp end parallel do
!
      end subroutine cal_position_pvr_modelview
!
! -----------------------------------------------------------------------
!
      subroutine chenge_direction_pvr_modelview                         &
     &         (model_mat, numnod, xx, x_nod_model)
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(inout) :: x_nod_model(numnod,4)
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do private(inod)
      do inod = 1, numnod
          x_nod_model(inod,1) =  model_mat(1,1) * xx(inod,1)            &
     &                         + model_mat(1,2) * xx(inod,2)            &
     &                         + model_mat(1,3) * xx(inod,3)
          x_nod_model(inod,2)  = model_mat(2,1) * xx(inod,1)            &
     &                         + model_mat(2,2) * xx(inod,2)            &
     &                         + model_mat(2,3) * xx(inod,3)
          x_nod_model(inod,3) =  model_mat(3,1) * xx(inod,1)            &
     &                         + model_mat(3,2) * xx(inod,2)            &
     &                         + model_mat(3,3) * xx(inod,3)
          x_nod_model(inod,4) =  model_mat(4,1) * xx(inod,1)            &
     &                         + model_mat(4,2) * xx(inod,2)            &
     &                         + model_mat(4,3) * xx(inod,3)
      end do
!$omp end parallel do
!
      end subroutine chenge_direction_pvr_modelview
!
! -----------------------------------------------------------------------
!
      subroutine overwte_position_pvr_screen(project_mat, numnod,       &
     &                                       x_nod_screen)
!
      real(kind = kreal), intent(in) :: project_mat(4,4)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(inout) :: x_nod_screen(numnod,4)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: coef, x1, x2, x3, x4
!
!
!$omp parallel do private(inod,x1,x2,x3,x4,coef)
      do inod = 1, numnod
        x1 =  project_mat(1,1)*x_nod_screen(inod,1)                     &
     &      + project_mat(1,2)*x_nod_screen(inod,2)                     &
     &      + project_mat(1,3)*x_nod_screen(inod,3)                     &
     &      + project_mat(1,4)*x_nod_screen(inod,4)
        x2 =  project_mat(2,1)*x_nod_screen(inod,1)                     &
     &      + project_mat(2,2)*x_nod_screen(inod,2)                     &
     &      + project_mat(2,3)*x_nod_screen(inod,3)                     &
     &      + project_mat(2,4)*x_nod_screen(inod,4)
        x3 =  project_mat(3,1)*x_nod_screen(inod,1)                     &
     &      + project_mat(3,2)*x_nod_screen(inod,2)                     &
     &      + project_mat(3,3)*x_nod_screen(inod,3)                     &
     &      + project_mat(3,4)*x_nod_screen(inod,4)
        x4 =  project_mat(4,1)*x_nod_screen(inod,1)                     &
     &      + project_mat(4,2)*x_nod_screen(inod,2)                     &
     &      + project_mat(4,3)*x_nod_screen(inod,3)                     &
     &      + project_mat(4,4)*x_nod_screen(inod,4)
!
        coef = one / x4
        x_nod_screen(inod,1) = x1 * coef
        x_nod_screen(inod,2) = x2 * coef
        x_nod_screen(inod,3) = x3 * coef
      end do
!$omp end parallel do
!
      end subroutine overwte_position_pvr_screen
!
! -----------------------------------------------------------------------
!
      subroutine overwte_pvr_domain_on_screen(model_mat, project_mat,   &
     &          num_pvr_surf, xx_pvr_domain)
!
      real(kind = kreal), intent(in) :: model_mat(4,4)
      real(kind = kreal), intent(in) :: project_mat(4,4)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      real(kind = kreal), intent(inout)                                 &
     &                   :: xx_pvr_domain(4*num_pvr_surf,4)
!
      integer(kind = kint) :: inod, ntot
      real(kind = kreal) :: coef
!
      ntot = 4*num_pvr_surf
      call overwrite_projection_at_once                                 &
     &   (ntot, model_mat, project_mat, xx_pvr_domain(1,1))
!
!$omp parallel do private(coef,inod)
      do inod = 1, ntot
          coef = one / xx_pvr_domain(inod,4)
          xx_pvr_domain(inod,1) = xx_pvr_domain(inod,1) * coef
          xx_pvr_domain(inod,2) = xx_pvr_domain(inod,2) * coef
          xx_pvr_domain(inod,3) = xx_pvr_domain(inod,3) * coef
      end do
!$omp end parallel do
!
      end subroutine overwte_pvr_domain_on_screen
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_projection_at_once(nnod, A1, A2, x)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: A1(4,4), A2(4,4)
      real(kind = kreal), intent(inout) :: x(nnod,4)
!
      real(kind = kreal) :: x1, x2, x3, x4
      integer(kind = kint) :: inod
!
!$omp parallel do private(inod,x1,x2,x3,x4)
      do inod = 1, nnod
        x1 = A1(1,1)*x(inod,1) + A1(1,2)*x(inod,2)                      &
     &     + A1(1,3)*x(inod,3) + A1(1,4) * one
        x2 = A1(2,1)*x(inod,1) + A1(2,2)*x(inod,2)                      &
     &     + A1(2,3)*x(inod,3) + A1(2,4) * one
        x3 = A1(3,1)*x(inod,1) + A1(3,2)*x(inod,2)                      &
     &     + A1(3,3)*x(inod,3) + A1(3,4) * one
        x4 = A1(4,1)*x(inod,1) + A1(4,2)*x(inod,2)                      &
     &     + A1(4,3)*x(inod,3) + A1(4,4) * one
!
        x(inod,1) = A2(1,1)*x1 + A2(1,2)*x2 + A2(1,3)*x3 + A2(1,4) * x4
        x(inod,2) = A2(2,1)*x1 + A2(2,2)*x2 + A2(2,3)*x3 + A2(2,4) * x4
        x(inod,3) = A2(3,1)*x1 + A2(3,2)*x2 + A2(3,3)*x3 + A2(3,4) * x4
        x(inod,4) = A2(4,1)*x1 + A2(4,2)*x2 + A2(4,3)*x3 + A2(4,4) * x4
      end do
!$omp end parallel do
!
      end subroutine overwrite_projection_at_once
!
! -----------------------------------------------------------------------
!
      end module set_position_pvr_screen
