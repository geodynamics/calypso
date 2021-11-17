!>@file   find_pvr_surf_domain.f90
!!@brief  module find_pvr_surf_domain
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2011
!
!>@brief Construct subdomain surface information for PVR
!!
!!@verbatim
!!      subroutine find_each_pvr_surf_domain                            &
!!     &         (ele, surf, draw_param, pvr_bound)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!      subroutine set_pvr_domain_surface_data(n_pvr_pixel, node, surf, &
!!     &          modelview_mat, projection_mat, pvr_bound)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!!        real(kind = kreal), intent(in) :: modelview_mat(4,4)
!!        real(kind = kreal), intent(in) :: projection_mat(4,4)
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!      subroutine norm_on_model_pvr_domains(node, surf, modelview_mat, &
!!     &          num_pvr_surf, item_pvr_surf_domain,                   &
!!     &          screen_norm_pvr_domain)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        real(kind = kreal), intent(in) :: modelview_mat(4,4)
!!        integer(kind = kint), intent(in) :: num_pvr_surf
!!        integer(kind = kint), intent(in)                              &
!!     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!!        real(kind = kreal), intent(inout)                             &
!!     &                    :: screen_norm_pvr_domain(3,num_pvr_surf)
!!      subroutine deallocate_pvr_surf_domain(num_pvr, pvr_bound)
!!@endverbatim
!
      module find_pvr_surf_domain
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
!
      implicit  none
!
      integer(kind = kint), allocatable, private :: imark_sf(:)
!
      private :: range_on_screen_pvr_domains
      private :: range_on_pixel_pvr_domains
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine find_each_pvr_surf_domain                              &
     &         (ele, surf, draw_param, pvr_bound)
!
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_geometries_in_pvr_screen
      use find_selected_domain_bd
      use pvr_surface_enhancement
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(rendering_parameter), intent(in) :: draw_param
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
      integer(kind = kint) :: num_pvr_sf_local
!
!
      allocate(imark_sf(surf%numsurf))
!$omp parallel workshare
      imark_sf(1:surf%numsurf) = 0
!$omp end parallel workshare
!
      call mark_selected_domain_bd                                      &
     &   (ele%numele, surf%numsurf, surf%isf_4_ele,                     &
     &    draw_param%iflag_used_ele, imark_sf)
      call count_selected_domain_bd                                     &
     &   (surf%numsurf, imark_sf, num_pvr_sf_local)
!
      call alloc_pvr_surf_domain_item(num_pvr_sf_local, pvr_bound)
!
      call s_find_selected_domain_bd(ele%numele, surf%numsurf,          &
     &    surf%iele_4_surf, imark_sf, draw_param%iflag_used_ele,        &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf)
!
      deallocate(imark_sf)
!
      end subroutine find_each_pvr_surf_domain
!
! -----------------------------------------------------------------------
!
      subroutine set_pvr_domain_surface_data(n_pvr_pixel, node, surf,   &
     &          modelview_mat, projection_mat, pvr_bound)
!
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use ordering_pvr_sf_domain_grp
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: projection_mat(4,4)
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
!$omp parallel
      call range_on_screen_pvr_domains                                  &
     &   (node, surf, modelview_mat, projection_mat,                    &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_posi, pvr_bound%screen_w,                    &
     &    pvr_bound%screen_xrng, pvr_bound%screen_yrng,                 &
     &    pvr_bound%screen_zrng)
      call range_on_pixel_pvr_domains                                   &
     &   (n_pvr_pixel, pvr_bound%num_pvr_surf,                          &
     &    pvr_bound%screen_xrng, pvr_bound%screen_yrng,                 &
     &    pvr_bound%isurf_xrng,  pvr_bound%jsurf_yrng)
!$omp end parallel
!
      call s_ordering_pvr_sf_domain_grp(pvr_bound)
!
      end subroutine set_pvr_domain_surface_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine norm_on_model_pvr_domains(node, surf, modelview_mat,   &
     &          num_pvr_surf, item_pvr_surf_domain,                     &
     &          screen_norm_pvr_domain)
!
      use cal_fline_in_cube
      use set_position_pvr_screen
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_norm_pvr_domain(3,num_pvr_surf)
!
      integer(kind = kint) :: inum, iele, k1, isurf
      real(kind = kreal) :: x31(3), x42(3), vlen
      real(kind = kreal) :: xx4_model_sf(4,num_linear_sf,nsurf_4_ele)
!
!
!$omp parallel do private(inum,iele,k1,isurf,xx4_model_sf,x31,x42,vlen)
      do inum = 1, num_pvr_surf
        iele = item_pvr_surf_domain(1,inum)
        k1 =   item_pvr_surf_domain(2,inum)
        isurf = abs(surf%isf_4_ele(iele,k1))
!
        call position_on_each_ele_sfs_wone                              &
     &     (surf, node%numnod, node%xx, iele, xx4_model_sf)
        call overwte_to_modelview_each_ele(modelview_mat,               &
     &      (num_linear_sf*nsurf_4_ele), xx4_model_sf(1,1,1))
        x31(1:3) = xx4_model_sf(1:3,3,k1) - xx4_model_sf(1:3,1,k1)
        x42(1:3) = xx4_model_sf(1:3,4,k1) - xx4_model_sf(1:3,2,k1)
!
        screen_norm_pvr_domain(1,inum)                                  &
     &                  = (x31(2)*x42(3) - x31(3)*x42(2))               &
     &                   * dble(surf%isf_4_ele(iele,k1) /isurf)
        screen_norm_pvr_domain(2,inum)                                  &
     &                  = (x31(3)*x42(1) - x31(1)*x42(3))               &
     &                   * dble(surf%isf_4_ele(iele,k1) /isurf)
        screen_norm_pvr_domain(3,inum)                                  &
     &                  = (x31(1)*x42(2) - x31(2)*x42(1))               &
     &                   * dble(surf%isf_4_ele(iele,k1) /isurf)
!
        vlen = sqrt(screen_norm_pvr_domain(1,inum)**2                   &
     &              + screen_norm_pvr_domain(2,inum)**2                 &
     &              + screen_norm_pvr_domain(3,inum)**2)
!
        if(vlen .gt. zero) then
          screen_norm_pvr_domain(1,inum)                                &
     &                  = screen_norm_pvr_domain(1,inum) / vlen
          screen_norm_pvr_domain(2,inum)                                &
     &                  = screen_norm_pvr_domain(2,inum) / vlen
          screen_norm_pvr_domain(3,inum)                                &
     &                  = screen_norm_pvr_domain(3,inum) / vlen
        end if
      end do
!$omp end parallel do
!
      end subroutine norm_on_model_pvr_domains
!
! -----------------------------------------------------------------------
!
      subroutine range_on_screen_pvr_domains                            &
     &         (node, surf, modelview_mat, projection_mat,              &
     &          num_pvr_surf, item_pvr_surf_domain,                     &
     &          screen_posi_pvr_domain, screen_w_pvr_domain,            &
     &          screen_xrng_pvr_domain, screen_yrng_pvr_domain,         &
     &          screen_zrng_pvr_domain)
!
      use cal_fline_in_cube
      use set_position_pvr_screen
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
!
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: projection_mat(4,4)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      integer(kind = kint), intent(in)                                  &
     &                    :: item_pvr_surf_domain(2,num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_posi_pvr_domain(3,num_pvr_surf)
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_w_pvr_domain(num_pvr_surf)
!
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_xrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_yrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(inout)                                 &
     &                    :: screen_zrng_pvr_domain(2,num_pvr_surf)
!
      integer(kind = kint) :: inum, iele, k1, isurf
      real(kind = kreal) :: x1(3), x2(3), x3(3), x4(3), w(4)
      real(kind = kreal) :: xx4_model_sf(4,num_linear_sf,nsurf_4_ele)
!
!
!$omp do private (inum,iele,k1,isurf,xx4_model_sf,x1,x2,x3,x4,w)
      do inum = 1, num_pvr_surf
        iele = item_pvr_surf_domain(1,inum)
        k1 =   item_pvr_surf_domain(2,inum)
        isurf = abs(surf%isf_4_ele(iele,k1))
!
        call position_on_each_ele_sfs_wone                              &
     &     (surf, node%numnod, node%xx, iele, xx4_model_sf)
        call project_once_each_element(modelview_mat, projection_mat,   &
     &      (num_linear_sf*nsurf_4_ele), xx4_model_sf(1,1,1))
        x1(1:3) = xx4_model_sf(1:3,1,k1)
        x2(1:3) = xx4_model_sf(1:3,2,k1)
        x3(1:3) = xx4_model_sf(1:3,3,k1)
        x4(1:3) = xx4_model_sf(1:3,4,k1)
        w(1:4) =  xx4_model_sf(4,1:4,k1)
!
        screen_posi_pvr_domain(1:3,inum)                                &
     &           = (x1(1:3) + x2(1:3) + x3(1:3) + x4(1:3)) / four
        screen_w_pvr_domain(inum) = (w(1)+w(2)+w(3)+w(4)) / four
!
        screen_xrng_pvr_domain(1,inum) = min(x1(1),x2(1),x3(1),x4(1))
        screen_xrng_pvr_domain(2,inum) = max(x1(1),x2(1),x3(1),x4(1))
        screen_yrng_pvr_domain(1,inum) = min(x1(2),x2(2),x3(2),x4(2))
        screen_yrng_pvr_domain(2,inum) = max(x1(2),x2(2),x3(2),x4(2))
        screen_zrng_pvr_domain(1,inum) = min(x1(3),x2(3),x3(3),x4(3))
        screen_zrng_pvr_domain(2,inum) = max(x1(3),x2(3),x3(3),x4(3))
      end do
!$omp end do
!
      end subroutine range_on_screen_pvr_domains
!
! -----------------------------------------------------------------------
!
      subroutine range_on_pixel_pvr_domains(n_pvr_pixel, num_pvr_surf,  &
     &          screen_xrng_pvr_domain, screen_yrng_pvr_domain,         &
     &          isurf_xrng_pvr_domain, jsurf_yrng_pvr_domain)
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!
      integer(kind = kint), intent(in) :: num_pvr_surf
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_xrng_pvr_domain(2,num_pvr_surf)
      real(kind = kreal), intent(in)                                    &
     &                    :: screen_yrng_pvr_domain(2,num_pvr_surf)
!
      integer(kind = kint), intent(inout)                               &
     &                    :: isurf_xrng_pvr_domain(2,num_pvr_surf)
      integer(kind = kint), intent(inout)                               &
     &                    :: jsurf_yrng_pvr_domain(2,num_pvr_surf)
!
      integer(kind = kint) :: inum
!
!
!$omp do private (inum)
        do inum = 1, num_pvr_surf
          isurf_xrng_pvr_domain(1,inum)                                 &
     &          = nint( (screen_xrng_pvr_domain(1,inum) + one)          &
     &           * half * dble(n_pvr_pixel(1)) )
          isurf_xrng_pvr_domain(2,inum)                                 &
     &          = nint( (screen_xrng_pvr_domain(2,inum) + one)          &
     &           * half * dble(n_pvr_pixel(1)) )
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &          = nint( (screen_yrng_pvr_domain(1,inum) + one)          &
     &           * half * dble(n_pvr_pixel(2)) )
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &          = nint( (screen_yrng_pvr_domain(2,inum) + one)          &
     &           * half * dble(n_pvr_pixel(2)) )
!
          isurf_xrng_pvr_domain(1,inum)                                 &
     &       = max(isurf_xrng_pvr_domain(1,inum),ione)
          isurf_xrng_pvr_domain(1,inum)                                 &
     &       = min(isurf_xrng_pvr_domain(1,inum),n_pvr_pixel(1))
          isurf_xrng_pvr_domain(2,inum)                                 &
     &       = max(isurf_xrng_pvr_domain(2,inum),ione)
          isurf_xrng_pvr_domain(2,inum)                                 &
     &       = min(isurf_xrng_pvr_domain(2,inum),n_pvr_pixel(1))
!
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &       = max(jsurf_yrng_pvr_domain(1,inum),ione)
          jsurf_yrng_pvr_domain(1,inum)                                 &
     &       = min(jsurf_yrng_pvr_domain(1,inum),n_pvr_pixel(2))
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &       = max(jsurf_yrng_pvr_domain(2,inum),ione)
          jsurf_yrng_pvr_domain(2,inum)                                 &
     &       = min(jsurf_yrng_pvr_domain(2,inum),n_pvr_pixel(2))
      end do
!$omp end do
!
      end subroutine range_on_pixel_pvr_domains
!
! -----------------------------------------------------------------------
!
      end module find_pvr_surf_domain
