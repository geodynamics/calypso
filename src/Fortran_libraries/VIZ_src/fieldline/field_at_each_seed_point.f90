!>@file   field_at_each_seed_point.f90
!!@brief  module field_at_each_seed_point
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Evaluate field data at each seed points
!!
!!@verbatim
!!      subroutine cal_FLINE_element_size(node, ele, ele_size)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(inout) :: ele_size(ele%numele)
!!      subroutine seed_distance_from_ele_center                        &
!!     &         (ele, xx_surf_start_fline, ele_size,                   &
!!     &          idx_fln_dist, dist_flne, num_search)
!!        type(element_data), intent(in) :: ele
!!        real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
!!        real(kind = kreal), intent(in) :: ele_size(ele%numele)
!!        integer(kind = kint), intent(inout) :: num_search
!!        integer(kind = kint), intent(inout) :: idx_fln_dist(ele%numele)
!!        real(kind = kreal), intent(inout) :: dist_flne(ele%numele)
!!
!!      integer(kind = kint) function                                   &
!!     &      surface_mode_in_each_ele(error_level, xi_surf_start_fline)
!!        real(kind = kreal), intent(in) :: error_level
!!        real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!!
!!      subroutine cal_each_seed_velocity_in_ele                        &
!!     &         (ele, n_point, d_velo, iele_surf_start_fline,          &
!!     &          xi_surf_start_fline, v_fline_start)
!!        type(element_data), intent(in) :: ele
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(in) :: d_nod(n_point,3)
!!        integer(kind = kint), intent(in) :: iele_surf_start_fline(1)
!!        real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!!        real(kind = kreal), intent(inout) :: v_fline_start(4)
!!@endverbatim
!
      module field_at_each_seed_point
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_ctl_params_viz_fields
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_FLINE_element_size(node, ele, ele_size)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      real(kind = kreal), intent(inout) :: ele_size(ele%numele)
!
      real(kind = kreal) :: x(ele%nnod_4_ele)
      real(kind = kreal) :: y(ele%nnod_4_ele)
      real(kind = kreal) :: z(ele%nnod_4_ele)
      real(kind = kreal) :: size_max(3)
      integer(kind = kint) :: inod, iele, k1
!
!$omp parallel do private(iele,k1,inod,x,y,z)
      do iele = 1, ele%numele
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          x(k1) = node%xx(inod,1)
          y(k1) = node%xx(inod,2)
          z(k1) = node%xx(inod,3)
        end do
        size_max(1) = maxval(x) - minval(x)
        size_max(2) = maxval(y) - minval(y)
        size_max(3) = maxval(z) - minval(z)
        ele_size(iele) = sqrt(size_max(1)*size_max(1)                   &
     &                      + size_max(2)*size_max(2)                   &
     &                      + size_max(3)*size_max(3))
      end do
!$omp end parallel do
!
      end subroutine cal_FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      subroutine seed_distance_from_ele_center                          &
     &         (ele, xx_surf_start_fline, ele_size,                     &
     &          idx_fln_dist, dist_flne, num_search)
!
      type(element_data), intent(in) :: ele
      real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
      real(kind = kreal), intent(in) :: ele_size(ele%numele)
!
      integer(kind = kint), intent(inout) :: num_search
      integer(kind = kint), intent(inout) :: idx_fln_dist(ele%numele)
      real(kind = kreal), intent(inout) :: dist_flne(ele%numele)
!
      integer(kind = kint) :: iele
      real(kind = kreal) :: dist
!
!
      num_search = 0
      do iele = 1, ele%numele
        dist = sqrt((xx_surf_start_fline(1) - ele%x_ele(iele,1))**2     &
     &            + (xx_surf_start_fline(2) - ele%x_ele(iele,2))**2     &
     &            + (xx_surf_start_fline(3) - ele%x_ele(iele,3))**2)
        if(dist .le. ele_size(iele)) then
          num_search = num_search + 1
          idx_fln_dist(num_search) = iele
          dist_flne(num_search) =    dist
        end if
      end do
!
      end subroutine seed_distance_from_ele_center
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &      surface_mode_in_each_ele(error_level, xi_surf_start_fline)
!
      real(kind = kreal), intent(in) :: error_level
      real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!
      integer(kind = kint) :: i_mode
!
      i_mode = 0
      if(abs(xi_surf_start_fline(1)+one) .lt. error_level) i_mode = 1
      if(abs(xi_surf_start_fline(1)-one) .lt. error_level) i_mode = 2
      if(abs(xi_surf_start_fline(2)+one) .lt. error_level) i_mode = 3
      if(abs(xi_surf_start_fline(2)-one) .lt. error_level) i_mode = 4
      if(abs(xi_surf_start_fline(3)+one) .lt. error_level) i_mode = 5
      if(abs(xi_surf_start_fline(3)-one) .lt. error_level) i_mode = 6
!
      surface_mode_in_each_ele = i_mode
!
      end function surface_mode_in_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine cal_each_seed_velocity_in_ele                          &
     &         (ele, n_point, d_velo, iele_surf_start_fline,            &
     &          xi_surf_start_fline, v_fline_start)
!
      use sel_interpolate_scalar
!
      type(element_data), intent(in) :: ele
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_velo(n_point,3)
!
      integer(kind = kint), intent(in) :: iele_surf_start_fline(1)
      real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
!
      real(kind = kreal), intent(inout) :: v_fline_start(4)
!
! 
      call sel_sgl_interpolate_scalar_ele                               &
     &   (n_point, ele%numele, ele%nnod_4_ele, ele%ie, d_velo(1,1),     &
     &    iele_surf_start_fline(1), xi_surf_start_fline,                &
     &    v_fline_start(1))
      call sel_sgl_interpolate_scalar_ele                               &
     &   (n_point, ele%numele, ele%nnod_4_ele, ele%ie, d_velo(1,2),     &
     &    iele_surf_start_fline(1), xi_surf_start_fline,                &
     &    v_fline_start(2))
      call sel_sgl_interpolate_scalar_ele                               &
     &   (n_point, ele%numele, ele%nnod_4_ele, ele%ie, d_velo(1,3),     &
     &    iele_surf_start_fline(1), xi_surf_start_fline,                &
     &    v_fline_start(3))
      v_fline_start(4) = one
!
      end subroutine cal_each_seed_velocity_in_ele
!
!  ---------------------------------------------------------------------
!
      end module field_at_each_seed_point
