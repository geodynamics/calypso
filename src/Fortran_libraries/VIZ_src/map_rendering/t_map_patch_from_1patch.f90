!>@file   t_map_patch_from_1patch.f90
!!@brief  module t_map_patch_from_1patch
!!
!!@author  H. Matsui
!!@date Programmed in May, 2023
!
!>@brief Divided triangle patch for map projection
!!
!!@verbatim
!!      subroutine alloc_map_patch_from_1patch(map_e)
!!      subroutine dealloc_map_patch_from_1patch(map_e)
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!
!!      subroutine set_sph_position_4_map_patch(x_map_patch,            &
!!     &                                        rtp_map_patch)
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: x_map_patch(num_triangle,n_vector)
!!        real(kind = kreal), intent(inout)                             &
!!     &          :: rtp_map_patch(num_triangle,n_vector)
!!      subroutine patch_to_aitoff(rtp_map_patch, xy_map)
!!        real(kind = kreal), intent(in)                                &
!!     &          :: rtp_map_patch(num_triangle,n_vector)
!!        real(kind = kreal), intent(inout) :: xy_map(2,num_triangle)
!!@endverbatim
      module t_map_patch_from_1patch
!
      use m_precision
!
      use m_constants
      use m_phys_constants
      use m_geometry_constants
!
      implicit none
!
      real(kind = kreal), parameter, private :: EPSILON = 1.0d-9
!
      integer(kind = kint), parameter :: nmax_map_p = 3
!
      type map_patches_for_1patch
        integer(kind = kint) :: n_map_patch
!
        real(kind=kreal), allocatable :: xy_map(:,:,:)
        real(kind=kreal), allocatable :: d_map_patch(:,:)
!
        real(kind=kreal), allocatable :: x_map_patch(:,:,:)
        real(kind=kreal), allocatable :: rtp_map_patch(:,:,:)
      end type map_patches_for_1patch
!
!      private :: nmax_map_p, x_map_patch, rtp_map_patch
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_map_patch_from_1patch(map_e)
!
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      allocate(map_e%xy_map(2,num_triangle,nmax_map_p))
      allocate(map_e%d_map_patch(num_triangle,nmax_map_p))
      allocate(map_e%x_map_patch(num_triangle,n_vector,nmax_map_p))
      allocate(map_e%rtp_map_patch(num_triangle,n_vector,nmax_map_p))
!
      map_e%xy_map(1:2,1:num_triangle,1:nmax_map_p) =  zero
      map_e%d_map_patch(1:num_triangle,1:nmax_map_p) = zero
      map_e%x_map_patch(1:num_triangle,1:n_vector,1:nmax_map_p) =  zero
      map_e%rtp_map_patch(1:num_triangle,1:n_vector,1:nmax_map_p)= zero
!
      end subroutine alloc_map_patch_from_1patch
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_map_patch_from_1patch(map_e)
!
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      deallocate(map_e%xy_map)
      deallocate(map_e%d_map_patch, map_e%x_map_patch)
      deallocate(map_e%rtp_map_patch)
!
      end subroutine dealloc_map_patch_from_1patch
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_sph_position_4_map_patch(x_map_patch,              &
     &                                        rtp_map_patch)
!
      use coordinate_converter
!
      real(kind = kreal), intent(inout)                                 &
     &          :: x_map_patch(num_triangle,n_vector)
      real(kind = kreal), intent(inout)                                 &
     &          :: rtp_map_patch(num_triangle,n_vector)
!
      integer(kind = kint) :: k1
      real(kind = kreal) :: x_center
      real(kind = kreal) :: y_center
      real(kind = kreal) :: ar_map(3), rs_map(3), as_map(3)
      real(kind = kreal) :: pi, yflag
!
!
      x_center = (x_map_patch(1,1) + x_map_patch(2,1)                   &
     &          + x_map_patch(3,1) ) / three
      y_center = (x_map_patch(1,2) + x_map_patch(2,2)                   &
     &          + x_map_patch(3,2) ) / three
!
      pi = four * atan(one)
      call position_2_sph(ithree, x_map_patch, rtp_map_patch(1,1),      &
     &                    rtp_map_patch(1,2), rtp_map_patch(1,3),       &
     &                    ar_map(1), rs_map(1), as_map(1))
      rtp_map_patch(1:3,3) = mod((rtp_map_patch(1:3,3)+pi),(two*pi))
!
      yflag = x_map_patch(1,2) * x_map_patch(2,2) * x_map_patch(3,2)
      if(yflag.eq.zero .and. x_center.le.zero) then
        if(y_center .le. zero) then
          if(abs(x_map_patch(1,2)) .lt. EPSILON                         &
     &      .and. x_map_patch(1,1).lt.zero) rtp_map_patch(1,3) = zero
          if(abs(x_map_patch(2,2)) .lt. EPSILON                         &
     &      .and. x_map_patch(2,1).lt.zero) rtp_map_patch(2,3) = zero
          if(abs(x_map_patch(3,2)) .lt. EPSILON                         &
     &      .and. x_map_patch(3,1).lt.zero) rtp_map_patch(3,3) = zero
        else
          if(abs(x_map_patch(1,2)) .lt. EPSILON                         &
     &      .and. x_map_patch(1,1).lt.zero) rtp_map_patch(1,3) = two*pi
          if(abs(x_map_patch(2,2)) .lt. EPSILON                         &
     &      .and. x_map_patch(2,1).lt.zero) rtp_map_patch(2,3) = two*pi
          if(abs(x_map_patch(3,2)) .lt. EPSILON                         &
     &      .and. x_map_patch(3,1).lt.zero) rtp_map_patch(3,3) = two*pi
        end if
!
      end if
!
      end subroutine set_sph_position_4_map_patch
!
!-----------------------------------------------------------------------
!
      subroutine patch_to_aitoff(rtp_map_patch, xy_map)
!
      use aitoff
!
      real(kind = kreal), intent(in)                                    &
     &          :: rtp_map_patch(num_triangle,n_vector)
      real(kind = kreal), intent(inout) :: xy_map(2,num_triangle)
!
      integer(kind = kint) :: k1
      real(kind = kreal) :: s_theta, c_theta, pi, phi_map
!
!
      pi = four * atan(one)
      do k1 = 1, num_triangle
        s_theta = sin(rtp_map_patch(k1,2))
        c_theta = cos(rtp_map_patch(k1,2))
!        phi_map = mod((rtp_map_patch(k1,3)+pi),(two*pi))
        call s_aitoff(s_theta, c_theta, rtp_map_patch(k1,3),            &
     &                xy_map(1,k1), xy_map(2,k1))
      end do
!
      end subroutine patch_to_aitoff
!
!-----------------------------------------------------------------------
!
      end module t_map_patch_from_1patch
