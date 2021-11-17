!>@file  cal_fline_in_cube.f90
!!       module cal_fline_in_cube
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief trace field line in one cube element
!!
!!@verbatim
!!      subroutine position_on_each_ele_surfs                           &
!!     &         (surf, numnod, xx, iele, xx4_ele_surf)
!!      subroutine position_on_each_ele_sfs_wone                        &
!!     &         (surf, numnod, xx, iele, xx4_ele_surf)
!!        type(surface_data), intent(in) :: surf
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(in) :: xx(numnod,3)
!!        integer(kind = kint), intent(in) :: iele, isf_org
!!        real(kind = kreal), intent(inout)                             &
!!     &             :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!!      subroutine find_line_end_in_1ele(iflag_dir, isf_org, fline, x0, &
!!     &          xx4_ele_surf, isf_tgt, x4_tgt, xi)
!!        integer(kind = kint), intent(in) :: iflag_dir
!!        integer(kind = kint), intent(in) :: isf_org
!!        real(kind = kreal), intent(in) :: fline(4), x0(4)
!!        real(kind = kreal), intent(in)                                &
!!     &             :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!!        integer(kind = kint), intent(inout) :: isf_tgt
!!        real(kind = kreal), intent(inout) :: x4_tgt(4)
!!        real(kind = kreal), intent(inout) :: xi(2)
!!
!!      subroutine cal_fline_to_square(x0, vec, x_quad, x_tgt, ierr)
!!      subroutine cal_filne_to_triangle(x0, vec, x_tri, x_tgt, ierr)
!!@endverbatim
!
      module cal_fline_in_cube
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use t_surface_data
!
!
      implicit  none
!
      integer(kind = kint), parameter :: iflag_forward_line =   1
      integer(kind = kint), parameter :: iflag_backward_line = -1
!
      private :: cal_fline_to_square, cal_filne_to_triangle
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine position_on_each_ele_surfs                             &
     &         (surf, numnod, xx, iele, xx4_ele_surf)
!
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout)                                 &
     &             :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
      integer(kind = kint) :: ksf, k2
      integer(kind = kint) :: inod, isurf
!
!
      do ksf = 1, nsurf_4_ele
        isurf = abs(surf%isf_4_ele(iele,ksf))
        do k2 = 1, num_linear_sf
          inod = surf%ie_surf(isurf,k2)
          xx4_ele_surf(1,k2,ksf) = xx(inod,1)
          xx4_ele_surf(2,k2,ksf) = xx(inod,2)
          xx4_ele_surf(3,k2,ksf) = xx(inod,3)
          xx4_ele_surf(4,k2,ksf) = zero
        end do
      end do
!
      end subroutine position_on_each_ele_surfs
!
!------------------------------------------------------------------
!
      subroutine position_on_each_ele_sfs_wone                          &
     &         (surf, numnod, xx, iele, xx4_ele_surf)
!
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
      integer(kind = kint), intent(in) :: iele
!
      real(kind = kreal), intent(inout)                                 &
     &             :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
      integer(kind = kint) :: ksf, k2
      integer(kind = kint) :: inod, isurf
!
!
      do ksf = 1, nsurf_4_ele
        isurf = abs(surf%isf_4_ele(iele,ksf))
        do k2 = 1, num_linear_sf
          inod = surf%ie_surf(isurf,k2)
          xx4_ele_surf(1,k2,ksf) = xx(inod,1)
          xx4_ele_surf(2,k2,ksf) = xx(inod,2)
          xx4_ele_surf(3,k2,ksf) = xx(inod,3)
          xx4_ele_surf(4,k2,ksf) = one
        end do
      end do
!
      end subroutine position_on_each_ele_sfs_wone
!
!------------------------------------------------------------------
!
      subroutine find_line_end_in_1ele(iflag_dir, isf_org, fline, x0,   &
     &          xx4_ele_surf, isf_tgt, x4_tgt, xi)
!
      integer(kind = kint), intent(in) :: iflag_dir
      integer(kind = kint), intent(in) :: isf_org
      real(kind = kreal), intent(in) :: fline(4), x0(4)
      real(kind = kreal), intent(in)                                    &
     &             :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_tgt
      real(kind = kreal), intent(inout) :: x4_tgt(4)
      real(kind = kreal), intent(inout) :: xi(2)
!
      real(kind = kreal) :: b_ray(4)
      integer(kind = kint) :: ierr
      integer(kind = kint) :: ist, ied, inc, k, ksf
!
!
      if(iflag_dir .eq. iflag_forward_line) then
        b_ray(1:4) = -fline(1:4)
      else
        b_ray(1:4) =  fline(1:4)
      end if
!
      if(isf_org .eq. 0) then
        ist =  1
        ied =  nsurf_4_ele
        inc =  1
      else if(mod(isf_org,itwo) .eq. ione) then
        ist =  1
        ied =  nsurf_4_ele-1
        inc =  1
      else
        ist =  nsurf_4_ele-1
        ied =  1
        inc = -1
      end if
!
      isf_tgt = izero
      do k = ist, ied, inc
        ksf = mod(isf_org+k-ione,nsurf_4_ele) + ione
        call cal_fline_to_square(x0, b_ray, xx4_ele_surf(1,1,ksf),      &
     &                           x4_tgt, xi, ierr)
        if(ierr.eq.zero) then
          isf_tgt = ksf
          exit
        end if
      end do
!
      if(isf_tgt .gt. izero) return
!
!      write(my_rank+60,'(i3,1p3e16.7)') (-ione), b_ray(1:4)
!      write(my_rank+60,'(i3,1p3e16.7)') izero, x0(1:4)
!
      do k = ist, ied, inc
        ksf = mod(isf_org+k-ione,nsurf_4_ele) + ione
        call cal_fline_to_square(x0, b_ray, xx4_ele_surf(1,1,ksf),      &
     &                           x4_tgt, xi, ierr)
      end do
!
      end subroutine find_line_end_in_1ele
!
!------------------------------------------------------------------
!
      subroutine cal_fline_to_square(x0, vec, x_quad, x4_tgt, xi, ierr)
!
      real(kind = kreal), intent(in) :: x_quad(4,num_linear_sf)
      real(kind = kreal), intent(in) :: vec(4), x0(4)
      real(kind = kreal), intent(inout) :: x4_tgt(4), xi(2)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind = kreal) :: x4_tri(4,num_triangle), sol(4,2)
!
      x4_tri(1:4,1) = x_quad(1:4,1)
      x4_tri(1:4,2) = x_quad(1:4,2)
      x4_tri(1:4,3) = x_quad(1:4,4)
!
      call cal_filne_to_triangle                                        &
     &   (x0, vec, x4_tri, x4_tgt, sol(1,1), ierr)
!
      if(ierr .eq. izero) then
        xi(1) = -one + two*sol(1,1)
        xi(2) = -one + two*sol(2,1)
        return
      end if
!
      x4_tri(1:4,1) = x_quad(1:4,3)
      x4_tri(1:4,2) = x_quad(1:4,2)
      x4_tri(1:4,3) = x_quad(1:4,4)
!
      call cal_filne_to_triangle                                        &
     &   (x0, vec, x4_tri, x4_tgt, sol(1,2), ierr)
      if(ierr .eq. izero) then
        xi(1) = one - two*sol(2,2)
        xi(2) = one - two*sol(1,2)
      end if
!
      end subroutine cal_fline_to_square
!
!------------------------------------------------------------------
!
      subroutine cal_filne_to_triangle(x0, v, x4_tri, x4_tgt, sol, ierr)
!
      use solver_33_array
!
      real(kind = kreal), intent(in) :: x4_tri(4,num_triangle)
      real(kind = kreal), intent(in) :: v(4), x0(4)
      real(kind = kreal), intent(inout) :: x4_tgt(4), sol(4)
      integer(kind = kint), intent(inout) :: ierr
!
      real(kind = kreal) :: rvec(4), mat(3,3)
!
!
      rvec(1:4) = x0(1:4) - x4_tri(1:4,1)
!
      mat(1:3,1) = x4_tri(1:3,2) - x4_tri(1:3,1)
      mat(1:3,2) = x4_tri(1:3,3) - x4_tri(1:3,1)
      mat(1:3,3) = -v(1:3)
      call solve_33_array(sol(1), rvec(1), mat)
!
      if(sol(3).gt.zero .and. sol(1).ge.zero .and. sol(2).ge.zero       &
     &   .and. (sol(1)+sol(2)).le.one) then
        x4_tgt(1:4) = x0(1:4) + sol(3) * v(1:4)
        ierr = 0
      else
        ierr = ione
      end if
!
      end subroutine cal_filne_to_triangle
!
!  ---------------------------------------------------------------------
!
      end module cal_fline_in_cube
