!>@file  pvr_axis_label.f90
!!       module pvr_axis_label
!!
!!@author H. Matsui
!!@date   Programmed in May. 2006
!
!> @brief Structures for parameteres for volume rendering
!!
!!@verbatim
!!      subroutine axis_direction_in_screen(pvr_screen)
!!        type(pvr_projected_position), intent(inout) :: pvr_screen
!!      subroutine set_pvr_axislabel(num_pixel, n_pvr_pixel, iscale,    &
!!     &                             pvr_screen, rgba_gl)
!!        integer(kind = kint), intent(in) :: num_pixel
!!        integer(kind = kint), intent(in) :: iscale
!!        integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        real(kind = kreal), intent(inout)  :: rgba_gl(4,num_pixel)
!!@endverbatim
!
      module pvr_axis_label
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      real(kind = kreal), parameter :: axis_vect(3,3)                   &
         = reshape((/1,0,0,  0,1,0,  0,0,1/), shape=(/3,3/))
      character(len=1), parameter :: axis_label(3) = (/'x','y','z'/)
!
      private :: axis_vect
      private :: find_draw_axis_order
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine axis_direction_in_screen(pvr_screen)
!
      use t_geometries_in_pvr_screen
      use set_position_pvr_screen
!
      type(pvr_projected_position), intent(inout) :: pvr_screen
!
!
      call cal_position_pvr_modelview(pvr_screen%modelview_mat,         &
     &    ithree, axis_vect, pvr_screen%axis_view)
      call find_draw_axis_order                                         &
     &   (pvr_screen%axis_view, pvr_screen%axis_order)
      call overwte_position_pvr_screen(pvr_screen%projection_mat,       &
     &    ithree, pvr_screen%axis_view)
!
     end subroutine axis_direction_in_screen
!
!  ---------------------------------------------------------------------
!
      subroutine set_pvr_axislabel(num_pixel, n_pvr_pixel, iscale,      &
     &                             pvr_screen, rgba_gl)
!
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use set_position_pvr_screen
      use draw_pvr_colorbar_nums
!
      integer(kind = kint), intent(in) :: num_pixel
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      real(kind = kreal), intent(inout)  :: rgba_gl(4,num_pixel)
!
      integer(kind = kint) :: i, j, l, n, m, i2, j2
      integer(kind = kint) :: length, ist_px, ist_py, inod
      real(kind = kreal) :: rlen, rhgt
      real(kind = kreal) :: r, rmax
!
!
      rmax = zero
      do m = 1, 3
        n = pvr_screen%axis_order(m)
        r = sqrt( pvr_screen%axis_view(n,1)**2                          &
     &          + pvr_screen%axis_view(n,2)**2)
        rmax = max(rmax,r)
      end do
!
      length = 30 * iscale
!      length =  int(0.13 * rmax * min(n_pvr_pixel(1),n_pvr_pixel(2)))
      rmax = one / rmax
      do m = 1, 3
        ist_px = int(length * 1.3, KIND(ist_px))
        ist_py = int(length * 1.3, KIND(ist_py))
        n = pvr_screen%axis_order(m)
        do j2 = -iscale/4, (iscale+1)/4
          do i2 = -iscale/4, (iscale+1)/4
            do l = 0, length
              rlen = l * pvr_screen%axis_view(n,1) * rmax
              rhgt = l * pvr_screen%axis_view(n,2) * rmax
              i = ist_px + int(rlen, KIND(i))
              j = ist_py + int(rhgt, KIND(j))
              inod = (j+j2)*n_pvr_pixel(1) + (i+i2) + 1
              rgba_gl(n,inod) = one
              rgba_gl(4,inod) = one
            end do
          end do
        end do
        rlen = (length+10) * pvr_screen%axis_view(n,1) * rmax
        rhgt = (length+12) * pvr_screen%axis_view(n,2) * rmax
        ist_px = ist_px + int(rlen, KIND(ist_px))
        ist_py = ist_py + int(rhgt, KIND(ist_py))
!
        call set_one_label(axis_label(n), iscale, ist_px, ist_py,       &
     &      n_pvr_pixel, num_pixel, rgba_gl)
      end do
!
     end subroutine set_pvr_axislabel
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine find_draw_axis_order(axis_view, axis_order)
!
      real(kind = kreal) :: axis_view(3,4)
      integer(kind = kint) :: axis_order(3)
!
      integer(kind = kint) :: i
!
      do i = 1, 3
        if      (axis_view(1,3) .lt. axis_view(2,3)                     &
     &     .and. axis_view(1,3) .lt. axis_view(3,3)) then
          axis_order(1) = 1
          if      (axis_view(2,3) .lt. axis_view(3,3)) then
            axis_order(2) = 2
            axis_order(3) = 3
          else
            axis_order(2) = 3
            axis_order(3) = 2
          end if
        else if      (axis_view(2,3) .lt. axis_view(3,3)) then
          axis_order(1) = 2
          if      (axis_view(1,3) .lt. axis_view(3,3)) then
            axis_order(2) = 1
            axis_order(3) = 3
          else
            axis_order(2) = 3
            axis_order(3) = 1
          end if
        else
          axis_order(1) = 3
          if      (axis_view(1,3) .lt. axis_view(2,3)) then
            axis_order(2) = 1
            axis_order(3) = 2
          else
            axis_order(2) = 2
            axis_order(3) = 1
          end if
        end if
      end do
!
      end subroutine find_draw_axis_order
!
!  ---------------------------------------------------------------------
!
      end module pvr_axis_label
