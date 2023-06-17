!>@file   draw_isoline_in_triangle.f90
!!@brief  module draw_isoline_in_triangle
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Fraw pixels on projected image
!!
!!@verbatim
!!      subroutine s_draw_isoline_in_triangle(nwidth, idots,            &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, xy_patch, d_patch, d_ref, color_ref,&
!!     &          rgba)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nwidth, idots
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: xy_patch(2,3)
!!        real(kind = kreal), intent(in) :: d_patch(3)
!!        real(kind = kreal), intent(in) :: d_ref
!!        real(kind = kreal), intent(in) :: color_ref(4)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!@endverbatim
      module draw_isoline_in_triangle
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: find_isoline_in_triangle, draw_pixel_on_isoline
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_draw_isoline_in_triangle(nwidth, idots,              &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, xy_patch, d_patch, d_ref, color_ref,  &
     &          rgba)
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nwidth, idots
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: xy_patch(2,3)
      real(kind = kreal), intent(in) :: d_patch(3)
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: iflag_count
      real(kind = kreal) :: xy_edge(2,2)
!
!
      call find_isoline_in_triangle                                     &
     &   (xy_patch, d_patch, d_ref, iflag_count, xy_edge)
      if(iflag_count .eq. 2) then
        call draw_pixel_on_isoline(nwidth, idots,                       &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      nxpixel, nypixel, xy_edge, color_ref, rgba)
      end if
!
      end subroutine s_draw_isoline_in_triangle
!
!  ---------------------------------------------------------------------
!
      subroutine draw_pixel_on_isoline(nwidth, idots,                   &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, xy_edge, color_ref, rgba)
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nwidth, idots
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: xy_edge(2,2)
      real(kind = kreal), intent(in) :: color_ref(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: ix1, ix2, iy1, iy2, ix, iy
      integer(kind = kint) :: ist, ied, jst, jed, i, j, i_img
      integer(kind = kint) :: ilen, nlen, isq
!
!
      isq = 2*idots
      ix1 = int(1 + dble(nxpixel-1) * (xy_edge(1,1) - xmin_frame)       &
     &             / (xmax_frame - xmin_frame))
      ix2 = int(1 + dble(nxpixel-1) * (xy_edge(1,2) - xmin_frame)       &
     &             / (xmax_frame - xmin_frame))
      iy1 = int(1 + dble(nypixel-1) * (xy_edge(2,1) - ymin_frame)       &
     &             / (ymax_frame - ymin_frame))
      iy2 = int(1 + dble(nypixel-1) * (xy_edge(2,2) - ymin_frame)       &
     &             / (ymax_frame - ymin_frame))
      nlen = max(abs(ix2-ix1),abs(iy2-iy1))
      do ilen = 0, nlen
        ix = ix1 + int(dble((ix2-ix1)*ilen) / dble(nlen))
        iy = iy1 + int(dble((iy2-iy1)*ilen) / dble(nlen))
        ist = ix - (nwidth-1) / 2
        ied = ix +  nwidth /    2
        jst = iy - (nwidth-1) / 2
        jed = iy +  nwidth /    2
        ist = max(ist,1)
        ied = max(ied,0)
        jst = max(jst,1)
        jed = max(jed,0)
        ist = min(ist,nxpixel+1)
        ied = min(ied,nxpixel)
        jst = min(jst,nypixel+1)
        jed = min(jed,nypixel)
        do j = jst, jed
          do i = ist, ied
            if(isq .gt. 0) then
              if(mod(j,isq).ge.idots .and. mod(i,isq).lt.idots) cycle
              if(mod(j,isq).lt.idots .and. mod(i,isq).ge.idots) cycle
            end if
!
            i_img = i + (j-1) * nxpixel
            rgba(1:4,i_img) = color_ref(1:4)
            rgba(4,i_img) =   one
          end do
        end do
      end do
!
      end subroutine draw_pixel_on_isoline
!
!  ---------------------------------------------------------------------
!
      subroutine find_isoline_in_triangle                               &
     &         (xy_patch, d_patch, d_ref, iflag_count, xy_edge)
!
      real(kind = kreal), intent(in) :: xy_patch(2,3)
      real(kind = kreal), intent(in) :: d_patch(3)
      real(kind = kreal), intent(in) :: d_ref
!
      integer(kind = kint), intent(inout) :: iflag_count
      real(kind = kreal), intent(inout) :: xy_edge(2,2)
!
      real(kind = kreal) :: ratio
!
      iflag_count = 0
      if((d_patch(2)-d_ref)*(d_patch(3)-d_ref) .le. zero) then
        if(d_patch(2) .eq. d_patch(3)) then
          xy_edge(1:2,1) = xy_patch(1:2,2)
          xy_edge(1:2,2) = xy_patch(1:2,3)
          iflag_count = 2
          return
        else
          iflag_count = iflag_count + 1
          ratio = (d_ref - d_patch(2)) / (d_patch(3) - d_patch(2))
          xy_edge(1:2,iflag_count) = (one - ratio) * xy_patch(1:2,2)    &
     &                                    + ratio *  xy_patch(1:2,3)
        end if
      end if
      if((d_patch(3)-d_ref)*(d_patch(1)-d_ref) .le. zero) then
        if(d_patch(3) .eq. d_patch(1)) then
          xy_edge(1:2,1) = xy_patch(1:2,3)
          xy_edge(1:2,2) = xy_patch(1:2,1)
          iflag_count = 2
          return
        else
          iflag_count = iflag_count + 1
          ratio = (d_ref - d_patch(3)) / (d_patch(1) - d_patch(3))
          xy_edge(1:2,iflag_count) = (one - ratio) * xy_patch(1:2,3)    &
     &                                    + ratio *  xy_patch(1:2,1)
        end if
      end if
      if(iflag_count .eq. 2) return
      if((d_patch(1)-d_ref)*(d_patch(2)-d_ref) .le. zero) then
        if(d_patch(1) .eq. d_patch(2)) then
          xy_edge(1:2,1) = xy_patch(1:2,1)
          xy_edge(1:2,2) = xy_patch(1:2,2)
          iflag_count = 2
        else
          iflag_count = iflag_count + 1
          ratio = (d_ref - d_patch(1)) / (d_patch(2) - d_patch(1))
          xy_edge(1:2,iflag_count) = (one - ratio) * xy_patch(1:2,1)    &
     &                                    + ratio *  xy_patch(1:2,2)
        end if
      end if
!
      end subroutine find_isoline_in_triangle
!
!  ---------------------------------------------------------------------
!
      end module draw_isoline_in_triangle
