!>@file   line_rgba_filtering_4_png.f90
!!        module line_rgba_filtering_4_png
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!>@brief Line filtering peration for PNG image compression
!!
!!@verbatim
!!      subroutine sub_filter_png_rgba_line                             &
!!     &         (npix_x, i_rgba_org, i_rgba_line)
!!      subroutine sub_unfilter_png_rgba_line(npix_x, i_rgba_line)
!!        integer(kind = 4), intent(in) :: npix_x
!!        integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
!!        integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!!
!!      subroutine up_filter_png_rgba_line                              &
!!     &         (npix_x, i_rgba_up, i_rgba_org, i_rgba_line)
!!      subroutine up_unfilter_png_rgba_line                            &
!!     &         (npix_x, i_rgba_up, i_rgba_line)
!!        integer(kind = 4), intent(in) :: npix_x
!!        integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!!        integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
!!        integer(kind = 4), intent(inout) :: i_rgb_line(4,0:npix_x)
!!
!!      subroutine ave_filter_png_rgba_line                             &
!!     &         (iflag_left_edge, iflag_bottom,                        &
!!     &          npix_x, i_rgba_up, i_rgba_line)
!!      subroutine ave_unfilter_png_rgba_line                           &
!!     &         (iflag_left_edge, iflag_bottom,                        &
!!     &          npix_x, i_rgba_up, i_rgba_line)
!!        integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
!!        integer(kind = 4), intent(in) :: npix_x
!!        integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!!        integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
!!        integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!!
!!      subroutine paeth_filter_png_rgba_line                           &
!!     &         (npix_x, i_rgba_up, i_rgba_org, i_rgba_line)
!!      subroutine paeth_unfilter_png_rgba_line                         &
!!     &         (npix_x, i_rgba_up, i_rgba_line)
!!        integer(kind = 4), intent(in) :: npix_x
!!        integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!!        integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
!!        integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!!
!!      subroutine non_filter_png_rgba_line                             &
!!     &         (n_rgb, npix_x, i_rgb_line, rgb_line)
!!        integer(kind = 4), intent(in) :: n_rgb, npix_x
!!        integer(kind = 4), intent(in) :: i_rgb_line(4,0:npix_x)
!!        character(len = 1), intent(inout) :: rgb_line(0:n_rgb*npix_x)
!!@endverbatim
!!
      module line_rgba_filtering_4_png
!
      use m_precision
!
      implicit none
!
      private :: set_paeth_predictor
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sub_filter_png_rgba_line                               &
     &         (npix_x, i_rgba_org, i_rgba_line)
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: i
!
!
!$omp parallel do
      do i = 1, npix_x
        i_rgba_line(1:4,i)                                              &
     &       = mod(i_rgba_org(1:4,i) - i_rgba_org(1:4,i-1)+256,256)
      end do
!$omp end parallel do
!
      end subroutine sub_filter_png_rgba_line
!
!------------------------------------------------------------------------
!
      subroutine sub_unfilter_png_rgba_line(npix_x, i_rgba_line)
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: i
!
      do i = 1, npix_x
        i_rgba_line(1:4,i)                                              &
     &       = mod((i_rgba_line(1:4,i)+i_rgba_line(1:4,i-1)+256), 256)
      end do
!
      end subroutine sub_unfilter_png_rgba_line
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine up_filter_png_rgba_line                                &
     &         (npix_x, i_rgba_up, i_rgba_org, i_rgba_line)
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
      integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: i
!
!$omp parallel do
      do i = 1, npix_x
        i_rgba_line(1:4,i)                                              &
     &       = mod((i_rgba_org(1:4,i)-i_rgba_up(1:4,i)+256),256)
      end do
!$omp end parallel do
!
      end subroutine up_filter_png_rgba_line
!
!------------------------------------------------------------------------
!
      subroutine up_unfilter_png_rgba_line                              &
     &         (npix_x, i_rgba_up, i_rgba_line)
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: i
!
      do i = 1, npix_x
        i_rgba_line(1:4,i)                                              &
     &         = mod((i_rgba_line(1:4,i) + i_rgba_up(1:4,i)+256), 256)
      end do
!
      end subroutine up_unfilter_png_rgba_line
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine ave_filter_png_rgba_line                               &
     &         (iflag_left_edge, iflag_bottom,                          &
     &          npix_x, i_rgba_up, i_rgba_org, i_rgba_line)
!
      integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
      integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
!
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
!$omp parallel do private(i,ipaeth)
      do i = 1, npix_x
        if(iflag_left_edge .eq. 1 .and. iflag_bottom .eq. 1             &
     &     .and. i .eq. 1) then
          ipaeth(1:4) = 0
        else if(i .eq. 1) then
          ipaeth(1:4) = i_rgba_up(1:4,i)
        else if(iflag_bottom .eq. 1) then
          ipaeth(1:4) = i_rgba_org(1:4,i-1)
        else
          ipaeth(1:4) = (i_rgba_org(1:4,i-1) + i_rgba_up(1:4,i)) / 2
        end if
        i_rgba_line(1:4,i)                                              &
     &           = mod((i_rgba_org(1:4,i)-ipaeth(1:4)+256),256)
      end do
!$omp end parallel do
!
      end subroutine ave_filter_png_rgba_line
!
!------------------------------------------------------------------------
!
      subroutine ave_unfilter_png_rgba_line                             &
     &         (iflag_left_edge, iflag_bottom,                          &
     &          npix_x, i_rgba_up, i_rgba_line)
!
      integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
!
      do i = 1, npix_x
        if(iflag_left_edge .eq. 1 .and. iflag_bottom .eq. 1             &
     &     .and. i .eq. 1) then
          ipaeth(1:4) = 0
        else if(i .eq. 1) then
          ipaeth(1:4) = i_rgba_up(1:4,i)
        else if(iflag_bottom .eq. 1) then
          ipaeth(1:4) = i_rgba_line(1:4,i-1)
        else
          ipaeth(1:4) = (i_rgba_line(1:4,i-1)+i_rgba_up(1:4,i)) / 2
        end if
        i_rgba_line(1:4,i)                                              &
     &           = mod((i_rgba_line(1:4,i)+ipaeth(1:4)+256),256)
      end do
!
      end subroutine ave_unfilter_png_rgba_line
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine paeth_filter_png_rgba_line                             &
     &         (npix_x, i_rgba_up, i_rgba_org, i_rgba_line)
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
      integer(kind = 4), intent(in) :: i_rgba_org(4,0:npix_x)
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
!$omp parallel do private(i,ipaeth)
      do i = 1, npix_x
        call set_paeth_predictor                                        &
     &     (i_rgba_org(1,i-1), i_rgba_up(1,i), i_rgba_up(1,i-1),        &
     &      ipaeth)
        i_rgba_line(1:4,i)                                              &
     &           = mod((i_rgba_org(1:4,i)-ipaeth(1:4)+256),256)
      end do 
!$omp end parallel do
!
      end subroutine paeth_filter_png_rgba_line
!
!------------------------------------------------------------------------
!
      subroutine paeth_unfilter_png_rgba_line                           &
     &         (npix_x, i_rgba_up, i_rgba_line)
!
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
      integer(kind = 4) :: ipaeth(4)
      integer(kind = 4) :: i
!
      do i = 1, npix_x
        call set_paeth_predictor                                        &
     &     (i_rgba_line(1,i-1), i_rgba_up(1,i), i_rgba_up(1,i-1),       &
     &      ipaeth)
        i_rgba_line(1:4,i)                                              &
     &           = mod((i_rgba_line(1:4,i)+ipaeth(1:4)+256),256)
        end do 
!
      end subroutine paeth_unfilter_png_rgba_line
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine set_paeth_predictor(ileft, iup, idiag, ipaeth)
!
      integer(kind = 4), intent(in) :: ileft(4), iup(4), idiag(4)
      integer(kind = 4), intent(inout) :: ipaeth(4)
!
      integer(kind = 4) :: itmp(4), jleft(4), jup(4), jdiag(4)
      integer(kind = 4) :: nd
!
      itmp(1:4) = ileft(1:4) + iup(1:4) - idiag(1:4)
!
      jleft(1:4) = abs(itmp(1:4) - ileft(1:4))
      jup(1:4) =   abs(itmp(1:4) - iup(1:4))
      jdiag(1:4) = abs(itmp(1:4) - idiag(1:4))
!
      do nd = 1, 4
        if(jleft(nd) .le. jup(nd) .and. jleft(nd) .le. jdiag(nd)) then
          ipaeth(nd) = ileft(nd)
        else if(jup(nd) .le. jdiag(nd)) then
          ipaeth(nd) = iup(nd)
        else
          ipaeth(nd) = idiag(nd)
        end if
      end do
!
      end subroutine set_paeth_predictor
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine non_filter_png_rgba_line                               &
     &         (n_rgb, npix_x, i_rgb_line, rgb_line)
!
      integer(kind = 4), intent(in) :: n_rgb, npix_x
      integer(kind = 4), intent(in) :: i_rgb_line(4,0:npix_x)
      character(len = 1), intent(inout) :: rgb_line(0:n_rgb*npix_x)
!
      integer(kind = 4) :: i
!
!$omp parallel do
      do i = 1, npix_x
        rgb_line(n_rgb*(i-1)+1:n_rgb*i) = char(i_rgb_line(1:n_rgb,i))
      end do
!$omp end parallel do
!
      end subroutine non_filter_png_rgba_line
!
!------------------------------------------------------------------------
!
      end module line_rgba_filtering_4_png
