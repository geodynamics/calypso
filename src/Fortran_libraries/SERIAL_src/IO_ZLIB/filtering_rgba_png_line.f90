!>@file   filtering_rgba_png_line.f90
!!        module filtering_rgba_png_line
!!
!! @author H. Matsui
!! @date   Programmed in Oct., 2021
!!
!!
!>@brief Filtering for PNG image
!!
!!@verbatim
!!      subroutine no_filter_png_image(n_rgb, npix_x, npix_y,           &
!!     &                               rgb, png_rgb)
!!      subroutine filter_png_image(n_rgb, npix_x, npix_y, rgb, png_rgb)
!!        integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
!!        character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
!!        character(len = 1), intent(inout)                             &
!!     &                   :: png_rgb(n_rgb*(npix_x+1)*npix_y)
!!
!!      subroutine unfilter_png_rgba_line                               &
!!     &         (iflag_filter, iflag_left_edge, iflag_bottom,          &
!!     &          npix_x, i_rgba_up, i_rgba_line)
!!        integer(kind = 4), intent(in) :: iflag_filter
!!        integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
!!        integer(kind = 4), intent(in) :: npix_x
!!        integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!!        integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!!      subroutine sel_filter_png_rgb                                   &
!!     &         (n_rgb, npix_x, npix_y, rgb, png_rgb)
!!        integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
!!        character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
!!        character(len = 1), intent(inout)                             &
!!     &                   :: png_rgb(n_rgb*(npix_x+1)*npix_y)
!!@endverbatim
!!
      module filtering_rgba_png_line
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
      subroutine no_filter_png_image(n_rgb, npix_x, npix_y,             &
     &                               rgb, png_rgb)
!
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
      character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
      character(len = 1), intent(inout)                                 &
     &                   :: png_rgb(n_rgb*(npix_x+1)*npix_y)
!
      integer(kind = 4) :: i, j, icou, jcou
!
!
      do j = 1, npix_y
        icou = 1 + (j-1)*(n_rgb*npix_x+1)
        png_rgb(icou) = char(0)
!$omp parallel do private(i,jcou)
        do i = 1, npix_x
          jcou = n_rgb*(i-1) + (npix_y-j)*(n_rgb*npix_x)
          icou = 1 + 3*(i-1) + (j-1)*(n_rgb*npix_x+1)
          png_rgb(icou+1:icou+n_rgb) = rgb(jcou+1:jcou+n_rgb)
        end do
!$omp end parallel do
      end do
!
      end subroutine no_filter_png_image
!
!------------------------------------------------------------------------
!
      subroutine filter_png_image(n_rgb, npix_x, npix_y, rgb, png_rgb)
!
      use line_rgba_filtering_4_png
      use transfer_to_long_integers
!
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
      character(len = 1), intent(in) :: rgb(n_rgb*npix_x*npix_y)
      character(len = 1), intent(inout)                                 &
     &                   :: png_rgb(n_rgb*(npix_x+1)*npix_y)
!
      integer(kind = 4), allocatable :: i_rgba_line(:,:,:)
      integer(kind = 4), allocatable :: i_rgba_up(:,:)
!
      integer(kind = 4) :: iflag_sel
      integer(kind = 4) :: i, j, icou, jcou
!
!
      allocate(i_rgba_line(4,0:npix_x,0:4))
      allocate(i_rgba_up(4,0:npix_x))
!$omp parallel workshare
      i_rgba_line(1:4,0:npix_x,0:4) = 0
      i_rgba_up(1:4,0:npix_x) = 0
!$omp end parallel workshare
!
      iflag_sel = 0
      do j = 1, npix_y
        i_rgba_line(1:4,0,0) = 0
!$omp parallel do private(i,jcou)
        do i = 1, npix_x
          jcou = n_rgb*(i-1) + (npix_y-j)*(n_rgb*npix_x)
          i_rgba_line(1:n_rgb,i,0) = iachar(rgb(jcou+1:jcou+n_rgb))
        end do
!$omp end parallel do
!
        i_rgba_up(1:4,0) = 0
        if(j .eq. 0) then
!$omp parallel workshare
          i_rgba_up(1:4,1:npix_x) = 0
!$omp end parallel workshare
        else
!$omp parallel do private(i,jcou)
          do i = 1, npix_x
            jcou = n_rgb*(i-1) + (npix_y-j+1)*(n_rgb*npix_x)
            i_rgba_up(1:n_rgb,i) = iachar(rgb(jcou+1:jcou+n_rgb))
          end do
!$omp end parallel do
        end if
!
        call sel_filter_png_rgba_line(j, n_rgb, npix_x, i_rgba_up,      &
     &                                i_rgba_line, iflag_sel)
!
        icou = 1 + (j-1)*(n_rgb*npix_x+1)
        png_rgb(icou) = char(iflag_sel)
!$omp parallel do private(i,icou)
        do i = 1, npix_x
          icou = 1 + n_rgb*(i-1) + (j-1)*(n_rgb*npix_x+1)
          png_rgb(icou+1:icou+n_rgb) = char(i_rgba_line(1:n_rgb,i,0))
        end do
!$omp end parallel do
      end do
      deallocate(i_rgba_line, i_rgba_up)
!
      end subroutine filter_png_image
!
!------------------------------------------------------------------------
!
      subroutine unfilter_png_image(n_rgb, npix_x, npix_y,              &
     &                              png_rgb, rgb)
!
      integer(kind = 4), intent(in) :: n_rgb, npix_x, npix_y
      character(len = 1), intent(in) :: png_rgb(n_rgb*(npix_x+1)*npix_y)
      character(len = 1), intent(inout) :: rgb(n_rgb*npix_x*npix_y)
!
      integer(kind = 4), allocatable :: i_rgb_line(:,:)
      integer(kind = 4), allocatable :: i_rgb_up(:,:)
!
      integer(kind = 4) :: iflag_filter
      integer(kind = 4) :: i, j, icou, jcou
!
!
      allocate(i_rgb_line(4,0:npix_x))
      allocate(i_rgb_up(4,0:npix_x))
!$omp parallel workshare
      i_rgb_line(1:4,0:npix_x) = 0
      i_rgb_up(1:4,0:npix_x) = 0
!$omp end parallel workshare
!
      do j = 1, npix_y
        iflag_filter = iachar(png_rgb(1+(j-1)*(n_rgb*npix_x+1)))
!        write(*,*) j, iflag_filter
!
!$omp parallel workshare
        i_rgb_up(1:4,0:npix_x) =   i_rgb_line(1:4,0:npix_x)
!$omp end parallel workshare
        i_rgb_line(1:4,0) = 0
!$omp parallel do private(i,icou)
        do i = 1, npix_x
          icou = 1 + n_rgb*(i-1) + (j-1)*(n_rgb*npix_x+1)
          i_rgb_line(1:n_rgb,i) = iachar(png_rgb(icou+1:icou+n_rgb))
        end do
!$omp end parallel do
!
        call unfilter_png_rgba_line(iflag_filter, 1, j,                 &
     &                               npix_x, i_rgb_up, i_rgb_line)
!
!$omp parallel do private(i,jcou)
        do i = 1, npix_x
          jcou = n_rgb*(i-1) + (npix_y-j)*(n_rgb*npix_x)
          rgb(jcou+1:jcou+n_rgb) = char(i_rgb_line(1:n_rgb,i))
        end do
!$omp end parallel do
      end do
      deallocate(i_rgb_line, i_rgb_up)
!
      end subroutine unfilter_png_image
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
      subroutine sel_filter_png_rgba_line(j, n_rgb, npix_x, i_rgba_up,  &
     &                                    i_rgba_line, iflag_sel)
!
      use line_rgba_filtering_4_png
      use transfer_to_long_integers
!
      integer(kind = 4), intent(in) :: j
      integer(kind = 4), intent(in) :: n_rgb, npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x,0:4)
      integer(kind = 4), intent(inout) :: iflag_sel
!
      integer(kind = 4) :: i, k, ic, i_ref
      integer(kind = 4) :: line_ave(0:4), line_sigma(0:4)
!
!
      do k = 1, 4
!$omp parallel workshare
        i_rgba_line(1:4,0:npix_x,k) = i_rgba_line(1:4,0:npix_x,0)
!$omp end parallel workshare
      end do
!
      call sub_filter_png_rgba_line(npix_x, i_rgba_line(1,0,0),         &
     &                              i_rgba_line(1,0,1))
!
      call up_filter_png_rgba_line(npix_x, i_rgba_up,                   &
     &    i_rgba_line(1,0,0), i_rgba_line(1,0,2))
!
      call ave_filter_png_rgba_line(1, j, npix_x, i_rgba_up,            &
     &    i_rgba_line(1,0,0), i_rgba_line(1,0,3))
!
      call paeth_filter_png_rgba_line(npix_x, i_rgba_up,                &
     &    i_rgba_line(1,0,0), i_rgba_line(1,0,4))
!
      line_ave(0:4) =   0
      line_sigma(0:4) = 0
!$omp parallel do private(k,i,ic)
      do k = 0, 4
        do i = 1, npix_x
          line_ave(k) = line_ave(k)                                     &
     &               + i_rgba_line(1,i,k) + i_rgba_line(2,i,k)          &
     &               + i_rgba_line(3,i,k) + i_rgba_line(4,i,k)
        end do
        line_ave(k) = line_ave(k) / (n_rgb*npix_x)
!
        do i = 1, npix_x
          do ic = 1, n_rgb
            line_sigma(k) = line_sigma(k)                               &
     &                   + abs(i_rgba_line(ic,i,k) - line_ave(k))
          end do
        end do
      end do
!$omp end parallel do
!
!
      iflag_sel = 0
      i_ref = line_sigma(0)
      do k = 1, 4
        if(line_sigma(k) .lt. i_ref) then
          iflag_sel = k
          i_ref = line_sigma(k)
        end if
      end do
!      write(*,*) 'iflag_sel', j, iflag_sel, line_sigma(0:4)
!
      if(iflag_sel .gt. 0) then
!$omp parallel do private(i)
        do i = 1, npix_x
          i_rgba_line(1:4,i,0) = i_rgba_line(1:4,i,iflag_sel)
        end do
!$omp end parallel do
      end if
!
      end subroutine sel_filter_png_rgba_line
!
!------------------------------------------------------------------------
!
      subroutine unfilter_png_rgba_line                                 &
     &         (iflag_filter, iflag_left_edge, iflag_bottom,            &
     &          npix_x, i_rgba_up, i_rgba_line)
!
      use line_rgba_filtering_4_png
!
      integer(kind = 4), intent(in) :: iflag_filter
      integer(kind = 4), intent(in) :: iflag_left_edge, iflag_bottom
      integer(kind = 4), intent(in) :: npix_x
      integer(kind = 4), intent(in) :: i_rgba_up(4,0:npix_x)
!
      integer(kind = 4), intent(inout) :: i_rgba_line(4,0:npix_x)
!
!
      if(iflag_filter .eq. 1) then
        call sub_unfilter_png_rgba_line(npix_x, i_rgba_line)
      else if(iflag_filter .eq. 2) then
        call up_unfilter_png_rgba_line                                  &
     &         (npix_x, i_rgba_up, i_rgba_line)
      else if(iflag_filter .eq. 3) then
        call ave_unfilter_png_rgba_line(iflag_left_edge, iflag_bottom,  &
     &      npix_x, i_rgba_up, i_rgba_line)
      else if(iflag_filter .eq. 4) then
        call paeth_unfilter_png_rgba_line                               &
     &         (npix_x, i_rgba_up, i_rgba_line)
      end if
!
      end subroutine unfilter_png_rgba_line
!
!------------------------------------------------------------------------
!
      end module filtering_rgba_png_line
