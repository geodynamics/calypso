!>@file   draw_pvr_colorbar_nums.f90
!!@brief  module draw_pvr_colorbar_nums
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Construct number bitmaps
!!
!!@verbatim
!!      integer(kind = kint) function l_bar_width()
!!      subroutine corners_4_right_colorbar                             &
!!     &         (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
!!      subroutine corners_4_bottom_colorbar                            &
!!     &         (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
!!
!!      subroutine gen_right_cbar_label(iscale, num_of_scale, c_minmax, &
!!     &       npix_img, isleeve_bar, ntot_pix, dimage)
!!      subroutine gen_right_zero_label(iscale, c_minmax, npix_img,     &
!!     &          isleeve_bar, ntot_pix, dimage)
!!      subroutine gen_bottom_cbar_label(iscale, num_of_scale, c_minmax,&
!!     &          npix_img, isleeve_bar, ntot_pix, dimage)
!!      subroutine gen_bottom_zero_label(iscale, c_minmax, npix_img,    &
!!     &          isleeve_bar, ntot_pix, dimage)
!!
!!      subroutine gen_time_label(iscale, time, npix_img,               &
!!     &                          ntot_pix, dimage)
!!      subroutine set_one_label(char1, iscale, ist_px, ist_py,         &
!!     &          npix_img, ntot_pix, dimage)
!!@endverbatim
!
      module  draw_pvr_colorbar_nums
!
      use m_precision
!
      use m_constants
      use set_color_4_pvr
!
      implicit none
!
      integer(kind = kint), parameter, private :: BAR_WIDTH = iten
      integer(kind = kint), parameter, private :: NUM_LENGTH = inine
      integer(kind = kint), parameter, private :: NUM_TIMELABEL = 14
!
      private :: set_numeric_labels
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function l_bar_width()
      l_bar_width = BAR_WIDTH
      end function l_bar_width
!
!  ---------------------------------------------------------------------
!
      subroutine corners_4_right_colorbar                               &
     &         (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
!
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: isleeve_bar
!
      integer(kind = kint), intent(inout) :: ist, jst, ied, jed
!
      ist = npix_img(1) - isleeve_bar
      ied = ist + BAR_WIDTH*iscale
      jst = (npix_img(2) - 20) / 10 + 10 - 6*iscale
      jed = (npix_img(2) - 20) / 10*5 + jst
!
      end subroutine corners_4_right_colorbar
!
!  ---------------------------------------------------------------------
!
      subroutine corners_4_bottom_colorbar                              &
     &         (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
!
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: isleeve_bar
!
      integer(kind = kint), intent(inout) :: ist, jst, ied, jed
!
!
      ist = 1.5 * isleeve_bar
      ied = npix_img(1) - 1.5 * isleeve_bar
      jst = 16 + 12*iscale + 20
      jed = jst + BAR_WIDTH*iscale
!
      end subroutine corners_4_bottom_colorbar
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_right_cbar_label(iscale, num_of_scale, c_minmax,   &
     &       npix_img, isleeve_bar, ntot_pix, dimage)
!
      integer(kind = kint), intent(in) :: num_of_scale
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: iscale, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: value
      real(kind = kreal) :: rhgt
      integer(kind = kint) :: i, j, k, inod
      integer(kind = kint) :: ist, jst, ied, jed
      integer(kind = kint) :: start_px(2)
      character(len=NUM_LENGTH) :: numeric
!
!
      call corners_4_right_colorbar                                     &
     &   (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
      do k = 1, num_of_scale
        value = (c_minmax(2)-c_minmax(1))                               &
     &         * dble(k-1) / dble(num_of_scale-1) + c_minmax(1)
!
        rhgt = dble(jed-jst) * dble(k-1) / dble(num_of_scale-1)
        start_px(1) = ist + iscale * BAR_WIDTH + ithree
        start_px(2) = jst + int(rhgt, KIND(start_px(1)))
!
        write(numeric,'(1pe9.2)') value
        call set_numeric_labels(NUM_LENGTH, numeric, iscale, start_px,  &
     &      npix_img, ntot_pix, dimage)
!
        do j = -iscale/4, (iscale+1)/4
          do i = ist, ied + 4
            inod = (start_px(2)+j) * npix_img(1) + i + 1
            dimage(1:4,inod) = one
          end do
        end do
      end do
!
      end subroutine gen_right_cbar_label
!
!  ---------------------------------------------------------------------
!
      subroutine gen_right_zero_label(iscale, c_minmax, npix_img,       &
     &          isleeve_bar, ntot_pix, dimage)
!
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: iscale, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: zero_rgb
      real(kind = kreal) :: rhgt
      integer(kind = kint) :: i, j, inod
      integer(kind = kint) :: ist, jst, ied, jed
      integer(kind = kint) :: start_px(2)
      character(len=NUM_LENGTH) :: numeric
!
!
      call corners_4_right_colorbar                                     &
     &   (iscale, npix_img, isleeve_bar, ist, jst, ied, jed)
!
      zero_rgb = (zero - c_minmax(1)) / (c_minmax(2) - c_minmax(1))
!
      rhgt = zero_rgb * dble(jed-jst)
      start_px(1) = ist + iscale * BAR_WIDTH + ithree
      start_px(2) = jst + int(rhgt, KIND(ntot_pix))
!
      write(numeric,'(1pe9.2)') zero
      call set_numeric_labels(NUM_LENGTH, numeric, iscale, start_px,    &
     &                        npix_img, ntot_pix, dimage)
!
      do j = -iscale/4, (iscale+1)/4
        do i = ist, ied + 4
          inod = (j+start_px(2)) * npix_img(1) + i + 1
          dimage(1:4,inod) = one
        end do
      end do
!
      end subroutine gen_right_zero_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_bottom_cbar_label(iscale, num_of_scale, c_minmax,  &
     &          npix_img, isleeve_bar, ntot_pix, dimage)
!
      integer(kind = kint), intent(in) :: num_of_scale
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: iscale, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: value
      real(kind = kreal) :: rhgt
      integer(kind = kint) :: i, j, k, inod
      integer(kind = kint) :: ist_h, jst_h, ied_h, jed_h
      integer(kind = kint) :: start_px(2)
      character(len=NUM_LENGTH) :: numeric
!
!
      call corners_4_bottom_colorbar                                    &
     &   (iscale, npix_img, isleeve_bar, ist_h, jst_h, ied_h, jed_h)
!
      do k = 1, num_of_scale
        value = (c_minmax(2)-c_minmax(1))                               &
     &         * dble(k-1) / dble(num_of_scale-1) + c_minmax(1)
!
        rhgt = dble(ied_h-ist_h) * dble(k-1) / dble(num_of_scale-1)
        start_px(1) = ist_h - iscale*NUM_LENGTH*4                       &
     &               + int(rhgt, KIND(start_px(1)))
        start_px(2) = jst_h - 12 * iscale - 5
!
        write(numeric,'(1pe9.2)') value
        call set_numeric_labels(NUM_LENGTH, numeric, iscale, start_px,  &
     &      npix_img, ntot_pix, dimage)
!
        do i = -iscale/4, (iscale+1)/4
          do j = jst_h-4, jed_h
            inod = (j-1) * npix_img(1)                                  &
     &            + ist_h + i + int(rhgt, KIND(start_px(1)))
            dimage(1:4,inod) = one
          end do
        end do
      end do
!
      end subroutine gen_bottom_cbar_label
!
!  ---------------------------------------------------------------------
!
      subroutine gen_bottom_zero_label(iscale, c_minmax, npix_img,      &
     &          isleeve_bar, ntot_pix, dimage)
!
      real(kind = kreal), intent(in) :: c_minmax(2)
      integer(kind = kint), intent(in) :: iscale, isleeve_bar
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      real(kind = kreal) :: zero_rgb
      real(kind = kreal) :: rhgt
      integer(kind = kint) :: j, i, inod
      integer(kind = kint) :: ist_h, jst_h, ied_h, jed_h
      integer(kind = kint) :: start_px(2)
      character(len=NUM_LENGTH) :: numeric
!
!
      call corners_4_bottom_colorbar                                    &
     &   (iscale, npix_img, isleeve_bar, ist_h, jst_h, ied_h, jed_h)
!
      zero_rgb = (zero - c_minmax(1)) / (c_minmax(2) - c_minmax(1))
!
      rhgt = zero_rgb * dble(ied_h-ist_h)
      start_px(1) = ist_h - iscale*NUM_LENGTH*4                         &
     &             + int(rhgt, KIND(start_px(1)))
      start_px(2) = jst_h - 12 * iscale - 5
!
      write(numeric,'(1pe9.2)') zero
      call set_numeric_labels(NUM_LENGTH, numeric, iscale, start_px,    &
     &                        npix_img, ntot_pix, dimage)
!
      do i = -iscale/4, (iscale+1)/4
        do j = jst_h-4, jed_h
          inod = (j-1) * npix_img(1)                                    &
     &          + ist_h + i + int(rhgt, KIND(start_px(1)))
          dimage(1:4,inod) = one
        end do
      end do
!
      end subroutine gen_bottom_zero_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gen_time_label(iscale, time, npix_img,                 &
     &                          ntot_pix, dimage)
!
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: npix_img(2)
      integer(kind = kint), intent(in) :: ntot_pix
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint) :: start_px(2)
      character(len=NUM_TIMELABEL) :: t_label
!
!
      start_px(1) = npix_img(1) - 8 * (NUM_TIMELABEL+1) * iscale
      start_px(2) = npix_img(2) - iten - 12 * iscale
!
      write(t_label,'(a3,1pe11.4)') 't =', time
      call set_numeric_labels(NUM_TIMELABEL, t_label, iscale,           &
     &                        start_px, npix_img, ntot_pix, dimage)
!
      end subroutine gen_time_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_numeric_labels(length, numeric, iscale, start_px,  &
     &          npix_img, ntot_pix, dimage)
!
      use pvr_font_texture
!
      integer(kind = kint), intent(in) :: length
      character(len=1), intent(in) :: numeric(length)
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: start_px(2)
      integer(kind = kint), intent(in) :: ntot_pix
      integer(kind = kint), intent(in) :: npix_img(2)
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint) :: m, ist_px, ist_py
      character(len=1) :: char1
!
!
      ist_px = start_px(1)
      ist_py = start_px(2) - 6*iscale
      do m = 1, length
        write(char1,'(a1)') numeric(m)
        call set_one_label(char1, iscale, ist_px, ist_py,               &
     &      npix_img, ntot_pix, dimage)
        ist_px = ist_px + 8 * iscale
      end do
!
      end subroutine set_numeric_labels
!
!  ---------------------------------------------------------------------
!
      subroutine set_one_label(char1, iscale, ist_px, ist_py,          &
     &          npix_img, ntot_pix, dimage)
!
      use pvr_font_texture
!
      character(len=1), intent(in) :: char1
      integer(kind = kint), intent(in) :: ist_px, ist_py
!
      integer(kind = kint), intent(in) :: iscale
      integer(kind = kint), intent(in) :: ntot_pix
      integer(kind = kint), intent(in) :: npix_img(2)
!
      real(kind = kreal), intent(inout) :: dimage(4,ntot_pix)
!
      integer(kind = kint) :: i, j, k, ic, jc
      integer(kind = kint) :: i_font(8,12)
      real(kind = kreal) :: r_font(10,14)
!
!
      call gen_font8_12(char1, i_font)
!
      r_font(1:10,1:14) = 0.0d0
!
      do i = 1, 8
        do j = 1, 12
          r_font(i,  j  ) = 0.2 * real(i_font(i,j))
          r_font(i+2,j  ) = 0.2 * real(i_font(i,j))
          r_font(i,  j+2) = 0.2 * real(i_font(i,j))
!          r_font(i+2,j+2) = 0.2 * real(i_font(i,j))
        end do
      end do
      do i = 1, 8
        do j = 1, 12
          r_font(i,  j+1) = 0.4 * real(i_font(i,j))
          r_font(i+2,j+1) = 0.4 * real(i_font(i,j))
          r_font(i+1,j  ) = 0.4 * real(i_font(i,j))
          r_font(i+1,j+2) = 0.4 * real(i_font(i,j))
        end do
      end do
      do i = 1, 8
        do j = 1, 12
          r_font(i+2,j+2) = 0.6 * real(i_font(i,j))
        end do
      end do
      do i = 1, 8
        do j = 1, 12
          r_font(i+1,j+1) = 1.0 * real(i_font(i,j))
        end do
      end do
!
      do i = 1, 10*iscale
        do j = 1, 14*iscale
!          k = ( (ist_py+j-1)*npix_img(1)+ist_px + i)
          k = ( (ist_py+j+1)*npix_img(1)+ist_px + i)
          if(k .gt. ntot_pix) cycle
          ic =  (i-1) / iscale + 1
          jc = 14 - (j-1) / iscale
          dimage(1:3,k) = dimage(1:3,k) + r_font(ic,jc)                 &
     &                     * (one - two*dimage(1:3,k))
          dimage(4,k) = r_font(ic,jc)
        end do
      end do
!
      end subroutine set_one_label
!
!  ---------------------------------------------------------------------
!
      end module draw_pvr_colorbar_nums
