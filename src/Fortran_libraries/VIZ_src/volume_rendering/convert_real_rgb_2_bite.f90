!convert_real_rgb_2_bite.f90
!      module convert_real_rgb_2_bite
!
!      subroutine cvt_double_rgba_to_char_rgb(num_pixel, rgba, crgb)
!      subroutine cvt_double_rgba_to_char_rgba(num_pixel, rgba, crgba)
!
!      subroutine set_rgb_background(num_pixel, rgba, bgcolor)
!
      module convert_real_rgb_2_bite
!
      use m_precision
!
      use m_constants
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_double_rgba_to_char_rgb(num_pixel, rgba, crgb)
!
      integer(kind = kint), intent(in) :: num_pixel
      real(kind = kreal), intent(in) :: rgba(4,num_pixel)
      character(len = 1), intent(inout) :: crgb(3,num_pixel)
      integer(kind = kint) :: i
      integer :: ir, ig, ib
!
!$omp parallel do private(i,ir,ig,ib)
      do i = 1, num_pixel
        ir = int( rgba(1,i)*256.0d0)
        ig = int( rgba(2,i)*256.0d0)
        ib = int( rgba(3,i)*256.0d0)
!
        if(ir.lt.0) then
          ir = 0
        else if(ir.gt. 255) then
          ir = 255
        end if
!
        if(ig.lt.0) then
          ig = 0
        else if(ig.gt. 255) then
          ig = 255
        end if
!
        if(ib.lt.0) then
          ib = 0
        else if(ib.gt. 255) then
          ib = 255
        end if
!
          crgb(1,i) = char(ir)
          crgb(2,i) = char(ig)
          crgb(3,i) = char(ib)
      end do
!$omp end parallel do
!
      end subroutine cvt_double_rgba_to_char_rgb
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_double_rgba_to_char_rgba(num_pixel, rgba, crgba)
!
      integer(kind = kint), intent(in) :: num_pixel
      real(kind = kreal), intent(in) :: rgba(4,num_pixel)
      character(len = 1), intent(inout) :: crgba(4,num_pixel)
      integer(kind = kint) :: i
      integer :: ir, ig, ib, ia
!
!$omp parallel do private(i,ir,ig,ib,ia)
      do i = 1, num_pixel
        ir = int( rgba(1,i)*256.0d0)
        ig = int( rgba(2,i)*256.0d0)
        ib = int( rgba(3,i)*256.0d0)
        ia = int( rgba(4,i)*256.0d0)
!
        if(ir.lt.0) then
          ir = 0
        else if(ir .gt. 255) then
          ir = 255
        end if
!
        if(ig.lt.0) then
          ig = 0
        else if(ig .gt. 255) then
          ig = 255
        end if
!
        if(ib.lt.0) then
          ib = 0
        else if(ib .gt. 255) then
          ib = 255
        end if
!
        if(ia.lt.0) then
          ia = 0
        else if(ia .gt. 255) then
          ia = 255
        end if
!
          crgba(1,i) = char(ir)
          crgba(2,i) = char(ig)
          crgba(3,i) = char(ib)
          crgba(4,i) = char(ia)
      end do
!$omp end parallel do
!
      end subroutine cvt_double_rgba_to_char_rgba
!
!  ---------------------------------------------------------------------
!
      subroutine set_rgb_background(num_pixel, rgba, bgcolor)
!
      integer(kind = kint), intent(in) :: num_pixel
      real(kind = kreal), intent(in) :: bgcolor(3)
      real(kind = kreal), intent(inout) :: rgba(4,num_pixel)
!
      integer(kind = kint) :: i
!
!$omp parallel do private(i)
      do i = 1, num_pixel
        if(     rgba(1,i) .lt. (bgcolor(1)+0.004)                       &
     &    .and. rgba(2,i) .lt. (bgcolor(2)+0.004)                       &
     &    .and. rgba(3,i) .lt. (bgcolor(3)+0.004) ) then
          rgba(1,i) = bgcolor(1)
          rgba(2,i) = bgcolor(2)
          rgba(3,i) = bgcolor(3)
        end if
      end do
!$omp end parallel do
!
      end subroutine set_rgb_background
!
!  ---------------------------------------------------------------------
!
      end module convert_real_rgb_2_bite
