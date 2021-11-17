!
!      module cal_jacobian_3d_linear
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June, 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_3d_8(numnod, numele,                   &
!     &          np_smp, iele_smp_stack, ie, xx, xjac, axjac,           &
!     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,      &
!     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
!> \brief     Caliculate jacobian by linear shape function
      module cal_jacobian_3d_linear
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!>      Caliculate jacobian by 3-d linear shape function
      subroutine s_cal_jacobian_3d_8(numnod, numele,                    &
     &          np_smp, iele_smp_stack, ie, xx, xjac, axjac,            &
     &          dnx, dny, dnz, dxidx, deidx, dzidx, dxidy, deidy,       &
     &          dzidy, dxidz, deidz, dzidz, dnxi, dnei, dnzi)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele, num_t_linear)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: dnxi(num_t_linear)
      real(kind = kreal), intent(in) :: dnei(num_t_linear)
      real(kind = kreal), intent(in) :: dnzi(num_t_linear)
!
      real(kind = kreal), intent(inout) :: dxidx(numele)
      real(kind = kreal), intent(inout) :: deidx(numele)
      real(kind = kreal), intent(inout) :: dzidx(numele)
      real(kind = kreal), intent(inout) :: dxidy(numele)
      real(kind = kreal), intent(inout) :: deidy(numele)
      real(kind = kreal), intent(inout) :: dzidy(numele)
      real(kind = kreal), intent(inout) :: dxidz(numele)
      real(kind = kreal), intent(inout) :: deidz(numele)
      real(kind = kreal), intent(inout) :: dzidz(numele)
!
      real(kind = kreal), intent(inout) :: xjac(numele)
      real(kind = kreal), intent(inout) :: axjac(numele)
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_linear)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_linear)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_linear)
!
      integer(kind = kint) :: ip, ist, ied, iele
!
      real(kind = kreal) :: dxxi, dxei, dxzi
      real(kind = kreal) :: dyxi, dyei, dyzi
      real(kind = kreal) :: dzxi, dzei, dzzi
      real(kind = kreal) :: xj11, xj12, xj13
      real(kind = kreal) :: xj21, xj22, xj23
      real(kind = kreal) :: xj31, xj32, xj33
!
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,iele,i1,i2,i3,i4,i5,i6,i7,i8,                          &
!$omp&   dxxi,dxei,dxzi,dyxi,dyei,dyzi,dzxi,dzei,dzzi,                  &
!$omp&   xj11,xj12,xj13,xj21,xj22,xj23,xj31,xj32,xj33)
      do ip = 1, np_smp
        ist = iele_smp_stack(ip-1) + 1
        ied = iele_smp_stack(ip)
!
!cdir nodep noloopchg
        do iele = ist, ied
!
          i1 =  ie(iele, 1)
          i2 =  ie(iele, 2)
          i3 =  ie(iele, 3)
          i4 =  ie(iele, 4)
          i5 =  ie(iele, 5)
          i6 =  ie(iele, 6)
          i7 =  ie(iele, 7)
          i8 =  ie(iele, 8)
!
          dxxi =  xx(i1, 1)*dnxi( 1) + xx(i2, 1)*dnxi( 2)               &
     &          + xx(i3, 1)*dnxi( 3) + xx(i4, 1)*dnxi( 4)               &
     &          + xx(i5, 1)*dnxi( 5) + xx(i6, 1)*dnxi( 6)               &
     &          + xx(i7, 1)*dnxi( 7) + xx(i8, 1)*dnxi( 8)
!
          dxei =  xx(i1, 1)*dnei( 1) + xx(i2, 1)*dnei( 2)               &
     &          + xx(i3, 1)*dnei( 3) + xx(i4, 1)*dnei( 4)               &
     &          + xx(i5, 1)*dnei( 5) + xx(i6, 1)*dnei( 6)               &
     &          + xx(i7, 1)*dnei( 7) + xx(i8, 1)*dnei( 8)
!
          dxzi =  xx(i1, 1)*dnzi( 1) + xx(i2, 1)*dnzi( 2)               &
     &          + xx(i3, 1)*dnzi( 3) + xx(i4, 1)*dnzi( 4)               &
     &          + xx(i5, 1)*dnzi( 5) + xx(i6, 1)*dnzi( 6)               &
     &          + xx(i7, 1)*dnzi( 7) + xx(i8, 1)*dnzi( 8)
!
!
          dyxi =  xx(i1, 2)*dnxi( 1) + xx(i2, 2)*dnxi( 2)               &
     &          + xx(i3, 2)*dnxi( 3) + xx(i4, 2)*dnxi( 4)               &
     &          + xx(i5, 2)*dnxi( 5) + xx(i6, 2)*dnxi( 6)               &
     &          + xx(i7, 2)*dnxi( 7) + xx(i8, 2)*dnxi( 8)
!
          dyei =  xx(i1, 2)*dnei( 1) + xx(i2, 2)*dnei( 2)               &
     &          + xx(i3, 2)*dnei( 3) + xx(i4, 2)*dnei( 4)               &
     &          + xx(i5, 2)*dnei( 5) + xx(i6, 2)*dnei( 6)               &
     &          + xx(i7, 2)*dnei( 7) + xx(i8, 2)*dnei( 8)
!
          dyzi =  xx(i1, 2)*dnzi( 1) + xx(i2, 2)*dnzi( 2)               &
     &          + xx(i3, 2)*dnzi( 3) + xx(i4, 2)*dnzi( 4)               &
     &          + xx(i5, 2)*dnzi( 5) + xx(i6, 2)*dnzi( 6)               &
     &          + xx(i7, 2)*dnzi( 7) + xx(i8, 2)*dnzi( 8)
!
!
          dzxi =  xx(i1, 3)*dnxi( 1) + xx(i2, 3)*dnxi( 2)               &
     &          + xx(i3, 3)*dnxi( 3) + xx(i4, 3)*dnxi( 4)               &
     &          + xx(i5, 3)*dnxi( 5) + xx(i6, 3)*dnxi( 6)               &
     &          + xx(i7, 3)*dnxi( 7) + xx(i8, 3)*dnxi( 8)
!
          dzei =  xx(i1, 3)*dnei( 1) + xx(i2, 3)*dnei( 2)               &
     &          + xx(i3, 3)*dnei( 3) + xx(i4, 3)*dnei( 4)               &
     &          + xx(i5, 3)*dnei( 5) + xx(i6, 3)*dnei( 6)               &
     &          + xx(i7, 3)*dnei( 7) + xx(i8, 3)*dnei( 8)
!
          dzzi =  xx(i1, 3)*dnzi( 1) + xx(i2, 3)*dnzi( 2)               &
     &          + xx(i3, 3)*dnzi( 3) + xx(i4, 3)*dnzi( 4)               &
     &          + xx(i5, 3)*dnzi( 5) + xx(i6, 3)*dnzi( 6)               &
     &          + xx(i7, 3)*dnzi( 7) + xx(i8, 3)*dnzi( 8)
!
!
!
          xj11 = dyei*dzzi - dyzi*dzei
          xj12 = dyzi*dzxi - dyxi*dzzi
          xj13 = dyxi*dzei - dyei*dzxi
!
          xj21 = dzei*dxzi - dzzi*dxei
          xj22 = dzzi*dxxi - dzxi*dxzi
          xj23 = dzxi*dxei - dzei*dxxi
!
          xj31 = dxei*dyzi - dxzi*dyei
          xj32 = dxzi*dyxi - dxxi*dyzi
          xj33 = dxxi*dyei - dxei*dyxi
!
!
          xjac(iele) = dxxi*dyei*dzzi                                   &
     &               + dxei*dyzi*dzxi                                   &
     &               + dxzi*dyxi*dzei                                   &
     &             - ( dxzi*dyei*dzxi                                   &
     &               + dxxi*dyzi*dzei                                   &
     &               + dxei*dyxi*dzzi)
!
          if (xjac(iele) .eq. 0.0d0) then
            axjac(iele) = 1.0d+30
          else 
            axjac(iele) = 1.0d00 / xjac(iele)
          end if
!
!
          dxidx(iele) = xj11 * axjac(iele)
          deidx(iele) = xj12 * axjac(iele)
          dzidx(iele) = xj13 * axjac(iele)
!
          dxidy(iele) = xj21 * axjac(iele)
          deidy(iele) = xj22 * axjac(iele)
          dzidy(iele) = xj23 * axjac(iele)
!
          dxidz(iele) = xj31 * axjac(iele)
          deidz(iele) = xj32 * axjac(iele)
          dzidz(iele) = xj33 * axjac(iele)
!
!
          dnx(iele, 1)= (xj11*dnxi( 1) + xj12*dnei( 1) + xj13*dnzi( 1)) &
     &                 * axjac(iele)
          dnx(iele, 2)= (xj11*dnxi( 2) + xj12*dnei( 2) + xj13*dnzi( 2)) &
     &                 * axjac(iele)
          dnx(iele, 3)= (xj11*dnxi( 3) + xj12*dnei( 3) + xj13*dnzi( 3)) &
     &                 * axjac(iele)
          dnx(iele, 4)= (xj11*dnxi( 4) + xj12*dnei( 4) + xj13*dnzi( 4)) &
     &                 * axjac(iele)
          dnx(iele, 5)= (xj11*dnxi( 5) + xj12*dnei( 5) + xj13*dnzi( 5)) &
     &                 * axjac(iele)
          dnx(iele, 6)= (xj11*dnxi( 6) + xj12*dnei( 6) + xj13*dnzi( 6)) &
     &                 * axjac(iele)
          dnx(iele, 7)= (xj11*dnxi( 7) + xj12*dnei( 7) + xj13*dnzi( 7)) &
     &                 * axjac(iele)
          dnx(iele, 8)= (xj11*dnxi( 8) + xj12*dnei( 8) + xj13*dnzi( 8)) &
     &                 * axjac(iele)
!
          dny(iele, 1)= (xj21*dnxi( 1) + xj22*dnei( 1) + xj23*dnzi( 1)) &
     &                 * axjac(iele)
          dny(iele, 2)= (xj21*dnxi( 2) + xj22*dnei( 2) + xj23*dnzi( 2)) &
     &                 * axjac(iele)
          dny(iele, 3)= (xj21*dnxi( 3) + xj22*dnei( 3) + xj23*dnzi( 3)) &
     &                 * axjac(iele)
          dny(iele, 4)= (xj21*dnxi( 4) + xj22*dnei( 4) + xj23*dnzi( 4)) &
     &                 * axjac(iele)
          dny(iele, 5)= (xj21*dnxi( 5) + xj22*dnei( 5) + xj23*dnzi( 5)) &
     &                 * axjac(iele)
          dny(iele, 6)= (xj21*dnxi( 6) + xj22*dnei( 6) + xj23*dnzi( 6)) &
     &                * axjac(iele)
          dny(iele, 7)= (xj21*dnxi( 7) + xj22*dnei( 7) + xj23*dnzi( 7)) &
     &                 * axjac(iele)
          dny(iele, 8)= (xj21*dnxi( 8) + xj22*dnei( 8) + xj23*dnzi( 8)) &
     &                 * axjac(iele)
!
          dnz(iele, 1)= (xj31*dnxi( 1) + xj32*dnei( 1) + xj33*dnzi( 1)) &
     &                 * axjac(iele)
          dnz(iele, 2)= (xj31*dnxi( 2) + xj32*dnei( 2) + xj33*dnzi( 2)) &
     &                 * axjac(iele)
          dnz(iele, 3)= (xj31*dnxi( 3) + xj32*dnei( 3) + xj33*dnzi( 3)) &
     &                 * axjac(iele)
          dnz(iele, 4)= (xj31*dnxi( 4) + xj32*dnei( 4) + xj33*dnzi( 4)) &
     &                 * axjac(iele)
          dnz(iele, 5)= (xj31*dnxi( 5) + xj32*dnei( 5) + xj33*dnzi( 5)) &
     &                 * axjac(iele)
          dnz(iele, 6)= (xj31*dnxi( 6) + xj32*dnei( 6) + xj33*dnzi( 6)) &
     &                * axjac(iele)
          dnz(iele, 7)= (xj31*dnxi( 7) + xj32*dnei( 7) + xj33*dnzi( 7)) &
     &                 * axjac(iele)
          dnz(iele, 8)= (xj31*dnxi( 8) + xj32*dnei( 8) + xj33*dnzi( 8)) &
     &                 * axjac(iele)
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_jacobian_3d_8
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_3d_linear
