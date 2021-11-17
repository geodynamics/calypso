!
!      module cal_jacobian_2d_lag
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_2d_9(numnod, numsurf,                  &
!     &          ie_surf, xx, np_smp, isurf_smp_stack, xjac, axjac,     &
!     &          xsf, ysf, zsf, dnxi, dnei)
!
      module cal_jacobian_2d_lag
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
      subroutine s_cal_jacobian_2d_9(numnod, numsurf,                   &
     &          ie_surf, xx, np_smp, isurf_smp_stack, xjac, axjac,      &
     &          xsf, ysf, zsf, dnxi, dnei)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numsurf
      integer(kind = kint), intent(in) :: ie_surf(numsurf, num_lag_sf)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: isurf_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: dnxi(num_lag_sf)
      real(kind = kreal), intent(in) :: dnei(num_lag_sf)
!
      real(kind = kreal), intent(inout) :: xjac(numsurf)
      real(kind = kreal), intent(inout) :: axjac(numsurf)
      real(kind = kreal), intent(inout) :: xsf(numsurf)
      real(kind = kreal), intent(inout) :: ysf(numsurf)
      real(kind = kreal), intent(inout) :: zsf(numsurf)
!
      integer(kind = kint) :: ip, ist, ied, isurf
!
      real(kind = kreal) :: dxxi, dxei
      real(kind = kreal) :: dyxi, dyei
      real(kind = kreal) :: dzxi, dzei
!
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8, i9
!
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,isurf,i1,i2,i3,i4,i5,i6,i7,i8,i9,                      &
!$omp&   dxxi,dxei,dyxi,dyei,dzxi,dzei)
      do ip = 1, np_smp
        ist = isurf_smp_stack(ip-1) + 1
        ied = isurf_smp_stack(ip)
!
!cdir nodep noloopchg
        do isurf = ist, ied
!
          i1 =  ie_surf(isurf, 1)
          i2 =  ie_surf(isurf, 2)
          i3 =  ie_surf(isurf, 3)
          i4 =  ie_surf(isurf, 4)
          i5 =  ie_surf(isurf, 5)
          i6 =  ie_surf(isurf, 6)
          i7 =  ie_surf(isurf, 7)
          i8 =  ie_surf(isurf, 8)
          i9 =  ie_surf(isurf, 9)
!
          dxxi =  xx(i1, 1)*dnxi( 1) + xx(i2, 1)*dnxi( 2)               &
     &          + xx(i3, 1)*dnxi( 3) + xx(i4, 1)*dnxi( 4)               &
     &          + xx(i5, 1)*dnxi( 5) + xx(i6, 1)*dnxi( 6)               &
     &          + xx(i7, 1)*dnxi( 7) + xx(i8, 1)*dnxi( 8)               &
     &          + xx(i9, 1)*dnxi( 9)
!
          dxei =  xx(i1, 1)*dnei( 1) + xx(i2, 1)*dnei( 2)               &
     &          + xx(i3, 1)*dnei( 3) + xx(i4, 1)*dnei( 4)               &
     &          + xx(i5, 1)*dnei( 5) + xx(i6, 1)*dnei( 6)               &
     &          + xx(i7, 1)*dnei( 7) + xx(i8, 1)*dnei( 8)               &
     &          + xx(i9, 1)*dnei( 9)
!
!
          dyxi =  xx(i1, 2)*dnxi( 1) + xx(i2, 2)*dnxi( 2)               &
     &          + xx(i3, 2)*dnxi( 3) + xx(i4, 2)*dnxi( 4)               &
     &          + xx(i5, 2)*dnxi( 5) + xx(i6, 2)*dnxi( 6)               &
     &          + xx(i7, 2)*dnxi( 7) + xx(i8, 2)*dnxi( 8)               &
     &          + xx(i9, 2)*dnxi( 9)
!
          dyei =  xx(i1, 2)*dnei( 1) + xx(i2, 2)*dnei( 2)               &
     &          + xx(i3, 2)*dnei( 3) + xx(i4, 2)*dnei( 4)               &
     &          + xx(i5, 2)*dnei( 5) + xx(i6, 2)*dnei( 6)               &
     &          + xx(i7, 2)*dnei( 7) + xx(i8, 2)*dnei( 8)               &
     &          + xx(i9, 2)*dnei( 9)
!
!
          dzxi =  xx(i1, 3)*dnxi( 1) + xx(i2, 3)*dnxi( 2)               &
     &          + xx(i3, 3)*dnxi( 3) + xx(i4, 3)*dnxi( 4)               &
     &          + xx(i5, 3)*dnxi( 5) + xx(i6, 3)*dnxi( 6)               &
     &          + xx(i7, 3)*dnxi( 7) + xx(i8, 3)*dnxi( 8)               &
     &          + xx(i9, 3)*dnxi( 9)
!
          dzei =  xx(i1, 3)*dnei( 1) + xx(i2, 3)*dnei( 2)               &
     &          + xx(i3, 3)*dnei( 3) + xx(i4, 3)*dnei( 4)               &
     &          + xx(i5, 3)*dnei( 5) + xx(i6, 3)*dnei( 6)               &
     &          + xx(i7, 3)*dnei( 7) + xx(i8, 3)*dnei( 8)               &
     &          + xx(i9, 3)*dnei( 9)
!
!
!
          xsf(isurf) = dyxi*dzei - dzxi*dyei
          ysf(isurf) = dzxi*dxei - dxxi*dzei
          zsf(isurf) = dxxi*dyei - dyxi*dxei
!
          xjac(isurf) = sqrt( xsf(isurf)*xsf(isurf)                     &
     &                      + ysf(isurf)*ysf(isurf)                     &
     &                      + zsf(isurf)*zsf(isurf) )
!
          if (xjac(isurf) .eq. 0.0d0) then
            axjac(isurf) = 1.0d+30
          else 
            axjac(isurf) = 1.0d00 / xjac(isurf)
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_jacobian_2d_9
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_2d_lag
