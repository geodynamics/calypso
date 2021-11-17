!
!      module cal_jacobian_1d
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_1d_2(numnod, numedge,                  &
!    &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,     &
!    &           xeg, yeg, zeg,  dnxi)
!      subroutine s_cal_jacobian_1d_3(numnod, numedge, nnod_4_edge,     &
!    &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,     &
!    &           xeg, yeg, zeg,  dnxi)
!      subroutine s_cal_jacobian_1d_2_3(numnod, numedge, nnod_4_edge,   &
!    &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,     &
!    &           xeg, yeg, zeg,  dnxi)
!
!
!      subroutine cal_x_jacobian_1d_2(numnod, numedge, nnod_4_edge,     &
!    &           ie_edge, xx, xjac, axjac, xeg, dnxi)
!      subroutine cal_x_jacobian_1d_3(numnod, numedge, nnod_4_edge,     &
!    &           ie_edge, xx, xjac, axjac, xeg, dnxi)
!      subroutine cal_x_jacobian_1d_2_3(numnod, numedge, nnod_4_edge,   &
!    &           ie_edge, xx, xjac, axjac, xeg, dnxi)
!
      module cal_jacobian_1d
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
      subroutine s_cal_jacobian_1d_2(numnod, numedge,                   &
     &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,     &
     &           xeg, yeg, zeg,  dnxi)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint), intent(in)                                  &
     &                     :: ie_edge(numedge, num_linear_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: dnxi(num_linear_edge)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
      real(kind = kreal), intent(inout) :: yeg(numedge)
      real(kind = kreal), intent(inout) :: zeg(numedge)
!
      integer(kind = kint) :: ip, ist, ied, iedge
      integer(kind = kint) :: i1,  i2
!
!
!$omp parallel do private(ist,ied,iedge,i1,i2)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
!
!cdir nodep noloopchg
        do iedge = ist, ied
!
          i1 =  ie_edge(iedge, 1)
          i2 =  ie_edge(iedge, 2)
!
          xeg(iedge) =  xx(i1, 1)*dnxi( 1) + xx(i2, 1)*dnxi( 2)
!
          yeg(iedge) =  xx(i1, 2)*dnxi( 1) + xx(i2, 2)*dnxi( 2)
!
          zeg(iedge) =  xx(i1, 3)*dnxi( 1) + xx(i2, 3)*dnxi( 2)
!
          xjac(iedge) = sqrt( xeg(iedge)*xeg(iedge)                     &
     &                      + yeg(iedge)*yeg(iedge)                     &
     &                      + zeg(iedge)*zeg(iedge) )
!
          if (xjac(iedge) .eq. 0.0d0) then
            axjac(iedge) = 1.0d+30
          else 
            axjac(iedge) = 1.0d00 / xjac(iedge)
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_jacobian_1d_2
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_1d_3(numnod, numedge,                   &
     &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,     &
     &           xeg, yeg, zeg, dnxi)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint), intent(in)                                  &
     &                      :: ie_edge(numedge, num_quad_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_edge)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
      real(kind = kreal), intent(inout) :: yeg(numedge)
      real(kind = kreal), intent(inout) :: zeg(numedge)
!
      integer(kind = kint) :: ip, ist, ied, iedge
      integer(kind = kint) :: i1,  i2,  i3
!
!
!$omp parallel do private(ist,ied,iedge,i1,i2,i3)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
!
!cdir nodep noloopchg
        do iedge = ist, ied
!
          i1 =  ie_edge(iedge, 1)
          i2 =  ie_edge(iedge, 2)
          i3 =  ie_edge(iedge, 3)
!
          xeg(iedge) =  xx(i1, 1)*dnxi( 1) + xx(i2, 1)*dnxi( 2)         &
     &                + xx(i3, 1)*dnxi( 3)
!
          yeg(iedge) =  xx(i1, 2)*dnxi( 1) + xx(i2, 2)*dnxi( 2)         &
     &                + xx(i3, 2)*dnxi( 3)
!
          zeg(iedge) =  xx(i1, 3)*dnxi( 1) + xx(i2, 3)*dnxi( 2)         &
     &                + xx(i3, 3)*dnxi( 3)
!
!
          xjac(iedge) = sqrt( xeg(iedge)*xeg(iedge)                     &
     &                      + yeg(iedge)*yeg(iedge)                     &
     &                      + zeg(iedge)*zeg(iedge) )
!
          if (xjac(iedge) .eq. 0.0d0) then
            axjac(iedge) = 1.0d+30
          else 
            axjac(iedge) = 1.0d00 / xjac(iedge)
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_jacobian_1d_3
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_1d_2_3(numnod, numedge,                 &
     &           ie_edge, xx, np_smp, iedge_smp_stack, xjac, axjac,     &
     &           xeg, yeg, zeg,  dnxi)
!
      use m_constants
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge
      integer(kind = kint), intent(in)                                  &
     &                      :: ie_edge(numedge, num_linear_edge)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_edge)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
      real(kind = kreal), intent(inout) :: yeg(numedge)
      real(kind = kreal), intent(inout) :: zeg(numedge)
!
      integer(kind = kint) :: ip, ist, ied, iedge
      integer(kind = kint) :: i1,  i2
      real(kind = kreal) :: x1, x2, x3, y1, y2, y3, z1, z2, z3
!
!
!$omp parallel do private(ist,ied,iedge,i1,i2,x1,x2,x3,y1,y2,y3,z1,z2,z3)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
!
!cdir nodep noloopchg
        do iedge = ist, ied
!
          i1 =  ie_edge(iedge, 1)
          i2 =  ie_edge(iedge, 2)
!
          x1 = xx(i1,1)
          x2 = half * (xx(i1,1) + xx(i2,1))
          x3 = xx(i2,1)
!
          y1 = xx(i1,2)
          y2 = half * (xx(i1,2) + xx(i2,2))
          y3 = xx(i2,2)
!
          z1 = xx(i1,3)
          z2 = half * (xx(i1,3) + xx(i2,3))
          z3 = xx(i2,3)
!
!
          xeg(iedge) =  x1*dnxi( 1) + x2*dnxi( 2) + x3*dnxi( 3)
          yeg(iedge) =  y1*dnxi( 1) + y2*dnxi( 2) + y3*dnxi( 3)
          zeg(iedge) =  z1*dnxi( 1) + z2*dnxi( 2) + z3*dnxi( 3)
!
!
          xjac(iedge) = sqrt( xeg(iedge)*xeg(iedge)                     &
     &                      + yeg(iedge)*yeg(iedge)                     &
     &                      + zeg(iedge)*zeg(iedge) )
!
          if (xjac(iedge) .eq. 0.0d0) then
            axjac(iedge) = 1.0d+30
          else 
            axjac(iedge) = 1.0d00 / xjac(iedge)
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_jacobian_1d_2_3
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_x_jacobian_1d_2(numnod, numedge, nnod_4_edge,      &
    &           ie_edge, xx, xjac, axjac, xeg, dnxi)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge, nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod)
!
      real(kind = kreal), intent(in) :: dnxi(2)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
!
      integer(kind = kint) :: iedge, i1,  i2
!
!
!cdir nodep noloopchg
      do iedge = 1, numedge-1
        i1 =  ie_edge(iedge,1)
        i2 =  ie_edge(iedge,2)
!
        xeg(iedge) =  xx(i1)*dnxi( 1) + xx(i2)*dnxi( 2)
        xjac(iedge) = sqrt( xeg(iedge)*xeg(iedge)  )
!
        if (xjac(iedge) .eq. 0.0d0) then
          axjac(iedge) = 1.0d+30
        else 
          axjac(iedge) = 1.0d00 / xjac(iedge)
        end if
      end do
!
      end subroutine cal_x_jacobian_1d_2
!
!-----------------------------------------------------------------------
!
      subroutine cal_x_jacobian_1d_3(numnod, numedge, nnod_4_edge,      &
    &           ie_edge, xx, xjac, axjac, xeg, dnxi)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge, nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod)
!
      real(kind = kreal), intent(in) :: dnxi(3)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
!
      integer(kind = kint) :: iedge
      integer(kind = kint) :: i1,  i2,  i3
!
!
!cdir nodep noloopchg
      do iedge = 1, numedge-2, 2
        i1 =  ie_edge(iedge,1)
        i2 =  ie_edge(iedge,2)
        i3 =  ie_edge(iedge,3)
!
        xeg(iedge) = xx(i1)*dnxi(1) + xx(i2)*dnxi(2) + xx(i3)*dnxi(3)
        xjac(iedge) = sqrt( xeg(iedge)*xeg(iedge) )
!
        if (xjac(iedge) .eq. 0.0d0) then
          axjac(iedge) = 1.0d+30
        else 
          axjac(iedge) = 1.0d00 / xjac(iedge)
        end if
      end do
!
      end subroutine cal_x_jacobian_1d_3
!
!-----------------------------------------------------------------------
!
      subroutine cal_x_jacobian_1d_2_3(numnod, numedge, nnod_4_edge,    &
    &           ie_edge, xx, xjac, axjac, xeg, dnxi)
!
      use m_constants
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge, nnod_4_edge)
      real(kind = kreal), intent(in) :: xx(numnod)
!
      real(kind = kreal), intent(in) :: dnxi(3)
!
      real(kind = kreal), intent(inout) :: xjac(numedge)
      real(kind = kreal), intent(inout) :: axjac(numedge)
      real(kind = kreal), intent(inout) :: xeg(numedge)
!
      integer(kind = kint) :: iedge
      integer(kind = kint) :: i1,  i2
!
!
!cdir nodep noloopchg
      do iedge = 1, numedge
        i1 =  ie_edge(iedge,1)
        i2 =  ie_edge(iedge,2)
!
        xeg(iedge) = xx(i1)*dnxi(1) + half*(xx(i1) + xx(i2))*dnxi(2)    &
     &             + xx(i2)*dnxi(3)
        xjac(iedge) = sqrt( xeg(iedge)*xeg(iedge) )
!
        if (xjac(iedge) .eq. 0.0d0) then
          axjac(iedge) = 1.0d+30
        else 
          axjac(iedge) = 1.0d00 / xjac(iedge)
        end if
      end do
!
      end subroutine cal_x_jacobian_1d_2_3
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_1d
