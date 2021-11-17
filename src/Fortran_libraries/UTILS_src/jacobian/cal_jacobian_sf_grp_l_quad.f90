!cal_jacobian_sf_grp_l_quad.f90
!      module cal_jacobian_sf_grp_l_quad
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine s_cal_jacobian_sf_grp_4_8(numnod, numele,             &
!     &          ie, xx, num_surf, num_surf_bc, surf_item,              &
!     &          np_smp, num_surf_smp, isurf_grp_smp_stack,             &
!     &          xjac, axjac,  xsf, ysf, zsf, dnxi, dnei)
!
      module cal_jacobian_sf_grp_l_quad
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
      subroutine s_cal_jacobian_sf_grp_4_8(numnod, numele,              &
     &          ie, xx, num_surf, num_surf_bc, surf_item,               &
     &          np_smp, num_surf_smp, isurf_grp_smp_stack,              &
     &          xjac, axjac,  xsf, ysf, zsf, dnxi, dnei)
!
      use m_constants
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele, num_t_linear)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: np_smp
      integer (kind=kint), intent(in) :: num_surf_smp
      integer (kind=kint), intent(in)                                   &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
!
      integer (kind=kint), intent(in) :: num_surf
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      real(kind = kreal), intent(in) :: dnxi(num_quad_sf)
      real(kind = kreal), intent(in) :: dnei(num_quad_sf)
!
      real(kind = kreal), intent(inout) :: xjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: axjac(num_surf_bc)
      real(kind = kreal), intent(inout) :: xsf(num_surf_bc)
      real(kind = kreal), intent(inout) :: ysf(num_surf_bc)
      real(kind = kreal), intent(inout) :: zsf(num_surf_bc)
!
      integer(kind = kint) :: ip, igrp, ist, ied, inum, iele, isf, jp
!
      real(kind = kreal) :: dxxi, dxei
      real(kind = kreal) :: dyxi, dyei
      real(kind = kreal) :: dzxi, dzei
!
      real(kind = kreal) :: x1, x2, x3, x4, x5, x6, x7, x8
      real(kind = kreal) :: y1, y2, y3, y4, y5, y6, y7, y8
      real(kind = kreal) :: z1, z2, z3, z4, z5, z6, z7, z8
      integer(kind = kint) :: i1,  i2,  i3,  i4
      integer(kind = kint) :: il1, il2, il3, il4
!
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,igrp,jp,inum,iele,isf,                                 &
!$omp&   i1,i2,i3,i4,il1,il2,il3,il4,                                   &
!$omp&   dxxi,dxei,dyxi,dyei,dzxi,dzei,x1,x2,x3,x4,x5,x6,x7,x8,         &
!$omp&   y1,y2,y3,y4,y5,y6,y7,y8,z1,z2,z3,z4,z5,z6,z7,z8)
      do ip = 1, np_smp
        do igrp = 1, num_surf
          jp = np_smp*(igrp-1) + ip
          ist = isurf_grp_smp_stack(jp-1) + 1
          ied = isurf_grp_smp_stack(jp)
!
!cdir nodep noloopchg
          do inum = ist, ied
!
            iele = surf_item(1,inum)
            isf = surf_item(2,inum)
!
            il1 =  node_on_sf_4(1,isf)
            il2 =  node_on_sf_4(2,isf)
            il3 =  node_on_sf_4(3,isf)
            il4 =  node_on_sf_4(4,isf)
!
            i1 =  ie(iele,il1)
            i2 =  ie(iele,il2)
            i3 =  ie(iele,il3)
            i4 =  ie(iele,il4)
!
            x1 = xx(i1,1)
            x2 = xx(i2,1)
            x3 = xx(i3,1)
            x4 = xx(i4,1)
            x5 = half * (xx(i1,1) + xx(i2,1))
            x6 = half * (xx(i2,1) + xx(i3,1))
            x7 = half * (xx(i3,1) + xx(i4,1))
            x8 = half * (xx(i4,1) + xx(i1,1))
!
            y1 = xx(i1,2)
            y2 = xx(i2,2)
            y3 = xx(i3,2)
            y4 = xx(i4,2)
            y5 = half * (xx(i1,2) + xx(i2,2))
            y6 = half * (xx(i2,2) + xx(i3,2))
            y7 = half * (xx(i3,2) + xx(i4,2))
            y8 = half * (xx(i4,2) + xx(i1,2))
!
            z1 = xx(i1,3)
            z2 = xx(i2,3)
            z3 = xx(i3,3)
            z4 = xx(i4,3)
            z5 = half * (xx(i1,3) + xx(i2,3))
            z6 = half * (xx(i2,3) + xx(i3,3))
            z7 = half * (xx(i3,3) + xx(i4,3))
            z8 = half * (xx(i4,3) + xx(i1,3))
!
            dxxi =  x1*dnxi( 1) + x2*dnxi( 2)                           &
     &            + x3*dnxi( 3) + x4*dnxi( 4)                           &
     &            + x5*dnxi( 5) + x6*dnxi( 6)                           &
     &            + x7*dnxi( 7) + x8*dnxi( 8)
!
            dxei =  x1*dnei( 1) + x2*dnei( 2)                           &
     &            + x3*dnei( 3) + x4*dnei( 4)                           &
     &            + x5*dnei( 5) + x6*dnei( 6)                           &
     &            + x7*dnei( 7) + x8*dnei( 8)
!
!
            dyxi =  y1*dnxi( 1) + y2*dnxi( 2)                           &
     &            + y3*dnxi( 3) + y4*dnxi( 4)                           &
     &            + y5*dnxi( 5) + y6*dnxi( 6)                           &
     &            + y7*dnxi( 7) + y8*dnxi( 8)
!
            dyei =  y1*dnei( 1) + y2*dnei( 2)                           &
     &            + y3*dnei( 3) + y4*dnei( 4)                           &
     &            + y5*dnei( 5) + y6*dnei( 6)                           &
     &            + y7*dnei( 7) + y8*dnei( 8)
!
!
            dzxi =  z1*dnxi( 1) + z2*dnxi( 2)                           &
     &            + z3*dnxi( 3) + z4*dnxi( 4)                           &
     &            + z5*dnxi( 5) + z6*dnxi( 6)                           &
     &            + z7*dnxi( 7) + z8*dnxi( 8)
!
            dzei =  z1*dnei( 1) + z2*dnei( 2)                           &
     &            + z3*dnei( 3) + z4*dnei( 4)                           &
     &            + z5*dnei( 5) + z6*dnei( 6)                           &
     &            + z7*dnei( 7) + z8*dnei( 8)
!
!
!
            xsf(inum) = dyxi*dzei - dzxi*dyei
            ysf(inum) = dzxi*dxei - dxxi*dzei
            zsf(inum) = dxxi*dyei - dyxi*dxei
!
            xjac(inum) = sqrt( xsf(inum)*xsf(inum)                      &
     &                       + ysf(inum)*ysf(inum)                      &
     &                       + zsf(inum)*zsf(inum) )
!
            if (xjac(inum) .eq. 0.0d0) then
              axjac(inum) = 1.0d+30
            else 
              axjac(inum) = 1.0d00 / xjac(inum)
            end if
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine s_cal_jacobian_sf_grp_4_8
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_sf_grp_l_quad
