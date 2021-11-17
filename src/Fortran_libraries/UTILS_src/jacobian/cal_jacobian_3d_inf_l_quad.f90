!cal_jacobian_3d_inf_l_quad.f90
!      module cal_jacobian_3d_inf_l_quad
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June, 2006
!        modified by H. Matsui on Dec., 2008
!
!      subroutine s_cal_jacobian_3d_inf_8_20(numnod, numele, nnod_4_ele,&
!     &          np_smp, ie, xx, num_surf_bc, surf_item,                &
!     &          ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,          &
!     &          isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,       &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi,                 &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!
!> @brief     Caliculate jacobian by quadrature shape function
!>      for infinity quadrature element
!
      module cal_jacobian_3d_inf_l_quad
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
!>      Caliculate jacobian by quadrature infinity shape function
!>      for linear element
      subroutine s_cal_jacobian_3d_inf_8_20(numnod, numele,             &
     &          np_smp, ie, xx, num_surf_bc, surf_item,                 &
     &          ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,           &
     &          isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,        &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi,                  &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use m_geometry_constants
      use m_constants
!
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: ie(numele, num_t_linear)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer (kind=kint), intent(in) :: num_surf_bc
      integer (kind=kint), intent(in) :: surf_item(2,num_surf_bc)
!
      integer (kind=kint), intent(in) :: ngrp_sf_infty
      integer (kind=kint), intent(in) :: id_grp_sf_infty(ngrp_sf_infty)
!
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &              :: isurf_grp_smp_stack(0:num_surf_smp)
!
      real(kind = kreal), intent(in) :: dnxi(num_t_quad)
      real(kind = kreal), intent(in) :: dnei(num_t_quad)
      real(kind = kreal), intent(in) :: dnzi(num_t_quad)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_quad,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_quad,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_quad,nsurf_4_ele)
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
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_quad)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_quad)
!
      integer(kind = kint) :: i, ip, ist, ied, iele
      integer(kind = kint) :: igrp, id_sf, inum, isf
!
      real(kind = kreal) :: dxxi, dxei, dxzi
      real(kind = kreal) :: dyxi, dyei, dyzi
      real(kind = kreal) :: dzxi, dzei, dzzi
      real(kind = kreal) :: xj11, xj12, xj13
      real(kind = kreal) :: xj21, xj22, xj23
      real(kind = kreal) :: xj31, xj32, xj33
!
      real(kind = kreal) :: x01, x02, x03, x04, x05, x06, x07, x08
      real(kind = kreal) :: x09, x10, x11, x12, x13, x14, x15, x16
      real(kind = kreal) :: x17, x18, x19, x20
      real(kind = kreal) :: y01, y02, y03, y04, y05, y06, y07, y08
      real(kind = kreal) :: y09, y10, y11, y12, y13, y14, y15, y16
      real(kind = kreal) :: y17, y18, y19, y20
      real(kind = kreal) :: z01, z02, z03, z04, z05, z06, z07, z08
      real(kind = kreal) :: z09, z10, z11, z12, z13, z14, z15, z16
      real(kind = kreal) :: z17, z18, z19, z20
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
      do i = 1, ngrp_sf_infty
        igrp = id_grp_sf_infty(i)
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,id_sf,inum,iele,isf,i1,i2,i3,i4,i5,i6,i7,i8,           &
!$omp&   xj11,xj12,xj13,xj21,xj22,xj23,xj31,xj32,xj33,                  &
!$omp&   x01,x02,x03,x04,x05,x06,x07,x08,x09,x10,x11,x12,x13,x14,x15,   &
!$omp&   x16,x17,x18,x19,x20,y01,y02,y03,y04,y05,y06,y07,y08,y09,y10,   &
!$omp&   y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,z01,z02,z03,z04,z05,   &
!$omp&   z06,z07,z08,z09,z10,z11,z12,z13,z14,z15,z16,z17,z18,z19,z20)
        do ip = 1, np_smp
          id_sf = np_smp*(igrp-1) + ip
          ist = isurf_grp_smp_stack(id_sf-1)+1
          ied = isurf_grp_smp_stack(id_sf)
!
!cdir nodep noloopchg
          do inum = ist, ied
            iele = surf_item(1,inum)
            isf = surf_item(2,inum)
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
            x01 = xx(i1,1)
            x02 = xx(i2,1)
            x03 = xx(i3,1)
            x04 = xx(i4,1)
            x05 = xx(i5,1)
            x06 = xx(i6,1)
            x07 = xx(i7,1)
            x08 = xx(i8,1)
            x09 = half * (xx(i1,1) + xx(i2,1))
            x10 = half * (xx(i2,1) + xx(i3,1))
            x11 = half * (xx(i3,1) + xx(i4,1))
            x12 = half * (xx(i4,1) + xx(i1,1))
            x13 = half * (xx(i5,1) + xx(i6,1))
            x14 = half * (xx(i6,1) + xx(i7,1))
            x15 = half * (xx(i7,1) + xx(i8,1))
            x16 = half * (xx(i8,1) + xx(i5,1))
            x17 = half * (xx(i1,1) + xx(i5,1))
            x18 = half * (xx(i2,1) + xx(i6,1))
            x19 = half * (xx(i3,1) + xx(i7,1))
            x20 = half * (xx(i4,1) + xx(i8,1))
!
            y01 = xx(i1,2)
            y02 = xx(i2,2)
            y03 = xx(i3,2)
            y04 = xx(i4,2)
            y05 = xx(i5,2)
            y06 = xx(i6,2)
            y07 = xx(i7,2)
            y08 = xx(i8,2)
            y09 = half * (xx(i1,2) + xx(i2,2))
            y10 = half * (xx(i2,2) + xx(i3,2))
            y11 = half * (xx(i3,2) + xx(i4,2))
            y12 = half * (xx(i4,2) + xx(i1,2))
            y13 = half * (xx(i5,2) + xx(i6,2))
            y14 = half * (xx(i6,2) + xx(i7,2))
            y15 = half * (xx(i7,2) + xx(i8,2))
            y16 = half * (xx(i8,2) + xx(i5,2))
            y17 = half * (xx(i1,2) + xx(i5,2))
            y18 = half * (xx(i2,2) + xx(i6,2))
            y19 = half * (xx(i3,2) + xx(i7,2))
            y20 = half * (xx(i4,2) + xx(i8,2))
!
            z01 = xx(i1,3)
            z02 = xx(i2,3)
            z03 = xx(i3,3)
            z04 = xx(i4,3)
            z05 = xx(i5,3)
            z06 = xx(i6,3)
            z07 = xx(i7,3)
            z08 = xx(i8,3)
            z09 = half * (xx(i1,3) + xx(i2,3))
            z10 = half * (xx(i2,3) + xx(i3,3))
            z11 = half * (xx(i3,3) + xx(i4,3))
            z12 = half * (xx(i4,3) + xx(i1,3))
            z13 = half * (xx(i5,3) + xx(i6,3))
            z14 = half * (xx(i6,3) + xx(i7,3))
            z15 = half * (xx(i7,3) + xx(i8,3))
            z16 = half * (xx(i8,3) + xx(i5,3))
            z17 = half * (xx(i1,3) + xx(i5,3))
            z18 = half * (xx(i2,3) + xx(i6,3))
            z19 = half * (xx(i3,3) + xx(i7,3))
            z20 = half * (xx(i4,3) + xx(i8,3))
!
            dxxi =  x01*dnxi_inf( 1,isf) + x02*dnxi_inf( 2,isf)   &
     &            + x03*dnxi_inf( 3,isf) + x04*dnxi_inf( 4,isf)   &
     &            + x05*dnxi_inf( 5,isf) + x06*dnxi_inf( 6,isf)   &
     &            + x07*dnxi_inf( 7,isf) + x08*dnxi_inf( 8,isf)   &
     &            + x09*dnxi_inf( 9,isf) + x10*dnxi_inf(10,isf)   &
     &            + x11*dnxi_inf(11,isf) + x12*dnxi_inf(12,isf)   &
     &            + x13*dnxi_inf(13,isf) + x14*dnxi_inf(14,isf)   &
     &            + x15*dnxi_inf(15,isf) + x16*dnxi_inf(16,isf)   &
     &            + x17*dnxi_inf(17,isf) + x18*dnxi_inf(18,isf)   &
     &            + x19*dnxi_inf(19,isf) + x20*dnxi_inf(20,isf)
!
            dxei =  x01*dnei_inf( 1,isf) + x02*dnei_inf( 2,isf)   &
     &            + x03*dnei_inf( 3,isf) + x04*dnei_inf( 4,isf)   &
     &            + x05*dnei_inf( 5,isf) + x06*dnei_inf( 6,isf)   &
     &            + x07*dnei_inf( 7,isf) + x08*dnei_inf( 8,isf)   &
     &            + x09*dnei_inf( 9,isf) + x10*dnei_inf(10,isf)   &
     &            + x11*dnei_inf(11,isf) + x12*dnei_inf(12,isf)   &
     &            + x13*dnei_inf(13,isf) + x14*dnei_inf(14,isf)   &
     &            + x15*dnei_inf(15,isf) + x16*dnei_inf(16,isf)   &
     &            + x17*dnei_inf(17,isf) + x18*dnei_inf(18,isf)   &
     &            + x19*dnei_inf(19,isf) + x20*dnei_inf(20,isf)
!
            dxzi =  x01*dnzi_inf( 1,isf) + x02*dnzi_inf( 2,isf)   &
     &            + x03*dnzi_inf( 3,isf) + x04*dnzi_inf( 4,isf)   &
     &            + x05*dnzi_inf( 5,isf) + x06*dnzi_inf( 6,isf)   &
     &            + x07*dnzi_inf( 7,isf) + x08*dnzi_inf( 8,isf)   &
     &            + x09*dnzi_inf( 9,isf) + x10*dnzi_inf(10,isf)   &
     &            + x11*dnzi_inf(11,isf) + x12*dnzi_inf(12,isf)   &
     &            + x13*dnzi_inf(13,isf) + x14*dnzi_inf(14,isf)   &
     &            + x15*dnzi_inf(15,isf) + x16*dnzi_inf(16,isf)   &
     &            + x17*dnzi_inf(17,isf) + x18*dnzi_inf(18,isf)   &
     &            + x19*dnzi_inf(19,isf) + x20*dnzi_inf(20,isf)
!
!
            dyxi =  y01*dnxi_inf( 1,isf) + y02*dnxi_inf( 2,isf)   &
     &            + y03*dnxi_inf( 3,isf) + y04*dnxi_inf( 4,isf)   &
     &            + y05*dnxi_inf( 5,isf) + y06*dnxi_inf( 6,isf)   &
     &            + y07*dnxi_inf( 7,isf) + y08*dnxi_inf( 8,isf)   &
     &            + y09*dnxi_inf( 9,isf) + y10*dnxi_inf(10,isf)   &
     &            + y11*dnxi_inf(11,isf) + y12*dnxi_inf(12,isf)   &
     &            + y13*dnxi_inf(13,isf) + y14*dnxi_inf(14,isf)   &
     &            + y15*dnxi_inf(15,isf) + y16*dnxi_inf(16,isf)   &
     &            + y17*dnxi_inf(17,isf) + y18*dnxi_inf(18,isf)   &
     &            + y19*dnxi_inf(19,isf) + y20*dnxi_inf(20,isf)
!
            dyei =  y01*dnei_inf( 1,isf) + y02*dnei_inf( 2,isf)   &
     &            + y03*dnei_inf( 3,isf) + y04*dnei_inf( 4,isf)   &
     &            + y05*dnei_inf( 5,isf) + y06*dnei_inf( 6,isf)   &
     &            + y07*dnei_inf( 7,isf) + y08*dnei_inf( 8,isf)   &
     &            + y09*dnei_inf( 9,isf) + y10*dnei_inf(10,isf)   &
     &            + y11*dnei_inf(11,isf) + y12*dnei_inf(12,isf)   &
     &            + y13*dnei_inf(13,isf) + y14*dnei_inf(14,isf)   &
     &            + y15*dnei_inf(15,isf) + y16*dnei_inf(16,isf)   &
     &            + y17*dnei_inf(17,isf) + y18*dnei_inf(18,isf)   &
     &            + y19*dnei_inf(19,isf) + y20*dnei_inf(20,isf)
!
            dyzi =  y01*dnzi_inf( 1,isf) + y02*dnzi_inf( 2,isf)   &
     &            + y03*dnzi_inf( 3,isf) + y04*dnzi_inf( 4,isf)   &
     &            + y05*dnzi_inf( 5,isf) + y06*dnzi_inf( 6,isf)   &
     &            + y07*dnzi_inf( 7,isf) + y08*dnzi_inf( 8,isf)   &
     &            + y09*dnzi_inf( 9,isf) + y10*dnzi_inf(10,isf)   &
     &            + y11*dnzi_inf(11,isf) + y12*dnzi_inf(12,isf)   &
     &            + y13*dnzi_inf(13,isf) + y14*dnzi_inf(14,isf)   &
     &            + y15*dnzi_inf(15,isf) + y16*dnzi_inf(16,isf)   &
     &            + y17*dnzi_inf(17,isf) + y18*dnzi_inf(18,isf)   &
     &            + y19*dnzi_inf(19,isf) + y20*dnzi_inf(20,isf)
!
!
            dzxi =  z01*dnxi_inf( 1,isf) + z02*dnxi_inf( 2,isf)   &
     &            + z03*dnxi_inf( 3,isf) + z04*dnxi_inf( 4,isf)   &
     &            + z05*dnxi_inf( 5,isf) + z06*dnxi_inf( 6,isf)   &
     &            + z07*dnxi_inf( 7,isf) + z08*dnxi_inf( 8,isf)   &
     &            + z09*dnxi_inf( 9,isf) + z10*dnxi_inf(10,isf)   &
     &            + z11*dnxi_inf(11,isf) + z12*dnxi_inf(12,isf)   &
     &            + z13*dnxi_inf(13,isf) + z14*dnxi_inf(14,isf)   &
     &            + z15*dnxi_inf(15,isf) + z16*dnxi_inf(16,isf)   &
     &            + z17*dnxi_inf(17,isf) + z18*dnxi_inf(18,isf)   &
     &            + z19*dnxi_inf(19,isf) + z20*dnxi_inf(20,isf)
!
            dzei =  z01*dnei_inf( 1,isf) + z02*dnei_inf( 2,isf)   &
     &            + z03*dnei_inf( 3,isf) + z04*dnei_inf( 4,isf)   &
     &            + z05*dnei_inf( 5,isf) + z06*dnei_inf( 6,isf)   &
     &            + z07*dnei_inf( 7,isf) + z08*dnei_inf( 8,isf)   &
     &            + z09*dnei_inf( 9,isf) + z10*dnei_inf(10,isf)   &
     &            + z11*dnei_inf(11,isf) + z12*dnei_inf(12,isf)   &
     &            + z13*dnei_inf(13,isf) + z14*dnei_inf(14,isf)   &
     &            + z15*dnei_inf(15,isf) + z16*dnei_inf(16,isf)   &
     &            + z17*dnei_inf(17,isf) + z18*dnei_inf(18,isf)   &
     &            + z19*dnei_inf(19,isf) + z20*dnei_inf(20,isf)
!
            dzzi =  z01*dnzi_inf( 1,isf) + z02*dnzi_inf( 2,isf)   &
     &            + z03*dnzi_inf( 3,isf) + z04*dnzi_inf( 4,isf)   &
     &            + z05*dnzi_inf( 5,isf) + z06*dnzi_inf( 6,isf)   &
     &            + z07*dnzi_inf( 7,isf) + z08*dnzi_inf( 8,isf)   &
     &            + z09*dnzi_inf( 9,isf) + z10*dnzi_inf(10,isf)   &
     &            + z11*dnzi_inf(11,isf) + z12*dnzi_inf(12,isf)   &
     &            + z13*dnzi_inf(13,isf) + z14*dnzi_inf(14,isf)   &
     &            + z15*dnzi_inf(15,isf) + z16*dnzi_inf(16,isf)   &
     &            + z17*dnzi_inf(17,isf) + z18*dnzi_inf(18,isf)   &
     &            + z19*dnzi_inf(19,isf) + z20*dnzi_inf(20,isf)
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
          dnx(iele, 9)= (xj11*dnxi( 9) + xj12*dnei( 9) + xj13*dnzi( 9)) &
     &                 * axjac(iele)
          dnx(iele,10)= (xj11*dnxi(10) + xj12*dnei(10) + xj13*dnzi(10)) &
     &                 * axjac(iele)
          dnx(iele,11)= (xj11*dnxi(11) + xj12*dnei(11) + xj13*dnzi(11)) &
     &                 * axjac(iele)
          dnx(iele,12)= (xj11*dnxi(12) + xj12*dnei(12) + xj13*dnzi(12)) &
     &                 * axjac(iele)
          dnx(iele,13)= (xj11*dnxi(13) + xj12*dnei(13) + xj13*dnzi(13)) &
     &                 * axjac(iele)
          dnx(iele,14)= (xj11*dnxi(14) + xj12*dnei(14) + xj13*dnzi(14)) &
     &                 * axjac(iele)
          dnx(iele,15)= (xj11*dnxi(15) + xj12*dnei(15) + xj13*dnzi(15)) &
     &                 * axjac(iele)
          dnx(iele,16)= (xj11*dnxi(16) + xj12*dnei(16) + xj13*dnzi(16)) &
     &                 * axjac(iele)
          dnx(iele,17)= (xj11*dnxi(17) + xj12*dnei(17) + xj13*dnzi(17)) &
     &                 * axjac(iele)
          dnx(iele,18)= (xj11*dnxi(18) + xj12*dnei(18) + xj13*dnzi(18)) &
     &                 * axjac(iele)
          dnx(iele,19)= (xj11*dnxi(19) + xj12*dnei(19) + xj13*dnzi(19)) &
     &                 * axjac(iele)
          dnx(iele,20)= (xj11*dnxi(20) + xj12*dnei(20) + xj13*dnzi(20)) &
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
          dny(iele, 9)= (xj21*dnxi( 9) + xj22*dnei( 9) + xj23*dnzi( 9)) &
     &                 * axjac(iele)
          dny(iele,10)= (xj21*dnxi(10) + xj22*dnei(10) + xj23*dnzi(10)) &
     &                 * axjac(iele)
          dny(iele,11)= (xj21*dnxi(11) + xj22*dnei(11) + xj23*dnzi(11)) &
     &                 * axjac(iele)
          dny(iele,12)= (xj21*dnxi(12) + xj22*dnei(12) + xj23*dnzi(12)) &
     &                 * axjac(iele)
          dny(iele,13)= (xj21*dnxi(13) + xj22*dnei(13) + xj23*dnzi(13)) &
     &                 * axjac(iele)
          dny(iele,14)= (xj21*dnxi(14) + xj22*dnei(14) + xj23*dnzi(14)) &
     &                 * axjac(iele)
          dny(iele,15)= (xj21*dnxi(15) + xj22*dnei(15) + xj23*dnzi(15)) &
     &                 * axjac(iele)
          dny(iele,16)= (xj21*dnxi(16) + xj22*dnei(16) + xj23*dnzi(16)) &
     &                * axjac(iele)
          dny(iele,17)= (xj21*dnxi(17) + xj22*dnei(17) + xj23*dnzi(17)) &
     &                 * axjac(iele)
          dny(iele,18)= (xj21*dnxi(18) + xj22*dnei(18) + xj23*dnzi(18)) &
     &                 * axjac(iele)
          dny(iele,19)= (xj21*dnxi(19) + xj22*dnei(19) + xj23*dnzi(19)) &
     &                 * axjac(iele)
          dny(iele,20)= (xj21*dnxi(20) + xj22*dnei(20) + xj23*dnzi(20)) &
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
          dnz(iele, 9)= (xj31*dnxi( 9) + xj32*dnei( 9) + xj33*dnzi( 9)) &
     &                 * axjac(iele)
          dnz(iele,10)= (xj31*dnxi(10) + xj32*dnei(10) + xj33*dnzi(10)) &
     &                 * axjac(iele)
          dnz(iele,11)= (xj31*dnxi(11) + xj32*dnei(11) + xj33*dnzi(11)) &
     &                 * axjac(iele)
          dnz(iele,12)= (xj31*dnxi(12) + xj32*dnei(12) + xj33*dnzi(12)) &
     &                 * axjac(iele)
          dnz(iele,13)= (xj31*dnxi(13) + xj32*dnei(13) + xj33*dnzi(13)) &
     &                 * axjac(iele)
          dnz(iele,14)= (xj31*dnxi(14) + xj32*dnei(14) + xj33*dnzi(14)) &
     &                 * axjac(iele)
          dnz(iele,15)= (xj31*dnxi(15) + xj32*dnei(15) + xj33*dnzi(15)) &
     &                 * axjac(iele)
          dnz(iele,16)= (xj31*dnxi(16) + xj32*dnei(16) + xj33*dnzi(16)) &
     &                * axjac(iele)
          dnz(iele,17)= (xj31*dnxi(17) + xj32*dnei(17) + xj33*dnzi(17)) &
     &                 * axjac(iele)
          dnz(iele,18)= (xj31*dnxi(18) + xj32*dnei(18) + xj33*dnzi(18)) &
     &                 * axjac(iele)
          dnz(iele,19)= (xj31*dnxi(19) + xj32*dnei(19) + xj33*dnzi(19)) &
     &                 * axjac(iele)
          dnz(iele,20)= (xj31*dnxi(20) + xj32*dnei(20) + xj33*dnzi(20)) &
     &                 * axjac(iele)
!
        end do
      end do
!$omp end parallel do
!
      end do
!
      end subroutine s_cal_jacobian_3d_inf_8_20
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_3d_inf_l_quad
