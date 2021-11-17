!
!      module cal_jacobian_3d_inf_linear
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine s_cal_jacobian_3d_inf_8(numnod, numele,               &
!     &          np_smp, ie, xx, num_surf_bc, surf_item,                &
!     &          ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,          &
!     &          isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,       &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi,                 &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      module cal_jacobian_3d_inf_linear
!
      use m_precision
!
      use m_geometry_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_jacobian_3d_inf_8(numnod, numele,                &
     &          np_smp, ie, xx, num_surf_bc, surf_item,                 &
     &          ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,           &
     &          isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,        &
     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,               &
     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi,                  &
     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      use m_geometry_constants
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
      real(kind = kreal), intent(in) :: dnxi(num_t_linear)
      real(kind = kreal), intent(in) :: dnei(num_t_linear)
      real(kind = kreal), intent(in) :: dnzi(num_t_linear)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_linear,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_linear,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_linear,nsurf_4_ele)
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
      integer(kind = kint) :: i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
!
!
      do i = 1, ngrp_sf_infty
       igrp = id_grp_sf_infty(i)
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,id_sf,inum,iele,isf,i1,i2,i3,i4,i5,i6,i7,i8,           &
!$omp&   dxxi,dxei,dxzi,dyxi,dyei,dyzi,dzxi,dzei,dzzi,                  &
!$omp&   xj11,xj12,xj13,xj21,xj22,xj23,xj31,xj32,xj33)
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
            dxxi =  xx(i1, 1)*dnxi_inf( 1,isf)                          &
     &            + xx(i2, 1)*dnxi_inf( 2,isf)                          &
     &            + xx(i3, 1)*dnxi_inf( 3,isf)                          &
     &            + xx(i4, 1)*dnxi_inf( 4,isf)                          &
     &            + xx(i5, 1)*dnxi_inf( 5,isf)                          &
     &            + xx(i6, 1)*dnxi_inf( 6,isf)                          &
     &            + xx(i7, 1)*dnxi_inf( 7,isf)                          &
     &            + xx(i8, 1)*dnxi_inf( 8,isf)
!
            dxei =  xx(i1, 1)*dnei_inf( 1,isf)                          &
     &            + xx(i2, 1)*dnei_inf( 2,isf)                          &
     &            + xx(i3, 1)*dnei_inf( 3,isf)                          &
     &            + xx(i4, 1)*dnei_inf( 4,isf)                          &
     &            + xx(i5, 1)*dnei_inf( 5,isf)                          &
     &            + xx(i6, 1)*dnei_inf( 6,isf)                          &
     &            + xx(i7, 1)*dnei_inf( 7,isf)                          &
     &            + xx(i8, 1)*dnei_inf( 8,isf)
!
            dxzi =  xx(i1, 1)*dnzi_inf( 1,isf)                          &
     &            + xx(i2, 1)*dnzi_inf( 2,isf)                          &
     &            + xx(i3, 1)*dnzi_inf( 3,isf)                          &
     &            + xx(i4, 1)*dnzi_inf( 4,isf)                          &
     &            + xx(i5, 1)*dnzi_inf( 5,isf)                          &
     &            + xx(i6, 1)*dnzi_inf( 6,isf)                          &
     &            + xx(i7, 1)*dnzi_inf( 7,isf)                          &
     &            + xx(i8, 1)*dnzi_inf( 8,isf)
!
!
            dyxi =  xx(i1, 2)*dnxi_inf( 1,isf)                          &
     &            + xx(i2, 2)*dnxi_inf( 2,isf)                          &
     &            + xx(i3, 2)*dnxi_inf( 3,isf)                          &
     &            + xx(i4, 2)*dnxi_inf( 4,isf)                          &
     &            + xx(i5, 2)*dnxi_inf( 5,isf)                          &
     &            + xx(i6, 2)*dnxi_inf( 6,isf)                          &
     &            + xx(i7, 2)*dnxi_inf( 7,isf)                          &
     &            + xx(i8, 2)*dnxi_inf( 8,isf)
!
            dyei =  xx(i1, 2)*dnei_inf( 1,isf)                          &
     &            + xx(i2, 2)*dnei_inf( 2,isf)                          &
     &            + xx(i3, 2)*dnei_inf( 3,isf)                          &
     &            + xx(i4, 2)*dnei_inf( 4,isf)                          &
     &            + xx(i5, 2)*dnei_inf( 5,isf)                          &
     &            + xx(i6, 2)*dnei_inf( 6,isf)                          &
     &            + xx(i7, 2)*dnei_inf( 7,isf)                          &
     &            + xx(i8, 2)*dnei_inf( 8,isf)
!
            dyzi =  xx(i1, 2)*dnzi_inf( 1,isf)                          &
     &            + xx(i2, 2)*dnzi_inf( 2,isf)                          &
     &            + xx(i3, 2)*dnzi_inf( 3,isf)                          &
     &            + xx(i4, 2)*dnzi_inf( 4,isf)                          &
     &            + xx(i5, 2)*dnzi_inf( 5,isf)                          &
     &            + xx(i6, 2)*dnzi_inf( 6,isf)                          &
     &            + xx(i7, 2)*dnzi_inf( 7,isf)                          &
     &            + xx(i8, 2)*dnzi_inf( 8,isf)
!
!
            dzxi =  xx(i1, 3)*dnxi_inf( 1,isf)                          &
     &            + xx(i2, 3)*dnxi_inf( 2,isf)                          &
     &            + xx(i3, 3)*dnxi_inf( 3,isf)                          &
     &            + xx(i4, 3)*dnxi_inf( 4,isf)                          &
     &            + xx(i5, 3)*dnxi_inf( 5,isf)                          &
     &            + xx(i6, 3)*dnxi_inf( 6,isf)                          &
     &            + xx(i7, 3)*dnxi_inf( 7,isf)                          &
     &            + xx(i8, 3)*dnxi_inf( 8,isf)
!
            dzei =  xx(i1, 3)*dnei_inf( 1,isf)                          &
     &            + xx(i2, 3)*dnei_inf( 2,isf)                          &
     &            + xx(i3, 3)*dnei_inf( 3,isf)                          &
     &            + xx(i4, 3)*dnei_inf( 4,isf)                          &
     &            + xx(i5, 3)*dnei_inf( 5,isf)                          &
     &            + xx(i6, 3)*dnei_inf( 6,isf)                          &
     &            + xx(i7, 3)*dnei_inf( 7,isf)                          &
     &            + xx(i8, 3)*dnei_inf( 8,isf)
!
            dzzi =  xx(i1, 3)*dnzi_inf( 1,isf)                          &
     &            + xx(i2, 3)*dnzi_inf( 2,isf)                          &
     &            + xx(i3, 3)*dnzi_inf( 3,isf)                          &
     &            + xx(i4, 3)*dnzi_inf( 4,isf)                          &
     &            + xx(i5, 3)*dnzi_inf( 5,isf)                          &
     &            + xx(i6, 3)*dnzi_inf( 6,isf)                          &
     &            + xx(i7, 3)*dnzi_inf( 7,isf)                          &
     &            + xx(i8, 3)*dnzi_inf( 8,isf)
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
      end do
!
      end subroutine s_cal_jacobian_3d_inf_8
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_3d_inf_linear
