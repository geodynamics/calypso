!
!      module cal_jacobian_3d_inf_lag
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine s_cal_jacobian_3d_inf_27(numnod, numele,              &
!     &          np_smp, ie, xx, num_surf_bc, surf_item,                &
!     &          ngrp_sf_infty, id_grp_sf_infty, num_surf_smp,          &
!     &          isurf_grp_smp_stack, xjac, axjac, dnx, dny, dnz,       &
!     &          dxidx, deidx, dzidx, dxidy, deidy, dzidy,              &
!     &          dxidz, deidz, dzidz, dnxi, dnei, dnzi,                 &
!     &          dnxi_inf, dnei_inf, dnzi_inf)
!
      module cal_jacobian_3d_inf_lag
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
      subroutine s_cal_jacobian_3d_inf_27(numnod, numele,               &
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
      integer(kind = kint), intent(in) :: ie(numele, num_t_lag)
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
      real(kind = kreal), intent(in) :: dnxi(num_t_lag)
      real(kind = kreal), intent(in) :: dnei(num_t_lag)
      real(kind = kreal), intent(in) :: dnzi(num_t_lag)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: dnxi_inf(num_t_lag,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnei_inf(num_t_lag,nsurf_4_ele)
      real(kind = kreal), intent(in)                                    &
     &                   :: dnzi_inf(num_t_lag,nsurf_4_ele)
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
      real(kind = kreal), intent(inout) :: dnx(numele,num_t_lag)
      real(kind = kreal), intent(inout) :: dny(numele,num_t_lag)
      real(kind = kreal), intent(inout) :: dnz(numele,num_t_lag)
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
      integer(kind = kint) :: i9,  i10, i11, i12, i13, i14, i15, i16
      integer(kind = kint) :: i17, i18, i19, i20, i21, i22, i23, i24
      integer(kind = kint) :: i25, i26, i27
!
!
      do i = 1, ngrp_sf_infty
        igrp = id_grp_sf_infty(i)
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,id_sf,inum,iele,isf,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,    &
!$omp&   i11,i12,i13,i14,i15,i16,i17,i18,i19,i20,i21,i22,i23,i24,       &
!$omp&   i25,i26,i27,dxxi,dxei,dxzi,dyxi,dyei,dyzi,dzxi,dzei,dzzi,      &
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
            i9 =  ie(iele, 9)
            i10 = ie(iele,10)
            i11 = ie(iele,11)
            i12 = ie(iele,12)
            i13 = ie(iele,13)
            i14 = ie(iele,14)
            i15 = ie(iele,15)
            i16 = ie(iele,16)
            i17 = ie(iele,17)
            i18 = ie(iele,18)
            i19 = ie(iele,19)
            i20 = ie(iele,20)
            i20 = ie(iele,20)
            i21 = ie(iele,21)
            i22 = ie(iele,22)
            i23 = ie(iele,23)
            i24 = ie(iele,24)
            i25 = ie(iele,25)
            i26 = ie(iele,26)
            i27 = ie(iele,27)
!
            dxxi =  xx(i1, 1)*dnxi_inf( 1,isf)                          &
     &            + xx(i2, 1)*dnxi_inf( 2,isf)                          &
     &            + xx(i3, 1)*dnxi_inf( 3,isf)                          &
     &            + xx(i4, 1)*dnxi_inf( 4,isf)                          &
     &            + xx(i5, 1)*dnxi_inf( 5,isf)                          &
     &            + xx(i6, 1)*dnxi_inf( 6,isf)                          &
     &            + xx(i7, 1)*dnxi_inf( 7,isf)                          &
     &            + xx(i8, 1)*dnxi_inf( 8,isf)                          &
     &            + xx(i9, 1)*dnxi_inf( 9,isf)                          &
     &            + xx(i10,1)*dnxi_inf(10,isf)                          &
     &            + xx(i11,1)*dnxi_inf(11,isf)                          &
     &            + xx(i12,1)*dnxi_inf(12,isf)                          &
     &            + xx(i13,1)*dnxi_inf(13,isf)                          &
     &            + xx(i14,1)*dnxi_inf(14,isf)                          &
     &            + xx(i15,1)*dnxi_inf(15,isf)                          &
     &            + xx(i16,1)*dnxi_inf(16,isf)                          &
     &            + xx(i17,1)*dnxi_inf(17,isf)                          &
     &            + xx(i18,1)*dnxi_inf(18,isf)                          &
     &            + xx(i19,1)*dnxi_inf(19,isf)                          &
     &            + xx(i20,1)*dnxi_inf(20,isf)                          &
     &            + xx(i21,1)*dnxi_inf(21,isf)                          &
     &            + xx(i22,1)*dnxi_inf(22,isf)                          &
     &            + xx(i23,1)*dnxi_inf(23,isf)                          &
     &            + xx(i24,1)*dnxi_inf(24,isf)                          &
     &            + xx(i25,1)*dnxi_inf(25,isf)                          &
     &            + xx(i26,1)*dnxi_inf(26,isf)                          &
     &            + xx(i27,1)*dnxi_inf(27,isf)
!
            dxei =  xx(i1, 1)*dnei_inf( 1,isf)                          &
     &            + xx(i2, 1)*dnei_inf( 2,isf)                          &
     &            + xx(i3, 1)*dnei_inf( 3,isf)                          &
     &            + xx(i4, 1)*dnei_inf( 4,isf)                          &
     &            + xx(i5, 1)*dnei_inf( 5,isf)                          &
     &            + xx(i6, 1)*dnei_inf( 6,isf)                          &
     &            + xx(i7, 1)*dnei_inf( 7,isf)                          &
     &            + xx(i8, 1)*dnei_inf( 8,isf)                          &
     &            + xx(i9, 1)*dnei_inf( 9,isf)                          &
     &            + xx(i10,1)*dnei_inf(10,isf)                          &
     &            + xx(i11,1)*dnei_inf(11,isf)                          &
     &            + xx(i12,1)*dnei_inf(12,isf)                          &
     &            + xx(i13,1)*dnei_inf(13,isf)                          &
     &            + xx(i14,1)*dnei_inf(14,isf)                          &
     &            + xx(i15,1)*dnei_inf(15,isf)                          &
     &            + xx(i16,1)*dnei_inf(16,isf)                          &
     &            + xx(i17,1)*dnei_inf(17,isf)                          &
     &            + xx(i18,1)*dnei_inf(18,isf)                          &
     &            + xx(i19,1)*dnei_inf(19,isf)                          &
     &            + xx(i20,1)*dnei_inf(20,isf)                          &
     &            + xx(i21,1)*dnei_inf(21,isf)                          &
     &            + xx(i22,1)*dnei_inf(22,isf)                          &
     &            + xx(i23,1)*dnei_inf(23,isf)                          &
     &            + xx(i24,1)*dnei_inf(24,isf)                          &
     &            + xx(i25,1)*dnei_inf(25,isf)                          &
     &            + xx(i26,1)*dnei_inf(26,isf)                          &
     &            + xx(i27,1)*dnei_inf(27,isf)
!
            dxzi =  xx(i1, 1)*dnzi_inf( 1,isf)                          &
     &            + xx(i2, 1)*dnzi_inf( 2,isf)                          &
     &            + xx(i3, 1)*dnzi_inf( 3,isf)                          &
     &            + xx(i4, 1)*dnzi_inf( 4,isf)                          &
     &            + xx(i5, 1)*dnzi_inf( 5,isf)                          &
     &            + xx(i6, 1)*dnzi_inf( 6,isf)                          &
     &            + xx(i7, 1)*dnzi_inf( 7,isf)                          &
     &            + xx(i8, 1)*dnzi_inf( 8,isf)                          &
     &            + xx(i9, 1)*dnzi_inf( 9,isf)                          &
     &            + xx(i10,1)*dnzi_inf(10,isf)                          &
     &            + xx(i11,1)*dnzi_inf(11,isf)                          &
     &            + xx(i12,1)*dnzi_inf(12,isf)                          &
     &            + xx(i13,1)*dnzi_inf(13,isf)                          &
     &            + xx(i14,1)*dnzi_inf(14,isf)                          &
     &            + xx(i15,1)*dnzi_inf(15,isf)                          &
     &            + xx(i16,1)*dnzi_inf(16,isf)                          &
     &            + xx(i17,1)*dnzi_inf(17,isf)                          &
     &            + xx(i18,1)*dnzi_inf(18,isf)                          &
     &            + xx(i19,1)*dnzi_inf(19,isf)                          &
     &            + xx(i20,1)*dnzi_inf(20,isf)                          &
     &            + xx(i21,1)*dnzi_inf(21,isf)                          &
     &            + xx(i22,1)*dnzi_inf(22,isf)                          &
     &            + xx(i23,1)*dnzi_inf(23,isf)                          &
     &            + xx(i24,1)*dnzi_inf(24,isf)                          &
     &            + xx(i25,1)*dnzi_inf(25,isf)                          &
     &            + xx(i26,1)*dnzi_inf(26,isf)                          &
     &            + xx(i27,1)*dnzi_inf(27,isf)
!
!
            dyxi =  xx(i1, 2)*dnxi_inf( 1,isf)                          &
     &            + xx(i2, 2)*dnxi_inf( 2,isf)                          &
     &            + xx(i3, 2)*dnxi_inf( 3,isf)                          &
     &            + xx(i4, 2)*dnxi_inf( 4,isf)                          &
     &            + xx(i5, 2)*dnxi_inf( 5,isf)                          &
     &            + xx(i6, 2)*dnxi_inf( 6,isf)                          &
     &            + xx(i7, 2)*dnxi_inf( 7,isf)                          &
     &            + xx(i8, 2)*dnxi_inf( 8,isf)                          &
     &            + xx(i9, 2)*dnxi_inf( 9,isf)                          &
     &            + xx(i10,2)*dnxi_inf(10,isf)                          &
     &            + xx(i11,2)*dnxi_inf(11,isf)                          &
     &            + xx(i12,2)*dnxi_inf(12,isf)                          &
     &            + xx(i13,2)*dnxi_inf(13,isf)                          &
     &            + xx(i14,2)*dnxi_inf(14,isf)                          &
     &            + xx(i15,2)*dnxi_inf(15,isf)                          &
     &            + xx(i16,2)*dnxi_inf(16,isf)                          &
     &            + xx(i17,2)*dnxi_inf(17,isf)                          &
     &            + xx(i18,2)*dnxi_inf(18,isf)                          &
     &            + xx(i19,2)*dnxi_inf(19,isf)                          &
     &            + xx(i20,2)*dnxi_inf(20,isf)                          &
     &            + xx(i21,2)*dnxi_inf(21,isf)                          &
     &            + xx(i22,2)*dnxi_inf(22,isf)                          &
     &            + xx(i23,2)*dnxi_inf(23,isf)                          &
     &            + xx(i24,2)*dnxi_inf(24,isf)                          &
     &            + xx(i25,2)*dnxi_inf(25,isf)                          &
     &            + xx(i26,2)*dnxi_inf(26,isf)                          &
     &            + xx(i27,2)*dnxi_inf(27,isf)
!
            dyei =  xx(i1, 2)*dnei_inf( 1,isf)                          &
     &            + xx(i2, 2)*dnei_inf( 2,isf)                          &
     &            + xx(i3, 2)*dnei_inf( 3,isf)                          &
     &            + xx(i4, 2)*dnei_inf( 4,isf)                          &
     &            + xx(i5, 2)*dnei_inf( 5,isf)                          &
     &            + xx(i6, 2)*dnei_inf( 6,isf)                          &
     &            + xx(i7, 2)*dnei_inf( 7,isf)                          &
     &            + xx(i8, 2)*dnei_inf( 8,isf)                          &
     &            + xx(i9, 2)*dnei_inf( 9,isf)                          &
     &            + xx(i10,2)*dnei_inf(10,isf)                          &
     &            + xx(i11,2)*dnei_inf(11,isf)                          &
     &            + xx(i12,2)*dnei_inf(12,isf)                          &
     &            + xx(i13,2)*dnei_inf(13,isf)                          &
     &            + xx(i14,2)*dnei_inf(14,isf)                          &
     &            + xx(i15,2)*dnei_inf(15,isf)                          &
     &            + xx(i16,2)*dnei_inf(16,isf)                          &
     &            + xx(i17,2)*dnei_inf(17,isf)                          &
     &            + xx(i18,2)*dnei_inf(18,isf)                          &
     &            + xx(i19,2)*dnei_inf(19,isf)                          &
     &            + xx(i20,2)*dnei_inf(20,isf)                          &
     &            + xx(i21,2)*dnei_inf(21,isf)                          &
     &            + xx(i22,2)*dnei_inf(22,isf)                          &
     &            + xx(i23,2)*dnei_inf(23,isf)                          &
     &            + xx(i24,2)*dnei_inf(24,isf)                          &
     &            + xx(i25,2)*dnei_inf(25,isf)                          &
     &            + xx(i26,2)*dnei_inf(26,isf)                          &
     &            + xx(i27,2)*dnei_inf(27,isf)
!
            dyzi =  xx(i1, 2)*dnzi_inf( 1,isf)                          &
     &            + xx(i2, 2)*dnzi_inf( 2,isf)                          &
     &            + xx(i3, 2)*dnzi_inf( 3,isf)                          &
     &            + xx(i4, 2)*dnzi_inf( 4,isf)                          &
     &            + xx(i5, 2)*dnzi_inf( 5,isf)                          &
     &            + xx(i6, 2)*dnzi_inf( 6,isf)                          &
     &            + xx(i7, 2)*dnzi_inf( 7,isf)                          &
     &            + xx(i8, 2)*dnzi_inf( 8,isf)                          &
     &            + xx(i9, 2)*dnzi_inf( 9,isf)                          &
     &            + xx(i10,2)*dnzi_inf(10,isf)                          &
     &            + xx(i11,2)*dnzi_inf(11,isf)                          &
     &            + xx(i12,2)*dnzi_inf(12,isf)                          &
     &            + xx(i13,2)*dnzi_inf(13,isf)                          &
     &            + xx(i14,2)*dnzi_inf(14,isf)                          &
     &            + xx(i15,2)*dnzi_inf(15,isf)                          &
     &            + xx(i16,2)*dnzi_inf(16,isf)                          &
     &            + xx(i17,2)*dnzi_inf(17,isf)                          &
     &            + xx(i18,2)*dnzi_inf(18,isf)                          &
     &            + xx(i19,2)*dnzi_inf(19,isf)                          &
     &            + xx(i20,2)*dnzi_inf(20,isf)                          &
     &            + xx(i21,2)*dnzi_inf(21,isf)                          &
     &            + xx(i22,2)*dnzi_inf(22,isf)                          &
     &            + xx(i23,2)*dnzi_inf(23,isf)                          &
     &            + xx(i24,2)*dnzi_inf(24,isf)                          &
     &            + xx(i25,2)*dnzi_inf(25,isf)                          &
     &            + xx(i26,2)*dnzi_inf(26,isf)                          &
     &            + xx(i27,2)*dnzi_inf(27,isf)
!
!
            dzxi =  xx(i1, 3)*dnxi_inf( 1,isf)                          &
     &            + xx(i2, 3)*dnxi_inf( 2,isf)                          &
     &            + xx(i3, 3)*dnxi_inf( 3,isf)                          &
     &            + xx(i4, 3)*dnxi_inf( 4,isf)                          &
     &            + xx(i5, 3)*dnxi_inf( 5,isf)                          &
     &            + xx(i6, 3)*dnxi_inf( 6,isf)                          &
     &            + xx(i7, 3)*dnxi_inf( 7,isf)                          &
     &            + xx(i8, 3)*dnxi_inf( 8,isf)                          &
     &            + xx(i9, 3)*dnxi_inf( 9,isf)                          &
     &            + xx(i10,3)*dnxi_inf(10,isf)                          &
     &            + xx(i11,3)*dnxi_inf(11,isf)                          &
     &            + xx(i12,3)*dnxi_inf(12,isf)                          &
     &            + xx(i13,3)*dnxi_inf(13,isf)                          &
     &            + xx(i14,3)*dnxi_inf(14,isf)                          &
     &            + xx(i15,3)*dnxi_inf(15,isf)                          &
     &            + xx(i16,3)*dnxi_inf(16,isf)                          &
     &            + xx(i17,3)*dnxi_inf(17,isf)                          &
     &            + xx(i18,3)*dnxi_inf(18,isf)                          &
     &            + xx(i19,3)*dnxi_inf(19,isf)                          &
     &            + xx(i20,3)*dnxi_inf(20,isf)                          &
     &            + xx(i21,3)*dnxi_inf(21,isf)                          &
     &            + xx(i22,3)*dnxi_inf(22,isf)                          &
     &            + xx(i23,3)*dnxi_inf(23,isf)                          &
     &            + xx(i24,3)*dnxi_inf(24,isf)                          &
     &            + xx(i25,3)*dnxi_inf(25,isf)                          &
     &            + xx(i26,3)*dnxi_inf(26,isf)                          &
     &            + xx(i27,3)*dnxi_inf(27,isf)
!
            dzei =  xx(i1, 3)*dnei_inf( 1,isf)                          &
     &            + xx(i2, 3)*dnei_inf( 2,isf)                          &
     &            + xx(i3, 3)*dnei_inf( 3,isf)                          &
     &            + xx(i4, 3)*dnei_inf( 4,isf)                          &
     &            + xx(i5, 3)*dnei_inf( 5,isf)                          &
     &            + xx(i6, 3)*dnei_inf( 6,isf)                          &
     &            + xx(i7, 3)*dnei_inf( 7,isf)                          &
     &            + xx(i8, 3)*dnei_inf( 8,isf)                          &
     &            + xx(i9, 3)*dnei_inf( 9,isf)                          &
     &            + xx(i10,3)*dnei_inf(10,isf)                          &
     &            + xx(i11,3)*dnei_inf(11,isf)                          &
     &            + xx(i12,3)*dnei_inf(12,isf)                          &
     &            + xx(i13,3)*dnei_inf(13,isf)                          &
     &            + xx(i14,3)*dnei_inf(14,isf)                          &
     &            + xx(i15,3)*dnei_inf(15,isf)                          &
     &            + xx(i16,3)*dnei_inf(16,isf)                          &
     &            + xx(i17,3)*dnei_inf(17,isf)                          &
     &            + xx(i18,3)*dnei_inf(18,isf)                          &
     &            + xx(i19,3)*dnei_inf(19,isf)                          &
     &            + xx(i20,3)*dnei_inf(20,isf)                          &
     &            + xx(i21,3)*dnei_inf(21,isf)                          &
     &            + xx(i22,3)*dnei_inf(22,isf)                          &
     &            + xx(i23,3)*dnei_inf(23,isf)                          &
     &            + xx(i24,3)*dnei_inf(24,isf)                          &
     &            + xx(i25,3)*dnei_inf(25,isf)                          &
     &            + xx(i26,3)*dnei_inf(26,isf)                          &
     &            + xx(i27,3)*dnei_inf(27,isf)
!
            dzzi =  xx(i1, 3)*dnzi_inf( 1,isf)                          &
     &            + xx(i2, 3)*dnzi_inf( 2,isf)                          &
     &            + xx(i3, 3)*dnzi_inf( 3,isf)                          &
     &            + xx(i4, 3)*dnzi_inf( 4,isf)                          &
     &            + xx(i5, 3)*dnzi_inf( 5,isf)                          &
     &            + xx(i6, 3)*dnzi_inf( 6,isf)                          &
     &            + xx(i7, 3)*dnzi_inf( 7,isf)                          &
     &            + xx(i8, 3)*dnzi_inf( 8,isf)                          &
     &            + xx(i9, 3)*dnzi_inf( 9,isf)                          &
     &            + xx(i10,3)*dnzi_inf(10,isf)                          &
     &            + xx(i11,3)*dnzi_inf(11,isf)                          &
     &            + xx(i12,3)*dnzi_inf(12,isf)                          &
     &            + xx(i13,3)*dnzi_inf(13,isf)                          &
     &            + xx(i14,3)*dnzi_inf(14,isf)                          &
     &            + xx(i15,3)*dnzi_inf(15,isf)                          &
     &            + xx(i16,3)*dnzi_inf(16,isf)                          &
     &            + xx(i17,3)*dnzi_inf(17,isf)                          &
     &            + xx(i18,3)*dnzi_inf(18,isf)                          &
     &            + xx(i19,3)*dnzi_inf(19,isf)                          &
     &            + xx(i20,3)*dnzi_inf(20,isf)                          &
     &            + xx(i21,3)*dnzi_inf(21,isf)                          &
     &            + xx(i22,3)*dnzi_inf(22,isf)                          &
     &            + xx(i23,3)*dnzi_inf(23,isf)                          &
     &            + xx(i24,3)*dnzi_inf(24,isf)                          &
     &            + xx(i25,3)*dnzi_inf(25,isf)                          &
     &            + xx(i26,3)*dnzi_inf(26,isf)                          &
     &            + xx(i27,3)*dnzi_inf(27,isf)
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
          dnx(iele,21)= (xj11*dnxi(21) + xj12*dnei(21) + xj13*dnzi(21)) &
     &                 * axjac(iele)
          dnx(iele,22)= (xj11*dnxi(22) + xj12*dnei(22) + xj13*dnzi(22)) &
     &                 * axjac(iele)
          dnx(iele,23)= (xj11*dnxi(23) + xj12*dnei(23) + xj13*dnzi(23)) &
     &                 * axjac(iele)
          dnx(iele,24)= (xj11*dnxi(24) + xj12*dnei(24) + xj13*dnzi(24)) &
     &                 * axjac(iele)
          dnx(iele,25)= (xj11*dnxi(25) + xj12*dnei(25) + xj13*dnzi(25)) &
     &                 * axjac(iele)
          dnx(iele,26)= (xj11*dnxi(26) + xj12*dnei(26) + xj13*dnzi(26)) &
     &                 * axjac(iele)
          dnx(iele,27)= (xj11*dnxi(27) + xj12*dnei(27) + xj13*dnzi(27)) &
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
          dny(iele,21)= (xj21*dnxi(21) + xj22*dnei(21) + xj23*dnzi(21)) &
     &                 * axjac(iele)
          dny(iele,22)= (xj21*dnxi(22) + xj22*dnei(22) + xj23*dnzi(22)) &
     &                 * axjac(iele)
          dny(iele,23)= (xj21*dnxi(23) + xj22*dnei(23) + xj23*dnzi(23)) &
     &                 * axjac(iele)
          dny(iele,24)= (xj21*dnxi(24) + xj22*dnei(24) + xj23*dnzi(24)) &
     &                 * axjac(iele)
          dny(iele,25)= (xj21*dnxi(25) + xj22*dnei(25) + xj23*dnzi(25)) &
     &                 * axjac(iele)
          dny(iele,26)= (xj21*dnxi(26) + xj22*dnei(26) + xj23*dnzi(26)) &
     &                * axjac(iele)
          dny(iele,27)= (xj21*dnxi(27) + xj22*dnei(27) + xj23*dnzi(27)) &
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
          dnz(iele,21)= (xj31*dnxi(21) + xj32*dnei(21) + xj33*dnzi(21)) &
     &                 * axjac(iele)
          dnz(iele,22)= (xj31*dnxi(22) + xj32*dnei(22) + xj33*dnzi(22)) &
     &                 * axjac(iele)
          dnz(iele,23)= (xj31*dnxi(23) + xj32*dnei(23) + xj33*dnzi(23)) &
     &                 * axjac(iele)
          dnz(iele,24)= (xj31*dnxi(24) + xj32*dnei(24) + xj33*dnzi(24)) &
     &                 * axjac(iele)
          dnz(iele,25)= (xj31*dnxi(25) + xj32*dnei(25) + xj33*dnzi(25)) &
     &                 * axjac(iele)
          dnz(iele,26)= (xj31*dnxi(26) + xj32*dnei(26) + xj33*dnzi(26)) &
     &                * axjac(iele)
          dnz(iele,27)= (xj31*dnxi(27) + xj32*dnei(27) + xj33*dnzi(27)) &
     &                 * axjac(iele)
!
        end do
      end do
!$omp end parallel do
!
      end do
!
      end subroutine s_cal_jacobian_3d_inf_27
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_3d_inf_lag
