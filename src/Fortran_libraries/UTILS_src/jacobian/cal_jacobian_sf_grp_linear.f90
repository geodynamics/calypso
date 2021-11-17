!
!      module cal_jacobian_sf_grp_linear
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by H. Matsui on June. 2006
!
!      subroutine s_cal_jacobian_sf_grp_4(numnod, numele,               &
!     &          ie, xx, num_surf, num_surf_bc, surf_item,              &
!     &          np_smp, num_surf_smp, isurf_grp_smp_stack,             &
!     &          xjac, axjac,  xsf, ysf, zsf, dnxi, dnei)
!
      module cal_jacobian_sf_grp_linear
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
      subroutine s_cal_jacobian_sf_grp_4(numnod, numele,                &
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
      real(kind = kreal), intent(in) :: dnxi(num_linear_sf)
      real(kind = kreal), intent(in) :: dnei(num_linear_sf)
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
      integer(kind = kint) :: i1,  i2,  i3,  i4
      integer(kind = kint) :: il1, il2, il3, il4
!
!
!$omp parallel do private                                               &
!$omp&  (ist,ied,igrp,jp,inum,iele,isf,                                 &
!$omp&   i1,i2,i3,i4,il1,il2,il3,il4,                                   &
!$omp&   dxxi,dxei,dyxi,dyei,dzxi,dzei)
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
            dxxi =  xx(i1, 1)*dnxi( 1) + xx(i2, 1)*dnxi( 2)             &
     &            + xx(i3, 1)*dnxi( 3) + xx(i4, 1)*dnxi( 4)
!
            dxei =  xx(i1, 1)*dnei( 1) + xx(i2, 1)*dnei( 2)             &
     &            + xx(i3, 1)*dnei( 3) + xx(i4, 1)*dnei( 4)
!
!
            dyxi =  xx(i1, 2)*dnxi( 1) + xx(i2, 2)*dnxi( 2)             &
     &            + xx(i3, 2)*dnxi( 3) + xx(i4, 2)*dnxi( 4)
!
            dyei =  xx(i1, 2)*dnei( 1) + xx(i2, 2)*dnei( 2)             &
     &            + xx(i3, 2)*dnei( 3) + xx(i4, 2)*dnei( 4)
!
!
            dzxi =  xx(i1, 3)*dnxi( 1) + xx(i2, 3)*dnxi( 2)             &
     &            + xx(i3, 3)*dnxi( 3) + xx(i4, 3)*dnxi( 4)
!
            dzei =  xx(i1, 3)*dnei( 1) + xx(i2, 3)*dnei( 2)             &
     &            + xx(i3, 3)*dnei( 3) + xx(i4, 3)*dnei( 4)
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
      end subroutine s_cal_jacobian_sf_grp_4
!
!-----------------------------------------------------------------------
!
      end module cal_jacobian_sf_grp_linear
