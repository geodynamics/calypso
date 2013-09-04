!>@file   cvt_sph_asym_t_2_xyz_smp.f90
!!@brief  module cvt_sph_asym_t_2_xyz_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert anti-symmetric tensor from spherical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!      subroutine cal_xyz_asym_t_by_sph_smp(np_smp, numnod,            &
!!     &          inod_smp_stack, t_sph, txyz, xx, r, s, a_r, a_s)
!!
!!      subroutine overwrite_xyz_asym_t_by_sph_smp(np_smp, numnod,      &
!!     &          inod_smp_stack, tensor, xx, r, s, a_r, a_s)
!!
!!      subroutine cal_xy_asym_t_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_sph, v_xy, xx, r, s, a_r)
!!      subroutine cal_zx_asym_t_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_sph, v_zx, xx, r, s, a_r, a_s)
!!      subroutine cal_yz_asym_t_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, t_sph, v_yz, xx, r, s, a_r, a_s)
!!
!!   uxuy = (ar) *   (               -s *up*ur + z   *ut*up)
!!   uzux = (aras) * ( r*x *ur*ut + y*z *up*ur + s*y *ut*up)
!!   uyuz = (aras) * (-r*y *ur*ut + x*z *up*ur + s*x *ut*up)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  xx(numnod,3) position in Cartesian coordinate
!!@n @param  r(numnod)    radius
!!@n @param  s(numnod)    cylindrical radius
!!@n @param  a_r(numnod)  1 / r
!!@n @param  a_s(numnod)  1 / s
!!
!!@n @param  tensor(numnod,3)
!!                    anti-symmetric tensor
!!@n @param  t_sph(numnod,3)
!!                    anti-symmetric tensor in spherical coordinate
!!@n @param  tsph(numnod,3)
!!                    anti-symmetric tensor in Cartesian coordinate
!!@n @param  t_rt(numnod)
!!                    @f$ T_{r\theta} @f$ in Cartesian coordinate
!!@n @param  t_pr(numnod)
!!                    @f$ T_{\phi r} @f$  in Cartesian coordinate
!!@n @param  t_tp(numnod)
!!                     @f$ T_{\theta \phi} @f$ in Cartesian coordinate
!
      module cvt_sph_asym_t_2_xyz_smp
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_xyz_asym_t_by_sph_smp(np_smp, numnod,              &
     &          inod_smp_stack, t_sph, txyz, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_sph(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = t_sph(inod,1)
           tpr = t_sph(inod,2)
           ttp = t_sph(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             txyz(inod,2) =  trt
             txyz(inod,3) =  tpr
             txyz(inod,1) =  ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             txyz(inod,2) =  trt
             txyz(inod,3) =  tpr
             txyz(inod,1) =  ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             txyz(inod,2) =  trt
             txyz(inod,3) = -tpr
             txyz(inod,1) = -ttp
           else
             txyz(inod,1) = ( -tpr *  s(inod)                           &
     &                       + ttp * xx(inod,3) ) * a_r(inod)
!
             txyz(inod,2) = (  trt * xx(inod,1)* r(inod)                &
     &                       + tpr * xx(inod,2)*xx(inod,3)              &
     &                       + ttp * xx(inod,2)* s(inod) )              &
     &                     * a_r(inod) * a_s(inod)
!
             txyz(inod,3) = ( -trt * xx(inod,2)* r(inod)                &
     &                       + tpr * xx(inod,1)*xx(inod,3)              &
     &                       + ttp * xx(inod,1)* s(inod) )              &
     &                     * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_asym_t_by_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_xyz_asym_t_by_sph_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = tensor(inod,1)
           tpr = tensor(inod,2)
           ttp = tensor(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             tensor(inod,2) =  trt
             tensor(inod,3) =  tpr
             tensor(inod,1) =  ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             tensor(inod,2) =  trt
             tensor(inod,3) =  tpr
             tensor(inod,1) =  ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             tensor(inod,2) =  trt
             tensor(inod,3) = -tpr
             tensor(inod,1) = -ttp
           else
             tensor(inod,1) = ( -tpr *  s(inod)                         &
     &                         + ttp * xx(inod,3) ) * a_r(inod)
!
             tensor(inod,2) = (  trt * xx(inod,1)* r(inod)              &
     &                         + tpr * xx(inod,2)*xx(inod,3)            &
     &                         + ttp * xx(inod,2)* s(inod) )            &
     &                       * a_r(inod) * a_s(inod)
!
             tensor(inod,3) = ( -trt * xx(inod,2)* r(inod)              &
     &                         + tpr * xx(inod,1)*xx(inod,3)            &
     &                         + ttp * xx(inod,1)* s(inod) )            &
     &                       * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_xyz_asym_t_by_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_xy_asym_t_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, t_sph, v_xy, xx, r, s, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_sph(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: v_xy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = t_sph(inod,1)
           tpr = t_sph(inod,2)
           ttp = t_sph(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_xy(inod) =  ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             v_xy(inod) =  ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             v_xy(inod) = -ttp
           else
             v_xy(inod) =   ( -tpr *  s(inod)                           &
     &                       + ttp * xx(inod,3) ) * a_r(inod)
           end if
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xy_asym_t_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zx_asym_t_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, t_sph, v_zx, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_sph(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_zx(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = t_sph(inod,1)
           tpr = t_sph(inod,2)
           ttp = t_sph(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_zx(inod) =  trt
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             v_zx(inod) =  trt
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             v_zx(inod) =  trt
           else
             v_zx(inod) =   (  trt * xx(inod,1)* r(inod)                &
     &                       + tpr * xx(inod,2)*xx(inod,3)              &
     &                       + ttp * xx(inod,2)* s(inod) )              &
     &                     * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_zx_asym_t_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yz_asym_t_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, t_sph, v_yz, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: t_sph(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_yz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trt, tpr, ttp
!
!
!$omp parallel do private(inod,ist,ied,trt,tpr,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trt = t_sph(inod,1)
           tpr = t_sph(inod,2)
           ttp = t_sph(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_yz(inod) =  tpr
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             v_yz(inod) =  tpr
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             v_yz(inod) = -tpr
           else
             v_yz(inod) =   ( -trt * xx(inod,2)* r(inod)                &
     &                       + tpr * xx(inod,1)*xx(inod,3)              &
     &                       + ttp * xx(inod,1)* s(inod) )              &
     &                     * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_yz_asym_t_by_sph_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_sph_asym_t_2_xyz_smp
