!>@file   cvt_sph_tensor_2_xyz_smp.f90
!!        module cvt_sph_tensor_2_xyz_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!!
!>@brief Convert symmetric tensor from spherical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!      subroutine cal_xyz_tensor_by_sph_smp(np_smp, numnod,            &
!!     &          inod_smp_stack, tensor, txyz, xx, yy, zz, r, s,       &
!!     &          a_r, a_s)
!!
!!      subroutine overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,      &
!!     &          inod_smp_stack, tensor, xx, yy, zz, r, s, a_r, a_s)
!!
!!      subroutine cal_xx_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xx, xx, yy, zz, r, s,       &
!!     &           a_r, a_s)
!!      subroutine cal_xy_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xy, xx, yy, zz, r, s,       &
!!     &           a_r, a_s)
!!      subroutine cal_xz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xz, xx, yy, zz, r, s,       &
!!     &           a_r, a_s)
!!      subroutine cal_yy_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_yy, xx, yy, zz, r, s,       &
!!     &           a_r, a_s)
!!      subroutine cal_yz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_yz, xx, yy, zz, r, s,       &
!!     &           a_r, a_s)
!!      subroutine cal_zz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_zz, xx, yy, zz, r, s, a_r)
!!
!!  uxux = (ar*as)^2 *(x*x*s*s *urur + x*x*z*s *urut - x*y*s*r * urup
!!                   + x*x*z*s *utur + x*x*z*z *utut - x*y*z*r * utup
!!                   - x*y*s*r *upur - x*y*z*r *uput + y*y*r*r * upup)
!!  uxuy = (ar*as)^2 *(x*y*s*s *urur + x*y*z*s *urut + x*x*s*r * urup
!!                   + x*y*z*s *utur + x*y*z*z *utut + x*x*z*r * utup
!!                   - y*y*s*r *upur - y*y*z*r *uput - x*y*r*r * upup)
!!  uxuz = (ar*as)^2 *(x*z*s*s *urur - x*s*s*s *urut + 0
!!                   + x*z*z*s *utur - x*z*s*s *utut + 0
!!                   - y*z*s*r *upur + y*s*s*r *uput + 0)
!!  uyuy = (ar*as)^2 *(y*y*s*s *urur + y*y*s*z *urut + x*y*s*r * urup
!!                   + y*y*z*s *utur + y*y*z*z *utut + x*y*z*r * utup
!!                   + x*y*s*r *upur + x*y*z*r *uput + x*x*r*r * upup)
!!  uyuz = (ar*as)^2 *(y*z*s*s *urur - y*s*s*s *urut + 0
!!                   + y*z*z*s *utur - y*z*s*s *utut - 0
!!                   + x*z*s*r *upur - x*r*s*s *uput + 0)
!!  uzuz = (ar*as)^2 *(z*z*s*s *urur - z*s*s*s *urut + 0
!!                   - z*s*s*s *utur + s*s*s*s *utut + 0
!!                   + 0 + 0 + 0)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  xx(numnod) position in Cartesian coordinate
!!@n @param  yy(numnod) position in Cartesian coordinate
!!@n @param  zz(numnod) position in Cartesian coordinate
!!@n @param  r(numnod)    radius
!!@n @param  s(numnod)    cylindrical radius
!!@n @param  a_r(numnod)  1 / r
!!@n @param  a_s(numnod)  1 / s
!!
!!@n @param  tensor(numnod,6)
!!                    symmetric tensor in spherical coordinate
!!@n @param  txyz(numnod,6)
!!                    symmetric tensor in Cartesian coordinate
!!@n @param  t_xx(numnod)
!!                    @f$ T_{xx} @f$ in Cartesian coordinate
!!@n @param  t_xy(numnod)
!!                    @f$ T_{xy} @f$ in Cartesian coordinate
!!@n @param  t_xz(numnod)
!!                    @f$ T_{xz} @f$ in Cartesian coordinate
!!@n @param  t_yy(numnod)
!!                    @f$ T_{yy} @f$ in Cartesian coordinate
!!@n @param  t_yz(numnod)
!!                    @f$ T_{yz} @f$ in Cartesian coordinate
!!@n @param  t_zz(numnod)
!!                    @f$ T_{zz} @f$ in Cartesian coordinate
!
!
      module cvt_sph_tensor_2_xyz_smp
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
      subroutine cal_xyz_tensor_by_sph_smp(np_smp, numnod,              &
     &          inod_smp_stack, tensor, txyz, xx, yy, zz, r, s,         &
     &          a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp, ar2s2
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp,ar2s2)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
           tpp = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             txyz(inod,6) =   trr
             txyz(inod,3) =   trt
             txyz(inod,5) =   trp
             txyz(inod,1) =   ttt
             txyz(inod,2) =   ttp
             txyz(inod,4) =   tpp
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             txyz(inod,6) =   trr
             txyz(inod,3) =   trt
             txyz(inod,5) =   trp
             txyz(inod,1) =   ttt
             txyz(inod,2) =   ttp
             txyz(inod,4) =   tpp
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             txyz(inod,6) =   trr
             txyz(inod,3) =   trt
             txyz(inod,5) = - trp
             txyz(inod,1) =   ttt
             txyz(inod,2) = - ttp
             txyz(inod,4) =   tpp
           else
             ar2s2 = a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
             txyz(inod,1)                                               &
     &          =  ( trr * xx(inod)*xx(inod)* s(inod)* s(inod)          &
     &         + two*trt * xx(inod)*xx(inod)*zz(inod)* s(inod)          &
     &         - two*trp * xx(inod)*yy(inod)* s(inod)* r(inod)          &
     &         +     ttt * xx(inod)*xx(inod)*zz(inod)*zz(inod)          &
     &         - two*ttp * xx(inod)*yy(inod)*zz(inod)* r(inod)          &
     &         +     tpp * yy(inod)*yy(inod)* r(inod)* r(inod))         &
     &          * ar2s2
!
             txyz(inod,2)                                               &
     &           = ( trr * xx(inod)*yy(inod)* s(inod)* s(inod)          &
     &         + two*trt * xx(inod)*yy(inod)*zz(inod)* s(inod)          &
     &         +     trp *  s(inod) * r(inod)                           &
     &              * (xx(inod)*xx(inod) - yy(inod)*yy(inod))           &
     &         +     ttt * xx(inod)*yy(inod)*zz(inod)*zz(inod)          &
     &         +     ttp * zz(inod) * r(inod)                           &
     &              * (xx(inod)*xx(inod) - yy(inod)*yy(inod))           &
     &         -     tpp * xx(inod)*yy(inod)* r(inod)* r(inod))         &
     &          * ar2s2
!
             txyz(inod,3)                                               &
     &           = ( trr * xx(inod)*zz(inod)* s(inod)                   &
     &         +     trt * xx(inod)                                     &
     &              * (zz(inod)*zz(inod) - s(inod)*s(inod))             &
     &         -     trp * yy(inod)*zz(inod)* r(inod)                   &
     &         -     ttt * xx(inod)*zz(inod)* s(inod)                   &
     &         +     ttp * yy(inod) *s(inod)* r(inod))                  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             txyz(inod,4)                                               &
     &           = ( trr * yy(inod)*yy(inod)* s(inod)* s(inod)          &
     &         + two*trt * yy(inod)*yy(inod)*zz(inod)* s(inod)          &
     &         + two*trp * xx(inod)*yy(inod)* s(inod)* r(inod)          &
     &         +     ttt * yy(inod)*yy(inod)*zz(inod)*zz(inod)          &
     &         + two*ttp * xx(inod)*yy(inod)*zz(inod)* r(inod)          &
     &         +     tpp * xx(inod)*xx(inod)* r(inod)* r(inod))         &
     &          * ar2s2
!
             txyz(inod,5)                                               &
     &           = ( trr * yy(inod)*zz(inod)*s(inod)                    &
     &         +     trt * yy(inod)                                     &
     &              * (zz(inod)*zz(inod) - s(inod)*s(inod))             &
     &         -     ttt * yy(inod)*zz(inod)* s(inod)                   &
     &         +     trp * xx(inod)*zz(inod)* r(inod)                   &
     &         -     ttp * xx(inod)* s(inod)* r(inod))                  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             txyz(inod,6)                                               &
     &           = ( trr * zz(inod)*zz(inod)                            &
     &         - two*trt * zz(inod)* s(inod)                            &
     &         +     ttt *  s(inod)* s(inod))                           &
     &          * a_r(inod)*a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xyz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, xx, yy, zz, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp, ar2s2
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp,ar2s2)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
           tpp = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             tensor(inod,6) =   trr
             tensor(inod,3) =   trt
             tensor(inod,5) =   trp
             tensor(inod,1) =   ttt
             tensor(inod,2) =   ttp
             tensor(inod,4) =   tpp
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             tensor(inod,6) =   trr
             tensor(inod,3) =   trt
             tensor(inod,5) =   trp
             tensor(inod,1) =   ttt
             tensor(inod,2) =   ttp
             tensor(inod,4) =   tpp
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             tensor(inod,6) =   trr
             tensor(inod,3) =   trt
             tensor(inod,5) = - trp
             tensor(inod,1) =   ttt
             tensor(inod,2) = - ttp
             tensor(inod,4) =   tpp
           else
             ar2s2 = a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
             tensor(inod,1)                                             &
     &          =  ( trr * xx(inod)*xx(inod)* s(inod)* s(inod)          &
     &         + two*trt * xx(inod)*xx(inod)*zz(inod)* s(inod)          &
     &         - two*trp * xx(inod)*yy(inod)* s(inod)* r(inod)          &
     &         +     ttt * xx(inod)*xx(inod)*zz(inod)*zz(inod)          &
     &         - two*ttp * xx(inod)*yy(inod)*zz(inod)* r(inod)          &
     &         +     tpp * yy(inod)*yy(inod)* r(inod)* r(inod))         &
     &          * ar2s2
!
             tensor(inod,2)                                             &
     &           = ( trr * xx(inod)*yy(inod)* s(inod)  * s(inod)        &
     &         + two*trt * xx(inod)*yy(inod)*zz(inod)* s(inod)          &
     &         +     trp *  s(inod)* r(inod)                            &
     &              * (xx(inod)*xx(inod) - yy(inod)*yy(inod))           &
     &         +     ttt * xx(inod)*yy(inod)*zz(inod)*zz(inod)          &
     &         +     ttp * zz(inod)* r(inod)                            &
     &              * (xx(inod)*xx(inod) - yy(inod)*yy(inod))           &
     &         -     tpp * xx(inod)*yy(inod)* r(inod)* r(inod) )        &
     &          * ar2s2
!
             tensor(inod,3)                                             &
     &           = ( trr * xx(inod)*zz(inod)* s(inod)                   &
     &         +     trt * xx(inod)                                     &
     &              * (zz(inod)*zz(inod) - s(inod)*s(inod))             &
     &         -     trp * yy(inod)*zz(inod)* r(inod)                   &
     &         -     ttt * xx(inod)*zz(inod)* s(inod)                   &
     &         +     ttp * yy(inod) *s(inod)* r(inod))                 &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             tensor(inod,4)                                             &
     &           = ( trr * yy(inod)*yy(inod)* s(inod)* s(inod)          &
     &         + two*trt * yy(inod)*yy(inod)*zz(inod)* s(inod)          &
     &         + two*trp * xx(inod)*yy(inod)* s(inod)* r(inod)          &
     &         +     ttt * yy(inod)*yy(inod)*zz(inod)*zz(inod)          &
     &         + two*ttp * xx(inod)*yy(inod)*zz(inod)* r(inod)          &
     &         +     tpp * xx(inod)*xx(inod)* r(inod)* r(inod))         &
     &          * ar2s2
!
             tensor(inod,5)                                             &
     &           = ( trr * yy(inod)*zz(inod)*s(inod)                    &
     &         +     trt * yy(inod)                                     &
     &              * (zz(inod)*zz(inod) - s(inod)*s(inod))             &
     &         -     ttt * yy(inod)*zz(inod)* s(inod)                   &
     &         +     trp * xx(inod)*zz(inod)* r(inod)                   &
     &         -     ttp * xx(inod)* s(inod)* r(inod))                  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             tensor(inod,6)                                             &
     &           = ( trr * zz(inod)*zz(inod)                            &
     &         - two*trt * zz(inod)* s(inod)                            &
     &         +     ttt *  s(inod)* s(inod))                           &
     &          * a_r(inod)*a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_xyz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_xx_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xx, xx, yy, zz, r, s,         &
     &          a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xx(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp, ar2s2
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp,ar2s2)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
           tpp = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             t_xx(inod) =   ttt
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_xx(inod) =   ttt
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_xx(inod) =   ttt
           else
             ar2s2 = a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
             t_xx(inod)                                                 &
     &          =  ( trr * xx(inod)*xx(inod)* s(inod)  * s(inod)        &
     &         + two*trt * xx(inod)*xx(inod)*zz(inod)* s(inod)          &
     &         - two*trp * xx(inod)*yy(inod)* s(inod)  * r(inod)        &
     &         +     ttt * xx(inod)*xx(inod)*zz(inod)*zz(inod)          &
     &         - two*ttp * xx(inod)*yy(inod)*zz(inod)* r(inod)          &
     &         +     tpp * yy(inod)*yy(inod)* r(inod)  * r(inod))       &
     &          * ar2s2
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xx_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_xy_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xy, xx, yy, zz, r, s,         &
     &          a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp, ar2s2
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp,ar2s2)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
           tpp = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             t_xy(inod) =   ttp
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_xy(inod) =   ttp
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_xy(inod) = - ttp
           else
             ar2s2 = a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
             t_xy(inod)                                                 &
     &           = ( trr * xx(inod)*yy(inod)* s(inod)* s(inod)          &
     &         + two*trt * xx(inod)*yy(inod)*zz(inod)* s(inod)          &
     &         +     trp *  s(inod)* r(inod)                            &
     &              * (xx(inod)*xx(inod) - yy(inod)*yy(inod))           &
     &         +     ttt * xx(inod)*yy(inod)*zz(inod)*zz(inod)          &
     &         +     ttp * zz(inod)* r(inod)                            &
     &              * (xx(inod)*xx(inod) - yy(inod)*yy(inod))           &
     &         -     tpp * xx(inod)*yy(inod)* r(inod)* r(inod))         &
     &          * ar2s2
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xy_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_xz_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xz, xx, yy, zz, r, s,         &
     &          a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
!
           if ( r(inod).eq.0.0 ) then
             t_xz(inod) =   trt
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_xz(inod) =   trt
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_xz(inod) =   trt
           else
             t_xz(inod)                                                 &
     &           = ( trr * xx(inod)*zz(inod)* s(inod)                   &
     &         +     trt * xx(inod)                                     &
     &              * (zz(inod)*zz(inod) - s(inod)*s(inod))             &
     &         -     trp * yy(inod)*zz(inod)* r(inod)                   &
     &         -     ttt * xx(inod)*zz(inod)* s(inod)                   &
     &         +     ttp * yy(inod) *s(inod)* r(inod))                  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_xz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yy_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_yy, xx, yy, zz, r, s,         &
     &          a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_yy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp, ar2s2
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp,ar2s2)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
           tpp = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             t_yy(inod) =   tpp
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_yy(inod) =   tpp
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_yy(inod) =   tpp
           else
             ar2s2 = a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
             t_yy(inod)                                                 &
     &           = ( trr * yy(inod)*yy(inod)* s(inod)* s(inod)          &
     &         + two*trt * yy(inod)*yy(inod)*zz(inod)* s(inod)          &
     &         + two*trp * xx(inod)*yy(inod)* s(inod)* r(inod)          &
     &         +     ttt * yy(inod)*yy(inod)*zz(inod)*zz(inod)          &
     &         + two*ttp * xx(inod)*yy(inod)*zz(inod)* r(inod)          &
     &         +     tpp * xx(inod)*xx(inod)* r(inod)* r(inod))         &
     &          * ar2s2
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_yy_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yz_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_yz, xx, yy, zz, r, s,         &
     &          a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_yz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           trp = tensor(inod,3)
           ttt = tensor(inod,4)
           ttp = tensor(inod,5)
!
           if ( r(inod).eq.0.0 ) then
             t_yz(inod) =   trp
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_yz(inod) =   trp
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_yz(inod) = - trp
           else
             t_yz(inod)                                                 &
     &           = ( trr * yy(inod)*zz(inod)*s(inod)                    &
     &         +     trt * yy(inod)                                     &
     &              * (zz(inod)*zz(inod) - s(inod)*s(inod))             &
     &         -     ttt * yy(inod)*zz(inod)* s(inod)                   &
     &         +     trp * xx(inod)*zz(inod)* r(inod)                   &
     &         -     ttp * xx(inod)* s(inod)* r(inod))                  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_yz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zz_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_zz, xx, zz, r, s, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: t_zz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, ttt
!
!$omp parallel do private(inod,ist,ied,trr,trt,ttt)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           ttt = tensor(inod,4)
!
           if ( r(inod).eq.0.0 ) then
             t_zz(inod) =   trr
           else if ( s(inod).eq.0.0 .and. zz(inod) .gt. 0) then
             t_zz(inod) =   trr
           else if ( s(inod).eq.0.0 .and. zz(inod) .lt. 0) then
             t_zz(inod) =   trr
           else
             t_zz(inod)                                                 &
     &           = ( trr * zz(inod)*zz(inod)                            &
     &         - two*trt * zz(inod)* s(inod)                            &
     &         +     ttt *  s(inod)* s(inod))                           &
     &          * a_r(inod)*a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_zz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_sph_tensor_2_xyz_smp
