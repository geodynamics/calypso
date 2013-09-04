!>@file   cvt_sph_tensor_2_xyz_smp.f90
!!@brief  module cvt_sph_tensor_2_xyz_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert symmetric tensor from spherical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!      subroutine cal_xyz_tensor_by_sph_smp(np_smp, numnod,            &
!!     &          inod_smp_stack, tensor, txyz, xx, r, s, a_r, a_s)
!!
!!      subroutine overwrite_xyz_tensor_by_sph_smp(np_smp, numnod,      &
!!     &          inod_smp_stack, tensor, xx, r, s, a_r, a_s)
!!
!!      subroutine cal_xx_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xx, xx, r, s, a_r, a_s)
!!      subroutine cal_xy_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xy, xx, r, s, a_r, a_s)
!!      subroutine cal_xz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xz, xx, r, s, a_r, a_s)
!!      subroutine cal_yy_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_yy, xx, r, s, a_r, a_s)
!!      subroutine cal_yz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_yz, xx, r, s, a_r, a_s)
!!      subroutine cal_zz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_zz, xx, r, s, a_r)
!!
!!  uxux = (ar*as)^2 *(x*x*s*s *ur*ur + x*x*z*s *ur*ut - x*y*s*r * ur*up
!!                   + x*x*z*s *ut*ur + x*x*z*z *ut*ut - x*y*z*r * ut*up
!!                   - x*y*s*r *up*ur - x*y*z*r *up*ut + y*y*r*r * up*up)
!!  uxuy = (ar*as)^2 *(x*y*s*s *ur*ur + x*y*z*s *ur*ut + x*x*s*r * ur*up
!!                   + x*y*z*s *ut*ur + x*y*z*z *ut*ut + x*x*z*r * ut*up
!!                   - y*y*s*r *up*ur - y*y*z*r *up*ut - x*y*r*r * up*up)
!!  uxuz = (ar*as)^2 *(x*z*s*s *ur*ur - x*s*s*s *ur*ut + 0
!!                   + x*z*z*s *ut*ur - x*z*s*s *ut*ut + 0
!!                   - y*z*s*r *up*ur + y*s*s*r *up*ut + 0)
!!  uyuy = (ar*as)^2 *(y*y*s*s *ur*ur + y*y*s*z *ur*ut + x*y*s*r * ur*up
!!                   + y*y*z*s *ut*ur + y*y*z*z *ut*ut + x*y*z*r * ut*up
!!                   + x*y*s*r *up*ur + x*y*z*r *up*ut + x*x*r*r * up*up)
!!  uyuz = (ar*as)^2 *(y*z*s*s *ur*ur - y*s*s*s *ur*ut + 0
!!                   + y*z*z*s *ut*ur - y*z*s*s *ut*ut - 0
!!                   + x*z*s*r *up*ur - x*r*s*s *up*ut + 0)
!!  uzuz = (ar*as)^2 *(z*z*s*s *ur*ur - z*s*s*s *ur*ut + 0
!!                   - z*s*s*s *ut*ur + s*s*s*s *ut*ut + 0
!!                   + 0 + 0 + 0)
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
     &          inod_smp_stack, tensor, txyz, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             txyz(inod,6) =   trr
             txyz(inod,3) =   trt
             txyz(inod,5) =   trp
             txyz(inod,1) =   ttt
             txyz(inod,2) =   ttp
             txyz(inod,4) =   tpp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             txyz(inod,6) =   trr
             txyz(inod,3) =   trt
             txyz(inod,5) = - trp
             txyz(inod,1) =   ttt
             txyz(inod,2) = - ttp
             txyz(inod,4) =   tpp
           else
             txyz(inod,1)                                               &
     &          =  ( trr * xx(inod,1)*xx(inod,1)* s(inod)  * s(inod)    &
     &         + two*trt * xx(inod,1)*xx(inod,1)*xx(inod,3)* s(inod)    &
     &         + two*trp * xx(inod,1)*xx(inod,2)* s(inod)  * r(inod)    &
     &         +     ttt * xx(inod,1)*xx(inod,1)*xx(inod,3)*xx(inod,3)  &
     &         + two*ttp * xx(inod,1)*xx(inod,2)*xx(inod,3)* r(inod)    &
     &         +     tpp * xx(inod,2)*xx(inod,2)* r(inod)  * r(inod) )  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
!
             txyz(inod,2)                                               &
     &           = ( trr * xx(inod,1)*xx(inod,2)* s(inod)  * s(inod)    &
     &         +     trt * xx(inod,1)*xx(inod,2)*xx(inod,3)* s(inod)    &
     &         +     trp *  s(inod)  * s(inod)                          &
     &              * (xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2))   &
     &         +     ttt * xx(inod,1)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         +     ttp * xx(inod,3)* r(inod)                          &
     &              * (xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2))   &
     &         -     tpp * xx(inod,1)*xx(inod,2)* r(inod)* r(inod) )    &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
!
             txyz(inod,3)                                               &
     &           = ( trr * xx(inod,1)*xx(inod,3) *s(inod)               &
     &         +     trt * xx(inod,1)                                   &
     &              * (xx(inod,3)*xx(inod,3) - s(inod)*s(inod))         &
     &         -     trp * xx(inod,2)*xx(inod,3) *r(inod)               &
     &         -     ttt * xx(inod,1)*xx(inod,3) *s(inod)               &
     &         +     ttp * xx(inod,2) *s(inod)   *r(inod) )             &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             txyz(inod,4)                                               &
     &           = ( trr * xx(inod,2)*xx(inod,2)* s(inod)  * s(inod)    &
     &         + two*trt * xx(inod,2)*xx(inod,2)*xx(inod,3)* s(inod)    &
     &         + two*trp * xx(inod,1)*xx(inod,2)* s(inod)  * r(inod)    &
     &         +     ttt * xx(inod,2)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         + two*ttp * xx(inod,1)*xx(inod,2)*xx(inod,3)* r(inod)    &
     &         +     tpp * xx(inod,1)*xx(inod,1)* r(inod)  * r(inod) )  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
!
             txyz(inod,5)                                               &
     &           = ( trr * xx(inod,2)*xx(inod,3)*s(inod)                &
     &         +     trt * xx(inod,2)                                   &
     &              * (xx(inod,3)*xx(inod,3) - s(inod)*s(inod))         &
     &         -     ttt * xx(inod,2)*xx(inod,3)*s(inod)                &
     &         +     trp * xx(inod,1)*xx(inod,3)*r(inod)                &
     &         -     ttp * xx(inod,1)* s(inod)  *r(inod) )              &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             txyz(inod,6)                                               &
     &           = ( trr * xx(inod,3)*xx(inod,3)                        &
     &         - two*trt * xx(inod,3)* s(inod)                          &
     &         +     ttt *  s(inod)  * s(inod) )                        &
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
       real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             tensor(inod,6) =   trr
             tensor(inod,3) =   trt
             tensor(inod,5) =   trp
             tensor(inod,1) =   ttt
             tensor(inod,2) =   ttp
             tensor(inod,4) =   tpp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             tensor(inod,6) =   trr
             tensor(inod,3) =   trt
             tensor(inod,5) = - trp
             tensor(inod,1) =   ttt
             tensor(inod,2) = - ttp
             tensor(inod,4) =   tpp
           else
             tensor(inod,1)                                             &
     &          =  ( trr * xx(inod,1)*xx(inod,1)* s(inod)  * s(inod)    &
     &         + two*trt * xx(inod,1)*xx(inod,1)*xx(inod,3)* s(inod)    &
     &         + two*trp * xx(inod,1)*xx(inod,2)* s(inod)  * r(inod)    &
     &         +     ttt * xx(inod,1)*xx(inod,1)*xx(inod,3)*xx(inod,3)  &
     &         + two*ttp * xx(inod,1)*xx(inod,2)*xx(inod,3)* r(inod)    &
     &         +     tpp * xx(inod,2)*xx(inod,2)* r(inod)  * r(inod) )  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
!
             tensor(inod,2)                                             &
     &           = ( trr * xx(inod,1)*xx(inod,2)* s(inod)  * s(inod)    &
     &         +     trt * xx(inod,1)*xx(inod,2)*xx(inod,3)* s(inod)    &
     &         +     trp *  s(inod)  * s(inod)                          &
     &              * (xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2))   &
     &         +     ttt * xx(inod,1)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         +     ttp * xx(inod,3)* r(inod)                          &
     &              * (xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2))   &
     &         -     tpp * xx(inod,1)*xx(inod,2)* r(inod)* r(inod) )    &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
!
             tensor(inod,3)                                             &
     &           = ( trr * xx(inod,1)*xx(inod,3) *s(inod)               &
     &         +     trt * xx(inod,1)                                   &
     &              * (xx(inod,3)*xx(inod,3) - s(inod)*s(inod))         &
     &         -     trp * xx(inod,2)*xx(inod,3) *r(inod)               &
     &         -     ttt * xx(inod,1)*xx(inod,3) *s(inod)               &
     &         +     ttp * xx(inod,2) *s(inod)   *r(inod) )             &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             tensor(inod,4)                                             &
     &           = ( trr * xx(inod,2)*xx(inod,2)* s(inod)  * s(inod)    &
     &         + two*trt * xx(inod,2)*xx(inod,2)*xx(inod,3)* s(inod)    &
     &         + two*trp * xx(inod,1)*xx(inod,2)* s(inod)  * r(inod)    &
     &         +     ttt * xx(inod,2)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         + two*ttp * xx(inod,1)*xx(inod,2)*xx(inod,3)* r(inod)    &
     &         +     tpp * xx(inod,1)*xx(inod,1)* r(inod)  * r(inod) )  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
!
             tensor(inod,5)                                             &
     &           = ( trr * xx(inod,2)*xx(inod,3)*s(inod)                &
     &         +     trt * xx(inod,2)                                   &
     &              * (xx(inod,3)*xx(inod,3) - s(inod)*s(inod))         &
     &         -     ttt * xx(inod,2)*xx(inod,3)*s(inod)                &
     &         +     trp * xx(inod,1)*xx(inod,3)*r(inod)                &
     &         -     ttp * xx(inod,1)* s(inod)  *r(inod) )              &
     &          * a_r(inod)*a_r(inod)*a_s(inod)
!
             tensor(inod,6)                                             &
     &           = ( trr * xx(inod,3)*xx(inod,3)                        &
     &         - two*trt * xx(inod,3)* s(inod)                          &
     &         +     ttt *  s(inod)  * s(inod) )                        &
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
     &          inod_smp_stack, tensor, t_xx, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xx(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_xx(inod) =   ttt
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_xx(inod) =   ttt
           else
             t_xx(inod)                                                 &
     &          =  ( trr * xx(inod,1)*xx(inod,1)* s(inod)  * s(inod)    &
     &         + two*trt * xx(inod,1)*xx(inod,1)*xx(inod,3)* s(inod)    &
     &         + two*trp * xx(inod,1)*xx(inod,2)* s(inod)  * r(inod)    &
     &         +     ttt * xx(inod,1)*xx(inod,1)*xx(inod,3)*xx(inod,3)  &
     &         + two*ttp * xx(inod,1)*xx(inod,2)*xx(inod,3)* r(inod)    &
     &         +     tpp * xx(inod,2)*xx(inod,2)* r(inod)  * r(inod) )  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
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
     &          inod_smp_stack, tensor, t_xy, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_xy(inod) =   ttp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_xy(inod) = - ttp
           else
             t_xy(inod)                                                 &
     &           = ( trr * xx(inod,1)*xx(inod,2)* s(inod)  * s(inod)    &
     &         +     trt * xx(inod,1)*xx(inod,2)*xx(inod,3)* s(inod)    &
     &         +     trp *  s(inod)  * s(inod)                          &
     &              * (xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2))   &
     &         +     ttt * xx(inod,1)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         +     ttp * xx(inod,3)* r(inod)                          &
     &              * (xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2))   &
     &         -     tpp * xx(inod,1)*xx(inod,2)* r(inod)* r(inod) )    &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
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
     &          inod_smp_stack, tensor, t_xz, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_xz(inod) =   trt
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_xz(inod) =   trt
           else
             t_xz(inod)                                                 &
     &           = ( trr * xx(inod,1)*xx(inod,3) *s(inod)               &
     &         +     trt * xx(inod,1)                                   &
     &              * (xx(inod,3)*xx(inod,3) - s(inod)*s(inod))         &
     &         -     trp * xx(inod,2)*xx(inod,3) *r(inod)               &
     &         -     ttt * xx(inod,1)*xx(inod,3) *s(inod)               &
     &         +     ttp * xx(inod,2) *s(inod)   *r(inod) )             &
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
     &          inod_smp_stack, tensor, t_yy, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_yy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp parallel do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_yy(inod) =   tpp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_yy(inod) =   tpp
           else
             t_yy(inod)                                                 &
     &           = ( trr * xx(inod,2)*xx(inod,2)* s(inod)  * s(inod)    &
     &         + two*trt * xx(inod,2)*xx(inod,2)*xx(inod,3)* s(inod)    &
     &         + two*trp * xx(inod,1)*xx(inod,2)* s(inod)  * r(inod)    &
     &         +     ttt * xx(inod,2)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         + two*ttp * xx(inod,1)*xx(inod,2)*xx(inod,3)* r(inod)    &
     &         +     tpp * xx(inod,1)*xx(inod,1)* r(inod)  * r(inod) )  &
     &          * a_r(inod)*a_r(inod)*a_s(inod)*a_s(inod)
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
     &          inod_smp_stack, tensor, t_yz, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_yz(inod) =   trp
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_yz(inod) = - trp
           else
             t_yz(inod)                                                 &
     &           = ( trr * xx(inod,2)*xx(inod,3)*s(inod)                &
     &         +     trt * xx(inod,2)                                   &
     &              * (xx(inod,3)*xx(inod,3) - s(inod)*s(inod))         &
     &         -     ttt * xx(inod,2)*xx(inod,3)*s(inod)                &
     &         +     trp * xx(inod,1)*xx(inod,3)*r(inod)                &
     &         -     ttp * xx(inod,1)* s(inod)  *r(inod) )              &
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
     &          inod_smp_stack, tensor, t_zz, xx, r, s, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
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
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_zz(inod) =   trr
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_zz(inod) =   trr
           else
             t_zz(inod)                                                 &
     &           = ( trr * xx(inod,3)*xx(inod,3)                        &
     &         - two*trt * xx(inod,3)* s(inod)                          &
     &         +     ttt *  s(inod)  * s(inod) )                        &
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
