!>@file   cvt_xyz_tensor_2_sph_smp.f90
!!@brief  module cvt_xyz_tensor_2_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert symmetric tensor from Cartesian coordinate
!!       to spherical coordinate
!!
!!@verbatim
!!      subroutine cal_sph_tensor_smp(np_smp, numnod, inod_smp_stack,   &
!!     &          tensor, tsph, xx, r, s, a_r, a_s)
!!
!!      subroutine overwrite_sph_tensor_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, xx, r, s, a_r, a_s)
!!
!!      subroutine cal_rr_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_rr, xx, r, s, a_r)
!!      subroutine cal_rt_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_rt, xx, r, s, a_r, a_s)
!!      subroutine cal_rp_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_rp, xx, r, s, a_r, a_s)
!!      subroutine cal_tt_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_tt, xx, r, s, a_r, a_s)
!!      subroutine cal_tp_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_tp, xx, r, s, a_r, a_s)
!!      subroutine cal_pp_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_pp, xx, r, s, a_s)
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
!!                    symmetric tensor in Cartesian coordinate
!!@n @param  txyz(numnod,6)
!!                    symmetric tensor in spherical coordinate
!!@n @param  t_rr(numnod)
!!                    @f$ T_{rr} @f$ in spherical coordinate
!!@n @param  t_rt(numnod)
!!                    @f$ T_{r \theta} @f$ in spherical coordinate
!!@n @param  t_rp(numnod)
!!                    @f$ T_{r \phi} @f$ in spherical coordinate
!!@n @param  t_tt(numnod)
!!                    @f$ T_{\theta \theta} @f$ in spherical coordinate
!!@n @param  t_tp(numnod)
!!                    @f$ T_{\theta \phi} @f$ in spherical coordinate
!!@n @param  t_pp(numnod)
!!                    @f$ T_{\phi \phi} @f$ in spherical coordinate
!
      module cvt_xyz_tensor_2_sph_smp
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
      subroutine cal_sph_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          tensor, tsph, xx, r, s, a_r, a_s)
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
       real(kind=kreal), intent(inout) :: tsph(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             tsph(inod,1) =       tzz
             tsph(inod,2) =       txz
             tsph(inod,3) =       tyz
             tsph(inod,4) =       txx
             tsph(inod,5) =       txy
             tsph(inod,6) =       tyy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             tsph(inod,1) =       tzz
             tsph(inod,2) =       txz
             tsph(inod,3) =       tyz
             tsph(inod,4) =       txx
             tsph(inod,5) =       txy
             tsph(inod,6) =       tyy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             tsph(inod,1) =       tzz
             tsph(inod,2) =       txz
             tsph(inod,3) =     - tyz
             tsph(inod,4) =       txx
             tsph(inod,5) =     - txy
             tsph(inod,6) =       tyy
           else
             tsph(inod,1)                                               &
     &          = (  txx * xx(inod,1)*xx(inod,1)                        &
     &         + two*txy * xx(inod,1)*xx(inod,2)                        &
     &         + two*txz * xx(inod,1)*xx(inod,3)                        &
     &         +     tyy * xx(inod,2)*xx(inod,2)                        &
     &         + two*tyz * xx(inod,2)*xx(inod,3)                        &
     &         +     tzz * xx(inod,3)*xx(inod,3) )                      &
     &          * a_r(inod) * a_r(inod)
!
             tsph(inod,2)                                               &
     &          = (  txx * xx(inod,1)*xx(inod,1)*xx(inod,3)             &
     &         + two*txy * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         +     txz * xx(inod,1)                                   &
     &                  *( xx(inod,3)*xx(inod,3) - s(inod)*s(inod) )    &
     &         +     tyy * xx(inod,2)*xx(inod,2)*xx(inod,3)             &
     &         +     tyz * xx(inod,2)                                   &
     &                  *( xx(inod,3)*xx(inod,3) - s(inod)*s(inod) )    &
     &         -     tzz *xx(inod,3)*s(inod)*s(inod) )                  &
     &          * a_r(inod) * a_r(inod) * a_s(inod)
!
             tsph(inod,3)                                               &
     &          = ( -txx*xx(inod,1)*xx(inod,2)                          &
     &         +     txy                                                &
     &               *( xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2) ) &
     &         -     txz*xx(inod,2)*xx(inod,3)                          &
     &         +     tyy*xx(inod,1)*xx(inod,2)                          &
     &         +     tyz*xx(inod,1)*xx(inod,3) )                        &
     &          * a_r(inod) * a_s(inod)
!
             tsph(inod,4)                                               &
     &          = (  txx * xx(inod,1)*xx(inod,1)*xx(inod,3)*xx(inod,3)  &
     &         + two*txy * xx(inod,1)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         - two*txz * xx(inod,1)*xx(inod,3)* s(inod)  * s(inod)    &
     &         +     tyy * xx(inod,2)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         - two*tyz * xx(inod,2)*xx(inod,3)* s(inod)  * s(inod)    &
     &         +     tzz *  s(inod)  * s(inod)  * s(inod)  * s(inod) )  &
     &          * a_r(inod) * a_r(inod) * a_s(inod) * a_s(inod)
!
             tsph(inod,5)                                               &
     &          = ( -txx * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         +     txy * xx(inod,3)                                   &
     &               *( xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2) ) &
     &         +     txz * xx(inod,2)* s(inod)  * s(inod)               &
     &         +     tyy * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         -     tyz * xx(inod,1)* s(inod)  * s(inod) )             &
     &              * a_r(inod) * a_s(inod) * a_s(inod)
!
             tsph(inod,6)                                               &
     &         = (    txx*xx(inod,2)*xx(inod,2)                         &
     &          - two*txy*xx(inod,1)*xx(inod,2)                         &
     &          +     tyy*xx(inod,1)*xx(inod,1) )                       &
     &              * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_tensor_smp(np_smp, numnod,               &
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
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             tensor(inod,1) =       tzz
             tensor(inod,2) =       txz
             tensor(inod,3) =       tyz
             tensor(inod,4) =       txx
             tensor(inod,5) =       txy
             tensor(inod,6) =       tyy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             tensor(inod,1) =       tzz
             tensor(inod,2) =       txz
             tensor(inod,3) =       tyz
             tensor(inod,4) =       txx
             tensor(inod,5) =       txy
             tensor(inod,6) =       tyy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             tensor(inod,1) =       tzz
             tensor(inod,2) =       txz
             tensor(inod,3) =     - tyz
             tensor(inod,4) =       txx
             tensor(inod,5) =     - txy
             tensor(inod,6) =       tyy
           else
             tensor(inod,1)                                             &
     &          = (  txx * xx(inod,1)*xx(inod,1)                        &
     &         + two*txy * xx(inod,1)*xx(inod,2)                        &
     &         + two*txz * xx(inod,1)*xx(inod,3)                        &
     &         +     tyy * xx(inod,2)*xx(inod,2)                        &
     &         + two*tyz * xx(inod,2)*xx(inod,3)                        &
     &         +     tzz * xx(inod,3)*xx(inod,3) )                      &
     &          * a_r(inod) * a_r(inod)
!
             tensor(inod,2)                                             &
     &          = (  txx * xx(inod,1)*xx(inod,1)*xx(inod,3)             &
     &         + two*txy * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         +     txz * xx(inod,1)                                   &
     &                  *( xx(inod,3)*xx(inod,3) - s(inod)*s(inod) )    &
     &         +     tyy * xx(inod,2)*xx(inod,2)*xx(inod,3)             &
     &         +     tyz * xx(inod,2)                                   &
     &                  *( xx(inod,3)*xx(inod,3) - s(inod)*s(inod) )    &
     &         -     tzz *xx(inod,3)*s(inod)*s(inod) )                  &
     &          * a_r(inod) * a_r(inod) * a_s(inod)
!
             tensor(inod,3)                                             &
     &          = ( -txx*xx(inod,1)*xx(inod,2)                          &
     &         +     txy                                                &
     &               *( xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2) ) &
     &         -     txz*xx(inod,2)*xx(inod,3)                          &
     &         +     tyy*xx(inod,1)*xx(inod,2)                          &
     &         +     tyz*xx(inod,1)*xx(inod,3) )                        &
     &          * a_r(inod) * a_s(inod)
!
             tensor(inod,4)                                             &
     &          = (  txx * xx(inod,1)*xx(inod,1)*xx(inod,3)*xx(inod,3)  &
     &         + two*txy * xx(inod,1)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         - two*txz * xx(inod,1)*xx(inod,3)* s(inod)  * s(inod)    &
     &         +     tyy * xx(inod,2)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         - two*tyz * xx(inod,2)*xx(inod,3)* s(inod)  * s(inod)    &
     &         +     tzz *  s(inod)  * s(inod)  * s(inod)  * s(inod) )  &
     &          * a_r(inod) * a_r(inod) * a_s(inod) * a_s(inod)
!
             tensor(inod,5)                                             &
     &          = ( -txx * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         +     txy * xx(inod,3)                                   &
     &               *( xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2) ) &
     &         +     txz * xx(inod,2)* s(inod)  * s(inod)               &
     &         +     tyy * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         -     tyz * xx(inod,1)* s(inod)  * s(inod) )             &
     &              * a_r(inod) * a_s(inod) * a_s(inod)
!
             tensor(inod,6)                                             &
     &         = (    txx*xx(inod,2)*xx(inod,2)                         &
     &          - two*txy*xx(inod,1)*xx(inod,2)                         &
     &          +     tyy*xx(inod,1)*xx(inod,1) )                       &
     &              * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_sph_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rr_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_rr, xx, r, s, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: t_rr(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             t_rr(inod) =       tzz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_rr(inod) =       tzz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_rr(inod) =       tzz
           else
             t_rr(inod)                                                 &
     &          = (  txx * xx(inod,1)*xx(inod,1)                        &
     &         + two*txy * xx(inod,1)*xx(inod,2)                        &
     &         + two*txz * xx(inod,1)*xx(inod,3)                        &
     &         +     tyy * xx(inod,2)*xx(inod,2)                        &
     &         + two*tyz * xx(inod,2)*xx(inod,3)                        &
     &         +     tzz * xx(inod,3)*xx(inod,3) )                      &
     &          * a_r(inod) * a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rr_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_rt_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_rt, xx, r, s, a_r, a_s)
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
       real(kind=kreal), intent(inout) :: t_rt(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             t_rt(inod) =       txz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_rt(inod) =       txz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_rt(inod) =       txz
           else
             t_rt(inod)                                                 &
     &          = (  txx * xx(inod,1)*xx(inod,1)*xx(inod,3)             &
     &         + two*txy * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         +     txz * xx(inod,1)                                   &
     &                  *( xx(inod,3)*xx(inod,3) - s(inod)*s(inod) )    &
     &         +     tyy * xx(inod,2)*xx(inod,2)*xx(inod,3)             &
     &         +     tyz * xx(inod,2)                                   &
     &                  *( xx(inod,3)*xx(inod,3) - s(inod)*s(inod) )    &
     &         -     tzz *xx(inod,3)*s(inod)*s(inod) )                  &
     &          * a_r(inod) * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rt_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_rp_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_rp, xx, r, s, a_r, a_s)
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
       real(kind=kreal), intent(inout) :: t_rp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
!
           if ( r(inod).eq.0.0 ) then
             t_rp(inod) =       tyz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_rp(inod) =       tyz
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_rp(inod) =     - tyz
           else
             t_rp(inod)                                                 &
     &          = ( -txx*xx(inod,1)*xx(inod,2)                          &
     &         +     txy                                                &
     &               *( xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2) ) &
     &         -     txz*xx(inod,2)*xx(inod,3)                          &
     &         +     tyy*xx(inod,1)*xx(inod,2)                          &
     &         +     tyz*xx(inod,1)*xx(inod,3) )                        &
     &          * a_r(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_rp_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_tt_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_tt, xx, r, s, a_r, a_s)
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
       real(kind=kreal), intent(inout) :: t_tt(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( r(inod).eq.0.0 ) then
             t_tt(inod) =       txx
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_tt(inod) =       txx
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_tt(inod) =       txx
           else
             t_tt(inod)                                                 &
     &          = (  txx * xx(inod,1)*xx(inod,1)*xx(inod,3)*xx(inod,3)  &
     &         + two*txy * xx(inod,1)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         - two*txz * xx(inod,1)*xx(inod,3)* s(inod)  * s(inod)    &
     &         +     tyy * xx(inod,2)*xx(inod,2)*xx(inod,3)*xx(inod,3)  &
     &         - two*tyz * xx(inod,2)*xx(inod,3)* s(inod)  * s(inod)    &
     &         +     tzz *  s(inod)  * s(inod)  * s(inod)  * s(inod) )  &
     &          * a_r(inod) * a_r(inod) * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tt_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_tp_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_tp, xx, r, s, a_r, a_s)
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
       real(kind=kreal), intent(inout) :: t_tp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,txz,tyy,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           txz = tensor(inod,3)
           tyy = tensor(inod,4)
           tyz = tensor(inod,5)
!
           if ( r(inod).eq.0.0 ) then
             t_tp(inod) =       txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_tp(inod) =       txy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_tp(inod) =     - txy
           else
             t_tp(inod)                                                 &
     &          = ( -txx * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         +     txy * xx(inod,3)                                   &
     &               *( xx(inod,1)*xx(inod,1) - xx(inod,2)*xx(inod,2) ) &
     &         +     txz * xx(inod,2)* s(inod)  * s(inod)               &
     &         +     tyy * xx(inod,1)*xx(inod,2)*xx(inod,3)             &
     &         -     tyz * xx(inod,1)* s(inod)  * s(inod) )             &
     &              * a_r(inod) * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_tp_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pp_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_pp, xx, r, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_pp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp parallel do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( r(inod).eq.0.0 ) then
             t_pp(inod) =       tyy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .gt. 0) then
             t_pp(inod) =       tyy
           else if ( s(inod).eq.0.0 .and. xx(inod,3) .lt. 0) then
             t_pp(inod) =       tyy
           else
             t_pp(inod)                                                 &
     &         = (    txx*xx(inod,2)*xx(inod,2)                         &
     &          - two*txy*xx(inod,1)*xx(inod,2)                         &
     &          +     tyy*xx(inod,1)*xx(inod,1) )                       &
     &              * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_pp_tensor_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_tensor_2_sph_smp
