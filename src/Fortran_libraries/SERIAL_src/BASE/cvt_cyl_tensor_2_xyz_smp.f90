!>@file   cvt_cyl_tensor_2_xyz_smp.f90
!!@brief  module cvt_cyl_tensor_2_xyz_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert symmetric tensor from cylindrical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!      subroutine cal_xyz_tensor_by_cyl_smp(np_smp, numnod,            &
!!     &          inod_smp_stack, tensor, txyz, xx, yy, s, a_s)
!!
!!      subroutine overwrite_xyz_tensor_by_cyl_smp(np_smp, numnod,      &
!!     &          inod_smp_stack, tensor, xx, yy, s, a_s)
!!
!!      subroutine cal_xx_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xx, xx, yy, s, a_s)
!!      subroutine cal_xy_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xy, xx, yy, s, a_s)
!!      subroutine cal_xz_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xz, xx, yy, s, a_s)
!!      subroutine cal_yy_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_yy, xx, yy, s, a_s)
!!      subroutine cal_yz_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_yz, xx, yy, s, a_s)
!!      subroutine cal_zz_tensor_by_cyl_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_xz)
!!
!!   uxux = (as)^2 *(x*x *us*us - x*y *us*up
!!                 - x*y *us*up + y*y *up*up)
!!   uyuy = (as)^2 *(x*y *us*us + x*x *us*up
!!                 - y*y *us*up + x*x *up*up)
!!   uxuz = (as)^2 *(x*s *us*uz - y*s *up*uz)
!!   uyuy = (as)^2 *(y*y *us*us + x*y *us*up
!!                 + x*y *us*up + x*x *up*up)
!!   uyuz = (as)^2 *(y*s *us*uz + x*s *up*uz)
!!   uzuz = (as)^2 * s*s* uz*uz
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  xx(numnod) position in Cartesian coordinate
!!@n @param  yy(numnod) position in Cartesian coordinate
!!@n @param  zz(numnod) position in Cartesian coordinate
!!@n @param  s(numnod)    cylindrical radius
!!@n @param  a_s(numnod)  1 / s
!!
!!@n @param  tensor(numnod,6)
!!                    symmetric tensor in cylindrical coordinate
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
      module cvt_cyl_tensor_2_xyz_smp
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
      subroutine cal_xyz_tensor_by_cyl_smp(np_smp, numnod,              &
     &          inod_smp_stack, tensor, txyz, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: txyz(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tsz, tpp, tpz, tzz
!
!
!$omp do private(inod,ist,ied,tss,tsp,tsz,tpp,tpz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tsz = tensor(inod,3)
           tpp = tensor(inod,4)
           tpz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( s(inod).eq.0.0 ) then
             txyz(inod,1) =   tss
             txyz(inod,2) =   tsp
             txyz(inod,3) =   tsz
             txyz(inod,4) =   tpp
             txyz(inod,5) =   tpz
             txyz(inod,6) =   tzz
           else
             txyz(inod,1) = ( tss * xx(inod)*xx(inod)                   &
     &                  - two*tsp * xx(inod)*yy(inod)                   &
     &                  +     tpp * yy(inod)*yy(inod) )                 &
     &                   * a_s(inod)*a_s(inod)
!
             txyz(inod,2) = ( tss * xx(inod)*yy(inod)                   &
     &                  +     tsp *(xx(inod)*xx(inod)                   &
     &                            - yy(inod)*yy(inod))                  &
     &                  +     tpp * xx(inod)*xx(inod) )                 &
     &                   * a_s(inod)*a_s(inod)
!
             txyz(inod,3) = ( tsz * xx(inod)                            &
     &                  -     tpz * yy(inod) )                          &
     &                   * a_s(inod)
!
             txyz(inod,4) = ( tss * yy(inod)*yy(inod)                   &
     &                  + two*tsp * xx(inod)*yy(inod)                   &
     &                  +     tpp * xx(inod)*xx(inod) )                 &
     &                   * a_s(inod)*a_s(inod)
!
             txyz(inod,5) = ( tsz * yy(inod)                            &
     &                  +     tpz * xx(inod) )                          &
     &                   * a_s(inod)
!
             txyz(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_xyz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_xyz_tensor_by_cyl_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tsz, tpp, tpz, tzz
!
!
!$omp do private(inod,ist,ied,tss,tsp,tsz,tpp,tpz,tzz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tsz = tensor(inod,3)
           tpp = tensor(inod,4)
           tpz = tensor(inod,5)
           tzz = tensor(inod,6)
!
           if ( s(inod).eq.0.0 ) then
             tensor(inod,1) =   tss
             tensor(inod,2) =   tsp
             tensor(inod,3) =   tsz
             tensor(inod,4) =   tpp
             tensor(inod,5) =   tpz
             tensor(inod,6) =   tzz
           else
             tensor(inod,1) = ( tss * xx(inod)*xx(inod)                 &
     &                    - two*tsp * xx(inod)*yy(inod)                 &
     &                    +     tpp * yy(inod)*yy(inod) )               &
     &                     * a_s(inod)*a_s(inod)
!
             tensor(inod,2) = ( tss * xx(inod)*yy(inod)                 &
     &                    +     tsp *(xx(inod)*xx(inod)                 &
     &                              - yy(inod)*yy(inod))                &
     &                    +     tpp * xx(inod)*xx(inod) )               &
     &                     * a_s(inod)*a_s(inod)
!
             tensor(inod,3) = ( tsz * xx(inod)                          &
     &                    -     tpz * yy(inod) )                        &
     &                     * a_s(inod)
!
             tensor(inod,4) = ( tss * yy(inod)*yy(inod)                 &
     &                    + two*tsp * xx(inod)*yy(inod)                 &
     &                    +     tpp * xx(inod)*xx(inod) )               &
     &                     * a_s(inod)*a_s(inod)
!
             tensor(inod,5) = ( tsz * yy(inod)                          &
     &                    +     tpz * xx(inod) )                        &
     &                     * a_s(inod)
!
             tensor(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine overwrite_xyz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_xx_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xx, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xx(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tpp
!
!
!$omp do private(inod,ist,ied,tss,tsp,tpp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tpp = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             t_xx(inod) =   tss
           else
             t_xx(inod) =   ( tss * xx(inod)*xx(inod)                   &
     &                  - two*tsp * xx(inod)*yy(inod)                   &
     &                  +     tpp * yy(inod)*yy(inod) )                 &
     &                   * a_s(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_xx_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_xy_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xy, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tpp
!
!
!$omp do private(inod,ist,ied,tss,tsp,tpp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tpp = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             t_xy(inod) =   tsp
           else
             t_xy(inod) =   ( tss * xx(inod)*yy(inod)                   &
     &                  +     tsp *(xx(inod)*xx(inod)                   &
     &                            - yy(inod)*yy(inod))                  &
     &                  +     tpp * xx(inod)*xx(inod) )                 &
     &                   * a_s(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_xy_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_xz_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xz, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_xz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsz, tpz
!
!
!$omp do private(inod,ist,ied,tsz,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsz = tensor(inod,3)
           tpz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             t_xz(inod) =   tsz
           else
             t_xz(inod) =   ( tsz * xx(inod)                            &
     &                  -     tpz * yy(inod) )                          &
     &                   * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_xz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yy_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_yy, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_yy(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tss, tsp, tpp
!
!
!$omp do private(inod,ist,ied,tss,tsp,tpp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tss = tensor(inod,1)
           tsp = tensor(inod,2)
           tpp = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             t_yy(inod) =   tpp
           else
             t_yy(inod) =   ( tss * yy(inod)*yy(inod)                   &
     &                  + two*tsp * xx(inod)*yy(inod)                   &
     &                  +     tpp * xx(inod)*xx(inod) )                 &
     &                   * a_s(inod)*a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_yy_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_yz_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_yz, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_yz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: tsz, tpz
!
!
!$omp do private(inod,ist,ied,tsz,tpz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           tsz = tensor(inod,3)
           tpz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             t_yz(inod) =   tpz
           else
             t_yz(inod) =   ( tsz * yy(inod)                          &
     &                  +     tpz * xx(inod) )                        &
     &                   * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_yz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zz_tensor_by_cyl_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_xz)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: t_xz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           t_xz(inod) = tensor(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_zz_tensor_by_cyl_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_tensor_2_xyz_smp
