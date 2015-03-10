!>@file   cvt_xyz_tensor_2_cyl_smp.f90
!!@brief  module cvt_xyz_tensor_2_cyl_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert symmetric tensor from Cartesian coordinate
!!       to cylindrical coordinate
!!
!!@verbatim
!!      subroutine cal_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,   &
!!     &          tensor, tcyl, xx, yy, s, a_s)
!!
!!      subroutine overwrite_cyl_tensor_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, xx, yy, s, a_s)
!!
!!      subroutine cal_ss_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_ss, xx, yy, s, a_s)
!!      subroutine cal_sp_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_sp, xx, yy, s, a_s)
!!      subroutine cal_sz_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_sz, xx, yy, s, a_s)
!!      subroutine cal_pp_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,&
!!     &          tensor, t_pp, xx, yy, s, a_s)
!!      subroutine cal_pz_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_pz, xx, yy, s, a_s)
!!      subroutine cal_zz_tensor_smp(np_smp, numnod, inod_smp_stack,    &
!!     &          tensor, t_zz)
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
!!                    symmetric tensor in Cartesian coordinate
!!@n @param  tcyl(numnod,6)
!!                    symmetric tensor in cylindrical coordinate
!!@n @param  t_ss(numnod)
!!                    @f$ T_{ss} @f$ in cylindrical coordinate
!!@n @param  t_sp(numnod)
!!                    @f$ T_{s \phi} @f$ in cylindrical coordinate
!!@n @param  t_sz(numnod)
!!                    @f$ T_{sz} @f$ in cylindrical coordinate
!!@n @param  t_pp(numnod)
!!                    @f$ T_{\phi \phi} @f$ in cylindrical coordinate
!!@n @param  t_pz(numnod)
!!                    @f$ T_{\phi z} @f$ in cylindrical coordinate
!!@n @param  t_zz(numnod)
!!                    @f$ T_{zz} @f$ in cylindrical coordinate
!
      module cvt_xyz_tensor_2_cyl_smp
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
      subroutine cal_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,     &
     &          tensor, tcyl, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: tcyl(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
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
           if ( s(inod).eq.0.0 ) then
             tcyl(inod,1) =       tensor(inod,1)
             tcyl(inod,2) =       tensor(inod,2)
             tcyl(inod,3) =       tensor(inod,3)
             tcyl(inod,4) =       tensor(inod,4)
             tcyl(inod,5) =       tensor(inod,5)
             tcyl(inod,6) =       tensor(inod,6)
           else
             tcyl(inod,1) =   (  txx * xx(inod)*xx(inod)                &
     &                     + two*txy * xx(inod)*yy(inod)                &
     &                     +     tyy * yy(inod)*yy(inod) )              &
     &                      * a_s(inod) * a_s(inod)
!
             tcyl(inod,2) =   ( -txx * xx(inod)*yy(inod)                &
     &                     +     txy * (xx(inod)*xx(inod)               &
     &                                - yy(inod)*yy(inod) )             &
     &                     +     tyy*xx(inod)*yy(inod) )                &
     &                      * a_s(inod) * a_s(inod)
!
             tcyl(inod,3) =   (  txz * xx(inod)                         &
     &                     +     tyz * yy(inod) ) * a_s(inod)
!
             tcyl(inod,4) =   (  txx * yy(inod)*yy(inod)                &
     &                     - two*txy * xx(inod)*yy(inod)                &
     &                     +     tyy * xx(inod)*xx(inod) )              &
     &                      * a_s(inod) * a_s(inod)
!
             tcyl(inod,5) =   ( -txz * yy(inod)                         &
     &                         + tyz * xx(inod) ) * a_s(inod)
!
             tcyl(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cyl_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_tensor_smp(np_smp, numnod,               &
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
       real(kind=kreal) :: txx, txy, txz, tyy, tyz, tzz
!
!
!$omp do private(inod,ist,ied,txx,txy,txz,tyy,tyz,tzz)
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
           if ( s(inod).eq.0.0 ) then
             tensor(inod,1) =       txx
             tensor(inod,2) =       txy
             tensor(inod,3) =       txz
             tensor(inod,4) =       tyy
             tensor(inod,5) =       tyz
             tensor(inod,6) =       tzz
           else
             tensor(inod,1) = (  txx * xx(inod)*xx(inod)                &
     &                     + two*txy * xx(inod)*yy(inod)                &
     &                     +     tyy * yy(inod)*yy(inod) )              &
     &                      * a_s(inod) * a_s(inod)
!
             tensor(inod,2) = ( -txx * xx(inod)*yy(inod)                &
     &                     +     txy * (xx(inod)*xx(inod)               &
     &                                - yy(inod)*yy(inod) )             &
     &                     +     tyy*xx(inod)*yy(inod) )                &
     &                      * a_s(inod) * a_s(inod)
!
             tensor(inod,3) = (  txz * xx(inod)                         &
     &                     +     tyz * yy(inod) ) * a_s(inod)
!
             tensor(inod,4) = (  txx * yy(inod)*yy(inod)                &
     &                     - two*txy * xx(inod)*yy(inod)                &
     &                     +     tyy * xx(inod)*xx(inod) )              &
     &                      * a_s(inod) * a_s(inod)
!
             tensor(inod,5) = ( -txz * yy(inod)                         &
     &                         + tyz * xx(inod) ) * a_s(inod)
!
             tensor(inod,6) = tzz
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine overwrite_cyl_tensor_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_ss_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_ss, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_ss(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             t_ss(inod) = txx
           else
             t_ss(inod) =     (  txx * xx(inod)*xx(inod)                &
     &                     + two*txy * xx(inod)*yy(inod)                &
     &                     +     tyy * yy(inod)*yy(inod) )              &
     &                      * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_ss_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_sp, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_sp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             t_sp(inod) = txy
           else
             t_sp(inod) =     ( -txx * xx(inod)*yy(inod)                &
     &                     +     txy * (xx(inod)*xx(inod)               &
     &                                - yy(inod)*yy(inod) )             &
     &                     +     tyy*xx(inod)*yy(inod) )                &
     &                      * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sp_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sz_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_sz, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_sz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txz, tyz
!
!
!$omp do private(inod,ist,ied,txz,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txz = tensor(inod,3)
           tyz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             t_sz(inod) = txz
           else
             t_sz(inod) =     (  txz * xx(inod)                         &
     &                     +     tyz * yy(inod) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sz_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pp_cyl_tensor_smp(np_smp, numnod, inod_smp_stack,  &
     &          tensor, t_pp, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_pp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txx, txy, tyy
!
!
!$omp do private(inod,ist,ied,txx,txy,tyy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txx = tensor(inod,1)
           txy = tensor(inod,2)
           tyy = tensor(inod,4)
!
           if ( s(inod).eq.0.0 ) then
             t_pp(inod) = tyy
           else
             t_pp(inod) =     (  txx * yy(inod)*yy(inod)                &
     &                     - two*txy * xx(inod)*yy(inod)                &
     &                     +     tyy * xx(inod)*xx(inod) )              &
     &                      * a_s(inod) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_pp_cyl_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pz_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_pz, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: t_pz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: txz, tyz
!
!
!$omp do private(inod,ist,ied,txz,tyz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           txz = tensor(inod,3)
           tyz = tensor(inod,5)
!
           if ( s(inod).eq.0.0 ) then
             t_pz(inod) = tyz
           else
             t_pz(inod) =     ( -txz * yy(inod)                         &
     &                         + tyz * xx(inod) ) * a_s(inod)
           end if
!
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_pz_tensor_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_zz_tensor_smp(np_smp, numnod, inod_smp_stack,      &
     &          tensor, t_zz)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: t_zz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           t_zz(inod) = tensor(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_zz_tensor_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_tensor_2_cyl_smp
