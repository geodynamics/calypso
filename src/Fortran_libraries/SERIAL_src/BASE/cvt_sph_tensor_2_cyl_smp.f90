!>@file   cvt_sph_tensor_2_cyl_smp.f90
!!@brief  module cvt_sph_tensor_2_cyl_smp
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!
!>@brief Convert symmetric tensor from Cartesian coordinate
!!       to cylindrical coordinate
!!
!!@verbatim
!!      subroutine cal_cyl_tensor_by_sph_smp(np_smp, numnod,            &
!!     &          inod_smp_stack, tensor, tcyl, theta)
!!
!!      subroutine overwrite_cyl_tensor_by_sph_smp(np_smp, numnod,      &
!!     &          inod_smp_stack, tensor, theta)
!!
!!      subroutine cal_ss_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_ss, theta)
!!      subroutine cal_sp_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_sp, theta)
!!      subroutine cal_sz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_sz, theta)
!!      subroutine cal_pp_cyl_tensor_by_sph_smp(np_smp, numnod,         &
!!     &          inod_smp_stack, tensor, t_pp)
!!      subroutine cal_pz_tensor_by_sph_smp(np_smp, numnod,             &
!!     &          inod_smp_stack, tensor, t_pz, theta)
!!      subroutine cal_cyl_zz_tensor_by_sph_smp(np_smp, numnod,         &
!!     &          inod_smp_stack, tensor, t_zz)
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  theta(numnod)  colatitude
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
      module cvt_sph_tensor_2_cyl_smp
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
      subroutine cal_cyl_tensor_by_sph_smp(np_smp, numnod,              &
     &          inod_smp_stack, tensor, tcyl, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: tcyl(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
             tcyl(inod,1)                                               &
     &          =    trr * sin( theta(inod) ) * sin( theta(inod) )      &
     &         + two*trt * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttt * cos( theta(inod) ) * cos( theta(inod) )
!
             tcyl(inod,2)                                               &
     &           =   trr * cos( theta(inod) ) *sin( theta(inod) )       &
     &         +     trt * (cos( theta(inod) )*cos( theta(inod) )       &
     &                    - sin( theta(inod) )*sin( theta(inod) ))      &
     &         -     ttt * sin( theta(inod) ) * cos( theta(inod) )
!
             tcyl(inod,3)                                               &
     &           =   trp * sin( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttp * cos( theta(inod) ) 
!
             tcyl(inod,4) = tpp
!
             tcyl(inod,5)                                               &
     &           =   trp * cos( theta(inod) )                           &
     &         -     ttp * sin( theta(inod) )
!
             tcyl(inod,6)                                               &
     &           =   trr * cos( theta(inod) ) * cos( theta(inod) )      &
     &         - two*trt * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttt * sin( theta(inod) ) * sin( theta(inod) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cyl_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_tensor_by_sph_smp(np_smp, numnod,        &
     &          inod_smp_stack, tensor, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: tensor(numnod,6)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, trp, ttt, ttp, tpp
!
!
!$omp do private(inod,ist,ied,trr,trt,trp,ttt,ttp,tpp)
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
           tensor(inod,1)                                               &
     &          =    trr * sin( theta(inod) ) * sin( theta(inod) )      &
     &         + two*trt * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttt * cos( theta(inod) ) * cos( theta(inod) )
!
           tensor(inod,2)                                               &
     &           =   trr * cos( theta(inod) ) *sin( theta(inod) )       &
     &         +     trt * (cos( theta(inod) )*cos( theta(inod) )       &
     &                    - sin( theta(inod) )*sin( theta(inod) ))      &
     &         -     ttt * sin( theta(inod) ) * cos( theta(inod) )
!
           tensor(inod,3)                                               &
     &           =   trp * sin( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttp * cos( theta(inod) ) 
!
           tensor(inod,4) = tpp
!
           tensor(inod,5)                                               &
     &           =   trp * cos( theta(inod) )                           &
     &         -     ttp * sin( theta(inod) )
!
           tensor(inod,6)                                               &
     &           =   trr * cos( theta(inod) ) * cos( theta(inod) )      &
     &         - two*trt * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttt * sin( theta(inod) ) * sin( theta(inod) )
        end do
      end do
!$omp end do nowait
!
      end subroutine overwrite_cyl_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_ss_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_ss, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_ss(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, ttt
!
!
!$omp do private(inod,ist,ied,trr,trt,ttt)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           ttt = tensor(inod,4)
!
             t_ss(inod)                                                 &
     &          =    trr * sin( theta(inod) ) * sin( theta(inod) )      &
     &         + two*trt * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttt * cos( theta(inod) ) * cos( theta(inod) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_ss_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_sp, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_sp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, ttt
!
!
!$omp do private(inod,ist,ied,trr,trt,ttt)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           ttt = tensor(inod,4)
!
             t_sp(inod)                                                 &
     &           =   trr * cos( theta(inod) ) *sin( theta(inod) )       &
     &         +     trt * (cos( theta(inod) )*cos( theta(inod) )       &
     &                    - sin( theta(inod) )*sin( theta(inod) ))      &
     &         -     ttt * sin( theta(inod) ) * cos( theta(inod) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sp_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sz_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_sz, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_sz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trp, ttp
!
!
!$omp do private(inod,ist,ied,trr,trp,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trp = tensor(inod,3)
           ttp = tensor(inod,5)
!
             t_sz(inod)                                                 &
     &           =   trp * sin( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttp * cos( theta(inod) ) 
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pp_cyl_tensor_by_sph_smp(np_smp, numnod,           &
     &          inod_smp_stack, tensor, t_pp)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
!
       real(kind=kreal), intent(inout) :: t_pp(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
             t_pp(inod) = tensor(inod,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_pp_cyl_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_pz_tensor_by_sph_smp(np_smp, numnod,               &
     &          inod_smp_stack, tensor, t_pz, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_pz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trp, ttp
!
!
!$omp do private(inod,ist,ied,trp,ttp)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trp = tensor(inod,3)
           ttp = tensor(inod,5)
!
             t_pz(inod)                                                 &
     &           =   trp * cos( theta(inod) )                           &
     &         -     ttp * sin( theta(inod) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_pz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_cyl_zz_tensor_by_sph_smp(np_smp, numnod,           &
     &          inod_smp_stack, tensor, t_zz, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: tensor(numnod,6)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: t_zz(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: trr, trt, ttt
!
!
!$omp do private(inod,ist,ied,trr,trt,ttt)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           trr = tensor(inod,1)
           trt = tensor(inod,2)
           ttt = tensor(inod,4)
!
             t_zz(inod)                                                 &
     &           =   trr * cos( theta(inod) ) * cos( theta(inod) )      &
     &         - two*trt * cos( theta(inod) ) * sin( theta(inod) )      &
     &         +     ttt * sin( theta(inod) ) * sin( theta(inod) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_cyl_zz_tensor_by_sph_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_sph_tensor_2_cyl_smp
