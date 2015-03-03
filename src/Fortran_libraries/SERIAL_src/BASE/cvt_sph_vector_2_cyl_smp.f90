!>@file   cvt_sph_vector_2_cyl_smp.f90
!!@brief  module cvt_sph_vector_2_cyl_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>@brief Convert vector from spherical coordinate
!!       to cylindrical coordinate
!!
!!@verbatim
!!*********************************************************************
!!*
!!*   convert vector from spherical coordinate to cylindrical coordinate
!!*      vs =  vr*sin(th) + vt*cos(th)
!!*      vp =                           vp
!!*      vz =  vr*cos(th) - vt*sin(th)
!!*
!!*********************************************************************
!!
!!      subroutine cvt_sph_vect_2_cyl_smp(np_smp, numnod,               &
!!     &          inod_smp_stack, v_cyl, v_sph, theta)
!!
!!      subroutine overwrite_sph_vect_2_cyl_smp(np_smp, numnod,         &
!!     &          inod_smp_stack, vect, theta)
!!
!!      subroutine cal_sph_2_s_comp_smp(np_smp, numnod, inod_smp_stack, &
!!     &          v_s, v_sph, theta)
!!
!!*********************************************************************
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  theta(numnod)  colatitude
!!
!!@n @param  v_sph(numnod,3) vector in spherical coordinate
!!
!!@n @param  v_cyl(numnod,3) vector in cylindrical coordinate
!!@n @param  v_s(numnod) s component of vector in cylindrical coordinate
!
      module cvt_sph_vector_2_cyl_smp
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cvt_sph_vect_2_cyl_smp(np_smp, numnod,                 &
     &          inod_smp_stack, v_cyl, v_sph, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: v_cyl(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          v_cyl(inod,1) =  v_sph(inod,1) * sin( theta(inod) )           &
     &                   + v_sph(inod,2) * cos( theta(inod) )
          v_cyl(inod,2) = v_sph(inod,3)
          v_cyl(inod,3) =  v_sph(inod,1) * cos( theta(inod) )           &
     &                   - v_sph(inod,2) * sin( theta(inod) )
         end do
       end do
!$omp end parallel do
!
      end subroutine cvt_sph_vect_2_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_vect_2_cyl_smp(np_smp, numnod,           &
     &          inod_smp_stack, vect, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp parallel do private(inod,ist,ied,vr,vt,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vr = vect(inod,1)
          vt = vect(inod,2)
          vp = vect(inod,3)
!
          vect(inod,1) =  vr * sin( theta(inod) )                       &
     &                  + vt * cos( theta(inod) )
          vect(inod,2) =  vp
          vect(inod,3) =  vr * cos( theta(inod) )                       &
     &                  - vt * sin( theta(inod) )
         end do
       end do
!$omp end parallel do
!
      end subroutine overwrite_sph_vect_2_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_2_s_comp_smp(np_smp, numnod, inod_smp_stack,   &
     &          v_s, v_sph, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in)    :: v_sph(numnod,3)
!
       real(kind=kreal), intent(inout) :: v_s(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(inod,ist,ied)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          v_s(inod) =      v_sph(inod,1) * sin( theta(inod) )           &
     &                   + v_sph(inod,2) * cos( theta(inod) )
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sph_2_s_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_sph_vector_2_cyl_smp
