!>@file   cvt_xyz_vector_2_cyl_smp.f90
!!@brief  module cvt_xyz_vector_2_cyl_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>@brief Convert symmetric tensor from Cartesian coordinate
!!       to cylindrical coordinate
!!
!!@verbatim
!!***********************************************************************
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vs =  vx*cos(phi) + vy*sin(phi)
!!*      vp = -vx*sin(phi) + vy*cos(phi)
!!*
!!***********************************************************************
!!
!!      subroutine cvt_vector_2_cyl_smp(np_smp, numnod,                 &
!!     &          inod_smp_stack, vect, v_cyl, xx, yy, rs, a_s)
!!
!!      subroutine overwrite_vector_2_cyl_smp(np_smp, numnod,           &
!!     &          inod_smp_stack, vect, xx, yy, rs, a_s)
!!
!!      subroutine cal_cylinder_r_comp_smp(np_smp, numnod,              &
!!     &          inod_smp_stack, vect, v_s, xx, yy, rs, a_s)
!!
!!***********************************************************************
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
!!@n @param  vect(numnod,3) vector in Cartesian coordinate
!!
!!@n @param  v_cyl(numnod,3) vector in cylindrical coordinate
!!@n @param  v_s(numnod) s component of vector in Cartesian coordinate
!
      module cvt_xyz_vector_2_cyl_smp
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
      subroutine cvt_vector_2_cyl_smp(np_smp, numnod,                   &
     &          inod_smp_stack, vect, v_cyl, xx, yy, rs, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_cyl(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( rs(inod).eq.0.0d0 ) then
             v_cyl(inod,1) = vx
             v_cyl(inod,2) = vy
           else
             v_cyl(inod,1) = ( vx*xx(inod) + vy*yy(inod)) * a_s(inod)
             v_cyl(inod,2) = (-vx*yy(inod) + vy*xx(inod)) * a_s(inod)
           end if
!
           v_cyl(inod,3) = vz
         end do
       end do
!$omp end do nowait
!
      end subroutine cvt_vector_2_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_vector_2_cyl_smp(np_smp, numnod,             &
     &          inod_smp_stack, vect, xx, yy, rs, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy
!
!
!$omp do private(inod,ist,ied,vx,vy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
!
           if ( rs(inod).eq.0.0d0 ) then
             vect(inod,1) = vx
             vect(inod,2) = vy
           else
             vect(inod,1) = ( vx*xx(inod) + vy*yy(inod)) * a_s(inod)
             vect(inod,2) = (-vx*yy(inod) + vy*xx(inod)) * a_s(inod)
           end if
!
         end do
       end do
!$omp end do nowait
!
      end subroutine overwrite_vector_2_cyl_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_cylinder_r_comp_smp(np_smp, numnod,                &
     &          inod_smp_stack, vect, v_s, xx, yy, rs, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: rs(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_s(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy
!
!
!$omp do private(inod,ist,ied,vx,vy)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
!
           if ( rs(inod).eq.0.0d0 ) then
             v_s(inod) = vx
           else
             v_s(inod) = (vx * xx(inod) + vy * yy(inod)) * a_s(inod)
           end if
!
         end do
       end do
!$omp end do nowait
!
      end subroutine cal_cylinder_r_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_vector_2_cyl_smp
