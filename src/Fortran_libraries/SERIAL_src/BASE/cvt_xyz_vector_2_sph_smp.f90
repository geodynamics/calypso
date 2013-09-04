!>@file   cvt_xyz_vector_2_sph_smp.f90
!!@brief  module cvt_xyz_vector_2_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>@brief Convert vector from Cartesian coordinate
!!       to cylindrical coordinate
!!
!!@verbatim
!!***********************************************************************
!!*
!!*   convert vector from certecian coordinate to spherical coordinate
!!*      vr =  vx*sin(th)*cos(phi) + vy*sin(th)*sin(phi) + vz*cos(phi)
!!*      vt =  vx*cos(th)*cos(phi) + vy*cos(th)*sin(phi) - vz*sin(phi)
!!*      vp = -vx*sin(phi) + vy*cos(phi)
!!*
!!*   convert vector from certecian coordinate to cylindrical coordinate
!!*      vs =  vx*cos(phi) + vy*sin(phi)
!!*
!!***********************************************************************
!!
!!      subroutine cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack, &
!!     &          vect, v_sph, xx, r, s, a_r, a_s)
!!
!!      subroutine overwrite_vector_2_sph_smp(np_smp, numnod,           &
!!     &          inod_smp_stack, vect, xx, r, s, a_r, a_s)
!!
!!      subroutine cal_radial_comp_smp(np_smp, numnod, inod_smp_stack,  &
!!     &          vect, v_r, xx, r, a_r)
!!      subroutine cal_theta_comp_smp(np_smp, numnod, inod_smp_stack,   &
!!     &          vect, v_theta, xx, r, s, a_r, a_s)
!!      subroutine cal_phi_comp_smp(np_smp, numnod, inod_smp_stack,     &
!!     &          vect, v_phi, xx, s, a_s)
!!***********************************************************************
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
!!@n @param  vect(numnod,3) vector in Cartesian coordinate
!!
!!@n @param  v_sph(numnod,3) vector in spherical coordinate
!!@n @param  v_r(numnod)   radial component of vector
!!                         in spherical coordinate
!!@n @param  v_theta(numnod)  meridional component of vector
!!                         in spherical coordinate
!!@n @param  v_phi(numnod)  zonal component of vector
!!                         in spherical coordinate
!
      module cvt_xyz_vector_2_sph_smp
!
      use m_precision
!
      implicit none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cvt_vector_2_sph_smp(np_smp, numnod, inod_smp_stack,   &
     &          vect, v_sph, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod), s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_sph(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp parallel do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_sph(inod,1) = vz
             v_sph(inod,2) = vx
             v_sph(inod,3) = vy
!
           else if ( s(inod).eq.0.0d0 ) then
             v_sph(inod,1) = vz * xx(inod,3) * a_r(inod)
             v_sph(inod,2) = vx * xx(inod,3) * a_r(inod)
             v_sph(inod,3) = vy
!
           else
!
             v_sph(inod,1) = ( vx * xx(inod,1)                  &
     &                       + vy * xx(inod,2)                  &
     &                       + vz * xx(inod,3) )                &
     &                        * a_r(inod)
!
             v_sph(inod,2) = ( vx * xx(inod,3)*xx(inod,1)       &
     &                       + vy * xx(inod,3)*xx(inod,2)       &
     &                       - vz * s(inod)  * s(inod)  )       &
     &                        * a_r(inod) * a_s(inod)
!
             v_sph(inod,3) = ( -vx * xx(inod,2)                &
     &                        + vy * xx(inod,1) )              &
     &                        * a_s(inod)
!
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cvt_vector_2_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_vector_2_sph_smp(np_smp, numnod,             &
     &          inod_smp_stack, vect, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod), s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_s(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp parallel do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             vect(inod,1) = vz
             vect(inod,2) = vx
             vect(inod,3) = vy
!
           else if ( s(inod).eq.0.0d0 ) then
             vect(inod,1) = vz*xx(inod,3) * a_r(inod)
             vect(inod,2) = vx*xx(inod,3) * a_r(inod)
             vect(inod,3) = vy
!
           else
!
             vect(inod,1) =  (  vx * xx(inod,1)                         &
     &                        + vy * xx(inod,2)                         &
     &                        + vz * xx(inod,3) )                       &
     &                         * a_r(inod)
!
             vect(inod,2) =  (  vx * xx(inod,3)*xx(inod,1)              &
     &                        + vy * xx(inod,3)*xx(inod,2)              &
     &                        - vz * s(inod)  *s(inod)  )               &
     &                         * a_r(inod) * a_s(inod)
!
             vect(inod,3) =  ( -vx * xx(inod,2)                         &
     &                         +vy * xx(inod,1) )                       &
     &                        * a_s(inod)
!
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine overwrite_vector_2_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_radial_comp_smp(np_smp, numnod, inod_smp_stack,    &
     &          vect, v_r, xx, r, a_r)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: v_r(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp parallel do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_r(inod) =       vz
           else
             v_r(inod) =     ( vx * xx(inod,1)                          &
     &                       + vy * xx(inod,2)                          &
     &                       + vz * xx(inod,3) )                        &
     &                        * a_r(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_radial_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_theta_comp_smp(np_smp, numnod, inod_smp_stack,     &
     &          vect, v_theta, xx, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod,3)
       real(kind=kreal), intent(in) :: r(numnod), s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_theta(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp parallel do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_theta(inod) = vx
           else if ( s(inod).eq.0.0d0 ) then
             v_theta(inod) = vx*xx(inod,3) * a_r(inod)
           else
             v_theta(inod) = ( vx * xx(inod,3)*xx(inod,1)               &
     &                       + vy * xx(inod,3)*xx(inod,2)               &
     &                       - vz * s(inod)  * s(inod)  )               &
     &                        * a_r(inod) * a_s(inod)
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_theta_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_phi_comp_smp(np_smp, numnod, inod_smp_stack,       &
     &          vect, v_phi, xx, s, a_s)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_phi(numnod)
       real(kind=kreal), intent(in)    :: xx(numnod,3)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp parallel do private(inod,ist,ied,vx,vy,vz)
       do ip = 1, np_smp
         ist = inod_smp_stack(ip-1) + 1
         ied = inod_smp_stack(ip)
         do inod = ist, ied
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( s(inod).eq.0.0d0 ) then
             v_phi(inod) =   vy
           else
             v_phi(inod) =   ( -vx * xx(inod,2) + vy * xx(inod,1) )     &
     &                        * a_s(inod)
           end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_phi_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_vector_2_sph_smp
