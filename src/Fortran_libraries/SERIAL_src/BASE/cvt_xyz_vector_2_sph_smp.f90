!>@file   cvt_xyz_vector_2_sph_smp.f90
!!        module cvt_xyz_vector_2_sph_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!!
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
!!      subroutine cvt_vector_2_sph_smp(numnod, vect, v_sph, xx, yy, zz,&
!!     &                                r, s, a_r, a_s)
!!
!!      subroutine overwrite_vector_2_sph_smp(numnod, vect, xx, yy, zz, &
!!     &                                      r, s, a_r, a_s)
!!
!!      subroutine cal_radial_comp_smp(numnod, vect, v_r,               &
!!     &                               xx, yy, zz, r, a_r)
!!      subroutine cal_theta_comp_smp(numnod, vect, v_theta,            &
!!     &                              xx, yy, zz, r, s, a_r, a_s)
!!      subroutine cal_phi_comp_smp(numnod, vect, v_phi, xx, yy, s, a_s)
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
      subroutine cvt_vector_2_sph_smp(numnod, vect, v_sph, xx, yy, zz,  &
     &                                r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod), s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_sph(numnod,3)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp do private(inod,vx,vy,vz)
         do inod = 1, numnod
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
             v_sph(inod,1) = vz * zz(inod) * a_r(inod)
             v_sph(inod,2) = vx * zz(inod) * a_r(inod)
             v_sph(inod,3) = vy
           else
             v_sph(inod,1) = (vx*xx(inod) + vy*yy(inod) + vz*zz(inod))  &
     &                        * a_r(inod)
             v_sph(inod,2) = ( vx * zz(inod)*xx(inod)                   &
     &                       + vy * zz(inod)*yy(inod)                   &
     &                       - vz * s(inod)  * s(inod)  )               &
     &                        * a_r(inod) * a_s(inod)
             v_sph(inod,3) = ( -vx * yy(inod) + vy * xx(inod) )         &
     &                        * a_s(inod)
          end if
!
        end do
!$omp end do nowait
!
      end subroutine cvt_vector_2_sph_smp
!
! -----------------------------------------------------------------------
!
      subroutine overwrite_vector_2_sph_smp(numnod, vect, xx, yy, zz,   &
     &                                      r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod), s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_s(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp do private(inod,vx,vy,vz)
         do inod = 1, numnod
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
             vect(inod,1) = vz*zz(inod) * a_r(inod)
             vect(inod,2) = vx*zz(inod) * a_r(inod)
             vect(inod,3) = vy
           else
             vect(inod,1) =  (vx*xx(inod) + vy*yy(inod) + vz*zz(inod))  &
     &                      * a_r(inod)
             vect(inod,2) =  (  vx * zz(inod)*xx(inod)                  &
     &                        + vy * zz(inod)*yy(inod)                  &
     &                        - vz * s(inod)  *s(inod)  )               &
     &                         * a_r(inod) * a_s(inod)
             vect(inod,3) =  ( -vx * yy(inod) + vy * xx(inod))          &
     &                      * a_s(inod)
          end if
!
        end do
!$omp end do nowait
!
      end subroutine overwrite_vector_2_sph_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_radial_comp_smp(numnod, vect, v_r,                 &
     &                               xx, yy, zz, r, a_r)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod)
!
       real(kind=kreal), intent(inout) :: v_r(numnod)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp do private(inod,vx,vy,vz)
         do inod = 1, numnod
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_r(inod) =  vz
           else
             v_r(inod) = (vx*xx(inod) + vy*yy(inod) + vz*zz(inod))      &
     &                  * a_r(inod)
           end if
!
        end do
!$omp end do nowait
!
      end subroutine cal_radial_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_theta_comp_smp(numnod, vect, v_theta,              &
     &                              xx, yy, zz, r, s, a_r, a_s)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: zz(numnod)
       real(kind=kreal), intent(in) :: r(numnod), s(numnod)
       real(kind=kreal), intent(in) :: a_r(numnod), a_s(numnod)
!
       real(kind=kreal), intent(inout) :: v_theta(numnod)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp do private(inod,vx,vy,vz)
         do inod = 1, numnod
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( r(inod).eq.0.0 ) then
             v_theta(inod) = vx
           else if ( s(inod).eq.0.0d0 ) then
             v_theta(inod) = vx*zz(inod) * a_r(inod)
           else
             v_theta(inod) = ( vx * zz(inod)*xx(inod)                   &
     &                       + vy * zz(inod)*yy(inod)                   &
     &                       - vz * s(inod)  * s(inod)  )               &
     &                        * a_r(inod) * a_s(inod)
          end if
!
        end do
!$omp end do nowait
!
      end subroutine cal_theta_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_phi_comp_smp(numnod, vect, v_phi, xx, yy, s, a_s)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: vect(numnod,3)
       real(kind=kreal), intent(inout) :: v_phi(numnod)
       real(kind=kreal), intent(in)    :: xx(numnod), yy(numnod)
       real(kind=kreal), intent(in) :: s(numnod)
       real(kind=kreal), intent(in) :: a_s(numnod)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vx, vy, vz
!
!
!$omp do private(inod,vx,vy,vz)
         do inod = 1, numnod
           vx = vect(inod,1)
           vy = vect(inod,2)
           vz = vect(inod,3)
!
           if ( s(inod).eq.0.0d0 ) then
             v_phi(inod) = vy
           else
             v_phi(inod) = (-vx*yy(inod) + vy*xx(inod)) * a_s(inod)
           end if
!
        end do
!$omp end do nowait
!
      end subroutine cal_phi_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_xyz_vector_2_sph_smp
