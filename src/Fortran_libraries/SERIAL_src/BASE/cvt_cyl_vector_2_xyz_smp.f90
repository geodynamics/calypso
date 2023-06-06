!>@file   cvt_cyl_vector_2_xyz_smp.f90
!!        module cvt_cyl_vector_2_xyz_smp
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!!
!>@brief Convert symmetric tensor from cylindrical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!*********************************************************************
!!*
!!*   convert vector from spherical coordinate to certecian coordinate
!!*      vx =  vs*cos(phi) - vp*sin(phi)
!!*      vy =  vs*sin(phi) + vp*cos(phi)
!!*      vz =  vz
!!*
!!*********************************************************************
!!
!!      subroutine cvt_cyl_vect_2_xyz_smp(numnod, vect, v_cyl, phi)
!!
!!      subroutine overwrite_cyl_vect_2_xyz_smp(numnod, vect, phi)
!!
!!      subroutine cvt_cyl_vect_2_x_comp_smp(numnod, v_x, v_cyl, phi)
!!      subroutine cvt_cyl_vect_2_y_comp_smp(numnod, v_y, v_cyl, phi)
!!
!!*********************************************************************
!!@endverbatim
!!
!!@n @param  numnod   Number of data points
!!@n @param  phi(numnod)  longitude
!!
!!@n @param  v_cyl(numnod,3) vector in cylindrical coordinate
!!
!!@n @param  vect(numnod,3) vector in Cartesian coordinate
!!@n @param  v_x(numnod) x component of vector in Cartesian coordinate
!!@n @param  v_y(numnod) y component of vector in Cartesian coordinate
!!@n @param  v_z(numnod) z component of vector in Cartesian coordinate
!
      module cvt_cyl_vector_2_xyz_smp
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
      subroutine cvt_cyl_vect_2_xyz_smp(numnod, vect, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(inout) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vs, vp
!
!
!$omp do private(inod,vs,vp)
        do inod = 1, numnod
          vs = v_cyl(inod,1)
          vp = v_cyl(inod,2)
!
          vect(inod,1) = ( vs * cos( phi(inod) )                        &
     &                   - vp * sin( phi(inod) ) )
          vect(inod,2) = ( vs * sin( phi(inod) )                        &
     &                   + vp * cos( phi(inod) ) )
          vect(inod,3) = v_cyl(inod,3)
!
        end do
!$omp end do nowait
!
      end subroutine cvt_cyl_vect_2_xyz_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_cyl_vect_2_xyz_smp(numnod, vect, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: phi(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vs, vp
!
!
!$omp do private(inod,vs,vp)
        do inod = 1, numnod
          vs = vect(inod,1)
          vp = vect(inod,2)
!
          vect(inod,1) = ( vs * cos( phi(inod) )                        &
     &                   - vp * sin( phi(inod) ) )
          vect(inod,2) = ( vs * sin( phi(inod) )                        &
     &                   + vp * cos( phi(inod) ) )
        end do
!$omp end do nowait
!
      end subroutine overwrite_cyl_vect_2_xyz_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cvt_cyl_vect_2_x_comp_smp(numnod, v_x, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in)    :: v_cyl(numnod,2)
       real(kind=kreal), intent(inout) :: v_x(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vs, vp
!
!
!$omp do private(inod,vs,vp)
        do inod = 1, numnod
          vs = v_cyl(inod,1)
          vp = v_cyl(inod,2)
!
          v_x(inod) = ( vs * cos( phi(inod) )                           &
     &                - vp * sin( phi(inod) ) )
        end do
!$omp end do nowait
!
      end subroutine cvt_cyl_vect_2_x_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cvt_cyl_vect_2_y_comp_smp(numnod, v_y, v_cyl, phi)
!
       integer (kind = kint), intent(in) :: numnod
       real(kind=kreal), intent(in) :: v_cyl(numnod,3)
       real(kind=kreal), intent(inout) :: v_y(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: inod
       real(kind=kreal) :: vs, vp
!
!
!$omp do private(inod,vs,vp)
        do inod = 1, numnod
          vs = v_cyl(inod,1)
          vp = v_cyl(inod,2)
!
          v_y(inod) = ( vs * sin( phi(inod) )                           &
     &                + vp * cos( phi(inod) ) )
        end do
!$omp end do nowait
!
      end subroutine cvt_cyl_vect_2_y_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_cyl_vector_2_xyz_smp
