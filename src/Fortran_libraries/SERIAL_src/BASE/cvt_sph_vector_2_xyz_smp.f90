!>@file   cvt_sph_vector_2_xyz_smp.f90
!!@brief  module cvt_sph_vector_2_xyz_smp
!!
!!@author H. Matsui
!!@date Programmed in June, 2005
!
!>@brief Convert vector from spherical coordinate
!!       to Cartesian coordinate
!!
!!@verbatim
!!*********************************************************************
!!*
!!*   convert vector from spherical coordinate to certecian coordinate
!!*      vx =  vr*sin(th)*cos(phi) + vt*cos(th)*cos(phi) - vp*sin(phi)
!!*      vy =  vr*sin(th)*sin(phi) + vt*cos(th)*sin(phi) + vp*cos(phi)
!!*      vz =  vr*cos(th) - vt*sin(th)
!!*
!!*********************************************************************
!!
!!      subroutine cvt_sph_vect_2_xyz_smp(np_smp, numnod,               &
!!     &          inod_smp_stack, vect, v_sph, theta, phi)
!!
!!      subroutine overwrite_sph_vect_2_xyz_smp(np_smp, numnod,         &
!!     &          inod_smp_stack, vect, theta, phi)
!!
!!      subroutine cal_sph_2_x_comp_smp(np_smp, numnod, inod_smp_stack, &
!!     &          v_x, v_sph, theta, phi)
!!      subroutine cal_sph_2_y_comp_smp(np_smp, numnod, inod_smp_stack, &
!!     &          v_y, v_sph, theta, phi)
!!      subroutine cal_sph_2_z_comp_smp(np_smp, numnod, inod_smp_stack, &
!!     &          v_z, v_sph, theta)
!!
!!*********************************************************************
!!@endverbatim
!!
!!@n @param  np_smp   Number of SMP processes
!!@n @param  numnod   Number of data points
!!@n @param  inod_smp_stack(0:np_smp)
!!                    End address of each SMP process
!!@n @param  theta(numnod)  colatitude
!!@n @param  phi(numnod)    longitude
!!
!!@n @param  v_sph(numnod,3) vector in spherical coordinate
!!
!!@n @param  vect(numnod,3) vector in Cartesian coordinate
!!@n @param  v_x(numnod) x component of vector in Cartesian coordinate
!!@n @param  v_y(numnod) y component of vector in Cartesian coordinate
!!@n @param  v_z(numnod) z component of vector in Cartesian coordinate
!
      module cvt_sph_vector_2_xyz_smp
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
      subroutine cvt_sph_vect_2_xyz_smp(np_smp, numnod,                 &
     &          inod_smp_stack, vect, v_sph, theta, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(inout) :: vect(numnod,3)
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp do private(inod,ist,ied,vr,vt,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vr = v_sph(inod,1)
          vt = v_sph(inod,2)
          vp = v_sph(inod,3)
!
          vect(inod,1) = ( vr * sin( theta(inod) )*cos( phi(inod) )     &
     &                   + vt * cos( theta(inod) )*cos( phi(inod) )     &
     &                   - vp * sin( phi(inod) )   )
!
          vect(inod,2) = ( vr * sin( theta(inod) )*sin( phi(inod) )     &
     &                   + vt * cos( theta(inod) )*sin( phi(inod) )     &
     &                   + vp * cos( phi(inod) )   )
!
          vect(inod,3) = ( vr * cos( theta(inod) )                      &
     &                   - vt * sin( theta(inod) ) )
!
         end do
       end do
!$omp end do nowait
!
      end subroutine cvt_sph_vect_2_xyz_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine overwrite_sph_vect_2_xyz_smp(np_smp, numnod,           &
     &          inod_smp_stack, vect, theta, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       real(kind=kreal), intent(inout) :: vect(numnod,3)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp do private(inod,ist,ied,vr,vt,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vr = vect(inod,1)
          vt = vect(inod,2)
          vp = vect(inod,3)
!
          vect(inod,1) = ( vr * sin( theta(inod) )*cos( phi(inod) )     &
     &                   + vt * cos( theta(inod) )*cos( phi(inod) )     &
     &                   - vp * sin( phi(inod) )   )
!
          vect(inod,2) = ( vr * sin( theta(inod) )*sin( phi(inod) )     &
     &                   + vt * cos( theta(inod) )*sin( phi(inod) )     &
     &                   + vp * cos( phi(inod) )   )
!
          vect(inod,3) = ( vr * cos( theta(inod) )                      &
     &                   - vt * sin( theta(inod) ) )
!
         end do
       end do
!$omp end do nowait
!
      end subroutine overwrite_sph_vect_2_xyz_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_2_x_comp_smp(np_smp, numnod, inod_smp_stack,   &
     &          v_x, v_sph, theta, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in)    :: v_sph(numnod,3)
       real(kind=kreal), intent(inout) :: v_x(numnod)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(in) :: phi(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp do private(inod,ist,ied,vr,vt,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vr = v_sph(inod,1)
          vt = v_sph(inod,2)
          vp = v_sph(inod,3)
!
          v_x(inod) = ( vr * sin(theta(inod))*cos(phi(inod))            &
     &                + vt * cos(theta(inod))*cos(phi(inod))            &
     &                - vp * sin(phi(inod))   )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sph_2_x_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_2_y_comp_smp(np_smp, numnod, inod_smp_stack,   &
     &          v_y, v_sph, theta, phi)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(inout) :: v_y(numnod)
       real(kind=kreal), intent(in) :: theta(numnod), phi(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vr, vt, vp
!
!
!$omp do private(inod,ist,ied,vr,vt,vp)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vr = v_sph(inod,1)
          vt = v_sph(inod,2)
          vp = v_sph(inod,3)
!
          v_y(inod) = ( vr * sin(theta(inod))*sin(phi(inod))            &
     &                + vt * cos(theta(inod))*sin(phi(inod))            &
     &                + vp * cos(phi(inod) ))
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sph_2_y_comp_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_2_z_comp_smp(np_smp, numnod, inod_smp_stack,   &
     &          v_z, v_sph, theta)
!
       integer (kind = kint), intent(in) :: np_smp, numnod
       integer (kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
       real(kind=kreal), intent(in) :: v_sph(numnod,3)
       real(kind=kreal), intent(in) :: theta(numnod)
       real(kind=kreal), intent(inout) :: v_z(numnod)
!
       integer (kind = kint) :: ip, inod, ist, ied
       real(kind=kreal) :: vr, vt
!
!
!$omp do private(inod,ist,ied,vr,vt)
      do ip = 1, np_smp
        ist = inod_smp_stack(ip-1) + 1
        ied = inod_smp_stack(ip)
        do inod = ist, ied
          vr = v_sph(inod,1)
          vt = v_sph(inod,2)
!
          v_z(inod) = ( vr * cos( theta(inod) )                         &
     &                - vt * sin( theta(inod) ) )
        end do
      end do
!$omp end do nowait
!
      end subroutine cal_sph_2_z_comp_smp
!
! -----------------------------------------------------------------------
!
      end module cvt_sph_vector_2_xyz_smp
