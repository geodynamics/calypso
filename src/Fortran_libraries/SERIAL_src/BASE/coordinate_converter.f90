!> @file  coordinate_converter.f90
!!      module coordinate_converter
!!
!! @author  H. Matsui
!! @date Programmed in Dec., 1997
!
!> @brief transfer position between Cartesian and spherical coordinates
!!
!!********************************************************************
!!
!!      subroutine position_2_sph (n, xx, r, theta, phi, a_r, rs, a_rs)
!!
!!       subroutine to convert position to spherical coordinate
!!
!!       n: number of points
!!      xx: position (x,y,z)
!!      r, theta, phi: position in spherical coordinate
!!      a_r = 1/r
!!      rs:  cylindrical radius
!!      a_rs = 1/rs
!!
!!********************************************************************
!!
!!      subroutine position_2_xyz(n, rr, theta, phi, xx, yy, zz)
!!
!!       subroutine to convert position from spherical coordinate
!!
!!      xx =  rr*sin(theta)*cos(phi)
!!      yy =  rr*sin(theta)*sin(phi)
!!      zz =  rr*cos(theta)
!!
!!********************************************************************
!!
!!      subroutine position_cyl_2_xyz(n, ss, phi, xx, yy)
!!
!!       subroutine to convert position from cylindrical coordinate
!!
!!********************************************************************
!!
!!      subroutine position_sph_2_cyl_radius(n, r, theta, rs, a_rs, a_r)
!!
!!       subroutine to convert position from spherical coordinate 
!!       to cylindrical radius
!!
!!********************************************************************
!!
!!      subroutine position_cyl_2_sph(n, zz, rs, r, theta, a_r, a_rs)
!!
!!       subroutine to convert position from cylindrical coordinate
!!       to spherical coordinate
!!
!!********************************************************************
!!@endverbatim
!!
!!@n @param  n         Number of points
!!@n @param  xx(n,3)   Position @f$ (x,y,z) @f$
!!@n @param  r(n)      Radius   @f$ r @f$
!!@n @param  theta(n)  Colatitude  @f$ \theta @f$
!!@n @param  phi(n)    Longitude   @f$ \phi @f$
!!@n @param  a_r(n)     @f$ 1/ r @f$
!!@n @param  rs(n)     Cylindrical radius  @f$ s @f$
!!@n @param  a_rs(n)    @f$ 1 / s @f$
!!
!!@n @param  xx(n)   position @f$ x @f$
!!@n @param  yy(n)   position @f$ y @f$
!!@n @param  zz(n)   position @f$ z @f$
!
!
      module coordinate_converter
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine position_2_sph (n, xx, r, theta, phi, a_r, rs, a_rs )
!
      integer (kind = kint), intent(in)    :: n
      real ( kind = kreal ), intent(in)    :: xx(n,3)
      real ( kind = kreal ), intent(inout) :: r(n), theta(n), phi(n)
      real ( kind = kreal ), intent(inout) :: a_r(n), rs(n), a_rs(n)
!
      integer (kind = kint ) :: inod
      real ( kind = kreal) :: ratio_x, ratio_z
      real ( kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
!$omp parallel do private(ratio_x,ratio_z)
      do inod = 1, n
!
        r(inod) = sqrt( xx(inod,1)**2 + xx(inod,2)**2 + xx(inod,3)**2 )
        rs(inod) = sqrt(xx(inod,1)**2+xx(inod,2)**2)
!
        if ( r(inod).eq.0.0 ) then
          theta(inod) = 0.0
          phi(inod) = 0.0
          a_r(inod) =  1.0d30
          a_rs(inod) = 1.0d30
!
        else if ( rs(inod).eq.0.0d0 ) then
!
          a_r(inod) =  1.0d0 / r(inod)
          a_rs(inod) = 1.0d30
          if ( xx(inod,3).ge. 0.0d0 ) then
            theta(inod) = 0.0d0
          else
            theta(inod) = pi
          end if
          phi(inod) = 0.0d0
!
        else
!
          a_r(inod) = 1.0d0 / r(inod)
          a_rs(inod) = 1.0d0 / rs(inod)
          ratio_x = xx(inod,1) * a_rs(inod)
          ratio_z = xx(inod,3) * a_r(inod)
!
          if (ratio_z .gt. 1.0d0) then
            theta(inod) = 0.0d0
          else if (ratio_z .lt.-1.0d0) then
            theta(inod) = pi
          else
            theta(inod) = acos(ratio_z)
          end if
!
          if ( xx(inod,2).eq.0.0d0 ) then
            if ( xx(inod,1).ge. 0.0d0 ) then
              phi(inod) = 0.0d0
            else
              phi(inod) = pi
            end if
          else if ( ratio_x .ge. 1.0d0 ) then
            phi(inod) = 0.0d0
          else if ( ratio_x .le. -1.0d0 ) then
            phi(inod) = pi
          else if ( xx(inod,2) .ge. 0.0d0 ) then
            phi(inod) = acos (ratio_x)
          else if ( xx(inod,2) .lt. 0.0d0 ) then
            phi(inod) = 2.0d0*pi - acos (ratio_x)
          end if
!
        end if
!
      end do
!$omp end parallel do
!
      end subroutine position_2_sph
!
! ----------------------------------------------------------------------
!
      subroutine position_2_xyz(n, rr, theta, phi, xx, yy, zz)
!
      integer (kind = kint) :: n
      real(kind = kreal), intent(inout) :: xx(n), yy(n), zz(n)
      real(kind = kreal), intent(in) :: rr(n), theta(n), phi(n)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, n
        xx(inod) = rr(inod)*sin(theta(inod))*cos(phi(inod))
        yy(inod) = rr(inod)*sin(theta(inod))*sin(phi(inod))
        zz(inod) = rr(inod)*cos(theta(inod))
      end do
!$omp end parallel do
!*
      return
      end subroutine position_2_xyz
!*
! ----------------------------------------------------------------------
!
      subroutine position_cyl_2_xyz(n, ss, phi, xx, yy)
!
      integer (kind = kint) :: n
      real(kind = kreal), intent(inout) :: xx(n), yy(n)
      real(kind = kreal), intent(in) :: ss(n), phi(n)
!
      integer (kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, n
        xx(inod) = ss(inod)*cos(phi(inod))
        yy(inod) = ss(inod)*sin(phi(inod))
      end do
!$omp end parallel do
!*
      end subroutine position_cyl_2_xyz
!*
! ----------------------------------------------------------------------
!
      subroutine position_sph_2_cyl_radius(n, r, theta, rs, a_rs, a_r)
!
      integer (kind = kint), intent(in)    :: n
      real ( kind = kreal ), intent(in) :: r(n), theta(n)
      real ( kind = kreal ), intent(inout) :: rs(n), a_rs(n), a_r(n)
!
      integer (kind = kint ) :: inod
!
!
!$omp parallel
!$omp do
      do inod = 1, n
        if( r(inod) .eq. 0.0d0 ) then
          a_r(inod) = 1.0d99
        else
          a_r(inod) = one / r(inod)
        end if
      end do
!$omp end do nowait
!
!$omp do
      do inod = 1, n
        rs(inod) = r(inod) * sin(theta(inod))
        if( rs(inod) .eq. 0.0d0 ) then
          a_rs(inod) = 1.0d99
        else
          a_rs(inod) = one / rs(inod)
        end if
      end do
!$omp end do
!$omp end parallel
!
      end subroutine position_sph_2_cyl_radius
!
! ----------------------------------------------------------------------
!
      subroutine position_cyl_2_sph(n, zz, rs, r, theta, a_r, a_rs)
!
      integer (kind = kint), intent(in)    :: n
      real ( kind = kreal ), intent(in)    :: zz(n), rs(n)
      real ( kind = kreal ), intent(inout) :: r(n), theta(n)
      real ( kind = kreal ), intent(inout) :: a_r(n), a_rs(n)
!
      integer (kind = kint ) :: inod
      real ( kind = kreal) :: ratio_z
      real ( kind = kreal) :: pi
!
!
      pi = four * atan(one)
!
!$omp parallel
!$omp do
      do inod = 1, n
        if ( rs(inod).eq.0.0 ) then
          a_rs(inod) =  1.0d99
        else
          a_rs(inod) = 1.0d0 / rs(inod)
        end if
      end do
!$omp end do nowait
!
!$omp do private(ratio_z)
      do inod = 1, n
        r(inod) = sqrt( zz(inod)**2 + rs(inod)**2)
!
        if    (zz(inod).eq.zero .and. rs(inod).eq.zero) then
          a_r(inod) =  1.0d99
          theta(inod) = 0.0
        else if(zz(inod).gt.zero  .and. rs(inod).eq.zero) then
          a_r(inod) = 1.0d0 / r(inod)
          theta(inod) = 0.0d0
        else if(zz(inod).lt.zero  .and. rs(inod).eq.zero) then
          a_r(inod) = 1.0d0 / r(inod)
          theta(inod) = pi
        else
          a_r(inod) = 1.0d0 / r(inod)
          ratio_z = zz(inod) * a_r(inod)
          theta(inod) = acos(ratio_z)
        end if
      end do
!$omp end do
!$omp end parallel
!
      end subroutine position_cyl_2_sph
!
! ----------------------------------------------------------------------
!
      end module coordinate_converter
