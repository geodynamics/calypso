!>@file   get_geometry_reference.f90
!!@brief  module get_geometry_reference
!!
!!@author H. Matsui
!!@date Programmed in Apr. 2018
!
!> @brief Get scalar information from position
!!
!!@verbatim
!!      real(kind = kreal) function                                     &
!!     &         s_get_geometry_reference(icomp_viz, xx)
!!        integer(kind = kint), intent(in) :: icomp_viz
!!        real(kind = kreal), intent(in) :: xx(3)
!!@endverbatim
!
      module get_geometry_reference
!
      use m_precision
      use m_constants
!
      private :: position_radius, position_cyl_radius
      private :: position_theta, position_phi
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function                                       &
     &         s_get_geometry_reference(icomp_viz, xx)
!
      use set_components_flags
!
      integer(kind = kint), intent(in) :: icomp_viz
      real(kind = kreal), intent(in) :: xx(3)
!
      real(kind = kreal) :: dat_viz
!
!
      dat_viz = 0.0d0
      if(icomp_viz.ge.icomp_X .and. icomp_viz.le.icomp_Z) then
        dat_viz = xx(icomp_viz)
!
      else if(icomp_viz .eq. icomp_RADIAL) then
        dat_viz = position_radius(xx)
      else if(icomp_viz .eq. icomp_THETA) then
        dat_viz = position_theta(xx)
      else if(icomp_viz .eq. icomp_PHI) then
        dat_viz = position_phi(xx)
!
      else if(icomp_viz .eq. icomp_CYLINDER_R) then
        dat_viz = position_cyl_radius(xx)
      end if
      s_get_geometry_reference = dat_viz
!
      end function s_get_geometry_reference
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      real(kind = kreal) function position_radius(xx)
!
      real(kind = kreal), intent(in) :: xx(3)
!
      position_radius = sqrt(xx(1)**2 + xx(2)**2 + xx(3)**2)
!
      end function position_radius
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function position_cyl_radius(xx)
!
      real(kind = kreal), intent(in) :: xx(3)
!
      position_cyl_radius = sqrt(xx(1)**2 + xx(2)**2)
!
      end function position_cyl_radius
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function position_theta(xx)
!
      real(kind = kreal), intent(in) :: xx(3)
!
      real(kind = kreal) :: pi, r, ratio_z, theta
!
      pi = four * atan(one)
      r = position_radius(xx)
      if(r .eq. 0.0d0) then
        theta = 0.0
      else
        ratio_z = xx(3) / r
        if (ratio_z .ge. 1.0d0) then
          theta = 0.0d0
        else if (ratio_z .le.-1.0d0) then
          theta = pi
        else
          theta = acos(ratio_z)
        end if
      end if
      position_theta = theta
!
      end function position_theta
!
!-----------------------------------------------------------------------
!
      real(kind = kreal) function position_phi(xx)
!
      real(kind = kreal), intent(in) :: xx(3)
!
      real(kind = kreal) :: pi, ss, ratio_x, phi
!
      pi = four * atan(one)
      ss = position_cyl_radius(xx)
      phi = 0.0d0
      if(ss .gt. 0.0d0) then
        ratio_x = xx(1) / ss
        if(ratio_x .ge. 1.0d0) then
          phi = 0.0d0
        else if(ratio_x .le. -1.0d0) then
          phi = pi
        else if(xx(2) .ge. 0.0d0) then
          phi = acos(ratio_x)
        else if(xx(2) .lt. 0.0d0) then
          phi = two * pi - acos(ratio_x)
        end if
      end if
      position_phi = phi
!
      end function position_phi
!
!-----------------------------------------------------------------------
!
      end module get_geometry_reference
