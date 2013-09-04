!
!      module set_global_sph_position
!
      module set_global_sph_position
!
!     Written by H. Matsui on Sep., 2007
!
      use m_precision
!
      use m_constants
      use m_geometry_parameter
      use m_geometry_data
      use m_spheric_parameter
      use coordinate_converter
!
      implicit none
!
      private :: s_set_global_sph_position, set_global_sph_pole_posi
      private :: set_global_sph_center_posi
!
!      subroutine set_sph_node_position_no_pole
!      subroutine set_sph_node_position_w_pole
!      subroutine set_sph_node_position_w_center
!
!      subroutine s_set_global_sph_position(inod)
!      subroutine set_global_sph_pole_posi(inod)
!      subroutine set_global_sph_center_posi(inod)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_node_position_no_pole
!
      integer(kind = kint) :: inod
!
      numnod = nidx_global_rtp(1)*nidx_global_rtp(2)*nidx_global_rtp(3)
      internal_node = numnod
!
      call allocate_node_geometry
!
      inod = 0
      call s_set_global_sph_position(inod)
!
      call position_2_xyz(numnod, radius, colatitude, longitude,        &
     &    xx(1,1), xx(1,2), xx(1,3) )
!
      end subroutine set_sph_node_position_no_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_node_position_w_pole
!
      integer(kind = kint) :: inod
!
!
      numnod = nidx_global_rtp(1)*nidx_global_rtp(2)*nidx_global_rtp(3) &
     &        + 2*nidx_global_rtp(1)
      internal_node = numnod
!
      call allocate_node_geometry
!
      inod = 0
      call s_set_global_sph_position(inod)
      call set_global_sph_pole_posi(inod)
!
      call position_2_xyz(numnod, radius, colatitude, longitude,        &
     &    xx(1,1), xx(1,2), xx(1,3) )
!
      end subroutine set_sph_node_position_w_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_node_position_w_center
!
      integer(kind = kint) :: inod
!
!
      numnod = nidx_global_rtp(1)*nidx_global_rtp(2)*nidx_global_rtp(3) &
     &        + 2*nidx_global_rtp(1) + 1
      internal_node = numnod
!
      call allocate_node_geometry
!
      inod = 0
      call s_set_global_sph_position(inod)
      call set_global_sph_pole_posi(inod)
      call set_global_sph_center_posi(inod)
!
      call position_2_xyz(numnod, radius, colatitude, longitude,        &
     &    xx(1,1), xx(1,2), xx(1,3) )
!
      end subroutine set_sph_node_position_w_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_set_global_sph_position(inod)
!
      use m_gauss_points
!
      integer(kind = kint), intent(inout) :: inod
      integer(kind = kint) :: kr, kt, kp
      real(kind = kreal) :: pi
!
!
      n_point = nidx_global_rtp(2)
      call allocate_gauss_points
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      pi = four*atan(one)
      do kp = 1, nidx_global_rtp(3)
        do kt = 1, nidx_global_rtp(2)
          do kr = 1, nidx_global_rtp(1)
            inod = inod + 1
            globalnodid(inod) = inod
            radius(inod) =     radius_1d_gl(kr)
            colatitude(inod) = w_colat(kt)
            longitude(inod) =  two*pi*dble(kp-1)                        &
     &                         / dble(nidx_global_rtp(3))
          end do
        end do
      end do
!
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine s_set_global_sph_position
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_pole_posi(inod)
!
      integer(kind = kint), intent(inout) :: inod
      integer(kind = kint) :: kr
      real(kind = kreal) :: pi
!
!
      pi = four*atan(one)
      do kr = 1, nidx_global_rtp(1)
        inod = inod + 1
        globalnodid(inod) = inod
        radius(inod) =     radius_1d_gl(kr)
        colatitude(inod) = pi
        longitude(inod) =  zero
      end do
      do kr = 1, nidx_global_rtp(1)
        inod = inod + 1
        globalnodid(inod) = inod
        radius(inod) =     radius_1d_gl(kr)
        colatitude(inod) = zero
        longitude(inod) =  zero
      end do
!
      end subroutine set_global_sph_pole_posi
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_center_posi(inod)
!
      integer(kind = kint), intent(inout) :: inod
!
!
      inod = inod + 1
      globalnodid(inod) = inod
      radius(inod) =     zero
      colatitude(inod) = zero
      longitude(inod) =  zero
!
      end subroutine set_global_sph_center_posi
!
! -----------------------------------------------------------------------
!
      end module set_global_sph_position
