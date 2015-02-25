!
!      module set_global_sph_position
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine set_sph_node_position_no_pole
!
      module set_global_sph_position
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
      private :: s_set_global_sph_position
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_node_position_no_pole
!
      numnod = nidx_global_rtp(1)*nidx_global_rtp(2)*nidx_global_rtp(3)
      internal_node = numnod
!
!
      call allocate_node_geometry
!
      call s_set_global_sph_position
!
      call position_2_xyz(numnod, radius, colatitude, longitude,        &
     &    xx(1,1), xx(1,2), xx(1,3) )
!
      end subroutine set_sph_node_position_no_pole
!
! -----------------------------------------------------------------------
!
      subroutine s_set_global_sph_position
!
      use m_gauss_points
!
      integer(kind = kint) :: inod
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
            inod = kr + (kt-ione)*nidx_global_rtp(1)                    &
     &                + (kp-ione)*nidx_global_rtp(1)*nidx_global_rtp(2)
            inod_global(inod) = inod
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
      end module set_global_sph_position
