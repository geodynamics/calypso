!
!      module set_surf_group_global_sph
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_surf_grp_sph_no_pole
!      subroutine set_surf_grp_sph_w_pole
!
      module set_surf_group_global_sph
!
      use m_precision
!
      use m_constants
      use m_surface_group
      use m_spheric_parameter
!
      implicit none
!
      private :: set_inner_sphere_surf_grp, set_outer_sphere_surf_grp
      private :: set_inner_pole_surf_grp, set_outer_pole_surf_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_surf_grp_sph_no_pole
!
      integer(kind = kint) :: icou
!
!
      num_surf = 3
      num_surf_bc = 3*(nidx_global_rtp(2)-1)*nidx_global_rtp(3)
!
      call allocate_surface_data
!
      surf_name(1) = 'ICB_surf'
      surf_name(2) = 'CMB_surf'
      surf_name(3) = 'to_Center_surf'
!
      surf_istack(0) = 0
      surf_istack(1) = surf_istack(0)                                   &
     &               + (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
      surf_istack(2) = surf_istack(1)                                   &
     &               + (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
      surf_istack(3) = surf_istack(2)                                   &
     &               + (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
!
      icou = 0
      call set_inner_sphere_surf_grp(icou, nlayer_ICB)
!
      call set_outer_sphere_surf_grp(icou, nlayer_CMB)
!
      call set_inner_sphere_surf_grp(icou, nlayer_2_center)
!
      end subroutine set_surf_grp_sph_no_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_surf_grp_sph_w_pole
!
      integer(kind = kint) :: icou
!
!
      num_surf = 3
      num_surf_bc = 3*(nidx_global_rtp(2)+1)*nidx_global_rtp(3)
!
      call allocate_surface_data
!
      surf_name(1) = 'ICB_surf'
      surf_name(2) = 'CMB_surf'
      surf_name(3) = 'to_Center_surf'
!
      surf_istack(0) = 0
      surf_istack(1) = surf_istack(0)                                   &
     &               + (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
      surf_istack(2) = surf_istack(1)                                   &
     &               + (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
      surf_istack(3) = surf_istack(2)                                   &
     &               + (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
!
      icou = 0
      call set_inner_sphere_surf_grp(icou, nlayer_ICB)
      call set_inner_pole_surf_grp(icou, nlayer_ICB)
!
      call set_outer_sphere_surf_grp(icou, nlayer_CMB)
      call set_outer_pole_surf_grp(icou, nlayer_CMB)
!
      call set_inner_sphere_surf_grp(icou, nlayer_2_center)
      call set_inner_pole_surf_grp(icou, nlayer_2_center)
!
      end subroutine set_surf_grp_sph_w_pole
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_inner_sphere_surf_grp(icou, kr)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kt, kp
!
      do kp = 1, nidx_global_rtp(3)
        do kt = 1, nidx_global_rtp(2) - 1
          icou = icou + 1
          surf_item(1,icou) = (kr  ) + (kt-1)*(nidx_global_rtp(1)-1)    &
     &                              + (kp-1)*(nidx_global_rtp(1)-1)     &
     &                                      *(nidx_global_rtp(2)-1)
          surf_item(2,icou) = ifive
        end do
      end do
!
      end subroutine set_inner_sphere_surf_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_outer_sphere_surf_grp(icou, kr)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kt, kp
!
      do kp = 1, nidx_global_rtp(3)
        do kt = 1, nidx_global_rtp(2) - 1
          icou = icou + 1
          surf_item(1,icou) = (kr-1) + (kt-1)*(nidx_global_rtp(1)-1)    &
     &                              + (kp-1)*(nidx_global_rtp(1)-1)     &
     &                                      *(nidx_global_rtp(2)-1)
          surf_item(2,icou) = isix
        end do
      end do
!
      end subroutine set_outer_sphere_surf_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_inner_pole_surf_grp(icou, kr)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kp
!
      do kp = 1, nidx_global_rtp(3)
        icou = icou + 1
        surf_item(1,icou) = (kr  ) + (kp-1)*(nidx_global_rtp(1)-1)      &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
        surf_item(2,icou) = ifive
      end do
!
      do kp = 1, nidx_global_rtp(3)
        icou = icou + 1
        surf_item(1,icou)  = (kr  ) + (kp-1)*(nidx_global_rtp(1)-1)     &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             * nidx_global_rtp(3)                 &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
        surf_item(2,icou) = ifive
      end do
!
      end subroutine set_inner_pole_surf_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_outer_pole_surf_grp(icou, kr)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kp
!
      do kp = 1, nidx_global_rtp(3)
        icou = icou + 1
        surf_item(1,icou) = (kr-1) + (kp-1)*(nidx_global_rtp(1)-1)      &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
        surf_item(2,icou) = isix
      end do
!
      do kp = 1, nidx_global_rtp(3)
        icou = icou + 1
        surf_item(1,icou)  = (kr-1) + (kp-1)*(nidx_global_rtp(1)-1)     &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             * nidx_global_rtp(3)                 &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
        surf_item(2,icou) = isix
      end do
!
      end subroutine set_outer_pole_surf_grp
!
! -----------------------------------------------------------------------
!
      end module set_surf_group_global_sph
