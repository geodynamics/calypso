!
!      module set_node_group_global_sph
!
      module set_node_group_global_sph
!
!     Written by H. Matsui on July, 2007
!
      use m_precision
!
      use m_node_group
      use m_spheric_parameter
!
      implicit none
!
      private :: set_nod_group_item_4_sphere
      private :: set_nod_group_item_4_poles, set_nod_pole_group_item
!
!      subroutine set_node_grp_sph_no_pole
!      subroutine set_node_grp_sph_w_pole
!      subroutine set_node_grp_sph_w_center
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_node_grp_sph_no_pole
!
      integer(kind = kint) :: icou
!
!
      num_bc = 3
      num_nod_bc = 3 * nidx_global_rtp(2)*nidx_global_rtp(3)
!
      call allocate_boundary_data
!
      bc_name(1) = ICB_nod_grp_name
      bc_name(2) = CMB_nod_grp_name
      bc_name(3) = CTR_nod_grp_name
!
      bc_istack(0) = 0
      bc_istack(1) = bc_istack(0)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3)
      bc_istack(2) = bc_istack(1)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3)
      bc_istack(3) = bc_istack(2)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3)
!
!      ICB
!
      icou = 0
      call set_nod_group_item_4_sphere(icou, nlayer_ICB)
!
!      CMB
!
      call set_nod_group_item_4_sphere(icou, nlayer_CMB)
!
!      to center
!
      call set_nod_group_item_4_sphere(icou, nlayer_2_center)
!
      end subroutine set_node_grp_sph_no_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_node_grp_sph_w_pole
!
      integer(kind = kint) :: icou
!
!
      num_bc = 5
      num_nod_bc = 3 * (nidx_global_rtp(2)*nidx_global_rtp(3) + 2)      &
     &            + 2 * nidx_global_rtp(1)
!
      call allocate_boundary_data
!
      bc_name(1) = ICB_nod_grp_name
      bc_name(2) = CMB_nod_grp_name
      bc_name(3) = CTR_nod_grp_name
!
      bc_name(4) = 'South_pole'
      bc_name(5) = 'North_pole'
!
      bc_istack(0) = 0
      bc_istack(1) = bc_istack(0)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3) + 2
      bc_istack(2) = bc_istack(1)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3) + 2
      bc_istack(3) = bc_istack(2)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3) + 2
!
      bc_istack(4) = bc_istack(3) + nidx_global_rtp(1)
      bc_istack(5) = bc_istack(4) + nidx_global_rtp(1)
!
!      ICB
!
      icou = 0
      call set_nod_group_item_4_sphere(icou, nlayer_ICB)
      call set_nod_group_item_4_poles(icou, nlayer_ICB)
!
!      CMB
!
      call set_nod_group_item_4_sphere(icou, nlayer_CMB)
      call set_nod_group_item_4_poles(icou, nlayer_CMB)
!
!      to center
!
      call set_nod_group_item_4_sphere(icou, nlayer_2_center)
      call set_nod_group_item_4_poles(icou, nlayer_2_center)
!
!     South pole
!
      call set_nod_pole_group_item(icou)
!
      end subroutine set_node_grp_sph_w_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_node_grp_sph_w_center
!
      integer(kind = kint) :: icou
!
!
      num_bc = 6
      num_nod_bc = 3 * (nidx_global_rtp(2)*nidx_global_rtp(3) + 2)      &
     &            + 2 * nidx_global_rtp(1) + 1
!
      call allocate_boundary_data
!
      bc_name(1) = ICB_nod_grp_name
      bc_name(2) = CMB_nod_grp_name
      bc_name(3) = CTR_nod_grp_name
!
      bc_name(4) = 'South_pole'
      bc_name(5) = 'North_pole'
      bc_name(6) = 'Center'
!
      bc_istack(0) = 0
      bc_istack(1) = bc_istack(0)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3) + 2
      bc_istack(2) = bc_istack(1)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3) + 2
      bc_istack(3) = bc_istack(2)                                       &
     &              + nidx_global_rtp(2)*nidx_global_rtp(3) + 2
!
      bc_istack(4) = bc_istack(3) + nidx_global_rtp(1)
      bc_istack(5) = bc_istack(4) + nidx_global_rtp(1)
      bc_istack(6) = bc_istack(5) + 1
!
!      ICB
!
      icou = 0
      call set_nod_group_item_4_sphere(icou, nlayer_ICB)
      call set_nod_group_item_4_poles(icou, nlayer_ICB)
!
!      CMB
!
      call set_nod_group_item_4_sphere(icou, nlayer_CMB)
      call set_nod_group_item_4_poles(icou, nlayer_CMB)
!
!      to center
!
      call set_nod_group_item_4_sphere(icou, nlayer_2_center)
      call set_nod_group_item_4_poles(icou, nlayer_2_center)
!
!     pole
!
      call set_nod_pole_group_item(icou)
!
      icou = icou + 1
      bc_item(icou) = 1 + 2*nidx_global_rtp(1) + nidx_global_rtp(1)     &
     &                      *nidx_global_rtp(2)*nidx_global_rtp(3)
!
      end subroutine set_node_grp_sph_w_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_nod_group_item_4_sphere(icou, kr)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kp, kt
!
      do kp = 1, nidx_global_rtp(3)
        do kt = 1, nidx_global_rtp(2)
          icou = icou + 1
          bc_item(icou) = (kr  )                                        &
     &                  + (kt-1)*nidx_global_rtp(1)                     &
     &                  + (kp-1)*nidx_global_rtp(1)*nidx_global_rtp(2)
        end do
      end do
!
      end subroutine set_nod_group_item_4_sphere
!
! -----------------------------------------------------------------------
!
      subroutine set_nod_group_item_4_poles(icou, kr)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(inout) :: icou
!
      icou = icou + 1
      bc_item(icou) = kr + nidx_global_rtp(1)                           &
     &                    *nidx_global_rtp(2)*nidx_global_rtp(3)
      icou = icou + 1
      bc_item(icou) = kr + nidx_global_rtp(1)                           &
     &                   + nidx_global_rtp(1)                           &
     &                    *nidx_global_rtp(2)*nidx_global_rtp(3)
!
      end subroutine set_nod_group_item_4_poles
!
! -----------------------------------------------------------------------
!
      subroutine set_nod_pole_group_item(icou)
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kr
!
!     South pole
!
      do kr = 1, nidx_global_rtp(1)
        icou = icou + 1
        bc_item(icou) = kr + nidx_global_rtp(1)                         &
     &                      *nidx_global_rtp(2)*nidx_global_rtp(3)
      end do
!
!     North pole
!
      do kr = 1, nidx_global_rtp(1)
        icou = icou + 1
        bc_item(icou) = kr + nidx_global_rtp(1) + nidx_global_rtp(1)    &
     &                      *nidx_global_rtp(2)*nidx_global_rtp(3)
      end do
!
      end subroutine set_nod_pole_group_item
!
! -----------------------------------------------------------------------
!
      end module set_node_group_global_sph
