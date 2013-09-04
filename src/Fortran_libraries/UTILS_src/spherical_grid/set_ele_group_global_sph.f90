!
!      module set_ele_group_global_sph
!
      module set_ele_group_global_sph
!
!     Written by H. Matsui on July, 2007
!
      use m_precision
!
      use m_constants
      use m_element_group
      use m_spheric_parameter
!
      implicit none
!
      private :: set_sphere_ele_grp, set_poles_ele_grp
      private :: set_center_ele_grp
!
!      subroutine set_ele_grp_sph_no_pole
!      subroutine set_ele_grp_sph_w_pole
!      subroutine set_ele_grp_sph_w_center
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_sph_no_pole
!
      integer(kind = kint) :: icou
!
!
      num_mat = 2
      num_mat_bc =    (nlayer_CMB-1)                                    &
     &              * (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
!
      call allocate_material_data
!
      mat_name(1) = 'inner_core'
      mat_name(2) = 'outer_core'
!
      mat_istack(0) = 0
      mat_istack(1) = mat_istack(0) + (nlayer_ICB - 1)                  &
     &              * (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
      mat_istack(2) = mat_istack(1) + (nlayer_CMB - nlayer_ICB)         &
     &              * (nidx_global_rtp(2)-1)*nidx_global_rtp(3)
!
      icou = 0
      call set_sphere_ele_grp(icou, ione, nlayer_ICB)
!
      call set_sphere_ele_grp(icou, nlayer_ICB, nlayer_CMB)
!
      end subroutine set_ele_grp_sph_no_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_sph_w_pole
!
      integer(kind = kint) :: icou
!
!
      num_mat = 2
      num_mat_bc =    (nlayer_CMB-1)                                    &
     &              * (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
!
      call allocate_material_data
!
      mat_name(1) = 'inner_core'
      mat_name(2) = 'outer_core'
!
      mat_istack(0) = 0
      mat_istack(1) = mat_istack(0) + (nlayer_ICB - 1)                  &
     &              * (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
      mat_istack(2) = mat_istack(1) + (nlayer_CMB - nlayer_ICB)         &
     &              * (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
!
      icou = 0
      call set_sphere_ele_grp(icou, ione, nlayer_ICB)
      call set_poles_ele_grp(icou, ione, nlayer_ICB)
!
      call set_sphere_ele_grp(icou, nlayer_ICB, nlayer_CMB)
      call set_poles_ele_grp(icou, nlayer_ICB, nlayer_CMB)
!
      end subroutine set_ele_grp_sph_w_pole
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_sph_w_center
!
      integer(kind = kint) :: icou
!
!
      num_mat = 2
      num_mat_bc =     nlayer_CMB                                       &
     &              * (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
!
      call allocate_material_data
!
      mat_name(1) = 'inner_core'
      mat_name(2) = 'outer_core'
!
      mat_istack(0) = 0
      mat_istack(1) = mat_istack(0) + nlayer_ICB                        &
     &              * (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
      mat_istack(2) = mat_istack(1) + (nlayer_CMB - nlayer_ICB)         &
     &              * (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
!
      icou = 0
      call set_sphere_ele_grp(icou, ione, nlayer_ICB)
      call set_poles_ele_grp(icou, ione, nlayer_ICB)
      call set_center_ele_grp(icou)
!
      call set_sphere_ele_grp(icou, nlayer_ICB, nlayer_CMB)
      call set_poles_ele_grp(icou, nlayer_ICB, nlayer_CMB)
!
      end subroutine set_ele_grp_sph_w_center
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sphere_ele_grp(icou, ist, ied)
!
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kr, kt, kp
!
      do kp = 1, nidx_global_rtp(3)
        do kt = 1, nidx_global_rtp(2) - 1
          do kr = ist, ied - 1
            icou = icou + 1
            mat_item(icou) = (kr  ) + (kt-1)*(nidx_global_rtp(1)-1)     &
     &                              + (kp-1)*(nidx_global_rtp(1)-1)     &
     &                                      *(nidx_global_rtp(2)-1)
          end do
        end do
      end do
!
      end subroutine set_sphere_ele_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_poles_ele_grp(icou, ist, ied)
!
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: kr, kp
!
      do kp = 1, nidx_global_rtp(3)
        do kr = ist, ied - 1
          icou = icou + 1
          mat_item(icou) = (kr  ) + (kp-1)*(nidx_global_rtp(1)-1)       &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
        end do
      end do
!
      do kp = 1, nidx_global_rtp(3)
        do kr = ist, ied - 1
          icou = icou + 1
          mat_item(icou) = (kr  ) + (kp-1)*(nidx_global_rtp(1)-1)       &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             * nidx_global_rtp(3)                 &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
        end do
      end do
!
      end subroutine set_poles_ele_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_center_ele_grp(icou)
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint) :: k
!
      do k = 1, (nidx_global_rtp(2)+1)*nidx_global_rtp(3)
          icou = icou + 1
          mat_item(icou) = (k   ) + 2*(nidx_global_rtp(1)-1)            &
     &                             * nidx_global_rtp(3)                 &
     &                            + (nidx_global_rtp(1)-1)              &
     &                             *(nidx_global_rtp(2)-1)              &
     &                             * nidx_global_rtp(3)
      end do
!
      end subroutine set_center_ele_grp
!
! -----------------------------------------------------------------------
!
      end module set_ele_group_global_sph
