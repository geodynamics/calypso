!
!      module set_psf_patch_4_by_surf_grp
!
!      Written by H. Matsui on March, 2007
!
!      subroutine count_num_patch_4_grp(num_surf_grp, istack_surf_grp,  &
!     &          igrp, istack_patch_smp)
!      subroutine set_patch_4_grp(numnod, numele, nnod_4_ele, ie,       &
!     &          num_surf_grp, ntot_surf_grp, istack_surf_grp,          &
!     &          item_surf_grp, igrp, id_n_on_n, istack_numele,         &
!     &          npatch_tot, istack_patch_smp, iele_global, ie_patch)
!
      module set_psf_patch_4_by_surf_grp
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_patch_4_grp(num_surf_grp, istack_surf_grp,   &
     &          igrp, istack_patch_smp)
!
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_surf_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_surf_grp(0:num_surf_grp)
      integer(kind = kint), intent(in) :: igrp
!
      integer(kind = kint), intent(inout) :: istack_patch_smp(0:np_smp)
!
      integer(kind = kint) :: ist, ied, max_4_smp
      integer(kind = kint), parameter :: ione = 1, itwo = 2
!
!
      ist = istack_patch_smp(0)
      ied = istack_patch_smp(0)                                         &
     &     + itwo*( istack_surf_grp(igrp) - istack_surf_grp(igrp-1) )
      call count_number_4_smp(np_smp, ist, ied,                         &
     &    istack_patch_smp, max_4_smp)
!
      end subroutine count_num_patch_4_grp
!
!  ---------------------------------------------------------------------
!
      subroutine set_patch_4_grp(numnod, numele, nnod_4_ele, ie,        &
     &          num_surf_grp, ntot_surf_grp, istack_surf_grp,           &
     &          item_surf_grp, igrp, id_n_on_n, istack_numele,          &
     &          npatch_tot, istack_patch_smp, iele_global, ie_patch)
!
      use m_machine_parameter
      use m_geometry_constants
      use m_quad_2_triangle
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_surf_grp, ntot_surf_grp
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_surf_grp(0:num_surf_grp)
      integer(kind = kint), intent(in)                                  &
     &                      :: item_surf_grp(2,ntot_surf_grp)
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint_gl), intent(in) :: id_n_on_n(numnod)
      integer(kind = kint_gl), intent(in) :: istack_numele
      integer(kind = kint), intent(in) :: npatch_tot
      integer(kind = kint), intent(in) :: istack_patch_smp(0:np_smp)
!
      integer(kind = kint_gl), intent(inout) :: iele_global(npatch_tot)
      integer(kind = kint), intent(inout) :: ie_patch(npatch_tot,3)
!
      integer(kind = kint) :: n, i, j, inum, iele, isurf, jnum
      integer(kind = kint) :: l1, l2, l3, k1, k2, k3, i1, i2, i3
!
!
      n = istack_surf_grp(igrp) - istack_surf_grp(igrp-1)
      do i = 1, n
        inum = istack_surf_grp(igrp-1) + i
        iele =  item_surf_grp(1,inum)
        isurf = item_surf_grp(2,inum)
!
        do j = 1, 2
          jnum = istack_patch_smp(0) + 2*(i-1) + j
          l1 = ie_quad_2_tri(1,j)
          l2 = ie_quad_2_tri(2,j)
          l3 = ie_quad_2_tri(3,j)
          k1 = node_on_sf_n_4(l1,isurf)
          k2 = node_on_sf_n_4(l2,isurf)
          k3 = node_on_sf_n_4(l3,isurf)
          i1 = ie(k1,iele)
          i2 = ie(k2,iele)
          i3 = ie(k3,iele)
          iele_global(jnum) = jnum + istack_numele
          ie_patch(jnum,1) = int(id_n_on_n(i1))
          ie_patch(jnum,2) = int(id_n_on_n(i2))
          ie_patch(jnum,3) = int(id_n_on_n(i3))
        end do
!
      end do
!
      end subroutine set_patch_4_grp
!
!  ---------------------------------------------------------------------
!
      end module set_psf_patch_4_by_surf_grp
