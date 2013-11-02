!set_sph_ele_group.f90
!      module set_sph_ele_group
!
!      subroutine count_sph_local_ele_group(ele_grp)
!      subroutine count_sph_local_ele_grp_item(ip_r, ip_t, ele_grp)
!      subroutine set_sph_local_ele_grp_item(ip_r, ip_t, ele_grp)
!
!     Written by H. Matsui on March, 2012
!
      module set_sph_ele_group
!
      use m_precision
      use m_constants
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      use t_group_data
!
      implicit none
!
      private :: count_ele_grp_item_on_sphere
      private ::  set_ele_grp_item_on_sphere
      private :: count_ele_grp_item_4_center, set_ele_grp_item_4_center
!
      integer(kind = kint), allocatable :: iflag_r(:)
      private :: iflag_r
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sph_ele_grp_flag
!
      integer(kind = kint) :: num
!
!
      num = nidx_global_rtp(1)
      allocate(iflag_r(num))
      if(nidx_global_rtp(1) .gt. 0) iflag_r = 0
!
      end subroutine allocate_sph_ele_grp_flag
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sph_ele_grp_flag
!
!
      deallocate(iflag_r)
!
      end subroutine deallocate_sph_ele_grp_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_ele_group(ele_grp)
!
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: igrp, num
!
!
      ele_grp%num_grp =  0
      do igrp = 1, num_radial_grp_rj
        iflag_r = 0
        num = istack_radial_grp_rj(igrp) - istack_radial_grp_rj(igrp-1)
        if(num .gt. 1) then
          ele_grp%num_grp =  ele_grp%num_grp + 1
        end if
      end do
!
      end subroutine count_sph_local_ele_group
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_ele_grp_item(ip_r, ip_t, ele_grp)
!
      use m_sph_mesh_1d_connect
      use set_stack_4_sph_groups
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: igrp, icou, knum, kr, kele, kst, ked
      integer(kind = kint) :: kl1, kl2, kg1, kg2
!
!
      icou = 0
      ele_grp%nitem_grp =  0
      do igrp = 1, num_radial_grp_rj
        kst = istack_radial_grp_rj(igrp-1) + 1
        ked = istack_radial_grp_rj(igrp)
        if( (ked-kst) .gt. 0) then
          icou = icou + 1
          ele_grp%grp_name(icou) = name_radial_grp_rj(igrp)
!
          iflag_r = 0
          do knum = kst, ked
            kr = item_radial_grp_rj(knum)
            iflag_r(kr) = 1
          end do
!
          do kele = 1, nele_sph_r(ip_r)
            kl1 = ie_sph_r(kele,1,ip_r)
            kl2 = ie_sph_r(kele,2,ip_r)
            kg1 = inod_sph_r(kl1,ip_r)
            kg2 = inod_sph_r(kl2,ip_r)
            if(iflag_r(kg1)*iflag_r(kg2) .gt. 0) then
              call count_ele_grp_item_on_sphere(ip_t,                   &
     &            ele_grp%nitem_grp(icou))
            else if(ele_grp%grp_name(icou) .eq. IC_ele_grp_name         &
     &           .and. iflag_r(kg1).gt.0) then
              call count_ele_grp_item_on_sphere(ip_t,                   &
     &            ele_grp%nitem_grp(icou))
            end if
          end do
!
!
          if(iflag_r(ione).eq.ione                                      &
     &       .and. iflag_center_r(ip_r) .gt. 0)  then
            call count_ele_grp_item_4_center(ip_t,                      &
     &          ele_grp%nitem_grp(icou))
          end if
!
        end if
      end do
!
      end subroutine count_sph_local_ele_grp_item
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_local_ele_grp_item(ip_r, ip_t, ele_grp)
!
      use m_sph_mesh_1d_connect
      use set_stack_4_sph_groups
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: igrp, icou, inum, kst, ked
      integer(kind = kint) :: knum, kr, kele
      integer(kind = kint) :: kl1, kl2, kg1, kg2
!
!
      icou = 0
      do igrp = 1, num_radial_grp_rj
        kst = istack_radial_grp_rj(igrp-1) + 1
        ked = istack_radial_grp_rj(igrp)
        if( (ked-kst) .gt. 0) then
          icou = icou + 1
          inum =  ele_grp%istack_grp(icou-1)
!
          iflag_r = 0
          do knum = kst, ked
            kr = item_radial_grp_rj(knum)
            iflag_r(kr) = 1
          end do
!
          do kele = 1, nele_sph_r(ip_r)
            kl1 = ie_sph_r(kele,1,ip_r)
            kl2 = ie_sph_r(kele,2,ip_r)
            kg1 = inod_sph_r(kl1,ip_r)
            kg2 = inod_sph_r(kl2,ip_r)
            if(iflag_r(kg1)*iflag_r(kg2) .gt. 0) then
              call set_ele_grp_item_on_sphere(ip_r, ip_t, kele,         &
     &            inum, ele_grp)
            else if(ele_grp%grp_name(icou) .eq. IC_ele_grp_name         &
     &           .and. iflag_r(kg1).gt.0) then
              call set_ele_grp_item_on_sphere(ip_r, ip_t, kele,         &
     &            inum, ele_grp)
            end if
          end do
!
          if(iflag_r(ione).eq.ione                                      &
     &         .and. iflag_center_r(ip_r) .gt. 0)  then
            call set_ele_grp_item_4_center(ip_t, inum, ele_grp)
          end if
!
        end if
      end do
!
      end subroutine set_sph_local_ele_grp_item
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_ele_grp_item_on_sphere(ip_t, nitem_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
      nitem_grp = nitem_grp + nele_sph_t(ip_t)*nidx_global_rtp(3)
!
!    Set elements for poles
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for south pole
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          nitem_grp = nitem_grp + nele_around_pole
        end if
!
!    Set elements for north pole
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          nitem_grp = nitem_grp + nele_around_pole
        end if
      end if
!
      end subroutine count_ele_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine count_ele_grp_item_4_center(ip_t, nitem_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
!
!    Set elements for Center elements
      if(iflag_Spole_t(ip_t) .gt. 0)  then
        nitem_grp = nitem_grp                                           &
     &             + (nidx_global_rtp(2)-1)*nidx_global_rtp(3)          &
     &             + nidx_global_rtp(3)
!
      else
        nitem_grp = nitem_grp + nele_sph_t(ip_t)*nidx_global_rtp(3)
!    Set element for north pole
         if(iflag_Npole_t(ip_t) .gt. 0)  then
           nitem_grp = nitem_grp + nele_around_pole
         end if
      end if
!
      end subroutine count_ele_grp_item_4_center
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_item_on_sphere(ip_r, ip_t, kr,             &
     &          inum, ele_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_ele_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, kr
      integer(kind = kint), intent(inout) :: inum
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: l, m
!
!
      do m = 1, nidx_global_rtp(3)
        do l = 1, nele_sph_t(ip_t)
          inum = inum + 1
          ele_grp%item_grp(inum)                                        &
     &                     = sph_shell_ele_id(ip_r, ip_t, kr, l, m)
        end do
      end do
!
!    Set elements for poles
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for south pole
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          do m = 1, nele_around_pole
            inum = inum + 1
            ele_grp%item_grp(inum) = sph_s_pole_ele_id(ip_r, kr, m)
          end do
        end if
!
!    Set elements for north pole
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          do m = 1, nele_around_pole
            inum = inum + 1
            ele_grp%item_grp(inum) = sph_n_pole_ele_id(ip_r, kr, m)
          end do
        end if
      end if
!
      end subroutine set_ele_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_item_4_center(ip_t, inum, ele_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_ele_addresses
!
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: inum
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: l, m
!
!
!    Set elements for Center elements
!
      if(iflag_Spole_t(ip_t) .gt. 0)  then
        do m = 1, nidx_global_rtp(3)
          do l = 1, nidx_global_rtp(2)-1
            inum = inum + 1
            ele_grp%item_grp(inum) = sph_inter_ctr_shell_ele_id(l, m)
          end do
        end do
!
!    Set element for south pole
        do m = 1, nele_around_pole
          inum = inum + 1
          ele_grp%item_grp(inum) = sph_inter_ctr_spole_ele_id(m)
        end do
!
!    Set element for north pole
        do m = 1, nele_around_pole
          inum = inum + 1
          ele_grp%item_grp(inum) = sph_inter_ctr_npole_ele_id(m)
        end do
!
      else
        do m = 1, nidx_global_rtp(3)
          do l = 1, nele_sph_t(ip_t)
            inum = inum + 1
            ele_grp%item_grp(inum)                                      &
     &          = sph_exter_ctr_shell_ele_id(ip_t, l, m)
          end do
        end do
!
!    Set element for north pole
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          do m = 1, nele_around_pole
            inum = inum + 1
            ele_grp%item_grp(inum)                                      &
     &           = sph_exter_ctr_npole_ele_id(m)
          end do
        end if
      end if
!
      end subroutine set_ele_grp_item_4_center
!
! -----------------------------------------------------------------------
!
      end module set_sph_ele_group
