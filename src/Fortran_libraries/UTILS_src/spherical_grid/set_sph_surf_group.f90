!set_sph_surf_group.f90
!      module set_sph_surf_group
!
!      subroutine count_sph_local_surf_group(surf_grp)
!      subroutine count_sph_local_surf_grp_item(ip_r, ip_t, surf_grp)
!      subroutine set_sph_local_surf_grp_item(ip_r, ip_t, surf_grp)
!
!     Written by H. Matsui on March, 2012
!
      module set_sph_surf_group
!
      use m_precision
      use m_constants
      use m_group_data_sph_specr
!
      use t_group_data
!
      implicit none
!
      private :: count_surf_grp_item_on_sphere
      private :: set_surf_grp_item_on_sphere
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_surf_group(surf_grp)
!
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: igrp, num
!
!
      surf_grp%num_grp =  0
      do igrp = 1, num_radial_grp_rj
        num = istack_radial_grp_rj(igrp) - istack_radial_grp_rj(igrp-1)
        if(num .eq. 1) then
          surf_grp%num_grp =  surf_grp%num_grp + 1
        end if
      end do
!
      end subroutine count_sph_local_surf_group
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_surf_grp_item(ip_r, ip_t, surf_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_stack_4_sph_groups
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: igrp, icou, knum, kr, kele, kst
      integer(kind = kint) :: kl1, kl2
!
!
      icou = 0
      surf_grp%nitem_grp =  0
      do igrp = 1, num_radial_grp_rj
        kst = istack_radial_grp_rj(igrp-1) + 1
        knum = istack_radial_grp_rj(igrp)
        if( (knum-kst) .eq. 0) then
          icou = icou + 1
          surf_grp%grp_name(icou) = name_radial_grp_rj(igrp)
!
          kr = item_radial_grp_rj(knum)
          if(surf_grp%grp_name(icou) .eq. ICB_nod_grp_name              &
     &         .or. surf_grp%grp_name(icou) .eq. CTR_nod_grp_name) then
            kl1 = irev_sph_r(kr,  ip_r)
            kl2 = irev_sph_r(kr+1,ip_r)
          else
            kl1 = irev_sph_r(kr-1,ip_r)
            kl2 = irev_sph_r(kr,  ip_r)
          end if
!
          do kele = 1, nele_sph_r(ip_r)
            if(ie_sph_r(kele,1,ip_r) .eq. kl1 &
     &         .and. ie_sph_r(kele,2,ip_r) .eq. kl2) then
              call count_surf_grp_item_on_sphere(ip_t,                  &
     &            surf_grp%nitem_grp(icou))
            end if
          end do
!
        end if
      end do
!
      end subroutine count_sph_local_surf_grp_item
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_local_surf_grp_item(ip_r, ip_t, surf_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use set_stack_4_sph_groups
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: igrp, icou, inum, kst
      integer(kind = kint) :: knum, kr, kele
      integer(kind = kint) :: kl1, kl2, isf
!
!
      icou = 0
      do igrp = 1, num_radial_grp_rj
        kst = istack_radial_grp_rj(igrp-1) + 1
        knum = istack_radial_grp_rj(igrp)
        if( (knum-kst) .eq. 0) then
          icou = icou + 1
          inum =  surf_grp%istack_grp(icou-1)
!
          kr = item_radial_grp_rj(knum)
          if(surf_grp%grp_name(icou) .eq. ICB_nod_grp_name              &
     &         .or. surf_grp%grp_name(icou) .eq. CTR_nod_grp_name) then
            kl1 = irev_sph_r(kr,  ip_r)
            kl2 = irev_sph_r(kr+1,ip_r)
            isf = ifive
          else
            kl1 = irev_sph_r(kr-1,ip_r)
            kl2 = irev_sph_r(kr,  ip_r)
            isf = isix
          end if
!
          do kele = 1, nele_sph_r(ip_r)
            if(ie_sph_r(kele,1,ip_r) .eq. kl1                           &
     &         .and. ie_sph_r(kele,2,ip_r) .eq. kl2) then
              call set_surf_grp_item_on_sphere(ip_r, ip_t, kele,        &
     &            inum, isf, surf_grp)
              exit
            end if
          end do
        end if
      end do
!
      end subroutine set_sph_local_surf_grp_item
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_surf_grp_item_on_sphere(ip_t, nitem_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
      nitem_grp = nitem_grp + nele_sph_t(ip_t)*nidx_global_fem(3)
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
      end subroutine count_surf_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine set_surf_grp_item_on_sphere(ip_r, ip_t, kr,            &
     &          inum, isf, surf_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_ele_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, kr
      integer(kind = kint), intent(inout) :: inum, isf
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: l, m
!
!
      do m = 1, nidx_global_fem(3)
        do l = 1, nele_sph_t(ip_t)
          inum = inum + 1
          surf_grp%item_sf_grp(1,inum)                                  &
     &                     = sph_shell_ele_id(ip_r, ip_t, kr, l, m)
          surf_grp%item_sf_grp(2,inum) = isf
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
            surf_grp%item_sf_grp(1,inum) = sph_s_pole_ele_id(ip_r, kr, m)
            surf_grp%item_sf_grp(2,inum) = isf
          end do
        end if
!
!    Set elements for north pole
!
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          do m = 1, nele_around_pole
            inum = inum + 1
            surf_grp%item_sf_grp(1,inum) = sph_n_pole_ele_id(ip_r, kr, m)
            surf_grp%item_sf_grp(2,inum) = isf
          end do
        end if
      end if
!
      end subroutine set_surf_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      end module set_sph_surf_group
