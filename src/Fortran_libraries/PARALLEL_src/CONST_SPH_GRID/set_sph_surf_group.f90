!set_sph_surf_group.f90
!      module set_sph_surf_group
!
!!      subroutine count_sph_local_surf_group(radial_rj_grp, surf_grp)
!!      subroutine count_sph_local_surf_grp_item                        &
!!     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, surf_grp)
!!      subroutine set_sph_local_surf_grp_item                          &
!!     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, surf_grp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(surface_group_data), intent(inout) :: surf_grp
!
!     Written by H. Matsui on March, 2012
!
      module set_sph_surf_group
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_group_data
      use t_sph_mesh_1d_connect
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
      subroutine count_sph_local_surf_group(radial_rj_grp, surf_grp)
!
      type(group_data), intent(in) :: radial_rj_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: igrp, num
!
!
      surf_grp%num_grp =  0
      do igrp = 1, radial_rj_grp%num_grp
        num = radial_rj_grp%istack_grp(igrp)                            &
     &       - radial_rj_grp%istack_grp(igrp-1)
        if(num .eq. 1) then
          surf_grp%num_grp =  surf_grp%num_grp + 1
        end if
      end do
!
      end subroutine count_sph_local_surf_group
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_surf_grp_item                          &
     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, surf_grp)
!
      use set_stack_4_sph_groups
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: igrp, icou, knum, kr, kele, kst
      integer(kind = kint) :: kl1, kl2
!
!
      icou = 0
      surf_grp%nitem_grp =  0
      do igrp = 1, radial_rj_grp%num_grp
        kst = radial_rj_grp%istack_grp(igrp-1) + 1
        knum = radial_rj_grp%istack_grp(igrp)
        if( (knum-kst) .eq. 0) then
          icou = icou + 1
          surf_grp%grp_name(icou) = radial_rj_grp%grp_name(igrp)
!
          kr = radial_rj_grp%item_grp(knum)
          if(surf_grp%grp_name(icou) .eq. ICB_nod_grp_name              &
     &         .or. surf_grp%grp_name(icou) .eq. CTR_nod_grp_name) then
            kl1 = stbl%irev_sph_r(kr,  ip_r)
            kl2 = stbl%irev_sph_r(kr+1,ip_r)
          else
            kl1 = stbl%irev_sph_r(kr-1,ip_r)
            kl2 = stbl%irev_sph_r(kr,  ip_r)
          end if
!
          do kele = 1, stbl%nele_sph_r(ip_r)
            if      (stbl%ie_sph_r(kele,1,ip_r) .eq. kl1                &
     &         .and. stbl%ie_sph_r(kele,2,ip_r) .eq. kl2) then
              call count_surf_grp_item_on_sphere                        &
     &           (ip_t, sph_params, stbl, surf_grp%nitem_grp(icou))
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
      subroutine set_sph_local_surf_grp_item                            &
     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, surf_grp)
!
      use set_stack_4_sph_groups
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: igrp, icou, inum, kst
      integer(kind = kint) :: knum, kr, kele
      integer(kind = kint) :: kl1, kl2, isf
!
!
      icou = 0
      do igrp = 1, radial_rj_grp%num_grp
        kst = radial_rj_grp%istack_grp(igrp-1) + 1
        knum = radial_rj_grp%istack_grp(igrp)
        if( (knum-kst) .eq. 0) then
          icou = icou + 1
          inum =  surf_grp%istack_grp(icou-1)
!
          kr = radial_rj_grp%item_grp(knum)
          if(surf_grp%grp_name(icou) .eq. ICB_nod_grp_name              &
     &         .or. surf_grp%grp_name(icou) .eq. CTR_nod_grp_name) then
            kl1 = stbl%irev_sph_r(kr,  ip_r)
            kl2 = stbl%irev_sph_r(kr+1,ip_r)
            isf = ifive
          else
            kl1 = stbl%irev_sph_r(kr-1,ip_r)
            kl2 = stbl%irev_sph_r(kr,  ip_r)
            isf = isix
          end if
!
          do kele = 1, stbl%nele_sph_r(ip_r)
            if      (stbl%ie_sph_r(kele,1,ip_r) .eq. kl1                &
     &         .and. stbl%ie_sph_r(kele,2,ip_r) .eq. kl2) then
              call set_surf_grp_item_on_sphere(ip_r, ip_t, kele,        &
     &            sph_params, stbl, inum, isf, surf_grp)
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
      subroutine count_surf_grp_item_on_sphere                          &
     &         (ip_t, sph_params, stbl, nitem_grp)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
      nitem_grp = nitem_grp                                             &
     &           + stbl%nele_sph_t(ip_t) * stbl%nidx_global_fem(3)
!
!    Set elements for poles
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole         &
     &  .or. sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for south pole
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          nitem_grp = nitem_grp + stbl%nele_around_pole
        end if
!
!    Set elements for north pole
!
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          nitem_grp = nitem_grp + stbl%nele_around_pole
        end if
      end if
!
      end subroutine count_surf_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine set_surf_grp_item_on_sphere(ip_r, ip_t, kr,            &
     &          sph_params, stbl, inum, isf, surf_grp)
!
      use cal_sph_ele_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, kr
      type(sph_shell_parameters), intent(in) :: sph_params
      type(comm_table_make_sph), intent(in) :: stbl
!
      integer(kind = kint), intent(inout) :: inum, isf
      type(surface_group_data), intent(inout) :: surf_grp
!
      integer(kind = kint) :: l, m
!
!
      do m = 1, stbl%nidx_global_fem(3)
        do l = 1, stbl%nele_sph_t(ip_t)
          inum = inum + 1
          surf_grp%item_sf_grp(1,inum)                                  &
     &         = sph_shell_ele_id(ip_r, ip_t, kr, l, m, stbl)
          surf_grp%item_sf_grp(2,inum) = isf
        end do
      end do
!
!    Set elements for poles
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole         &
     &  .or. sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set elements for south pole
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          do m = 1, stbl%nele_around_pole
            inum = inum + 1
            surf_grp%item_sf_grp(1,inum)                                &
     &           = sph_s_pole_ele_id(ip_r, kr, m, stbl)
            surf_grp%item_sf_grp(2,inum) = isf
          end do
        end if
!
!    Set elements for north pole
!
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          do m = 1, stbl%nele_around_pole
            inum = inum + 1
            surf_grp%item_sf_grp(1,inum)                                &
     &           = sph_n_pole_ele_id(ip_r, kr, m, stbl)
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
