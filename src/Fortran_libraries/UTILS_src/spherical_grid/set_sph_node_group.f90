!set_sph_node_group.f90
!      module set_sph_node_group
!
!      subroutine count_sph_local_node_group(nod_grp)
!      subroutine count_sph_local_node_grp_item(ip_r, ip_t, nod_grp)
!      subroutine set_sph_local_node_grp_item(ip_r, ip_t, nod_grp)
!
!     Written by H. Matsui on March, 2012
!
      module set_sph_node_group
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
      private :: count_node_grp_on_sphere, set_node_grp_item_on_sphere
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_node_group(nod_grp)
!
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: igrp, num
!
!
      nod_grp%num_grp =  0
      do igrp = 1, num_radial_grp_rj
        num = istack_radial_grp_rj(igrp) - istack_radial_grp_rj(igrp-1)
        if(num .eq. 1) then
          nod_grp%num_grp =  nod_grp%num_grp + 1
        end if
      end do
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
        nod_grp%num_grp =  nod_grp%num_grp + 2
      end if
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        nod_grp%num_grp =  nod_grp%num_grp + 1
      end if
!
      end subroutine count_sph_local_node_group
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_node_grp_item(ip_r, ip_t, nod_grp)
!
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: igrp, icou, knum, kr, num
!
!
      icou = 0
      nod_grp%nitem_grp =  0
      do igrp = 1, num_radial_grp_rj
        num = istack_radial_grp_rj(igrp) - istack_radial_grp_rj(igrp-1)
        if(num .eq. 1) then
          icou = icou + 1
          knum = istack_radial_grp_rj(igrp)
          kr = item_radial_grp_rj(knum)
!
          nod_grp%grp_name(icou) = name_radial_grp_rj(igrp)
          if(irev_sph_r(kr,ip_r) .gt. 0) then
            call count_node_grp_on_sphere(ip_r, ip_t,                   &
     &          irev_sph_r(kr,ip_r), nod_grp%nitem_grp(icou))
          else
            nod_grp%nitem_grp(icou) =  0
          end if
        end if
      end do
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    count nodes for south pole
        icou = icou + 1
        nod_grp%grp_name(icou) = 'South_pole'
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          nod_grp%nitem_grp(icou) = nnod_sph_r(ip_r)
        end if
!
!    count nodes for north pole
        icou = icou + 1
        nod_grp%grp_name(icou) = 'North_pole'
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          nod_grp%nitem_grp(icou) = nnod_sph_r(ip_r)
        else if(iflag_Npole_t(ip_t) .eq. 0)  then
          if(iflag_center_r(ip_r) .gt. 0                                &
             .and. iflag_Spole_t(ip_t) .gt. 0) then
            nod_grp%nitem_grp(icou) = 1
          end if
        end if
      end if
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        icou = icou + 1
        nod_grp%grp_name(icou) = 'Center'
        if(iflag_center_r(ip_r) .gt. 0) nod_grp%nitem_grp(icou) = 1
      end if
!
      end subroutine count_sph_local_node_grp_item
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_local_node_grp_item(ip_r, ip_t, nod_grp)
!
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: igrp, icou, knum, inum, kr, num
!
!
      icou = 0
      do igrp = 1, num_radial_grp_rj
        num = istack_radial_grp_rj(igrp) - istack_radial_grp_rj(igrp-1)
        if(num .eq. 1) then
          icou = icou + 1
          knum = istack_radial_grp_rj(igrp)
          kr = item_radial_grp_rj(knum)
!
          if(irev_sph_r(kr,ip_r) .gt. 0) then
            call set_node_grp_item_on_sphere(ip_r, ip_t,                &
     &          irev_sph_r(kr,ip_r), icou, nod_grp)
          end if
        end if
      end do
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_pole                    &
     &  .or. iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set nodes for south pole
        icou = icou + 1
        inum = nod_grp%istack_grp(icou-1)
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          do knum = 1, nnod_sph_r(ip_r)
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_s_pole_node_id(knum)
          end do
        end if
!
!    Set nodes for north pole
        icou = icou + 1
        inum = nod_grp%istack_grp(icou-1)
        if(iflag_Npole_t(ip_t) .gt. 0)  then
          do knum = 1, nnod_sph_r(ip_r)
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_n_pole_node_id(knum)
          end do
        else if(iflag_Npole_t(ip_t) .eq. 0)  then
          if(iflag_center_r(ip_r) .gt. 0                                &
             .and. iflag_Spole_t(ip_t) .gt. 0) then
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_center_np_node_id()
          end if
        end if
      end if
!
      if    (iflag_shell_mode .eq. iflag_MESH_w_center) then
        icou = icou + 1
        if(iflag_center_r(ip_r) .gt. 0)  then
          inum = nod_grp%istack_grp(icou-1) + 1
          nod_grp%item_grp(inum) = sph_center_node_id()
        end if
      end if
!
      end subroutine set_sph_local_node_grp_item
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_node_grp_on_sphere(ip_r, ip_t, knum, nitem_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, knum
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
      nitem_grp = nidx_global_rtp(3)*nnod_sph_t(ip_t)
!
      if(iflag_Spole_t(ip_t) .gt. 0) nitem_grp = nitem_grp + 1
      if(iflag_Npole_t(ip_t) .gt. 0) nitem_grp = nitem_grp + 1
!
!     Set nodes around center
!
      if(inod_sph_r(knum,ip_r) .eq. 1                                   &
     &     .and.  iflag_center_r(ip_r) .gt. 0)  then
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          nitem_grp = nitem_grp + nidx_global_rtp(3)*nnod_sph_ct
          if(iflag_Npole_t(ip_t) .eq. 0) nitem_grp = nitem_grp + 1
          end if
      end if
!
      end subroutine count_node_grp_on_sphere
!
! ----------------------------------------------------------------------
!
      subroutine set_node_grp_item_on_sphere(ip_r, ip_t, knum,          &
     &          icou, nod_grp)
!
      use m_spheric_parameter
      use m_sph_mesh_1d_connect
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, knum, icou
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: lnum, mnum, inum
!
!
      inum = nod_grp%istack_grp(icou-1)
      do mnum = 1, nidx_global_rtp(3)
        do lnum = 1, nnod_sph_t(ip_t)
          inum = inum + 1
          nod_grp%item_grp(inum) = sph_shell_node_id(ip_r, ip_t,        &
     &                            knum, lnum, mnum)
        end do
      end do
!
!    Set nodes for south pole
      if(iflag_Spole_t(ip_t) .gt. 0)  then
        inum = inum + 1
        nod_grp%item_grp(inum) = sph_s_pole_node_id(knum)
      end if
!
!    Set nodes for north pole
      if(iflag_Npole_t(ip_t) .gt. 0)  then
        inum = inum + 1
        nod_grp%item_grp(inum) = sph_n_pole_node_id(knum)
      end if
!
!     Set nodes around center
      if(inod_sph_r(knum,ip_r) .eq. 1                                   &
     &     .and.  iflag_center_r(ip_r) .gt. 0)  then
        if(iflag_Spole_t(ip_t) .gt. 0)  then
          do mnum = 1, nidx_global_rtp(3)
            do lnum = 1, nnod_sph_ct
              inum = inum + 1
              nod_grp%item_grp(inum)                                    &
     &               = sph_ctr_shell_node_id(nnod_sph_ct, lnum, mnum)
            end do
          end do
!
          if(iflag_Npole_t(ip_t) .eq. 0)  then
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_center_np_node_id()
          end if
        end if
      end if
!
      end subroutine set_node_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      end module set_sph_node_group
