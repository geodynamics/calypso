!set_sph_node_group.f90
!      module set_sph_node_group
!
!!      subroutine count_sph_local_node_group                           &
!!     &         (sph_params, radial_rj_grp, nod_grp)
!!      subroutine count_sph_local_node_grp_item                        &
!!     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, nod_grp)
!!      subroutine set_sph_local_node_grp_item                          &
!!     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, nod_grp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(group_data), intent(inout) :: nod_grp
!
!     Written by H. Matsui on March, 2012
!
      module set_sph_node_group
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
      private :: count_node_grp_on_sphere, set_node_grp_item_on_sphere
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_node_group                             &
     &         (sph_params, radial_rj_grp, nod_grp)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
!
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: igrp, num
!
!
      nod_grp%num_grp =  0
      do igrp = 1, radial_rj_grp%num_grp
        num = radial_rj_grp%istack_grp(igrp)                            &
     &       - radial_rj_grp%istack_grp(igrp-1)
        if(num .eq. 1) then
          nod_grp%num_grp =  nod_grp%num_grp + 1
        end if
      end do
!
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole         &
     &  .or. sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
        nod_grp%num_grp =  nod_grp%num_grp + 2
      end if
!
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
        nod_grp%num_grp =  nod_grp%num_grp + 1
      end if
!
      end subroutine count_sph_local_node_group
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_node_grp_item                          &
     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, nod_grp)
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: igrp, icou, knum, kr, num
!
!
      icou = 0
      nod_grp%nitem_grp =  0
      do igrp = 1, radial_rj_grp%num_grp
        num = radial_rj_grp%istack_grp(igrp)                            &
     &       - radial_rj_grp%istack_grp(igrp-1)
        if(num .eq. 1) then
          icou = icou + 1
          knum = radial_rj_grp%istack_grp(igrp)
          kr = radial_rj_grp%item_grp(knum)
!
          nod_grp%grp_name(icou) = radial_rj_grp%grp_name(igrp)
          if(stbl%irev_sph_r(kr,ip_r) .gt. 0) then
            call count_node_grp_on_sphere                               &
     &         (ip_r, ip_t, stbl%irev_sph_r(kr,ip_r), stbl,             &
     &          nod_grp%nitem_grp(icou))
          else
            nod_grp%nitem_grp(icou) =  0
          end if
        end if
      end do
!
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole         &
     &  .or. sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    count nodes for south pole
        icou = icou + 1
        nod_grp%grp_name(icou) = 'South_pole'
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          nod_grp%nitem_grp(icou) = stbl%nnod_sph_r(ip_r)
        end if
!
!    count nodes for north pole
        icou = icou + 1
        nod_grp%grp_name(icou) = 'North_pole'
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          nod_grp%nitem_grp(icou) = stbl%nnod_sph_r(ip_r)
        else if(stbl%iflag_Npole_t(ip_t) .eq. 0)  then
          if      (stbl%iflag_center_r(ip_r) .gt. 0                     &
             .and. stbl%iflag_Spole_t(ip_t) .gt. 0) then
            nod_grp%nitem_grp(icou) = 1
          end if
        end if
      end if
!
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
        icou = icou + 1
        nod_grp%grp_name(icou) = 'Center'
        if(stbl%iflag_center_r(ip_r) .gt. 0) then
          nod_grp%nitem_grp(icou) = 1
        end if
      end if
!
      end subroutine count_sph_local_node_grp_item
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_local_node_grp_item                            &
     &         (ip_r, ip_t, sph_params, radial_rj_grp, stbl, nod_grp)
!
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(comm_table_make_sph), intent(in) :: stbl
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: igrp, icou, knum, inum, kr, num
!
!
      icou = 0
      do igrp = 1, radial_rj_grp%num_grp
        num = radial_rj_grp%istack_grp(igrp)                            &
     &       - radial_rj_grp%istack_grp(igrp-1)
        if(num .eq. 1) then
          icou = icou + 1
          knum = radial_rj_grp%istack_grp(igrp)
          kr = radial_rj_grp%item_grp(knum)
!
          if(stbl%irev_sph_r(kr,ip_r) .gt. 0) then
            call set_node_grp_item_on_sphere(ip_r, ip_t,                &
     &          stbl%irev_sph_r(kr,ip_r), icou, stbl, nod_grp)
          end if
        end if
      end do
!
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_pole         &
     &  .or. sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
!
!    Set nodes for south pole
        icou = icou + 1
        inum = nod_grp%istack_grp(icou-1)
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          do knum = 1, stbl%nnod_sph_r(ip_r)
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_s_pole_node_id(knum)
          end do
        end if
!
!    Set nodes for north pole
        icou = icou + 1
        inum = nod_grp%istack_grp(icou-1)
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          do knum = 1, stbl%nnod_sph_r(ip_r)
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_n_pole_node_id(knum)
          end do
        else if(stbl%iflag_Npole_t(ip_t) .eq. 0)  then
          if      (stbl%iflag_center_r(ip_r) .gt. 0                     &
             .and. stbl%iflag_Spole_t(ip_t) .gt. 0) then
            inum = inum + 1
            nod_grp%item_grp(inum) = sph_center_np_node_id()
          end if
        end if
      end if
!
      if    (sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
        icou = icou + 1
        if(stbl%iflag_center_r(ip_r) .gt. 0)  then
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
      subroutine count_node_grp_on_sphere                               &
     &         (ip_r, ip_t, knum, stbl, nitem_grp)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t, knum
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
      nitem_grp = stbl%nidx_global_fem(3) * stbl%nnod_sph_t(ip_t)
!
      if(stbl%iflag_Spole_t(ip_t) .gt. 0) nitem_grp = nitem_grp + 1
      if(stbl%iflag_Npole_t(ip_t) .gt. 0) nitem_grp = nitem_grp + 1
!
!     Set nodes around center
!
      if       (stbl%inod_sph_r(knum,ip_r) .eq. 1                       &
     &    .and. stbl%iflag_center_r(ip_r) .gt. 0)  then
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          nitem_grp = nitem_grp                                         &
     &               + stbl%nidx_global_fem(3) * stbl%nnod_sph_ct
          if(stbl%iflag_Npole_t(ip_t) .eq. 0) nitem_grp = nitem_grp + 1
          end if
      end if
!
      end subroutine count_node_grp_on_sphere
!
! ----------------------------------------------------------------------
!
      subroutine set_node_grp_item_on_sphere(ip_r, ip_t, knum,          &
     &          icou, stbl, nod_grp)
!
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, knum, icou
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(group_data), intent(inout) :: nod_grp
!
      integer(kind = kint) :: lnum, mnum, inum
!
!
      inum = nod_grp%istack_grp(icou-1)
      do mnum = 1, stbl%nidx_global_fem(3)
        do lnum = 1, stbl%nnod_sph_t(ip_t)
          inum = inum + 1
          nod_grp%item_grp(inum)                                        &
     &         = sph_shell_node_id(ip_r, ip_t, knum, lnum, mnum, stbl)
        end do
      end do
!
!    Set nodes for south pole
      if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
        inum = inum + 1
        nod_grp%item_grp(inum) = sph_s_pole_node_id(knum)
      end if
!
!    Set nodes for north pole
      if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
        inum = inum + 1
        nod_grp%item_grp(inum) = sph_n_pole_node_id(knum)
      end if
!
!     Set nodes around center
      if      (stbl%inod_sph_r(knum,ip_r) .eq. 1                        &
     &   .and. stbl%iflag_center_r(ip_r) .gt. 0)  then
        if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
          do mnum = 1, stbl%nidx_global_fem(3)
            do lnum = 1, stbl%nnod_sph_ct
              inum = inum + 1
              nod_grp%item_grp(inum)                                    &
     &               = sph_ctr_shell_node_id(stbl%nnod_sph_ct,          &
     &                                       lnum, mnum)
            end do
          end do
!
          if(stbl%iflag_Npole_t(ip_t) .eq. 0)  then
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
