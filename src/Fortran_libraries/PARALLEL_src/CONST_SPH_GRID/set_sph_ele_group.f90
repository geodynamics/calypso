!set_sph_ele_group.f90
!      module set_sph_ele_group
!
!!      subroutine allocate_sph_ele_grp_flag(stbl)
!!      subroutine count_sph_local_ele_group(ele_grp, radial_rj_grp)
!!      subroutine count_sph_local_ele_grp_item                         &
!!     &         (ip_r, ip_t, sph_params, stbl, radial_rj_grp, ele_grp)
!!      subroutine set_sph_local_ele_grp_item                           &
!!     &         (ip_r, ip_t, sph_params, stbl, radial_rj_grp, ele_grp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(group_data), intent(inout) :: ele_grp
!
!     Written by H. Matsui on March, 2012
!
      module set_sph_ele_group
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
      private :: count_ele_grp_item_on_sphere
      private :: set_ele_grp_item_on_sphere
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
      subroutine allocate_sph_ele_grp_flag(stbl)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint) :: num
!
!
      num = stbl%nidx_global_fem(1)
      allocate(iflag_r(num))
      if(stbl%nidx_global_fem(1) .gt. 0) iflag_r = 0
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
      subroutine count_sph_local_ele_group(ele_grp, radial_rj_grp)
!
      type(group_data), intent(in) :: radial_rj_grp
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: igrp, num
!
!
      ele_grp%num_grp =  0
      do igrp = 1, radial_rj_grp%num_grp
        iflag_r = 0
        num = radial_rj_grp%istack_grp(igrp)                            &
     &       - radial_rj_grp%istack_grp(igrp-1)
        if(num .gt. 1) then
          ele_grp%num_grp =  ele_grp%num_grp + 1
        end if
      end do
!
      end subroutine count_sph_local_ele_group
!
! ----------------------------------------------------------------------
!
      subroutine count_sph_local_ele_grp_item                           &
     &         (ip_r, ip_t, sph_params, stbl, radial_rj_grp, ele_grp)
!
      use set_stack_4_sph_groups
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: igrp, icou, knum, kr, kele, kst, ked
      integer(kind = kint) :: kl1, kl2, kg1, kg2
!
!
      icou = 0
      ele_grp%nitem_grp =  0
      do igrp = 1, radial_rj_grp%num_grp
        kst = radial_rj_grp%istack_grp(igrp-1) + 1
        ked = radial_rj_grp%istack_grp(igrp)
        if( (ked-kst) .gt. 0) then
          icou = icou + 1
          ele_grp%grp_name(icou) = radial_rj_grp%grp_name(igrp)
!
          iflag_r = 0
          do knum = kst, ked
            kr = radial_rj_grp%item_grp(knum)
            iflag_r(kr) = 1
          end do
!
          do kele = 1, stbl%nele_sph_r(ip_r)
            kl1 = stbl%ie_sph_r(kele,1,ip_r)
            kl2 = stbl%ie_sph_r(kele,2,ip_r)
            kg1 = stbl%inod_sph_r(kl1,ip_r)
            kg2 = stbl%inod_sph_r(kl2,ip_r)
            if(iflag_r(kg1)*iflag_r(kg2) .gt. 0) then
              call count_ele_grp_item_on_sphere(ip_t, sph_params, stbl, &
     &            ele_grp%nitem_grp(icou))
            else if(ele_grp%grp_name(icou) .eq. IC_ele_grp_name         &
     &           .and. iflag_r(kg1).gt.0) then
              call count_ele_grp_item_on_sphere(ip_t, sph_params, stbl, &
     &            ele_grp%nitem_grp(icou))
            end if
          end do
!
!
          if(iflag_r(ione).eq.ione                                      &
     &       .and. stbl%iflag_center_r(ip_r) .gt. 0)  then
            call count_ele_grp_item_4_center(ip_t, stbl,                &
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
      subroutine set_sph_local_ele_grp_item                             &
     &         (ip_r, ip_t, sph_params, stbl, radial_rj_grp, ele_grp)
!
      use set_stack_4_sph_groups
      use cal_sph_node_addresses
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(group_data), intent(in) :: radial_rj_grp
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: igrp, icou, inum, kst, ked
      integer(kind = kint) :: knum, kr, kele
      integer(kind = kint) :: kl1, kl2, kg1, kg2
!
!
      icou = 0
      do igrp = 1, radial_rj_grp%num_grp
        kst = radial_rj_grp%istack_grp(igrp-1) + 1
        ked = radial_rj_grp%istack_grp(igrp)
        if( (ked-kst) .gt. 0) then
          icou = icou + 1
          inum =  ele_grp%istack_grp(icou-1)
!
          iflag_r = 0
          do knum = kst, ked
            kr = radial_rj_grp%item_grp(knum)
            iflag_r(kr) = 1
          end do
!
          do kele = 1, stbl%nele_sph_r(ip_r)
            kl1 = stbl%ie_sph_r(kele,1,ip_r)
            kl2 = stbl%ie_sph_r(kele,2,ip_r)
            kg1 = stbl%inod_sph_r(kl1,ip_r)
            kg2 = stbl%inod_sph_r(kl2,ip_r)
            if(iflag_r(kg1)*iflag_r(kg2) .gt. 0) then
              call set_ele_grp_item_on_sphere(ip_r, ip_t, kele,         &
     &            sph_params, stbl, inum, ele_grp)
            else if(ele_grp%grp_name(icou) .eq. IC_ele_grp_name         &
     &           .and. iflag_r(kg1).gt.0) then
              call set_ele_grp_item_on_sphere(ip_r, ip_t, kele,         &
     &            sph_params, stbl, inum, ele_grp)
            end if
          end do
!
          if(iflag_r(ione).eq.ione                                      &
     &         .and. stbl%iflag_center_r(ip_r) .gt. 0)  then
            call set_ele_grp_item_4_center(ip_t, stbl, inum, ele_grp)
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
      subroutine count_ele_grp_item_on_sphere                           &
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
      end subroutine count_ele_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine count_ele_grp_item_4_center(ip_t, stbl, nitem_grp)
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: nitem_grp
!
!
!
!    Set elements for Center elements
      if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
        nitem_grp = nitem_grp                                           &
     &           + (stbl%nidx_global_fem(2)-1)*stbl%nidx_global_fem(3)  &
     &            + stbl%nidx_global_fem(3)
!
      else
        nitem_grp = nitem_grp                                           &
     &             + stbl%nele_sph_t(ip_t) * stbl%nidx_global_fem(3)
!    Set element for north pole
         if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
           nitem_grp = nitem_grp + stbl%nele_around_pole
         end if
      end if
!
      end subroutine count_ele_grp_item_4_center
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_item_on_sphere(ip_r, ip_t, kr,             &
     &          sph_params, stbl, inum, ele_grp)
!
      use cal_sph_ele_addresses
!
      integer(kind = kint), intent(in) :: ip_r, ip_t, kr
      type(sph_shell_parameters), intent(in) :: sph_params
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(group_data), intent(inout) :: ele_grp
      integer(kind = kint), intent(inout) :: inum
!
      integer(kind = kint) :: l, m
!
!
      do m = 1, stbl%nidx_global_fem(3)
        do l = 1, stbl%nele_sph_t(ip_t)
          inum = inum + 1
          ele_grp%item_grp(inum)                                        &
     &         = sph_shell_ele_id(ip_r, ip_t, kr, l, m, stbl)
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
            ele_grp%item_grp(inum)                                      &
     &           = sph_s_pole_ele_id(ip_r, kr, m, stbl)
          end do
        end if
!
!    Set elements for north pole
!
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          do m = 1, stbl%nele_around_pole
            inum = inum + 1
            ele_grp%item_grp(inum)                                      &
     &           = sph_n_pole_ele_id(ip_r, kr, m, stbl)
          end do
        end if
      end if
!
      end subroutine set_ele_grp_item_on_sphere
!
! -----------------------------------------------------------------------
!
      subroutine set_ele_grp_item_4_center(ip_t, stbl, inum, ele_grp)
!
      use cal_sph_ele_addresses
!
      type(comm_table_make_sph), intent(in) :: stbl
      integer(kind = kint), intent(in) :: ip_t
      integer(kind = kint), intent(inout) :: inum
      type(group_data), intent(inout) :: ele_grp
!
      integer(kind = kint) :: l, m
!
!
!    Set elements for Center elements
!
      if(stbl%iflag_Spole_t(ip_t) .gt. 0)  then
        do m = 1, stbl%nidx_global_fem(3)
          do l = 1, stbl%nidx_global_fem(2)-1
            inum = inum + 1
            ele_grp%item_grp(inum)                                      &
     &           = sph_inter_ctr_shell_ele_id(l, m, stbl)
          end do
        end do
!
!    Set element for south pole
        do m = 1, stbl%nele_around_pole
          inum = inum + 1
          ele_grp%item_grp(inum) = sph_inter_ctr_spole_ele_id(m)
        end do
!
!    Set element for north pole
        do m = 1, stbl%nele_around_pole
          inum = inum + 1
          ele_grp%item_grp(inum) = sph_inter_ctr_npole_ele_id(m)
        end do
!
      else
        do m = 1, stbl%nidx_global_fem(3)
          do l = 1, stbl%nele_sph_t(ip_t)
            inum = inum + 1
            ele_grp%item_grp(inum)                                      &
     &          = sph_exter_ctr_shell_ele_id(ip_t, l, m, stbl)
          end do
        end do
!
!    Set element for north pole
        if(stbl%iflag_Npole_t(ip_t) .gt. 0)  then
          do m = 1, stbl%nele_around_pole
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
