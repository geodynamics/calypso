!
!      module const_merged_surf_4_group
!
!      Written by H. Matsui on Jan., 2007
!
!!      subroutine const_merged_surface_4_ele_grp                       &
!!     &         (node, ele, group, surf, mgd_sf_grp)
!!      subroutine const_merged_surface_4_sf_grp                        &
!!     &         (group, surf, mgd_sf_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(mesh_groups), intent(in) :: group
!!        type(surface_data), intent(in) :: surf
!!        type(viewer_ele_grp_surface), intent(inout) :: mgd_sf_grp
!
      module const_merged_surf_4_group
!
      use m_precision
      use t_sum_hash
      use t_mesh_data
      use t_surface_data
!
      implicit    none
!
      type(sum_hash_tbl), save :: surf_ele_tbl
      integer(kind=kint ), allocatable :: isf_isolate_ele_grp_tmp(:)
!
      private :: surf_ele_tbl, isf_isolate_ele_grp_tmp
      private :: allocate_iso_surf_4_egrp_tmp
      private ::  deallocate_iso_surf_4_egrp_tmp
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_iso_surf_4_egrp_tmp(ntot)
!
      integer(kind = kint), intent(in) :: ntot
!
      allocate( isf_isolate_ele_grp_tmp(ntot) )
      if(ntot .gt. 0) isf_isolate_ele_grp_tmp = 0
!
      end subroutine allocate_iso_surf_4_egrp_tmp
!
!   ---------------------------------------------------------------------
!
      subroutine deallocate_iso_surf_4_egrp_tmp
!
      deallocate( isf_isolate_ele_grp_tmp )
!
      end subroutine deallocate_iso_surf_4_egrp_tmp
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine const_merged_surface_4_ele_grp                         &
     &         (node, ele, group, surf, mgd_sf_grp)
!
      use t_viewer_ele_grp_surface
!
      use set_surface_hash
      use mark_surf_hash
      use set_surface_data
      use const_surface_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mesh_groups), intent(in) :: group
!
      type(surface_data), intent(inout) :: surf
      type(viewer_ele_grp_surface), intent(inout) :: mgd_sf_grp
!
      integer(kind = kint) :: igrp
      integer(kind = kint) :: ist_grp, ied_grp, nele_grp
!
!
      call alloc_n_iso_surf_4_ele_grp(group%ele_grp, mgd_sf_grp)
      call alloc_iso_surf_4_egrp_m(mgd_sf_grp)
!
      do igrp = 1, group%ele_grp%num_grp
        ist_grp =  group%ele_grp%istack_grp(igrp-1) + 1
        ied_grp =  group%ele_grp%istack_grp(igrp  )
        nele_grp = ied_grp - ist_grp + 1
!
        call alloc_sum_hash(node%numnod, ele%numele,                    &
     &      nsurf_4_ele, surf%nnod_4_surf, surf_ele_tbl)
!         surf_ele_tbl%ntot_id = node%numnod * surf%nnod_4_surf
!         surf_ele_tbl%ntot_list = nsurf_4_ele * ele%numele
!
        call const_part_surface_hash                                    &
     &     (ele, nele_grp, group%ele_grp%item_grp(ist_grp:ied_grp),     &
     &      surf_ele_tbl)
!
!   mark independent surface
!
!        write(*,*) 'mark_independent_surface', igrp
        call mark_independent_surface                                   &
     &     (ele%numele, ele%nnod_4_ele, ele%ie,                         &
     &      surf_ele_tbl%ntot_id, surf_ele_tbl%ntot_list,               &
     &      surf_ele_tbl%istack_hash, surf_ele_tbl%iend_hash,           &
     &      surf_ele_tbl%id_hash, surf_ele_tbl%iflag_hash)
!
!    count independent surfaces for element group
!
        ist_grp = mgd_sf_grp%istack_sf_iso_ele_grp(igrp-1)
!
        call allocate_iso_surf_4_egrp_tmp                               &
     &     (mgd_sf_grp%ntot_sf_iso_ele_grp)
        isf_isolate_ele_grp_tmp(1:ist_grp)                              &
     &          = mgd_sf_grp%isf_isolate_ele_grp(1:ist_grp)
        call dealloc_iso_surf_4_egrp_m(mgd_sf_grp)
!
!        write(*,*) 'count_part_surface', igrp
        call count_part_surface                                         &
     &     (nele_grp, surf_ele_tbl%ntot_list, surf_ele_tbl%iflag_hash,  &
     &      mgd_sf_grp%num_sf_iso_ele_grp(igrp) )
        mgd_sf_grp%istack_sf_iso_ele_grp(igrp)                          &
     &      = mgd_sf_grp%istack_sf_iso_ele_grp(igrp-1)                  &
     &       + mgd_sf_grp%num_sf_iso_ele_grp(igrp)
        mgd_sf_grp%ntot_sf_iso_ele_grp                                  &
     &       = mgd_sf_grp%istack_sf_iso_ele_grp(igrp)
!
!    set independent surfaces for element group
!
        call alloc_iso_surf_4_egrp_m(mgd_sf_grp)
        mgd_sf_grp%isf_isolate_ele_grp(1:ist_grp)                       &
     &          = isf_isolate_ele_grp_tmp(1:ist_grp)
        call deallocate_iso_surf_4_egrp_tmp
!
!        write(*,*) 'set_part_surface', igrp
        call set_part_surface(ele%numele,                               &
     &       nele_grp, mgd_sf_grp%num_sf_iso_ele_grp(igrp),             &
     &      surf%isf_4_ele,surf_ele_tbl%ntot_list,                      &
     &      surf_ele_tbl%id_hash, surf_ele_tbl%iflag_hash,              &
     &      mgd_sf_grp%isf_isolate_ele_grp(ist_grp+1))
!
        call dealloc_sum_hash(surf_ele_tbl)
      end do
!
!      call check_viewer_isurf_4_ele_grp                                &
!     &   (group%ele_grp, mgd_sf_grp)
!
      end subroutine const_merged_surface_4_ele_grp
!
!   ---------------------------------------------------------------------
!
      subroutine const_merged_surface_4_sf_grp                          &
     &         (group, surf, mgd_sf_grp)
!
      use t_viewer_ele_grp_surface
!
      type(mesh_groups), intent(in) :: group
      type(surface_data), intent(in) :: surf
      type(viewer_ele_grp_surface), intent(inout) :: mgd_sf_grp
!
      integer(kind= kint) :: i, iele, isf
!
      call alloc_iso_surf_4_sgrp_m(group%surf_grp, mgd_sf_grp)
!
      do i = 1, group%surf_grp%num_item
        iele = group%surf_grp%item_sf_grp(1,i)
        isf =  group%surf_grp%item_sf_grp(2,i)
        mgd_sf_grp%isf_surf_grp(i) = surf%isf_4_ele(iele,isf)
      end do
!
!      call check_viewer_isuf_4_surf_grp(group%surf_grp, mgd_sf_grp)
!
      end subroutine const_merged_surface_4_sf_grp
!
!   ---------------------------------------------------------------------
!
      end module const_merged_surf_4_group
