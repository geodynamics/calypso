!>@file   set_psf_nodes_4_by_surf_grp.f90
!!@brief  module set_psf_nodes_4_by_surf_grp
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Construct node data for secitoning using surface group
!!
!!@verbatim
!!      subroutine count_node_at_node_on_grp(igrp, internal_node,       &
!!     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,        &
!!     &          inod_surf_grp,  psf_list)
!!      subroutine set_node_at_node_on_grp(igrp, internal_node,         &
!!     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,        &
!!     &          inod_surf_grp, psf_list)
!!
!!      subroutine fill_0_node_on_edge_on_grp(psf_list)
!!@endverbatim
!
      module set_psf_nodes_4_by_surf_grp
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_at_node_on_grp(igrp, internal_node,         &
     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,          &
     &          inod_surf_grp,  psf_list)
!
      use cal_minmax_and_stacks
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: internal_node
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind=kint) :: inum, ist, ied, max_4_smp
!
!
      psf_list%internod_on_nod = 0
      ist = inod_stack_sf_grp(igrp-1)
      ied = inod_stack_sf_grp(igrp)
      do inum = ist, ied
        if(inod_surf_grp(inum) .le. internal_node) then
          psf_list%internod_on_nod = psf_list%internod_on_nod + 1
        end if
      end do
      psf_list%externod_on_nod = inod_stack_sf_grp(igrp  )              &
     &                          - inod_stack_sf_grp(igrp-1)             &
     &                          - psf_list%internod_on_nod
!
      call count_number_4_smp(np_smp, ione, psf_list%internod_on_nod,   &
     &    psf_list%istack_inter_n_on_n_smp, max_4_smp)
      call count_number_4_smp(np_smp, ione, psf_list%externod_on_nod,   &
     &    psf_list%istack_exter_n_on_n_smp, max_4_smp)
!
      end subroutine count_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_on_grp(igrp, internal_node,           &
     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,          &
     &          inod_surf_grp, psf_list)
!
      use m_constants
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: internal_node
!
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: num_surf, ntot_node_sf_grp
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      integer(kind = kint), intent(in)                                  &
     &                     :: inod_surf_grp(ntot_node_sf_grp)
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: i, icou, inod, jcou, nitem_surf
!
!
      icou = 0
      jcou = psf_list%internod_on_nod
      nitem_surf = inod_stack_sf_grp(igrp  )                            &
     &            - inod_stack_sf_grp(igrp-1)
      do i = 1, nitem_surf
        inod = inod_surf_grp(i)
        if(inod .le. internal_node) then
          icou = icou + 1
          psf_list%inod_int_nod(icou) = inod
          psf_list%id_n_on_n(inod) =    icou
        else
          jcou = jcou + 1
          psf_list%inod_ext_nod(jcou) = inod
          psf_list%id_n_on_n(inod) =    jcou
        end if
      end do
!
      end subroutine set_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine fill_0_node_on_edge_on_grp(psf_list)
!
      use t_psf_geometry_list
!
      type(sectioning_list), intent(inout) :: psf_list
!
!
      psf_list%internod_on_edge = 0
      psf_list%externod_on_edge = 0
      psf_list%istack_inter_n_on_e_smp(0:np_smp) = 0
      psf_list%istack_exter_n_on_e_smp(0:np_smp) = 0
!
      end subroutine fill_0_node_on_edge_on_grp
!
!  ---------------------------------------------------------------------
!
      end module set_psf_nodes_4_by_surf_grp
