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
!!     &          inod_surf_grp,  psf_g_list)
!!      subroutine count_position_4_psf_grp(psf_g_list, psf_node)
!!      subroutine set_node_at_node_on_grp(igrp, internal_node,         &
!!     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,        &
!!     &          inod_surf_grp, psf_g_list)
!!
!!      subroutine set_position_psf_grp(nnod, xx, istack_intnod,        &
!!     &          nnod_patch, inod_sum, xx_patch, psf_list, psf_g_list)
!!      subroutine set_field_on_psf_grp_xyz(nnod, nnod_patch,           &
!!     &          num_fld, ntot_comp, istack_comp_nod,                  &
!!     &          d_nod, ifield_psf, ncomp, psf_g_list)
!!@endverbatim
!
      module set_psf_nodes_4_by_surf_grp
!
      use m_precision
      use m_constants
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
     &          inod_surf_grp, psf_g_list)
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
      type(grp_section_list), intent(inout) :: psf_g_list
!
      integer(kind=kint) :: inum, ist, ied, max_4_smp
!
!
      psf_g_list%internod_on_nod = 0
      ist = inod_stack_sf_grp(igrp-1)
      ied = inod_stack_sf_grp(igrp)
      do inum = ist, ied
        if(inod_surf_grp(inum) .le. internal_node) then
          psf_g_list%internod_on_nod = psf_g_list%internod_on_nod + 1
        end if
      end do
      psf_g_list%externod_on_nod = inod_stack_sf_grp(igrp  )            &
     &                          - inod_stack_sf_grp(igrp-1)             &
     &                          - psf_g_list%internod_on_nod
!
      call count_number_4_smp(np_smp, ione, psf_g_list%internod_on_nod, &
     &    psf_g_list%istack_inter_n_on_n_smp, max_4_smp)
      call count_number_4_smp(np_smp, ione,                             &
     &    psf_g_list%externod_on_nod,                                   &
     &    psf_g_list%istack_exter_n_on_n_smp, max_4_smp)
!
      end subroutine count_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!
      subroutine count_position_4_psf_grp(psf_g_list, psf_node)
!
      use t_psf_geometry_list
      use t_geometry_data
      use cal_minmax_and_stacks
!
      type(grp_section_list), intent(in) :: psf_g_list
      type(node_data), intent(inout) :: psf_node
!
      integer(kind = kint) :: max_4_smp
!
!
      psf_node%internal_node =   psf_g_list%internod_on_nod
      psf_node%numnod =          psf_node%internal_node
      call count_number_4_smp(np_smp, ione, psf_node%internal_node,     &
     &    psf_node%istack_internal_smp, max_4_smp)
      call count_number_4_smp(np_smp, ione, psf_node%numnod,            &
     &    psf_node%istack_nod_smp, max_4_smp)
!
      end subroutine count_position_4_psf_grp
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_on_grp(igrp, internal_node,           &
     &          num_surf, ntot_node_sf_grp, inod_stack_sf_grp,          &
     &          inod_surf_grp, psf_g_list)
!
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
      type(grp_section_list), intent(inout) :: psf_g_list
!
      integer(kind = kint) :: i, icou, inod, jcou, nitem_surf
!
!
      icou = 0
      jcou = psf_g_list%internod_on_nod
      nitem_surf = inod_stack_sf_grp(igrp  )                            &
     &            - inod_stack_sf_grp(igrp-1)
      do i = 1, nitem_surf
        inod = inod_surf_grp(i)
        if(inod .le. internal_node) then
          icou = icou + 1
          psf_g_list%inod_int_nod(icou) = inod
          psf_g_list%id_n_on_n(inod) =    icou
        else
          jcou = jcou + 1
          psf_g_list%inod_ext_nod(jcou) = inod
          psf_g_list%id_n_on_n(inod) =    jcou
        end if
      end do
!
      end subroutine set_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_position_psf_grp(nnod, xx, istack_intnod,          &
     &          nnod_patch, inod_sum, xx_patch, psf_g_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint_gl), intent(in) :: istack_intnod
      integer(kind = kint), intent(in) :: nnod_patch
      type(grp_section_list), intent(in) :: psf_g_list
!
      integer(kind = kint_gl), intent(inout) :: inod_sum(nnod_patch)
      real(kind = kreal), intent(inout) :: xx_patch(nnod_patch,3)
!
      integer(kind = kint) :: ishift
!
!
      ishift = 0
      call set_position_at_nod_psf(nnod, xx, izero,                     &
     &    psf_g_list%internod_on_nod,                                   &
     &    psf_g_list%istack_inter_n_on_n_smp,                           &
     &    psf_g_list%inod_int_nod, istack_intnod, nnod_patch,           &
     &    inod_sum, xx_patch)
!
      ishift = ishift + psf_g_list%internod_on_nod
      call set_position_at_nod_psf(nnod, xx, ishift,                    &
     &    psf_g_list%externod_on_nod,                                   &
     &    psf_g_list%istack_exter_n_on_n_smp,                           &
     &    psf_g_list%inod_ext_nod, istack_intnod, nnod_patch,           &
     &    inod_sum, xx_patch)
!
      end subroutine set_position_psf_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_psf_grp_xyz(nnod, nnod_patch,             &
     &          num_fld, ntot_comp, istack_comp_nod,                    &
     &          d_nod, ifield_psf, ncomp, dat_tmp, psf_g_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod
!
      integer(kind = kint), intent(in) :: nnod_patch
      type(grp_section_list), intent(in) :: psf_g_list
!
      integer(kind = kint), intent(in) :: num_fld, ntot_comp
      integer(kind = kint), intent(in) :: istack_comp_nod(0:num_fld)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: ifield_psf, ncomp
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,6)
!
      integer(kind = kint) :: ifld, ishift
!
!
      ifld = istack_comp_nod(ifield_psf-1) + 1
      ishift = 0
      call set_field_at_nod_psf(nnod, ncomp, d_nod(1,ifld), izero,      &
     &    psf_g_list%internod_on_nod,                                   &
     &    psf_g_list%istack_inter_n_on_n_smp,                           &
     &    psf_g_list%inod_int_nod, nnod_patch, dat_tmp(1,1))
!
      ishift = ishift + psf_g_list%internod_on_nod
      call set_field_at_nod_psf(nnod, ncomp, d_nod(1,ifld), ishift,     &
     &    psf_g_list%externod_on_nod,                                   &
     &    psf_g_list%istack_exter_n_on_n_smp,                           &
     &    psf_g_list%inod_ext_nod, nnod_patch, dat_tmp(1,1))
!
      end subroutine set_field_on_psf_grp_xyz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_position_at_nod_psf(nnod, xx,                      &
     &          ishift, nnod_on_nod, istack_n_on_n_smp, inod_4_nod,     &
     &          istack_intnod, nnod_patch, inod_sum, xx_patch)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) :: nnod_on_nod, ishift
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: inod_4_nod(nnod_on_nod)
      integer(kind = kint_gl), intent(in) :: istack_intnod
      integer(kind = kint), intent(in) :: nnod_patch
!
      integer(kind = kint_gl), intent(inout) :: inod_sum(nnod_patch)
      real(kind = kreal), intent(inout) :: xx_patch(nnod_patch,3)
!
      integer(kind = kint) :: ip, ist, ied, inum
!
!
      call set_field_at_nod_psf(nnod, ithree, xx,                       &
     &   ishift, nnod_on_nod, istack_n_on_n_smp, inod_4_nod,            &
     &   nnod_patch, xx_patch)
!
!$omp parallel do private(ist,ied,inum)
      do ip = 1, np_smp
        ist = istack_n_on_n_smp(ip-1) + 1 + ishift
        ied = istack_n_on_n_smp(ip  ) + ishift
        do inum = ist, ied
          inod_sum(inum) = inum + istack_intnod
        end do
      end do
!$omp end parallel do
!
      end subroutine set_position_at_nod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_at_nod_psf(nnod, ncomp, d_nod,               &
     &          ishift, nnod_on_nod, istack_n_on_n_smp, inod_4_nod,     &
     &          nnod_patch, dat_tmp)
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: nnod_on_nod, ishift
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: inod_4_nod(nnod_on_nod)
      integer(kind = kint), intent(in) :: nnod_patch, ncomp
      real(kind = kreal), intent(in) :: d_nod(nnod,ncomp)
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, jnum, nd
!
!
!$omp parallel do private(ist,ied,inum,jnum,inod,nd)
      do ip = 1, np_smp
        ist = istack_n_on_n_smp(ip-1) + 1
        ied = istack_n_on_n_smp(ip  )
        do nd = 1, ncomp
          do inum = ist, ied
            jnum = inum + ishift
            inod = inod_4_nod(inum)
            dat_tmp(jnum,nd) = d_nod(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_field_at_nod_psf
!
!  ---------------------------------------------------------------------
!
      end module set_psf_nodes_4_by_surf_grp
