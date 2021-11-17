!set_fields_for_fieldline.f90
!
!      module set_fields_for_fieldline
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine set_local_field_4_fline                              &
!!     &         (node, nod_fld, fln_prm, fln_src)
!!        type(node_data), intent(in) :: node
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!      integer(kind = kint) function count_nsurf_for_starting          &
!!     &                            (ele, sf_grp, igrp_seed)
!!      subroutine set_isurf_for_starting                               &
!!     &         (ele, sf_grp, igrp_seed, fln_src)
!!        type(element_data), intent(in) :: ele
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!      subroutine s_set_fields_for_fieldline                           &
!!     &         (mesh, group, fln_prm, fln_src, fln_tce)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module set_fields_for_fieldline
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_local_field_4_fline                                &
     &         (node, nod_fld, fln_prm, fln_src)
!
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: i_field, ist_fld, num_comp
!
!
        i_field = fln_prm%ifield_4_fline
        ist_fld = nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
!
        if (iflag_debug .gt. 0) write(*,*)                              &
     &    'convert_comps_4_viz ifield_4_fline', i_field
        call convert_comps_4_viz(node%numnod, node%istack_nod_smp,      &
     &     node%xx, node%rr,node%a_r, node%ss, node%a_s, ithree,        &
     &     num_comp, fln_prm%icomp_4_fline,                             &
     &     nod_fld%d_fld(1,ist_fld+1), fln_src%vector_nod_fline)
!
        i_field = fln_prm%ifield_linecolor
        ist_fld = nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        if (iflag_debug .gt. 0) write(*,*)                              &
     &     'convert_comps_4_viz ifield_linecolor', i_field
        call convert_comps_4_viz(node%numnod, node%istack_nod_smp,      &
     &      node%xx, node%rr, node%a_r, node%ss, node%a_s, ione,        &
     &      num_comp, fln_prm%icomp_linecolor,                          &
     &      nod_fld%d_fld(1,ist_fld+1), fln_src%color_nod_fline)
!
      end subroutine set_local_field_4_fline
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_nsurf_for_starting            &
     &                            (ele, sf_grp, igrp_seed)
!
      integer(kind = kint), intent(in) :: igrp_seed
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint) :: isurf, iele, icou, ist, ied
!
!
      icou = 0
      ist = sf_grp%istack_grp(igrp_seed-1) + 1
      ied = sf_grp%istack_grp(igrp_seed)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) icou = icou + 1
      end do
!
      count_nsurf_for_starting = icou
!
      end function count_nsurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine set_isurf_for_starting                                 &
     &         (ele, sf_grp, igrp_seed, fln_src)
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      integer(kind = kint), intent(in) :: igrp_seed
!
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: isurf, inum, iele, ist, ied
!
!
      inum = 0
      ist = sf_grp%istack_grp(igrp_seed-1) + 1
      ied = sf_grp%istack_grp(igrp_seed)
      do isurf = ist, ied
        iele = sf_grp%item_sf_grp(1,isurf)
        if(ele%interior_ele(iele) .ne. izero) then
          inum = inum + 1
          fln_src%iele_start_item(1,inum) = sf_grp%item_sf_grp(1,isurf)
          fln_src%iele_start_item(2,inum) = sf_grp%item_sf_grp(2,isurf)
        end if
      end do
!
      end subroutine set_isurf_for_starting
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fields_for_fieldline                             &
     &         (mesh, group, fln_prm, fln_src, fln_tce)
!
      use t_mesh_data
      use start_surface_by_gl_table
      use start_surface_by_flux
      use start_surface_in_volume
      use start_surface_4_fline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
!
      if(fln_prm%id_fline_seed_type .eq. iflag_surface_group) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_flux'
        call s_start_surface_by_flux(mesh%node, mesh%ele, mesh%surf,    &
     &      fln_prm, fln_src, fln_tce)
      else if(fln_prm%id_fline_seed_type                                &
     &                           .eq. iflag_spray_in_domain) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_volume'
        call s_start_surface_by_volume                                  &
     &     (mesh%ele, group%ele_grp, fln_prm, fln_src, fln_tce)
      else if(fln_prm%id_fline_seed_type .eq. iflag_surface_list) then
        if(iflag_debug .gt. 0) write(*,*) 's_start_surface_by_gl_table'
        call s_start_surface_by_gl_table                                &
     &     (mesh%ele, group%ele_grp, fln_prm, fln_src)
      else if(fln_prm%id_fline_seed_type                                &
     &                           .eq. iflag_spray_in_domain) then
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 's_start_surface_4_fline'
      call s_start_surface_4_fline(mesh%node, mesh%ele, mesh%surf,      &
     &    fln_prm, fln_src, fln_tce)
      if(iflag_debug .gt. 0) write(*,*) 's_start_surface_4_fline end'
!
      end subroutine s_set_fields_for_fieldline
!
!  ---------------------------------------------------------------------
!
      end module set_fields_for_fieldline
