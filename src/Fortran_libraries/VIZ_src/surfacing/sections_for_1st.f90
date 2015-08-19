!sections_for_1st.f90
!      module sections_for_1st
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine init_visualize_surface
!      subroutine visualize_surface(istep_psf, istep_iso)
!
!      subroutine cross_section_init_1st
!      subroutine isosurface_init_1st
!
!      subroutine cross_section_main_1st(istep_psf)
!      subroutine isosurface_main_1st(istep_iso)
!
      module sections_for_1st
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize_surface
!
      use m_control_data_sections
      use m_cross_section
      use m_isosurface
!
      use set_psf_case_table
!
!
      if ( (num_psf_ctl+num_iso_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  call cross_section_init_1st
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) call isosurface_init_1st
!
      end subroutine init_visualize_surface
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_surface(istep_psf, istep_iso)
!
      use m_cross_section
      use m_isosurface
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
!
!
      if (num_psf.gt.0 .and. istep_psf.gt.0) then
        call cross_section_main_1st(istep_psf)
      end if
      if (num_iso.gt.0 .and. istep_iso.gt.0) then
        call isosurface_main_1st(istep_iso)
      end if
!
      end subroutine visualize_surface
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_init_1st
!
      use m_nod_comm_table
      use m_ele_sf_eg_comm_tables
      use m_geometry_data
      use m_group_data
      use m_surface_group_connect
      use m_node_phys_data
      use m_cross_section
!
!
      call cross_section_init(node1%numnod, node1%internal_node,        &
     &    ele1%numele, surf1%numsurf, edge1%numedge,                    &
     &    ele1%nnod_4_ele, edge1%nnod_4_edge, ele1%ie, edge1%ie_edge,   &
     &    surf1%isf_4_ele, edge1%iedge_4_sf, edge1%iedge_4_ele,         &
     &    nod_comm, edge_comm, ele1%interior_ele,                       &
     &    node1%xx, node1%istack_nod_smp, ele1%istack_ele_smp,          &
     &    surf1%istack_surf_smp, edge1%istack_edge_smp,                 &
     &    ele_grp1, sf_grp1, sf_grp_nod1, num_nod_phys, phys_nod_name)
!
      end subroutine cross_section_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_1st
!
      use m_geometry_data
      use m_group_data
      use m_node_phys_data
      use m_isosurface
!
!
      call isosurface_init                                              &
     &   (node1%numnod, ele1%numele, surf1%numsurf, edge1%numedge,      &
     &    edge1%nnod_4_edge, edge1%ie_edge, surf1%isf_4_ele,            &
     &    edge1%iedge_4_sf, ele1%interior_ele,                          &
     &    node1%istack_nod_smp, ele1%istack_ele_smp,                    &
     &    surf1%istack_surf_smp, edge1%istack_edge_smp,                 &
     &    ele_grp1, num_nod_phys, phys_nod_name)
!
      end subroutine isosurface_init_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main_1st(istep_psf)
!
      use m_geometry_data
      use m_node_phys_data
      use m_cross_section
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      call cross_section_main(istep_psf, node1%numnod,                  &
     &    edge1%numedge, edge1%nnod_4_edge, edge1%ie_edge,              &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component, d_nod)
!
      end subroutine cross_section_main_1st
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_1st(istep_iso)
!
      use m_geometry_data
      use m_node_phys_data
      use m_isosurface
      use m_ele_sf_eg_comm_tables
!
      integer(kind = kint), intent(in) :: istep_iso
!
      call isosurface_main                                              &
     &   (istep_iso, node1%numnod, node1%internal_node, ele1%numele,    &
     &    edge1%numedge, ele1%nnod_4_ele, edge1%nnod_4_edge,            &
     &    ele1%ie, edge1%ie_edge, edge1%iedge_4_ele, node1%xx,          &
     &    node1%rr, node1%a_r, node1%ss, node1%a_s,                     &
     &    node1%istack_nod_smp, edge_comm,                              &
     &    num_nod_phys, num_tot_nod_phys, istack_nod_component, d_nod)
!
      end subroutine isosurface_main_1st
!
!  ---------------------------------------------------------------------
!
      end module sections_for_1st
