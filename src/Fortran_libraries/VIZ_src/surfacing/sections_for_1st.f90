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
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_surface_group
      use m_surface_group_connect
      use m_node_phys_data
      use m_cross_section
!
!
      call cross_section_init(numnod, numele, numsurf, numedge,         &
     &          nnod_4_ele, nnod_4_edge, ie, ie_edge,                   &
     &          isf_4_ele, iedge_4_sf, iedge_4_ele,                     &
     &          interior_ele, inod_global, xx,                          &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_surf, num_surf_bc, surf_name, surf_istack,          &
     &          surf_item, ntot_node_sf_grp, inod_stack_sf_grp,         &
     &          inod_surf_grp, num_nod_phys, phys_nod_name)
!
      end subroutine cross_section_init_1st
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_init_1st
!
      use m_geometry_parameter
      use m_geometry_data
      use m_element_group
      use m_node_phys_data
      use m_isosurface
!
!
      call isosurface_init                                              &
     &         (numnod, numele, numsurf, numedge, nnod_4_edge,          &
     &          ie_edge, isf_4_ele, iedge_4_sf, interior_ele,           &
     &          inod_smp_stack, iele_smp_stack,                         &
     &          isurf_smp_stack, iedge_smp_stack,                       &
     &          num_mat, num_mat_bc, mat_name, mat_istack, mat_item,    &
     &          num_nod_phys, phys_nod_name)
!
      end subroutine isosurface_init_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cross_section_main_1st(istep_psf)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_cross_section
!
      integer(kind = kint), intent(in) :: istep_psf
!
!
      call cross_section_main(istep_psf, numnod, numedge,               &
     &          nnod_4_edge, ie_edge, num_nod_phys, num_tot_nod_phys,   &
     &          istack_nod_component, d_nod)
!
      end subroutine cross_section_main_1st
!
!  ---------------------------------------------------------------------
!
      subroutine isosurface_main_1st(istep_iso)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_isosurface
!
      integer(kind = kint), intent(in) :: istep_iso
!
      call isosurface_main(istep_iso,                                   &
     &          numnod, numele, numedge, nnod_4_ele, nnod_4_edge,       &
     &          ie, ie_edge, iedge_4_ele, inod_global,                  &
     &          xx, radius, a_radius, s_cylinder, a_s_cylinder,         &
     &          inod_smp_stack, num_nod_phys, num_tot_nod_phys,         &
     &          istack_nod_component, d_nod)
!
      end subroutine isosurface_main_1st
!
!  ---------------------------------------------------------------------
!
      end module sections_for_1st
