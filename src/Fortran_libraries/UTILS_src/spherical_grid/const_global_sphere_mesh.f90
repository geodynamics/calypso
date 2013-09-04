!
!      module const_global_sphere_mesh
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine s_const_global_sphere_mesh
!
      module const_global_sphere_mesh
!
      use m_precision
      use m_constants
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_global_sphere_mesh
!
      use m_machine_parameter
      use m_nod_comm_table
      use m_spheric_constants
      use m_geometry_data
      use m_read_mesh_data
      use m_sph_domain_group
!
      use set_global_sph_position
      use set_global_sph_ele_connect
      use set_node_group_global_sph
      use set_ele_group_global_sph
      use set_surf_group_global_sph
!
!
!      write(*,*) 'set_sph_domain_group'
      call set_sph_domain_group
!
      num_neib = izero
      ntot_import = izero
      ntot_export = izero
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      if (iflag_shell_mode .eq. iflag_MESH_same) then
!
        call set_sph_node_position_no_pole
        call set_sph_ele_connect_no_pole
!
        call set_node_grp_sph_no_pole
        call set_ele_grp_sph_no_pole
        call set_surf_grp_sph_no_pole
!
!
      else if (iflag_shell_mode .eq. iflag_MESH_w_pole) then
!
        if(iflag_debug .gt. 0)                                          &
     &        write(*,*) 'set_sph_node_position_w_pole'
        call set_sph_node_position_w_pole
        if(iflag_debug .gt. 0) write(*,*) 'set_sph_ele_connect_w_pole'
        call set_sph_ele_connect_w_pole
!
        if(iflag_debug .gt. 0) write(*,*) 'set_node_grp_sph_w_pole'
        call set_node_grp_sph_w_pole
        if(iflag_debug .gt. 0) write(*,*) 'set_ele_grp_sph_w_pole'
        call set_ele_grp_sph_w_pole
        if(iflag_debug .gt. 0) write(*,*) 'set_surf_grp_sph_w_pole'
        call set_surf_grp_sph_w_pole
!
!
      else if (iflag_shell_mode .eq. iflag_MESH_w_center) then
!
        call set_sph_node_position_w_center
        call set_sph_ele_connect_w_center
!
        call set_node_grp_sph_w_center
        call set_ele_grp_sph_w_center
        call set_surf_grp_sph_w_pole
!
      end if
!
      call allocate_element_geometry
!
      end subroutine s_const_global_sphere_mesh
!
!   --------------------------------------------------------------------
!
      end module const_global_sphere_mesh
