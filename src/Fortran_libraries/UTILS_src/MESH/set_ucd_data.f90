!set_ucd_data.f90
!      module set_ucd_data
!
!        programmed by H.Matsui on July, 2006
!
!      subroutine link_num_field_2_output
!      subroutine link_global_mesh_4_ucd
!      subroutine link_local_mesh_4_ucd
!      subroutine link_field_data_2_output
!      subroutine allocate_phys_data_by_output
!
!      subroutine set_ucd_data_from_IO
!      subroutine add_by_ucd_data
!      subroutine subtract_by_ucd_data
!
      module set_ucd_data
!
      use m_precision
      use m_constants
!
      use m_ucd_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_num_field_2_output
!
      use m_geometry_parameter
      use m_node_phys_data
!
!
      nnod_ucd = numnod
      ntot_comp_ucd = num_nod_phys_vis
!
      end subroutine link_num_field_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_global_mesh_4_ucd
!
      use m_geometry_parameter
      use m_geometry_data
      use set_and_cal_udt_data
!
!
      xx_ucd =>      xx
      inod_gl_ucd => globalnodid
!
      call count_udt_elements(internal_node, numele, nnod_4_ele, ie)
      call allocate_ucd_ele
!
      call set_udt_global_connect(internal_node, numele, nnod_4_ele,    &
     &    globalelmid, ie)
!
      end subroutine link_global_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_local_mesh_4_ucd
!
      use m_geometry_parameter
      use m_geometry_data
      use set_and_cal_udt_data
!
!
      call allocate_ucd_node
      call set_udt_local_nodes(numnod, xx)
!
      call count_udt_elements(internal_node, numele, nnod_4_ele, ie)
      call allocate_ucd_ele
!
      call set_udt_local_connect(internal_node, numele, nnod_4_ele, ie)
!
      end subroutine link_local_mesh_4_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_field_data_2_output
!
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
!
      nnod_ucd =      numnod
      num_field_ucd = num_nod_phys_vis
      ntot_comp_ucd = num_tot_nod_phys_vis
!
      istack_comp_ucd =>   istack_nod_component(0:num_field_ucd)
      num_comp_ucd =>      num_nod_component(1:num_field_ucd)
      phys_name_ucd =>     phys_nod_name(1:num_field_ucd)
!
      d_nod_ucd =>       d_nod(1:nnod_ucd,1:ntot_comp_ucd)
!
      end subroutine link_field_data_2_output
!
!-----------------------------------------------------------------------
!
      subroutine allocate_phys_data_by_output
!
      use m_node_phys_data
      use cal_minmax_and_stacks
!
!
      num_nod_phys =     num_field_ucd
      num_nod_phys_vis = num_field_ucd
!
      call allocate_phys_name
!
      num_nod_component(1:num_nod_phys) = num_comp_ucd(1:num_nod_phys)
      phys_nod_name(1:num_nod_phys)=  phys_name_ucd(1:num_nod_phys)
!
      call s_cal_total_and_stacks(num_nod_phys, num_nod_component,      &
     &    izero, istack_nod_component, num_tot_nod_phys)
      num_tot_nod_phys_vis = num_tot_nod_phys
!
      call allocate_data_arrays
!
      end subroutine allocate_phys_data_by_output
!
!-----------------------------------------------------------------------
!
      subroutine set_ucd_data_from_IO
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_and_cal_udt_data
!
!
      call set_field_by_udt_data(numnod, num_nod_phys,                  &
     &    num_tot_nod_phys, num_nod_component, phys_nod_name, d_nod)
!
      end subroutine set_ucd_data_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine add_by_ucd_data
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_and_cal_udt_data
!
!
      call add_field_by_udt_data(numnod, num_nod_phys,                  &
     &    num_tot_nod_phys, num_nod_component, phys_nod_name, d_nod)
!
      end subroutine add_by_ucd_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_by_ucd_data
!
      use m_geometry_parameter
      use m_node_phys_data
      use set_and_cal_udt_data
!
!
      call subtract_field_by_udt_data(numnod, num_nod_phys,             &
     &    num_tot_nod_phys, num_nod_component, phys_nod_name, d_nod)
!
      end subroutine subtract_by_ucd_data
!
! -----------------------------------------------------------------------
!
      end module set_ucd_data
