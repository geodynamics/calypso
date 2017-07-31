!>@file  set_ucd_data_to_type.f90
!!       module set_ucd_data_to_type
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Link field structure data to IO structure for data IO
!!
!!@verbatim
!!      subroutine link_local_mesh_2_ucd(node, ele, ucd)
!!      subroutine link_global_mesh_2_ucd(node, ele, ucd)
!!
!!      subroutine link_num_field_2_ucd(ele, ucd)
!!      subroutine link_node_data_2_ucd(node, ucd)
!!      subroutine link_ele_data_2_ucd(ele, ucd)
!!      subroutine link_field_data_to_ucd(node, phys_nod, ucd)
!!      subroutine link_nnod_stacks_2_ucd(nprocs, node, ele, m_ucd)
!!
!!      subroutine alloc_phys_name_type_by_output(ucd, phys_nod)
!!      subroutine alloc_phys_data_type_by_output(ucd, node, phys_nod)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine set_data_by_read_ucd                                 &
!!     &         (my_rank, istep_ucd, ucd_param, t_IO, ucd, nod_fld)
!!
!!      subroutine set_data_by_read_ucd_once(my_rank, istep_ucd,        &
!!     &          ucd_param, nod_fld, t_IO)
!!      subroutine add_ucd_to_data                                      &
!!     &         (my_rank, istep_ucd, ucd_param, nod_fld)
!!      subroutine subtract_by_ucd_data                                 &
!!     &         (my_rank, istep_ucd, ucd_param, nod_fld)
!!        type(field_IO_params), intent(in) :: ucd_param
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_ucd_data_to_type
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_phys_data
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_local_mesh_2_ucd(node, ele, ucd)
!
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call const_udt_local_nodes(node%numnod, node%xx, ucd)
      call const_udt_local_connect(node%internal_node,                  &
     &    ele%numele, ele%nnod_4_ele, ele%ie, ucd)
!
      end subroutine link_local_mesh_2_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_global_mesh_2_ucd(node, ele, ucd)
!
      use set_and_cal_udt_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_ucd(node, ucd)
      call const_udt_global_connect(node%internal_node,                 &
     &    ele%numele, ele%nnod_4_ele, ele%iele_global, ele%ie, ucd)
!
      end subroutine link_global_mesh_2_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_node_data_2_ucd(node, ucd)
!
      use set_ucd_data
!
      type(node_data), intent(in) :: node
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_output(node%numnod, node%inod_global,       &
     &    node%xx, ucd)
!
      end subroutine link_node_data_2_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_ele_data_2_ucd(ele, ucd)
!
      use set_ucd_data
!
      type(element_data), intent(in) :: ele
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_ele_data_2_output(ele%numele, ele%nnod_4_ele,           &
     &   ele%iele_global, ele%ie, ucd)
!
      end subroutine link_ele_data_2_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_num_field_2_ucd(phys_nod, ucd)
!
      use set_ucd_data
!
      type(phys_data), intent(in) :: phys_nod
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_num_field_2_output                                      &
     &   (phys_nod%n_point, phys_nod%num_phys_viz, ucd)
!
      end subroutine link_num_field_2_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_field_data_to_ucd(phys_nod, ucd)
!
      use set_ucd_data
!
      type(phys_data), intent(in) :: phys_nod
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_field_data_2_output                                     &
     &   (phys_nod%n_point, phys_nod%num_phys,                          &
     &    phys_nod%ntot_phys, phys_nod%num_phys_viz,                    &
     &    phys_nod%ntot_phys_viz, phys_nod%num_component,               &
     &    phys_nod%phys_name, phys_nod%d_fld, ucd)
!
      end subroutine link_field_data_to_ucd
!
!-----------------------------------------------------------------------
!
      subroutine link_nnod_stacks_2_ucd(nprocs, node, ele, m_ucd)
!
      use set_ucd_data
!
      integer(kind = kint),  intent(in) :: nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      call link_numnod_stacks_2_output(nprocs, node%istack_numnod,      &
     &    node%istack_internod, ele%istack_interele, m_ucd)
!
      end subroutine link_nnod_stacks_2_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_phys_name_type_by_output(ucd, phys_nod)
!
      use cal_minmax_and_stacks
!
      type(ucd_data), intent(in) :: ucd
      type(phys_data), intent(inout) :: phys_nod
!
!
      phys_nod%num_phys =     ucd%num_field
      phys_nod%num_phys_viz = ucd%num_field
!
      call alloc_phys_name_type(phys_nod)
!
      phys_nod%num_component(1:phys_nod%num_phys)                       &
     &           = ucd%num_comp(1:phys_nod%num_phys)
      phys_nod%phys_name(1:phys_nod%num_phys)                           &
     &           = ucd%phys_name(1:phys_nod%num_phys)
!
      call s_cal_total_and_stacks(phys_nod%num_phys,                    &
     &    phys_nod%num_component, izero, phys_nod%istack_component,     &
     &    phys_nod%ntot_phys)
      phys_nod%ntot_phys_viz = phys_nod%ntot_phys
!
      end subroutine alloc_phys_name_type_by_output
!
!-----------------------------------------------------------------------
!
      subroutine alloc_phys_data_type_by_output(ucd, node, phys_nod)
!
      type(ucd_data), intent(in) :: ucd
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
      call alloc_phys_name_type_by_output(ucd, phys_nod)
      call alloc_phys_data_type(node%numnod, phys_nod)
!
      end subroutine alloc_phys_data_type_by_output
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_data_by_read_ucd                                   &
     &         (my_rank, istep_ucd, ucd_param, t_IO, ucd, nod_fld)
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
      type(time_data), intent(inout) :: t_IO
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: nod_fld
!
!
      call sel_read_udt_file(my_rank, istep_ucd, ucd_param, t_IO, ucd)
      call set_field_by_udt_data(nod_fld%n_point, nod_fld%num_phys,     &
     &    nod_fld%ntot_phys, nod_fld%istack_component,                  &
     &    nod_fld%phys_name, nod_fld%d_fld, ucd)
!
      end subroutine set_data_by_read_ucd
!
! -----------------------------------------------------------------------
!
      subroutine set_data_by_read_ucd_once(my_rank, istep_ucd,          &
     &          ucd_param, nod_fld, t_IO)
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      type(field_IO_params), intent(in) :: ucd_param
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
!
      type(phys_data), intent(inout) :: nod_fld
      type(time_data), intent(inout) :: t_IO
!
      type(ucd_data) :: local_ucd
!
!
      local_ucd%nnod = nod_fld%n_point
      call sel_read_alloc_udt_file                                      &
     &   (my_rank, istep_ucd, ucd_param, t_IO, local_ucd)
      call set_field_by_udt_data(nod_fld%n_point, nod_fld%num_phys,     &
     &    nod_fld%ntot_phys, nod_fld%istack_component,                  &
     &    nod_fld%phys_name, nod_fld%d_fld, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine set_data_by_read_ucd_once
!
! -----------------------------------------------------------------------
!
      subroutine add_ucd_to_data                                        &
     &         (my_rank, istep_ucd, ucd_param, nod_fld)
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
!
      type(phys_data), intent(inout) :: nod_fld
!
      type(time_data) :: local_t_IO
      type(ucd_data) :: local_ucd
!
!
      local_ucd%nnod =  nod_fld%n_point
      call sel_read_alloc_udt_file                                      &
     &   (my_rank, istep_ucd, ucd_param, local_t_IO, local_ucd)
      call add_field_by_udt_data(nod_fld%n_point, nod_fld%num_phys,     &
     &    nod_fld%ntot_phys, nod_fld%istack_component,                  &
     &    nod_fld%phys_name, nod_fld%d_fld, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine add_ucd_to_data
!
! -----------------------------------------------------------------------
!
      subroutine subtract_by_ucd_data                                   &
     &         (my_rank, istep_ucd, ucd_param, nod_fld)
!
!
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      type(field_IO_params), intent(in) :: ucd_param
!
      type(phys_data), intent(inout) :: nod_fld
!
      type(time_data) :: local_t_IO
      type(ucd_data) :: local_ucd
!
!
      local_ucd%nnod = nod_fld%n_point
      call sel_read_alloc_udt_file                                      &
     &   (my_rank, istep_ucd, ucd_param, local_t_IO, local_ucd)
      call subtract_field_by_udt_data                                   &
     &   (nod_fld%n_point, nod_fld%num_phys,                            &
     &    nod_fld%ntot_phys, nod_fld%istack_component,                  &
     &    nod_fld%phys_name, nod_fld%d_fld, local_ucd)
      call deallocate_ucd_data(local_ucd)
!
      end subroutine subtract_by_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module set_ucd_data_to_type
