!>@file  set_ucd_data_to_type.f90
!!       module set_ucd_data_to_type
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in July, 2007
!
!> @brief Link field structure data to IO structure for data IO
!!
!!@verbatim
!!      subroutine link_num_field_type_2_output(node, ele, ucd)
!!      subroutine link_node_data_type_2_output(node, ucd)
!!      subroutine link_ele_data_type_2_output(ele, ucd)
!!      subroutine link_field_data_type_2_output(node, phys_nod, ucd)
!!      subroutine link_nnod_stacks_type_2_output                       &
!!     &         (nprocs, node, ele, m_ucd)
!!
!!      subroutine alloc_phys_name_type_by_output(ucd, phys_nod)
!!      subroutine alloc_phys_data_type_by_output(ucd, node, phys_nod)
!!      subroutine set_ucd_data_type_from_IO(my_rank, istep_ucd,        &
!!     &          numnod, ucd, phys_nod)
!!
!!      subroutine set_ucd_data_type_from_IO_once(my_rank, istep_ucd,   &
!!     &          numnod, ucd, phys_nod)
!!      subroutine add_by_ucd_data_type(my_rank, istep_ucd,             &
!!     &          numnod, ucd, phys_nod)
!!      subroutine subtract_by_ucd_data_type(my_rank, istep_ucd,        &
!!     &          numnod, ucd, phys_nod)
!!@endverbatim
!
      module set_ucd_data_to_type
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine link_node_data_type_2_output(node, ucd)
!
      use t_geometry_data
      use t_ucd_data
      use set_ucd_data
!
      type(node_data), intent(in) :: node
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_node_data_2_output(node%numnod, node%inod_global,       &
     &     node%xx, ucd)
!
      end subroutine link_node_data_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_ele_data_type_2_output(ele, ucd)
!
      use t_geometry_data
      use t_ucd_data
      use set_ucd_data
!
      type(element_data), intent(in) :: ele
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_ele_data_2_output(ele%numele, ele%nnod_4_ele,           &
     &   ele%iele_global, ele%ie, ucd)
!
      end subroutine link_ele_data_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_num_field_type_2_output(node, phys_nod, ucd)
!
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
      use set_ucd_data
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: phys_nod
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_num_field_2_output(node%numnod, phys_nod%num_phys_viz,  &
     &    ucd)
!
      end subroutine link_num_field_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_field_data_type_2_output(numnod, phys_nod, ucd)
!
      use t_phys_data
      use t_ucd_data
      use set_ucd_data
!
      integer(kind = kint),  intent(in) :: numnod
      type(phys_data), intent(in) :: phys_nod
      type(ucd_data), intent(inout) :: ucd
!
!
      call link_field_data_2_output(numnod, phys_nod%num_phys,          &
     &    phys_nod%ntot_phys, phys_nod%num_phys_viz,                    &
     &    phys_nod%ntot_phys_viz, phys_nod%num_component,               &
     &    phys_nod%phys_name, phys_nod%d_fld, ucd)
!
      end subroutine link_field_data_type_2_output
!
!-----------------------------------------------------------------------
!
      subroutine link_nnod_stacks_type_2_output                         &
     &         (nprocs, node, ele, m_ucd)
!
      use t_geometry_data
      use t_ucd_data
      use set_ucd_data
!
      integer(kind = kint),  intent(in) :: nprocs
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(merged_ucd_data), intent(inout) :: m_ucd
!
!
      call link_numnod_stacks_2_output(nprocs, node%istack_numnod,      &
     &    node%istack_internod, ele%istack_numele, m_ucd)
!
      end subroutine link_nnod_stacks_type_2_output
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_phys_name_type_by_output(ucd, phys_nod)
!
      use t_phys_data
      use t_ucd_data
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
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
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
!-----------------------------------------------------------------------
!
      subroutine set_ucd_data_type_from_IO(my_rank, istep_ucd,          &
     &          ucd, node, phys_nod)
!
      use t_geometry_data
      use t_phys_data
      use t_ucd_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      type(ucd_data), intent(inout) :: ucd
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: phys_nod
!
!
      call sel_read_udt_file(my_rank, istep_ucd, ucd)
      call set_field_by_udt_data(node%numnod, phys_nod%num_phys,        &
     &    phys_nod%ntot_phys, phys_nod%istack_component,                &
     &    phys_nod%phys_name, phys_nod%d_fld, ucd)
!
      end subroutine set_ucd_data_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_ucd_data_type_from_IO_once(my_rank, istep_ucd,     &
     &          numnod, ucd, phys_nod)
!
      use t_phys_data
      use t_ucd_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      integer(kind = kint),  intent(in) :: numnod
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: phys_nod
!
!
      ucd%nnod = numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
      call set_field_by_udt_data(numnod, phys_nod%num_phys,             &
     &    phys_nod%ntot_phys, phys_nod%istack_component,                &
     &    phys_nod%phys_name, phys_nod%d_fld, ucd)
      call deallocate_ucd_data(ucd)
!
      end subroutine set_ucd_data_type_from_IO_once
!
! -----------------------------------------------------------------------
!
      subroutine add_by_ucd_data_type(my_rank, istep_ucd,               &
     &          numnod, ucd, phys_nod)
!
      use t_phys_data
      use t_ucd_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      integer(kind = kint),  intent(in) :: numnod
      type(ucd_data), intent(inout) :: ucd
      type(phys_data), intent(inout) :: phys_nod
!
!
      ucd%nnod = numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
      call add_field_by_udt_data(numnod, phys_nod%num_phys,             &
     &    phys_nod%ntot_phys, phys_nod%istack_component,                &
     &    phys_nod%phys_name, phys_nod%d_fld, ucd)
      call deallocate_ucd_data(ucd)
!
      end subroutine add_by_ucd_data_type
!
! -----------------------------------------------------------------------
!
      subroutine subtract_by_ucd_data_type(my_rank, istep_ucd,          &
     &          numnod, ucd, phys_nod)
!
      use t_phys_data
      use t_ucd_data
      use set_and_cal_udt_data
      use ucd_IO_select
!
      integer(kind = kint),  intent(in) :: my_rank, istep_ucd
      integer(kind = kint),  intent(in) :: numnod
      type(phys_data), intent(inout) :: phys_nod
      type(ucd_data), intent(inout) :: ucd
!
!
      ucd%nnod = numnod
      call sel_read_alloc_udt_file(my_rank, istep_ucd, ucd)
      call subtract_field_by_udt_data(numnod, phys_nod%num_phys,        &
     &    phys_nod%ntot_phys, phys_nod%istack_component,                &
     &    phys_nod%phys_name, phys_nod%d_fld, ucd)
      call deallocate_ucd_data(ucd)
!
      end subroutine subtract_by_ucd_data_type
!
! -----------------------------------------------------------------------
!
      end module set_ucd_data_to_type
