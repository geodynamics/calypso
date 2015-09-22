!
!     module m_node_phys_address
!
!> @brief Nodal field data addresses for FEM
!
!     Written by H. Matsui
!
!      subroutine initialize_nod_field_data
!
      module m_node_phys_address
!
      use m_precision
      use m_constants
!
      use t_phys_address
!
!
      implicit  none
! 
!    label   for simulation
      character(len=kchara)   :: label_sim
!
!   address for nodal fields
      type(phys_address), save :: iphys
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine initialize_nod_field_data
!
      use m_geometry_data
      use m_node_phys_data
      use set_field_address
!
!
!  allocation for physical values
!
      call alloc_phys_data_type(node1%numnod, nod_fld1)
!
!   set address of nodal values
!
      call set_field_addresses(ione, nod_fld1%num_phys,                 &
     &    nod_fld1%phys_name, nod_fld1%num_component, iphys)
!
       end subroutine initialize_nod_field_data
!
!  --------------------------------------------------------------------
!
      end module m_node_phys_address
