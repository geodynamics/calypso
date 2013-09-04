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
      use m_node_phys_data
      use set_field_address
!
!
!  allocation for physical values
!
      call allocate_data_arrays
!
!   set address of nodal values
!
      call set_field_addresses(ione, num_nod_phys, phys_nod_name,       &
     &    num_nod_component, iphys)
!
       end subroutine initialize_nod_field_data
!
!  --------------------------------------------------------------------
!
      end module m_node_phys_address
