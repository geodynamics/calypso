!m_node_phys_data.f90
!     module m_node_phys_data
!
!> @brief nodal field data for FEM
!
!     Written by H. Matsui
!
!       subroutine deallocate_phys_name
!       subroutine deallocate_data_arrays
!
      module m_node_phys_data
!
      use m_precision
      use t_phys_data
      use t_phys_address
!
      implicit  none
!
!>       label   for simulation
      character(len=kchara)   :: label_sim
!
!>       Structure for nodal field data
      type(phys_data), save :: nod_fld1
!
!>       address for nodal fields
      type(phys_address), save :: iphys
!
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
       subroutine deallocate_phys_name
!
       call dealloc_phys_name_type(nod_fld1)
!
       end subroutine deallocate_phys_name
!
!  --------------------------------------------------------------------
!
       subroutine deallocate_data_arrays
!
       call dealloc_phys_data_type(nod_fld1)
!
       end subroutine deallocate_data_arrays
!
!  --------------------------------------------------------------------
!
      end module m_node_phys_data
