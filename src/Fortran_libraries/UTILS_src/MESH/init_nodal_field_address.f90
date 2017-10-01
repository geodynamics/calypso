!>@file   init_nodal_field_address.f90
!!        module init_nodal_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!
!> @brief Set start address for nodal field data
!!
!!@verbatim
!!      subroutine init_nod_fld_address(node, nod_fld, iphys_nod)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(phys_address), intent(inout) :: iphys_nod
!!@endverbatim
!!
!
      module init_nodal_field_address
!
      use m_precision
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine init_nod_fld_address(node, nod_fld, iphys_nod)
!
      use set_field_address
!
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
      type(phys_address), intent(inout) :: iphys_nod
!
!
      call alloc_phys_data_type(node%numnod, nod_fld)
      call set_field_addresses(ione, nod_fld%num_phys,                  &
     &    nod_fld%phys_name, nod_fld%num_component, iphys_nod)
!
      end subroutine init_nod_fld_address
!
!  --------------------------------------------------------------------
!
      end module init_nodal_field_address
