!>@file   t_SPH_mesh_field_array.f90
!!@brief  module t_SPH_mesh_field_array
!!
!!@author H. Matsui
!!@date Programmed on Sep., 2017
!!
!!@brief  indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine dealloc_sph_mesh_array(sph_array)
!!      subroutine alloc_sph_mesh_array(num_pe, sph_array)
!!        type(sph_mesh_array), intent(inout) :: sph_array
!!@endverbatim
!
!
      module t_SPH_mesh_field_array
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
!
      use calypso_mpi
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
      use t_phys_address
      use t_phys_data
!
      use t_sph_grid_maker_in_sim
!
      implicit none
!
!> Structure of spherical transform mesh information
      type sph_mesh_array
!>         Nnumber of subdomain
        integer :: num_pe
!>         spherical harmonics indexing data
        type(sph_grids), allocatable ::       sph(:)
!>         communication tables for spherical transform
        type(sph_comm_tables), allocatable :: comms(:)
!>         grouping data for harmonics indices
        type(sph_group_data), allocatable ::  sph_grps(:)
!
!>        address for spectr data (poloidal component for vector)
        type(phys_address) :: ipol
!>        Structure for field data
        type(phys_data), allocatable :: fld(:)
!
!>        Structure to check and construct spherical shell mesh
!        type(sph_grid_maker_in_sim) :: sph_maker
      end type sph_mesh_array
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sph_mesh_array(num_pe, sph_array)
!
      integer, intent(in) :: num_pe
      type(sph_mesh_array), intent(inout) :: sph_array
!
      sph_array%num_pe = num_pe
      allocate(sph_array%sph(sph_array%num_pe))
      allocate(sph_array%comms(sph_array%num_pe))
      allocate(sph_array%sph_grps(sph_array%num_pe))
      allocate(sph_array%fld(sph_array%num_pe))
!
      end subroutine alloc_sph_mesh_array
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sph_mesh_array(sph_array)
!
      type(sph_mesh_array), intent(inout) :: sph_array
!
!
      deallocate(sph_array%sph)
      deallocate(sph_array%comms)
      deallocate(sph_array%sph_grps)
      deallocate(sph_array%fld)
!
      end subroutine dealloc_sph_mesh_array
!
!  ---------------------------------------------------------------------
!
      end module t_SPH_mesh_field_array
