!>@file   mpi_load_mesh_data.f90
!!@brief  module mpi_load_mesh_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy FEM mesh data from IO structure
!!
!!@verbatim
!!      subroutine mpi_input_mesh(mesh_file, n_subdomain,               &
!!     &          mesh, group, nnod_4_surf, nnod_4_edge)
!!      subroutine mpi_input_mesh_geometry(mesh_file, mesh)
!!
!!      subroutine sync_group_name_4_empty(nprocs_mesh,                 &
!!     &          nod_grp, ele_grp, sf_grp)
!!
!!      subroutine mpi_output_mesh(mesh_file, mesh, group)
!!        type(field_IO_params), intent(in) ::  mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!@endverbatim
!
      module mpi_load_mesh_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine mpi_input_mesh(mesh_file, n_subdomain,                 &
     &          mesh, group, nnod_4_surf, nnod_4_edge)
!
      use mesh_MPI_IO_select
      use set_nnod_4_ele_by_type
      use load_mesh_data
!
      integer(kind = kint), intent(in) :: n_subdomain
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      integer(kind = kint), intent(inout) :: nnod_4_surf, nnod_4_edge
!
      type(mesh_data) :: fem_IO_m
!
!
      if(my_rank .lt. n_subdomain) then
        call sel_mpi_read_mesh(mesh_file, fem_IO_m)
        call set_mesh(fem_IO_m, mesh, group, nnod_4_surf, nnod_4_edge)
      else
        call set_zero_mesh_data(mesh, nnod_4_surf, nnod_4_edge)
      end if
!
      if(n_subdomain .ge. nprocs) return
!
      call sync_group_name_4_empty                                      &
     &    (n_subdomain, group%nod_grp, group%ele_grp, group%surf_grp)
!
      end subroutine mpi_input_mesh
!
! -----------------------------------------------------------------------
!
      subroutine mpi_input_mesh_geometry(mesh_file, mesh)
!
      use mesh_MPI_IO_select
      use load_mesh_data
!
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh
!
      type(mesh_geometry) :: mesh_IO_m
!
!
      call sel_mpi_read_mesh_geometry(mesh_file, mesh_IO_m)
      call set_mesh_geometry_data(mesh_IO_m,                            &
     &    mesh%nod_comm, mesh%node, mesh%ele)
!
      end subroutine mpi_input_mesh_geometry
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sync_group_name_4_empty(n_subdomain,                   &
     &          nod_grp, ele_grp, sf_grp)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: n_subdomain
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: sf_grp
!
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: nlen_comm, ntot_grp, num_grp_g(3)
      character(len=kchara), allocatable :: grp_name_g(:)
!
!
      if(my_rank .eq. 0) then
        num_grp_g(1) = nod_grp%num_grp
        num_grp_g(2) = ele_grp%num_grp
        num_grp_g(3) = sf_grp%num_grp
      end if
!
      call MPI_BCAST(num_grp_g, ithree, CALYPSO_INTEGER, 0,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ge. n_subdomain) then
        nod_grp%num_grp = num_grp_g(1)
        ele_grp%num_grp = num_grp_g(2)
        sf_grp%num_grp =  num_grp_g(3)
!
        call allocate_grp_type_num(nod_grp)
        call allocate_grp_type_num(ele_grp)
        call allocate_sf_grp_type_num(sf_grp)
      end if
!
      write(*,*) 'num_grp_g', my_rank, num_grp_g
!
      ntot_grp =  num_grp_g(1) + num_grp_g(2) + num_grp_g(3)
      allocate( grp_name_g(ntot_grp) )
!
      if(my_rank .eq. 0) then
        ied = nod_grp%num_grp
        grp_name_g(1:ied) = nod_grp%grp_name(1:ied)
        ist = nod_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp
        grp_name_g(ist:ied) = ele_grp%grp_name(1:ele_grp%num_grp)
        ist = nod_grp%num_grp + ele_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp + sf_grp%num_grp
        grp_name_g(ist:ied) = sf_grp%grp_name(1:sf_grp%num_grp)
      end if
!
      nlen_comm = kchara*ntot_grp
      call MPI_BCAST(grp_name_g, nlen_comm, CALYPSO_CHARACTER, 0,       &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ge. n_subdomain) then
        ied = nod_grp%num_grp
        nod_grp%grp_name(1:ied) =             grp_name_g(1:ied)
        ist = nod_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp
        ele_grp%grp_name(1:ele_grp%num_grp) = grp_name_g(ist:ied)
        ist = nod_grp%num_grp + ele_grp%num_grp + 1
        ied = nod_grp%num_grp + ele_grp%num_grp + sf_grp%num_grp
        sf_grp%grp_name(1:sf_grp%num_grp) =   grp_name_g(ist:ied)
!
        call allocate_grp_type_item(nod_grp)
        call allocate_grp_type_item(ele_grp)
        call allocate_sf_grp_type_item(sf_grp)
      end if
!
      deallocate(grp_name_g)
!
      end subroutine sync_group_name_4_empty
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine mpi_output_mesh(mesh_file, mesh, group)
!
      use mesh_MPI_IO_select
      use set_comm_table_4_IO
      use set_element_data_4_IO
      use copy_mesh_structures
      use load_mesh_data
!
      type(field_IO_params), intent(in) ::  mesh_file
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
!
      type(mesh_data) :: fem_IO_m
!
!
      call copy_comm_tbl_type(mesh%nod_comm, fem_IO_m%mesh%nod_comm)
      call copy_node_geometry_types(mesh%node, fem_IO_m%mesh%node)
      call copy_ele_connect_to_IO(mesh%ele, fem_IO_m%mesh%ele)
!
      call set_grp_data_to_IO                                           &
     &   (group%nod_grp, group%ele_grp, group%surf_grp, fem_IO_m%group)
!
!       save mesh information
      call sel_mpi_write_mesh_file(mesh_file, fem_IO_m)
!
      end subroutine mpi_output_mesh
!
! -----------------------------------------------------------------------
!
      end module mpi_load_mesh_data
