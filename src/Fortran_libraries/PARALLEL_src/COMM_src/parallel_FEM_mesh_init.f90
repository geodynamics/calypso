!>@file   parallel_FEM_mesh_init.f90
!!@brief  module parallel_FEM_mesh_init
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Top routines to transfer spherical harmonics grids data
!!       to FEM data for data visualization
!!
!!@verbatim
!!      subroutine FEM_mesh_init_with_IO                                &
!!     &        (iflag_output_SURF, mesh_file, mesh, group, ele_mesh)
!!      subroutine FEM_mesh_initialization(mesh, group, ele_mesh)
!!        type(field_io_params), intent(in) :: mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!@endverbatim
!!
      module parallel_FEM_mesh_init
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use calypso_mpi
!
      use t_mesh_data
!
      implicit none
!
      private :: check_whole_num_of_elements
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_mesh_init_with_IO                                  &
     &         (iflag_output_SURF, mesh_file, mesh, group, ele_mesh)
!
      use t_file_IO_parameter
      use t_read_mesh_data
!
      use m_array_for_send_recv
!
      use nod_phys_send_recv
      use node_monitor_IO
      use const_mesh_information
      use const_element_comm_tables
      use mesh_file_name_by_param
!
      integer(kind = kint), intent(in) :: iflag_output_SURF
      type(field_io_params), intent(in) :: mesh_file
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(surf_edge_IO_file) :: ele_mesh_IO
      integer(kind = kint) :: iflag_ele_mesh
!
!
      iflag_ele_mesh =  check_exist_ele_mesh(mesh_file, izero)          &
     &                + check_exist_surf_mesh(mesh_file, izero)         &
     &                + check_exist_edge_mesh(mesh_file, izero)
      if(iflag_ele_mesh .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'mpi_load_element_surface_edge'
        call mpi_load_element_surface_edge                              &
     &     (mesh_file, mesh, ele_mesh, ele_mesh_IO)
      end if
      call calypso_mpi_barrier
!
!  -------------------------------
!      if (iflag_debug.gt.0) write(*,*) 'set_local_node_id_4_monitor'
!      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
!  ------  In itialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh)
!
!  -----    construct geometry informations
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_ele_mesh .eq. 0) return
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(mesh, ele_mesh)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(mesh%ele)
      end if
!
      if(iflag_ele_mesh .ne. 0 .and. iflag_output_SURF .gt. 0) then
        call mpi_output_element_surface_edge                            &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
      end if
!
!      call deallocate_surface_geom_type(ele_mesh%surf)
!      call dealloc_edge_geometory(ele_mesh%edge)
!
      end subroutine FEM_mesh_init_with_IO
!
!-----------------------------------------------------------------------
!
      subroutine FEM_mesh_initialization(mesh, group, ele_mesh)
!
      use t_file_IO_parameter
      use t_read_mesh_data
!
      use m_array_for_send_recv
!
      use nod_phys_send_recv
      use node_monitor_IO
      use const_mesh_information
      use const_element_comm_tables
      use mesh_file_name_by_param
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!  -------------------------------
!      if (iflag_debug.gt.0) write(*,*) 'set_local_node_id_4_monitor'
!      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
!  ------  In itialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh)
!
!  -----    construct geometry informations
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(mesh, ele_mesh)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(mesh%ele)
      end if
!
!      call deallocate_surface_geom_type(ele_mesh%surf)
!      call dealloc_edge_geometory(ele_mesh%edge)
!
      end subroutine FEM_mesh_initialization
!
!-----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mpi_load_element_surface_edge                          &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
!
      use t_file_IO_parameter
      use t_read_mesh_data
      use load_element_mesh_data
      use element_mesh_MPI_IO_select
!
      type(field_io_params), intent(in) :: mesh_file
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(inout) :: ele_mesh
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call sel_mpi_read_ele_mesh(mesh_file, ele_mesh_IO)
      call set_ele_comm_tbl_from_IO                                     &
     &   (mesh%ele, ele_mesh%ele_comm, ele_mesh_IO)
!
!
      call sel_mpi_read_surf_mesh(mesh_file, ele_mesh_IO)
      call set_surface_mesh_from_IO                                     &
     &   (mesh%ele, ele_mesh%surf, ele_mesh%surf_comm, ele_mesh_IO)
!
!
      call sel_mpi_read_edge_mesh(mesh_file, ele_mesh_IO)
      call set_edge_mesh_from_IO(mesh%ele, ele_mesh%surf,               &
     &    ele_mesh%edge, ele_mesh%edge_comm, ele_mesh_IO)
!
      end subroutine mpi_load_element_surface_edge
!
!  ---------------------------------------------------------------------
!
      subroutine mpi_output_element_surface_edge                        &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
!
      use t_file_IO_parameter
      use t_read_mesh_data
      use load_element_mesh_data
      use element_mesh_MPI_IO_select
!
      type(field_io_params), intent(in) :: mesh_file
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(surf_edge_IO_file), intent(inout) :: ele_mesh_IO
!
!
      call set_ele_comm_tbl_to_IO                                       &
     &   (mesh%ele, ele_mesh%ele_comm, ele_mesh_IO)
      call sel_mpi_write_ele_mesh_file(mesh_file, ele_mesh_IO)
!
!
      call set_surface_mesh_to_IO                                       &
     &   (mesh%ele, ele_mesh%surf, ele_mesh%surf_comm, ele_mesh_IO)
      call sel_mpi_write_surf_mesh_file(mesh_file, ele_mesh_IO)
!
!
      call set_edge_mesh_to_IO(mesh%ele, ele_mesh%surf, ele_mesh%edge,  &
     &   ele_mesh%edge_comm, ele_mesh_IO)
      call sel_mpi_write_edge_mesh_file(mesh_file, ele_mesh_IO)
!
      end subroutine mpi_output_element_surface_edge
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_whole_num_of_elements(ele)
!
      use calypso_mpi
      use m_machine_parameter
      use t_geometry_data
!
      type(element_data), intent(inout) :: ele
!
      integer (kind = kint) :: iproc, iele, ist, ied
      integer (kind = kint) :: nele_l, nele_g
      integer (kind = kint) :: nele_smp(np_smp)
!
!
      nele_g = 0
      nele_l = 0
      nele_smp = 0
!
!$omp parallel do private(iele,ist,ied)
      do iproc = 1, np_smp
        ist = ele%istack_ele_smp(iproc-1)+1
        ied = ele%istack_ele_smp(iproc)
        do iele = ist, ied
          nele_smp(iproc) = nele_smp(iproc) + ele%interior_ele(iele)
        end do
      end do
!$omp end parallel do
!
      do iproc = 1, np_smp
        nele_l = nele_l + nele_smp(iproc)
      end do
!
      call MPI_allREDUCE ( nele_l, nele_g, 1, CALYPSO_INTEGER,          &
     &   MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if (my_rank.eq.0) write(*,*)                                      &
     &      'number of element for whole domain:  ', nele_g
!
      end subroutine check_whole_num_of_elements
!
! ----------------------------------------------------------------------
!
      end module parallel_FEM_mesh_init
