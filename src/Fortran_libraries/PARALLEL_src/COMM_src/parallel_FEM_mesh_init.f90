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
!!      subroutine FEM_mesh_initialization(mesh, group)
!!        type(field_io_params), intent(in) :: mesh_file
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
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
!      private :: check_whole_num_of_elements
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine FEM_mesh_initialization(mesh, group)
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
!
!  -------------------------------
!      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
!      call set_local_nod_4_monitor(mesh, group)
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
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos tako'
      call const_mesh_infos(my_rank, mesh, group)
!
      if(iflag_debug.gt.0) write(*,*) 'const_global_mesh_infos'
      call const_global_mesh_infos(mesh)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(mesh%ele)
      end if
!
      end subroutine FEM_mesh_initialization
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
