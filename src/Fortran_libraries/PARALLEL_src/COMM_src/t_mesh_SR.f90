!>@file   t_mesh_SR.f90
!!@brief  module t_mesh_SR
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in June, 2021
!
!>@brief  Work area for data communications for mesh data
!!
!!@verbatim
!!      subroutine FEM_comm_initialization(mesh, m_SR)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine init_nod_send_recv(mesh, SR_sig, SR_r, SR_i, SR_il)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!      subroutine init_send_recv(nod_comm, SR_sig, SR_r, SR_i, SR_il)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!@endverbatim
!!
      module t_mesh_SR
!
      use m_precision
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
      use t_vector_for_solver
!
      implicit none
!
!>      Structure of work area for mesh communications
      type mesh_SR
!>        Structure of communication flags
        type(send_recv_status) :: SR_sig
!>        Structure of communication buffer for 8-byte real
        type(send_recv_real_buffer) :: SR_r
!
!>        Structure of communication buffer for 4-byte integer
        type(send_recv_int_buffer) :: SR_i
!>        Structure of communication buffer for 8-byte integer
        type(send_recv_int8_buffer) :: SR_il
!
!>        Structure for vectors for solver
        type(vectors_4_solver) :: v_sol
      end type mesh_SR
!
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine FEM_comm_initialization(mesh, m_SR)
!
      use m_phys_constants
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call alloc_iccgN_vector                                           &
     &   (n_sym_tensor, mesh%node%numnod, m_SR%v_sol)
      call init_send_recv                                               &
     &   (mesh%nod_comm, m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i, m_SR%SR_il)
!
      end subroutine FEM_comm_initialization
!
! ----------------------------------------------------------------------
!
      subroutine init_nod_send_recv(mesh, SR_sig, SR_r, SR_i, SR_il)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: mesh
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call init_send_recv(mesh%nod_comm, SR_sig, SR_r, SR_i, SR_il)
!
      end subroutine init_nod_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine init_send_recv(nod_comm, SR_sig, SR_r, SR_i, SR_il)
!
      use m_phys_constants
      use t_comm_table
!
      type(communication_table), intent(in) :: nod_comm
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      type(send_recv_int8_buffer), intent(inout) :: SR_il
!
!
      call resize_work_SR                                               &
     &   (n_sym_tensor, nod_comm%num_neib, nod_comm%num_neib,           &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig, SR_r)
      call resize_iwork_SR_t(nod_comm%num_neib, nod_comm%num_neib,      &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig, SR_i)
      call resize_i8work_SR(nod_comm%num_neib, nod_comm%num_neib,       &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig, SR_il)
!
      end subroutine init_send_recv
!
! ----------------------------------------------------------------------
!
      end module t_mesh_SR
