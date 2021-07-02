!>@file   nod_phys_send_recv.f90
!!@brief  module nod_phys_send_recv
!!
!!@author H. Matsui
!!@date Programmed in July, 2005
!!      modified in July, 2008
!
!>@brief  Data communication for nodal field
!!
!!@verbatim
!!      subroutine nod_fields_send_recv(mesh, nod_fld,                  &
!!     &                                v_sol, SR_sig, SR_r)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(phys_data),intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine init_real_send_recv(nod_comm, SR_sig, SR_r)
!!      subroutine fields_send_recv(nod_comm, nod_fld,                  &
!!     &                            v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_data),intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!        type(send_recv_int8_buffer), intent(inout) :: SR_il
!!
!!      subroutine scalar_send_recv(id_phys, nod_comm, nod_fld,         &
!!     &                            v_sol, SR_sig, SR_r)
!!      subroutine vector_send_recv(id_phys, nod_comm, nod_fld,         &
!!     &                            v_sol, SR_sig, SR_r)
!!      subroutine sym_tensor_send_recv(id_phys, nod_comm, nod_fld,     &
!!     &                                v_sol, SR_sig, SR_r)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(phys_data),intent(inout) :: nod_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!         id_phys:  field ID of nodal fields
!!
!!      subroutine nod_scalar_send_recv(numnod, nod_comm,               &
!!     &                                scl_nod, v_sol, SR_sig, SR_r)
!!      subroutine nod_vector_send_recv(numnod, nod_comm,               &
!!     &                                vec_nod, v_sol, SR_sig, SR_r)
!!      subroutine nod_tensor_send_recv(numnod, nod_comm,               &
!!                                      tsr_nod, v_sol, SR_sig, SR_r)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
      module nod_phys_send_recv
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use calypso_mpi
      use t_mesh_data
      use t_phys_data
      use t_vector_for_solver
      use t_solver_SR
      use t_solver_SR_int
      use t_solver_SR_int8
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine nod_fields_send_recv(mesh, nod_fld,                    &
     &                                v_sol, SR_sig, SR_r)
!
      type(mesh_geometry), intent(in) :: mesh
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call fields_send_recv(mesh%nod_comm, nod_fld,                     &
     &                      v_sol, SR_sig, SR_r)
!
      end subroutine nod_fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_real_send_recv(nod_comm, SR_sig, SR_r)
!
      type(communication_table), intent(in) :: nod_comm
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call resize_work_SR                                               &
     &   (n_sym_tensor, nod_comm%num_neib, nod_comm%num_neib,           &
     &    nod_comm%ntot_export, nod_comm%ntot_import, SR_sig, SR_r)
!
      end subroutine init_real_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine fields_send_recv(nod_comm, nod_fld,                    &
     &                            v_sol, SR_sig, SR_r)
!
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer (kind=kint) :: i, ist
!
!
      do i = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i-1) + 1
!
        if (nod_fld%num_component(i) .eq. n_vector) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for vector of ', trim(nod_fld%phys_name(i))
          call vector_send_recv(ist, nod_comm, nod_fld,                 &
     &                          v_sol, SR_sig, SR_r)
!
        else if (nod_fld%num_component(i) .eq. n_scalar) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for scaler of ', trim(nod_fld%phys_name(i))
          call scalar_send_recv(ist, nod_comm, nod_fld,                 &
     &                          v_sol, SR_sig, SR_r)
!
        else if (nod_fld%num_component(i) .eq. n_sym_tensor) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for tensor of ', trim(nod_fld%phys_name(i))
          call sym_tensor_send_recv(ist, nod_comm, nod_fld,             &
     &                              v_sol, SR_sig, SR_r)
        end if
      end do
!
      end subroutine fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv(id_phys, nod_comm, nod_fld,           &
     &                            v_sol, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_scalar_send_recv(nod_fld%n_point, nod_comm,              &
     &    nod_fld%d_fld(1,id_phys), v_sol, SR_sig, SR_r)
!
      end subroutine scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv(id_phys, nod_comm, nod_fld,           &
     &                            v_sol, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_vector_send_recv(nod_fld%n_point, nod_comm,              &
     &    nod_fld%d_fld(1,id_phys), v_sol, SR_sig, SR_r)
!
      end subroutine vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_send_recv(id_phys, nod_comm, nod_fld,       &
     &                                v_sol, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call nod_tensor_send_recv(nod_fld%n_point, nod_comm,              &
     &    nod_fld%d_fld(1,id_phys), v_sol, SR_sig, SR_r)
!
      end subroutine sym_tensor_send_recv
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine nod_scalar_send_recv(numnod, nod_comm,                 &
     &                                scl_nod, v_sol, SR_sig, SR_r)
!
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: scl_nod(numnod)
!
      integer(kind=kint)  :: inod
!
!
!$omp parallel do
       do inod=1, numnod
        v_sol%x_vec(inod) = scl_nod(inod)
       end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_type(numnod, nod_comm,                      &
     &                           SR_sig, SR_r, v_sol%x_vec(1))
!
!$omp parallel do
      do inod=1, numnod
        scl_nod(inod) = v_sol%x_vec(inod)
      end do
!$omp end parallel do
!
      end subroutine nod_scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_vector_send_recv(numnod, nod_comm,                 &
     &                                vec_nod, v_sol, SR_sig, SR_r)
!
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: vec_nod(numnod,3)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod=1, numnod
        v_sol%x_vec(3*inod-2) = vec_nod(inod,1)
        v_sol%x_vec(3*inod-1) = vec_nod(inod,2)
        v_sol%x_vec(3*inod  ) = vec_nod(inod,3)
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_3_type(numnod, nod_comm,                    &
     &                             SR_sig, SR_r, v_sol%x_vec(1))
!
!$omp parallel do
      do inod=1, numnod
        vec_nod(inod,1) = v_sol%x_vec(3*inod-2)
        vec_nod(inod,2) = v_sol%x_vec(3*inod-1)
        vec_nod(inod,3) = v_sol%x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine nod_vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_tensor_send_recv(numnod, nod_comm,                 &
     &                                tsr_nod, v_sol, SR_sig, SR_r)
!
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
!
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: tsr_nod(numnod,6)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod=1, numnod
        v_sol%x_vec(6*inod-5) = tsr_nod(inod,1)
        v_sol%x_vec(6*inod-4) = tsr_nod(inod,2)
        v_sol%x_vec(6*inod-3) = tsr_nod(inod,3)
        v_sol%x_vec(6*inod-2) = tsr_nod(inod,4)
        v_sol%x_vec(6*inod-1) = tsr_nod(inod,5)
        v_sol%x_vec(6*inod  ) = tsr_nod(inod,6)
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_6_type(numnod, nod_comm,                    &
     &                             SR_sig, SR_r, v_sol%x_vec(1))
!
!$omp parallel do
      do inod=1, numnod
        tsr_nod(inod,1) = v_sol%x_vec(6*inod-5)
        tsr_nod(inod,2) = v_sol%x_vec(6*inod-4)
        tsr_nod(inod,3) = v_sol%x_vec(6*inod-3)
        tsr_nod(inod,4) = v_sol%x_vec(6*inod-2)
        tsr_nod(inod,5) = v_sol%x_vec(6*inod-1)
        tsr_nod(inod,6) = v_sol%x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine nod_tensor_send_recv
!
! ----------------------------------------------------------------------
!
      end module nod_phys_send_recv
