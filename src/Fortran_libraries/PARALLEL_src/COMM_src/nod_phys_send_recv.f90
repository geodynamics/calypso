!
!      module nod_phys_send_recv
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on July, 2008
!
!
!!      subroutine init_send_recv(nod_comm)
!!
!!      subroutine nod_fields_send_recv(nod_comm, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_data),intent(inout) :: nod_fld
!!
!!      subroutine scalar_send_recv(id_phys, nod_comm, nod_fld)
!!      subroutine vector_send_recv(id_phys, nod_comm, nod_fld)
!!      subroutine sym_tensor_send_recv(id_phys, nod_comm, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(phys_data),intent(inout) :: nod_fld
!!         id_phys:  field ID of nodal fields
!!
!!      subroutine nod_scalar_send_recv(numnod, nod_comm, scl_nod)
!!      subroutine nod_vector_send_recv(numnod, nod_comm, vec_nod)
!!      subroutine nod_tensor_send_recv(numnod, nod_comm, tsr_nod)
!
      module nod_phys_send_recv
!
      use m_precision
!
      use calypso_mpi
      use t_comm_table
      use t_phys_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_send_recv(nod_comm)
!
      use m_phys_constants
      use m_solver_SR
!
      type(communication_table), intent(in) :: nod_comm
!
!
      call resize_work_4_SR                                             &
     &   (n_sym_tensor, nod_comm%num_neib, nod_comm%num_neib,           &
     &    nod_comm%ntot_export, nod_comm%ntot_import)
      call resize_iwork_4_SR(nod_comm%num_neib, nod_comm%num_neib,      &
     &    nod_comm%ntot_export, nod_comm%ntot_import)
      call resize_i8work_4_SR(nod_comm%num_neib, nod_comm%num_neib,     &
     &    nod_comm%ntot_export, nod_comm%ntot_import)
!
      end subroutine init_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine nod_fields_send_recv(nod_comm, nod_fld)
!
      use m_machine_parameter
      use m_phys_constants
      use t_phys_data
!
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
      integer (kind=kint) :: i, ist
!
!
      do i = 1, nod_fld%num_phys
        ist = nod_fld%istack_component(i-1) + 1
!
        if (nod_fld%num_component(i) .eq. n_vector) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for vector of ', trim(nod_fld%phys_name(i))
          call vector_send_recv(ist, nod_comm, nod_fld)
!
        else if (nod_fld%num_component(i) .eq. n_scalar) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for scaler of ', trim(nod_fld%phys_name(i))
          call scalar_send_recv(ist, nod_comm, nod_fld)
!
        else if (nod_fld%num_component(i) .eq. n_sym_tensor) then
          if (iflag_debug .ge. iflag_routine_msg) write(*,*)            &
     &      'comm. for tensor of ', trim(nod_fld%phys_name(i))
          call sym_tensor_send_recv(ist, nod_comm, nod_fld)
        end if
      end do
!
      end subroutine nod_fields_send_recv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv(id_phys, nod_comm, nod_fld)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
!
!
      call nod_scalar_send_recv                                         &
     &   (nod_fld%n_point, nod_comm, nod_fld%d_fld(1,id_phys) )
!
      end subroutine scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv(id_phys, nod_comm, nod_fld)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
!
!
      call nod_vector_send_recv                                         &
     &   (nod_fld%n_point, nod_comm,  nod_fld%d_fld(1,id_phys))
!
      end subroutine vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine sym_tensor_send_recv(id_phys, nod_comm, nod_fld)
!
      use t_phys_data
!
      integer(kind = kint), intent(in) :: id_phys
      type(communication_table), intent(in) :: nod_comm
      type(phys_data),intent(inout) :: nod_fld
!
!
      call nod_tensor_send_recv(nod_fld%n_point, nod_comm,              &
     &    nod_fld%d_fld(1,id_phys))
!
      end subroutine sym_tensor_send_recv
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine nod_scalar_send_recv(numnod, nod_comm, scl_nod)
!
      use m_array_for_send_recv
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      real(kind = kreal), intent(inout) :: scl_nod(numnod)
!
      integer(kind=kint)  :: inod
!
!
!$omp parallel do
       do inod=1, numnod
        x_vec(inod) = scl_nod(inod)
       end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_type(numnod, nod_comm, x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, numnod
        scl_nod(inod) = x_vec(inod)
      end do
!$omp end parallel do
!
      end subroutine nod_scalar_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_vector_send_recv(numnod, nod_comm, vec_nod)
!
      use m_array_for_send_recv
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      real(kind = kreal), intent(inout) :: vec_nod(numnod,3)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod=1, numnod
        x_vec(3*inod-2) = vec_nod(inod,1)
        x_vec(3*inod-1) = vec_nod(inod,2)
        x_vec(3*inod  ) = vec_nod(inod,3)
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_3_type(numnod, nod_comm, x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, numnod
        vec_nod(inod,1) = x_vec(3*inod-2)
        vec_nod(inod,2) = x_vec(3*inod-1)
        vec_nod(inod,3) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine nod_vector_send_recv
!
! ----------------------------------------------------------------------
!
      subroutine nod_tensor_send_recv(numnod, nod_comm, tsr_nod)
!
      use m_array_for_send_recv
      use m_work_time
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: numnod
      type(communication_table), intent(in) :: nod_comm
      real(kind = kreal), intent(inout) :: tsr_nod(numnod,6)
!
      integer (kind = kint) :: inod
!
!$omp parallel do
      do inod=1, numnod
        x_vec(6*inod-5) = tsr_nod(inod,1)
        x_vec(6*inod-4) = tsr_nod(inod,2)
        x_vec(6*inod-3) = tsr_nod(inod,3)
        x_vec(6*inod-2) = tsr_nod(inod,4)
        x_vec(6*inod-1) = tsr_nod(inod,5)
        x_vec(6*inod  ) = tsr_nod(inod,6)
      end do
!$omp end parallel do
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_6_type(numnod, nod_comm, x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, numnod
        tsr_nod(inod,1) = x_vec(6*inod-5)
        tsr_nod(inod,2) = x_vec(6*inod-4)
        tsr_nod(inod,3) = x_vec(6*inod-3)
        tsr_nod(inod,4) = x_vec(6*inod-2)
        tsr_nod(inod,5) = x_vec(6*inod-1)
        tsr_nod(inod,6) = x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine nod_tensor_send_recv
!
! ----------------------------------------------------------------------
!
      end module nod_phys_send_recv
