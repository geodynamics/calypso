!
!      module nodal_vector_send_recv
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on Apr., 2008
!
!      subroutine init_send_recv
!
!      subroutine nod_scalar_send_recv(scl_nod)
!      subroutine nod_vector_send_recv(vec_nod)
!      subroutine nod_tensor_send_recv(tsr_nod)
!
      module nodal_vector_send_recv
!
      use m_precision
      use calypso_mpi
!
      use m_geometry_parameter
      use m_nod_comm_table
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_send_recv
!
      use m_nod_comm_table
      use m_phys_constants
      use m_solver_SR
!
!
      call resize_work_4_SR(n_sym_tensor, num_neib,                     &
     &    ntot_export, ntot_import)
      call resize_iwork_4_SR(num_neib, ntot_export, ntot_import)
!
      end subroutine init_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine nod_scalar_send_recv(scl_nod)
!
      use m_array_for_send_recv
      use m_work_time
      use solver_SR
!
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
      call SOLVER_SEND_RECV(numnod, num_neib, id_neib,                  &
     &                      istack_import, item_import,                 &
     &                      istack_export, item_export, x_vec(1) )
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
      subroutine nod_vector_send_recv(vec_nod)
!
      use m_array_for_send_recv
      use m_work_time
      use solver_SR_3
!
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
      call SOLVER_SEND_RECV_3(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export,               &
     &                        x_vec(1) )
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
      subroutine nod_tensor_send_recv(tsr_nod)
!
      use m_array_for_send_recv
      use m_work_time
      use solver_SR_6
!
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
      call SOLVER_SEND_RECV_6(numnod, num_neib, id_neib,                &
     &                        istack_import, item_import,               &
     &                        istack_export, item_export, x_vec(1) )
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
      end module nodal_vector_send_recv
