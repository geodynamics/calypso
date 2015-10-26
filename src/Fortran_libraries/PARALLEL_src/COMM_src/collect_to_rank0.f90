!>@file   collect_to_rank0.f90
!!@brief  module collect_to_rank0
!!
!!@author coded by H. Matsui
!!@date coded by H. Matsui (UC Davis) in june 2015
!
!>@brief  Collect data to rank 0
!!@n       All process need to know
!!@n       how many data are there in all processes
!!
!!@verbatim
!!      subroutine collect_vectors_to_rank0                             &
!!     &         (NP, NB, istack, X,   irank_tgt, ntot, X_merged)
!!      subroutine collect_integers_to_rank0                            &
!!     &         (NP, NB, istack, iX,  irank_tgt, ntot, iX_merged)
!!      subroutine collect_int8s_to_rank0                               &
!!     &         (NP, NB, istack, i8X, irank_tgt, ntot, i8X_merged)
!!@endverbatim
!!
!!@n @param  NP     Number of data points
!!@n @param  ntot  Number of data points after merged
!!
!!@n @param  NB    Number of components
!!@n @param  istack(0:NPROCS)  End points of data after merged
!!
!!@n @param  X(NP,NB)       vector data with NB components
!!@n @param  iX(NP,NB)      integer data with NB components
!!@n @param  i8X(NP,NB)     8 byte integer data with NB components
!!
!!@n @param  X_merged(ntot,NB)   merged vector data with NB components
!!@n @param  iX_merged(ntot,NB)  merged integer data with NB components
!!@n @param  i8X_merged(ntot,NB) merged 8 byte integer data 
!!                            with NB components
!
      module collect_to_rank0
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine collect_vectors_to_rank0                               &
     &         (NP, NB, istack, X, irank_tgt, ntot, X_merged)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      integer(kind = kint), intent(in) :: NP, NB
      real (kind=kreal), intent(in) :: X(NP,NB)
!
      integer(kind = kint), intent(in) :: irank_tgt
      integer(kind = kint), intent(in) :: ntot
      real (kind=kreal), intent(inout) :: X_merged(ntot,NB)
!
      integer(kind = kint) :: nd, i_rank, ist, num, inum, inod, knod
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
      call resize_work_4_SR(NB, nprocs-1, NP, ntot)
!
      nneib_send = 0
      nneib_recv = 0
      if(my_rank .ne. irank_tgt) then
        num = istack(my_rank+1) - istack(my_rank  )
        if(num .gt. 0) then
          nneib_send = 1
!$omp parallel private(nd)
          do nd = 1, NB
!$omp do private(inod,knod)
            do inod = 1, num
              knod = inod + (nd-1)*num
              WS(knod) = X(inod,nd)
            end do
!$omp end do nowait
          end do
!$omp end parallel
!
          call MPI_ISEND(WS(1), (NB*num), CALYPSO_REAL,                 &
     &                   irank_tgt, 0,     CALYPSO_COMM,                &
     &                   req1(1), ierr_MPI)
        end if
!
      else
        do i_rank = 0, nprocs-1
          ist = NB * istack(i_rank  ) 
          num = NB * (istack(i_rank+1) - istack(i_rank  ))
          if(i_rank.ne.irank_tgt .and. num .gt. 0) then
            nneib_recv = nneib_recv + 1
            call MPI_IRECV(WR(ist+1), num, CALYPSO_REAL,                &
     &                     i_rank, 0, CALYPSO_COMM,                     &
     &                     req2(nneib_recv), ierr_MPI)
          end if
        end do 
      end if
!
      call MPI_WAITALL(nneib_recv, req2(1), sta2(1,1), ierr_MPI)
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          ist = istack(i_rank)
          num = istack(i_rank+1) - istack(i_rank)
          if(num .le. 0) cycle
!
          if(i_rank .eq. irank_tgt) then
!$omp parallel private(nd)
            do nd = 1, NB
!$omp do private(inod)
              do inod = 1, num
                X_merged(inod+ist,nd) = X(inod,nd)
              end do
!$omp end do nowait
            end do
!$omp end parallel
!
          else
!$omp parallel private(nd)
            do nd = 1, NB
!$omp do private(inum,inod,knod)
              do inum = 1, num
                inod = inum + ist
                knod = inum + (nd-1) * num + NB * ist
                X_merged(inod,nd) = WR(knod)
              end do
!$omp end do nowait
            end do
!$omp end parallel
         end if
       end do
     end if
!
      call MPI_WAITALL(nneib_send, req1(1), sta1(1,1), ierr_MPI)
!
      end subroutine collect_vectors_to_rank0
!
! -----------------------------------------------------------------------
!
      subroutine collect_integers_to_rank0                              &
     &         (NP, NB, istack, iX, irank_tgt, ntot, iX_merged)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      integer(kind = kint), intent(in) :: NP, NB
      integer(kind = kint), intent(in) :: iX(NP,NB)
!
      integer(kind = kint), intent(in) :: irank_tgt
      integer(kind = kint), intent(in) :: ntot
      integer(kind = kint), intent(inout) :: iX_merged(ntot,NB)
!
      integer(kind = kint) :: nd, i_rank, ist, num, inum, inod, knod
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
      call resize_iwork_4_SR(nprocs-1, NB*NP, NB*ntot)
!
      nneib_send = 0
      nneib_recv = 0
      if(my_rank .ne. irank_tgt) then
        num = istack(my_rank+1) - istack(my_rank  )
        if(num .gt. 0) then
          nneib_send = 1
!$omp parallel private(nd)
          do nd = 1, NB
!$omp do private(inod,knod)
            do inod = 1, num
              knod = inod + (nd-1)*num
              iWS(knod) = iX(inod,nd)
            end do
!$omp end do nowait
          end do
!$omp end parallel
!
          call MPI_ISEND(iWS(1), (NB*num), CALYPSO_INTEGER,             &
     &                   irank_tgt, 0, CALYPSO_COMM,                    &
     &                   req1(1), ierr_MPI)
        end if
!
      else
        do i_rank = 0, nprocs-1
          ist = NB * istack(i_rank  ) 
          num = NB * (istack(i_rank+1) - istack(i_rank  ))
          if(i_rank.ne.irank_tgt .and. num .gt. 0) then
            nneib_recv = nneib_recv + 1
            call MPI_IRECV(iWR(ist+1), num, CALYPSO_INTEGER,            &
     &                     i_rank, 0, CALYPSO_COMM,                     &
     &                     req2(nneib_recv), ierr_MPI)
          end if
        end do 
      end if
!
      call MPI_WAITALL(nneib_recv, req2(1), sta2(1,1), ierr_MPI)
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          ist = istack(i_rank)
          num = istack(i_rank+1) - istack(i_rank)
          if(num .le. 0) cycle
!
          if(i_rank .eq. irank_tgt) then
!$omp parallel private(nd)
            do nd = 1, NB
!$omp do private(inod)
              do inod = 1, num
                iX_merged(inod+ist,nd) = iX(inod,nd)
              end do
!$omp end do nowait
            end do
!$omp end parallel
!
          else
!$omp parallel private(nd)
            do nd = 1, NB
!$omp do private(inum,inod,knod)
              do inum = 1, num
                inod = inum + ist
                knod = inum + (nd-1) * num + NB * ist
                iX_merged(inod,nd) = iWR(knod)
              end do
!$omp end do nowait
            end do
!$omp end parallel
         end if
       end do
     end if
!
      call MPI_WAITALL(nneib_send, req1(1), sta1(1,1), ierr_MPI)
!
      end subroutine collect_integers_to_rank0
!
! -----------------------------------------------------------------------
!
      subroutine collect_int8s_to_rank0                                 &
     &         (NP, NB, istack, i8X, irank_tgt, ntot, i8X_merged)
!
      use calypso_mpi
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: istack(0:nprocs)
      integer(kind = kint), intent(in) :: NP, NB
      integer(kind = kint_gl), intent(in) :: i8X(NP,NB)
!
      integer(kind = kint), intent(in) :: irank_tgt
      integer(kind = kint), intent(in) :: ntot
      integer(kind = kint_gl), intent(inout) :: i8X_merged(ntot,NB)
!
      integer(kind = kint) :: i_rank, ist, num, inum, inod, knod, nd
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
      call resize_i8work_sph_SR(ione, nprocs-1, NB*NP, NB*ntot)
!
      nneib_send = 0
      nneib_recv = 0
      num = istack(my_rank+1) - istack(my_rank  )
      if(num .gt. 0) then
!$omp parallel private(nd)
        do nd = 1, NB
!$omp do private(inod,knod)
          do inod = 1, num
            knod = inod + (nd-1)*num
            i8WS(knod) = i8X(inod,nd)
          end do
!$omp end do nowait
        end do
!$omp end parallel
      end if
!
      if(my_rank .ne. irank_tgt .and. num .gt. 0) then
        nneib_send = 1
        call MPI_ISEND(i8WS(1), (NB*num), CALYPSO_GLOBAL_INT,           &
     &                   irank_tgt, 0, CALYPSO_COMM,                    &
     &                   req1(1), ierr_MPI)
      end if
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          ist = NB * istack(i_rank  ) 
          num = NB * (istack(i_rank+1) - istack(i_rank  ))
          if(i_rank.ne.irank_tgt .and. num .gt. 0) then
            nneib_recv = nneib_recv + 1
            call MPI_IRECV(i8WR(ist+1), num, CALYPSO_GLOBAL_INT,        &
     &                     i_rank, 0, CALYPSO_COMM,                     &
     &                     req2(nneib_recv), ierr_MPI)
          end if
        end do 
      end if
!
      call MPI_WAITALL(nneib_recv, req2(1), sta2(1,1), ierr_MPI)
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          ist = istack(i_rank)
          num = istack(i_rank+1) - istack(i_rank)
          if(num .le. 0) cycle
!
          if(i_rank .eq. irank_tgt) then
!$omp parallel private(nd)
            do nd = 1, NB
!$omp do private(inod)
              do inod = 1, num
                i8X_merged(inod+ist,nd) = i8X(inod,nd)
              end do
!$omp end do nowait
            end do
!$omp end parallel
!
          else
!$omp parallel private(nd)
            do nd = 1, NB
!$omp do private(inum,inod,knod)
              do inum = 1, num
                inod = inum + ist
                knod = inum + (nd-1) * num + NB * ist
                i8X_merged(inod,nd) = i8WR(knod)
              end do
!$omp end do nowait
            end do
!$omp end parallel
          end if
        end do
      end if
!
      call MPI_WAITALL(nneib_send, req1(1), sta1(1,1), ierr_MPI)
!
      end subroutine collect_int8s_to_rank0
!
! -----------------------------------------------------------------------
!
      end module collect_to_rank0
