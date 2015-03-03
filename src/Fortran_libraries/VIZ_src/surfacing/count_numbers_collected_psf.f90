!count_numbers_collected_psf.f90
!      module count_numbers_collected_psf
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine count_numbers_4_psf_out(num_psf, istack_smp,         &
!!     &          ntot_out_psf, nmax_para_psf, istack_para_psf,         &
!!     &          istack_recv_psf,istack_out_psf)
!
      module count_numbers_collected_psf
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_numbers_4_psf_out(num_psf, istack_smp,           &
     &          ntot_out_psf, nmax_para_psf, istack_para_psf,           &
     &          istack_recv_psf,istack_out_psf)
!
      use calypso_mpi
      use m_machine_parameter
      use m_mpi_flags_4_section
      use copy_psf_data_to_SR
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_psf
      integer(kind = kint), intent(in) :: istack_smp(0:num_psf*np_smp)
!
      integer(kind = kint), intent(inout) :: ntot_out_psf
      integer(kind = kint), intent(inout) :: nmax_para_psf
      integer(kind = kint), intent(inout)                               &
     &                   :: istack_para_psf(0:num_psf*nprocs)
      integer(kind = kint), intent(inout)                               &
     &                   :: istack_recv_psf(0:num_psf*nprocs)
      integer(kind = kint), intent(inout) :: istack_out_psf(0:num_psf)
!
      integer(kind = kint) :: isend(num_psf), irecv(num_psf,nprocs)
      integer(kind = kint) :: num_para_psf(num_psf*nprocs)
      integer(kind = kint) :: num_recv_psf(num_psf*nprocs)
!
      integer(kind = kint) :: i, j, k, ip, ip_sent
      integer(kind = kint) :: ist, istack_begin
      integer(kind = kint) :: ntot_tmp, max_tmp, min_tmp
      integer(kind = kint), parameter :: rank0 = 0, ione = 1
!
!
      do i = 1, num_psf
        isend(i) = istack_smp(i*np_smp) - istack_smp((i-1)*np_smp)
      end do
!
      if (my_rank.ne.0) then
        call MPI_ISEND (isend, num_psf, CALYPSO_INTEGER, rank0, 0,      &
     &                  CALYPSO_COMM, req1_psf(1), ierr_MPI)
      end if
!
      if (my_rank.eq.rank0) then
        do ip = 2, nprocs
          ip_sent = ip - 1
          call MPI_IRECV (irecv(1,ip), num_psf, CALYPSO_INTEGER,        &
     &                    ip_sent, 0, CALYPSO_COMM, req2_psf(ip),       &
     &                    ierr_MPI)
        end do
!
        call MPI_WAITALL ((nprocs-1), req2_psf(2), sta2_psf(1,2),       &
     &                    ierr_MPI)
!
        do i = 1, num_psf
          j = (i-1)*nprocs + 1
          k = i
          num_para_psf(j) = isend(i)
          num_recv_psf(k) = isend(i)
          do ip = 2, nprocs
            j = (i-1)*nprocs + ip
            k = (ip-1)*num_psf + i
            num_para_psf(j) = irecv(i,ip)
            num_recv_psf(k) = irecv(i,ip)
          end do
        end do
!
      end if
!
      if (my_rank.ne.0) then
        call MPI_WAITALL (ione, req1_psf(1), sta1_psf(1,1), ierr_MPI)
      end if
!
!
!
      if (my_rank.eq.0) then
        nmax_para_psf = 0
        istack_out_psf(0) = 0
        istack_para_psf(0) = 0
        istack_recv_psf(0) = 0
!
        do i = 1, num_psf
          ist = (i-1)*nprocs
          istack_begin = istack_para_psf(ist)
          call s_cal_minmax_and_stacks(nprocs, num_para_psf(ist+1),     &
     &        istack_begin, istack_para_psf(ist), ntot_tmp, max_tmp,    &
     &        min_tmp)
!
          nmax_para_psf = max(nmax_para_psf, max_tmp)
          istack_out_psf(i) = ntot_tmp
        end do
        ntot_out_psf = istack_out_psf(num_psf)
!
        do ip = 1, nprocs
          ist = (ip-1)*num_psf
          istack_begin = istack_recv_psf(ist)
          call s_cal_total_and_stacks(num_psf, num_recv_psf(ist+1),     &
     &        istack_begin, istack_recv_psf(ist), ntot_tmp)
        end do
!
      end if
!
      end subroutine count_numbers_4_psf_out
!
! ----------------------------------------------------------------------
!
      end module count_numbers_collected_psf
