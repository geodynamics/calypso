!>@file   init_FFT_4_sph.F90
!!@brief  module init_FFT_4_sph
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine init_fourier_transform_4_sph                         &
!!     &         (ncomp, sph_rtp, comm_rtp, WK_FFTs, SR_r, iflag_FFT)
!!        integer(kind = kint), intent(in) :: ncomp
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        integer(kind = kint), intent(inout) :: iflag_FFT
!!
!!       Current problem
!!      FFTW crashes when both single and multi transforms are 
!!      comparaed
!!@endverbatim
!
      module init_FFT_4_sph
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_FFT_selector
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!
      use t_sph_FFT_selector
!
      implicit none
!
#ifdef FFTW3
      integer(kind = kint), parameter :: num_test =   6
#else
      integer(kind = kint), parameter :: num_test =   3
#endif
!
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_FFTPACK_ONCE,                                   &
#ifdef FFTW3
     &            iflag_FFTW_SINGLE,                                    &
     &            iflag_FFTW_ONCE,                                      &
     &            iflag_FFTW_COMPONENT,                                 &
#endif
     &            iflag_ISPACK1_ONCE,                                   &
     &            iflag_ISPACK3_ONCE/)
!
      real(kind = kreal) :: etime_shortest = -1.0e10
      integer(kind = kint) :: iflag_selected
!
      private :: num_test, list_test
!
      private :: etime_shortest, iflag_selected
      private :: s_select_fourier_transform, test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_fourier_transform_4_sph                           &
     &         (ncomp, sph_rtp, comm_rtp, WK_FFTs, SR_r, iflag_FFT)
!
      use t_solver_SR
!
      integer(kind = kint), intent(in) :: ncomp
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(send_recv_real_buffer), intent(inout) :: SR_r
      integer(kind = kint), intent(inout) :: iflag_FFT
!
!
      if(iflag_FFT .eq. iflag_SEARCH_FASTEST_FFT) then
        call s_select_fourier_transform(ncomp, sph_rtp, comm_rtp,       &
     &      SR_r%n_WS, SR_r%n_WR, SR_r%WS, SR_r%WR, WK_FFTs)
        iflag_FFT = iflag_selected
      end if
!
      call init_sph_FFT_select                                          &
     &   (my_rank, iflag_FFT, sph_rtp, comm_rtp, ncomp, ncomp, WK_FFTs)
!
      if(my_rank .gt. 0) return
      write(*,'(a,a,a,i3,a)') 'Selected Fourier transform: ',           &
     &         trim(chosen_fft_name(iflag_FFT)), ' (', iflag_FFT, ')'
!
      end subroutine init_fourier_transform_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine s_select_fourier_transform(ncomp, sph_rtp, comm_rtp,   &
     &          n_WS, n_WR, WS, WR, WK_FFTs)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp, n_WS, n_WR
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
      real(kind = kreal), allocatable :: X_rtp(:)
      real(kind = kreal) :: etime_fft(num_test)
      integer(kind = kint) :: i
!
!
      allocate(X_rtp(ncomp*sph_rtp%nnod_rtp))
      X_rtp = 0.0d0
!
      do i = 1, num_test
        call test_fourier_trans_vector                                 &
     &   (list_test(i), ncomp, sph_rtp, comm_rtp,                      &
     &    n_WS, n_WR, WS, WR, X_rtp, WK_FFTs, etime_fft(i))
      end do
      deallocate(X_rtp)
!
      i = minloc(etime_fft,1)
      iflag_selected = list_test(i)
      etime_shortest = minval(etime_fft)
!
      if(my_rank .gt. 0) return
      do i = 1, num_test
        call write_elapsed_4_FFT(list_test(i), etime_fft(i))
      end do
!
      end subroutine s_select_fourier_transform
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_vector                              &
     &         (iflag_FFT, ncomp, sph_rtp, comm_rtp,                    &
     &          n_WS, n_WR, WS, WR, X_rtp, WK_FFTs, etime_fft)
!
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in)  :: ncomp, n_WS, n_WR
!
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      real(kind = kreal), intent(inout) :: WS(n_WS)
      real(kind = kreal), intent(inout) :: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &                    :: X_rtp(sph_rtp%nnod_rtp,ncomp)
      real(kind = kreal), intent(inout) :: etime_fft
!
      real(kind = kreal) :: starttime, endtime
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_sph_FFT_select'
      call init_sph_FFT_select                                          &
     &   (my_rank, iflag_FFT, sph_rtp, comm_rtp, ncomp, ncomp, WK_FFTs)
!
      if(iflag_debug .gt. 0) write(*,*) 'back_FFT_select_from_recv'
      starttime = MPI_WTIME()
      call back_FFT_select_from_recv(sph_rtp, comm_rtp,                 &
     &    ncomp, n_WR, WR, X_rtp, WK_FFTs)
      call fwd_FFT_select_to_send(sph_rtp, comm_rtp,                    &
     &    ncomp, n_WS, X_rtp, WS, WK_FFTs)
      endtime = MPI_WTIME() - starttime
!
      if(iflag_debug .gt. 0) write(*,*) 'finalize_sph_FFT_select'
      call finalize_sph_FFT_select(sph_rtp, WK_FFTs)
!
      call calypso_mpi_allreduce_one_real(endtime, etime_fft, MPI_SUM)
      etime_fft = etime_fft / dble(nprocs)
!
      end subroutine test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      end module init_FFT_4_sph
