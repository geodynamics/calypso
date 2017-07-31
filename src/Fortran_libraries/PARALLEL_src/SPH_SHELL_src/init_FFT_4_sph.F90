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
!!     &         (ncomp, sph_rtp, comm_rtp, WK_FFTs)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
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
      use m_work_time
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!
      use t_sph_FFT_selector
!
      implicit none
!
      real(kind = kreal) :: etime_shortest = -1.0e10
      integer(kind = kint) :: iflag_selected
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
     &         (ncomp, sph_rtp, comm_rtp, WK_FFTs)
!
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: ncomp
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(work_for_FFTs), intent(inout) :: WK_FFTs
!
!
      if(iflag_FFT .eq. iflag_UNDEFINED_FFT) then
        call s_select_fourier_transform(ncomp, sph_rtp, comm_rtp,       &
     &     n_WS, n_WR, WS, WR, WK_FFTs)
        iflag_FFT = iflag_selected
      end if
!
      call init_sph_FFT_select(my_rank, sph_rtp, ncomp, WK_FFTs)
!
      if(my_rank .gt. 0) return
      write(*,'(a,i4)', advance='no') 'Selected Fourier transform: ',   &
     &                          iflag_FFT
!
      if     (iflag_FFT .eq. iflag_FFTPACK) then
        write(*,'(a)') ' (FFTPACK) '
      else if(iflag_FFT .eq. iflag_FFTW) then
        write(*,'(a)') ' (FFTW) '
      else if(iflag_FFT .eq. iflag_ISPACK) then
        write(*,'(a)') ' (ISPACK) '
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        write(*,'(a)') ' (FFTW_SINGLE) '
      end if
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
      real(kind = kreal) :: etime_fft(5)
!
!
      allocate(X_rtp(ncomp*sph_rtp%nnod_rtp))
      X_rtp = 0.0d0
!
      iflag_FFT = iflag_FFTPACK
      call test_fourier_trans_vector(ncomp, sph_rtp, comm_rtp,         &
     &    n_WS, n_WR, WS, WR, X_rtp, WK_FFTs, etime_fft(iflag_FFT))
!
!
#ifdef FFTW3
      iflag_FFT = iflag_FFTW
      call test_fourier_trans_vector(ncomp, sph_rtp, comm_rtp,         &
     &    n_WS, n_WR, WS, WR, X_rtp, WK_FFTs, etime_fft(iflag_FFTW))
!
      iflag_FFT = iflag_FFTW_SINGLE
      call test_fourier_trans_vector(ncomp, sph_rtp, comm_rtp,         &
     &    n_WS, n_WR, WS, WR, X_rtp, WK_FFTs,                          &
     &    etime_fft(iflag_FFTW_SINGLE))
#endif
!
      iflag_FFT = iflag_ISPACK
      call test_fourier_trans_vector(ncomp, sph_rtp, comm_rtp,         &
     &     n_WS, n_WR, WS, WR, X_rtp, WK_FFTs,                         &
     &     etime_fft(iflag_ISPACK))
      deallocate(X_rtp)
!
      iflag_selected = minloc(etime_fft,1)
      etime_shortest = minval(etime_fft)
!
      if(my_rank .gt. 0) return
        write(*,*)   '1: elapsed by FFTPACK: ',                         &
     &            etime_fft(iflag_FFTPACK)
        if(etime_fft(iflag_FFTW) .gt. zero) then
          write(*,*) '2: elapsed by FFTW3:   ', etime_fft(iflag_FFTW)
        end if
        if(etime_fft(iflag_FFTW_SINGLE) .gt. zero) then
          write(*,*) '3: elapsed by single FFTW3:   ',                  &
     &            etime_fft(iflag_FFTW_SINGLE)
        end if
        if(etime_fft(iflag_ISPACK) .gt. zero) then
          write(*,*) '4: elapsed by ISPACK:         ',                  &
     &            etime_fft(iflag_ISPACK)
        end if
!
      end subroutine s_select_fourier_transform
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_vector(ncomp, sph_rtp, comm_rtp,    &
     &          n_WS, n_WR, WS, WR, X_rtp, WK_FFTs, etime_fft)
!
      use calypso_mpi
!
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
      call init_sph_FFT_select(my_rank, sph_rtp, ncomp, WK_FFTs)
!
      if(iflag_debug .gt. 0) write(*,*) 'back_FFT_select_from_recv'
      starttime = MPI_WTIME()
      call back_FFT_select_from_recv                                    &
     &   (sph_rtp, comm_rtp, ncomp, n_WR, WR, X_rtp, WK_FFTs)
      call fwd_FFT_select_to_send                                       &
     &   (sph_rtp, comm_rtp, ncomp, n_WS, X_rtp, WS, WK_FFTs)
      endtime = MPI_WTIME() - starttime
      if(iflag_debug .gt. 0) write(*,*) 'fwd_FFT_select_to_send end'
!
      if(iflag_debug .gt. 0) write(*,*) 'finalize_sph_FFT_select'
      call finalize_sph_FFT_select(WK_FFTs)
!
      call MPI_allREDUCE (endtime, etime_fft, ione,                     &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_fft = etime_fft / dble(nprocs)
!
      end subroutine test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      end module init_FFT_4_sph
