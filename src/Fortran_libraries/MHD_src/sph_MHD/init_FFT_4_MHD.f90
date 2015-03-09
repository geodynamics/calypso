!>@file   init_FFT_4_MHD.F90
!!@brief  module init_FFT_4_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine init_fourier_transform_4_MHD(ncomp_sph_trans)
!!
!!       Current problem
!!      FFTW crashes when both single and multi transforms are 
!!      comparaed
!!@endverbatim
!
      module init_FFT_4_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use MHD_FFT_selector
!
      implicit none
!
      real(kind = kreal) :: etime_shortest = -1.0e10
      integer(kind = kint) :: iflag_selected
!
      private :: etime_shortest, iflag_selected
      private :: compare_FFT_4_MHD, test_fourier_trans_4_MHD
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_fourier_transform_4_MHD                           &
     &         (ncomp_tot, ncomp_fwd, ncomp_bwd)
!
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: ncomp_tot
      integer(kind = kint), intent(in) :: ncomp_fwd, ncomp_bwd
!
!
      if(iflag_FFT .eq. iflag_UNDEFINED_FFT) then
        call compare_FFT_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,         &
     &      n_WS, n_WR, WS, WR)
        iflag_FFT = iflag_selected
      end if
!
      call init_MHD_FFT_select                                          &
     &   (my_rank, ncomp_tot, ncomp_fwd, ncomp_bwd)
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
      else if(iflag_FFT .eq. iflag_FFTW_FIELD) then
        write(*,'(a)') ' (FFTW_FIELD) '
      end if
!
      end subroutine init_fourier_transform_4_MHD
!
! -----------------------------------------------------------------------
!
      subroutine compare_FFT_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,     &
     &          n_WS, n_WR, WS, WR)
!
      integer(kind = kint), intent(in) :: ncomp_tot
      integer(kind = kint), intent(in) :: ncomp_fwd, ncomp_bwd
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal) :: etime_fft(5)
!
!
      iflag_FFT = iflag_FFTPACK
      call test_fourier_trans_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,    &
     &    n_WS, n_WR, WS, WR, etime_fft(iflag_FFT))
!
!
#ifdef FFTW3
      iflag_FFT = iflag_FFTW
      call test_fourier_trans_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,    &
     &    n_WS, n_WR, WS, WR, etime_fft(iflag_FFTW))
!
      iflag_FFT = iflag_FFTW_FIELD
      call test_fourier_trans_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,    &
     &    n_WS, n_WR, WS, WR, etime_fft(iflag_FFTW_FIELD))
!
      iflag_FFT = iflag_FFTW_SINGLE
      call test_fourier_trans_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,    &
     &    n_WS, n_WR, WS, WR, etime_fft(iflag_FFTW_SINGLE))
#endif
!
!#ifdef FFTW3_C
!      iflag_FFT = iflag_FFTW
!      call test_fourier_trans_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,   &
!     &    n_WS, n_WR, WS, WR, etime_fft(iflag_FFTW))
!#endif
!
!      iflag_FFT = iflag_ISPACK
!      call test_fourier_trans_4_MHD(ncomp_tot, ncomp_fwd, ncomp_bwd,   &
!     &    n_WS, n_WR, WS, WR, etime_fft(iflag_ISPACK))
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
        if(etime_fft(iflag_FFTW_FIELD) .gt. zero) then
          write(*,*) '4: elapsed by field FFTW3:   ',                   &
     &            etime_fft(iflag_FFTW_FIELD)
        end if
!        if(etime_fft(iflag_ISPACK) .gt. zero) then
!          write(*,*) '5: elapsed by ISPACK:         ',                 &
!     &            etime_fft(iflag_ISPACK)
!        end if
!
      end subroutine compare_FFT_4_MHD
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_4_MHD(ncomp, ncomp_fwd, ncomp_bwd,  &
     &          n_WS, n_WR, WS, WR, etime_fft)
!
      use calypso_mpi
      use m_addresses_trans_sph_MHD
!
      integer(kind = kint), intent(in) :: ncomp, ncomp_fwd, ncomp_bwd
      integer(kind = kint), intent(in) :: n_WS, n_WR
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: etime_fft
!
      real(kind = kreal) :: starttime, endtime
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_MHD_FFT_select'
      call init_MHD_FFT_select(my_rank, ncomp, ncomp_fwd, ncomp_bwd)
!
      if(iflag_debug .gt. 0) write(*,*) 'back_MHD_FFT_sel_from_recv'
      starttime = MPI_WTIME()
      call back_MHD_FFT_sel_from_recv(ncomp_bwd, n_WR, WR, fld_rtp)
      call fwd_MHD_FFT_sel_from_recv(ncomp_fwd, n_WS, frc_rtp, WS)
      endtime = MPI_WTIME() - starttime
      if(iflag_debug .gt. 0) write(*,*) 'fwd_MHD_FFT_sel_from_recv end'
!
      if(iflag_debug .gt. 0) write(*,*) 'finalize_sph_FFT_select'
      call finalize_MHD_FFT_select
!
      call MPI_allREDUCE (endtime, etime_fft, ione,                     &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_fft = etime_fft / dble(nprocs)
!
      end subroutine test_fourier_trans_4_MHD
!
! -----------------------------------------------------------------------
!
      end module init_FFT_4_MHD
