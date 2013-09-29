!>@file   select_fourier_transform.F90
!!@brief  module select_fourier_transform
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine s_select_fourier_transform(ncomp, Nstacksmp)
!!@endverbatim
!
      module select_fourier_transform
!
      use m_precision
!
      use calypso_mpi
      use m_work_time
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_work_4_sph_trans
      use FFT_selector
!
      implicit none
!
      private :: test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_select_fourier_transform(ncomp, Nstacksmp)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: Nstacksmp(0:np_smp)
!
      integer(kind = kint) :: iflag_seelcted
      real(kind = kreal) :: etime_shortest
      real(kind = kreal) :: etime_fft(1:3)
!
!
      if(iflag_FFT .ne.  iflag_UNDEFINED_FFT) then
        call initialize_FFT_select(my_rank, np_smp, Nstacksmp,          &
     &      nidx_rtp(3))
        return
      end if
!

      iflag_FFT = iflag_FFTPACK
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFTPACK) )
!
      iflag_seelcted = iflag_FFTPACK
      etime_shortest = etime_fft(iflag_FFTPACK)
!
!
#ifdef FFTW3
      iflag_FFT = iflag_FFTW
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFTW))
!
      if(etime_fft(iflag_FFTW) .lt. etime_shortest) then
        iflag_seelcted = iflag_FFTW
        etime_shortest = etime_fft(iflag_FFTW)
      end if
#endif
!
      iflag_FFT = iflag_seelcted
      call initialize_FFT_select(my_rank, np_smp, Nstacksmp,            &
     &    nidx_rtp(3))
!
      if(my_rank .gt. 0) return
        write(*,'(a,i4)', advance='no') 'Selected Fourier transform: ', &
     &                          iflag_FFT
!
        if     (iflag_FFT .eq. iflag_FFTPACK) then
          write(*,'(a,a)') ' (FFTPACK) '
        else if(iflag_FFT .eq. iflag_FFTW) then
          write(*,'(a,a)') ' (FFTW) '
        end if
!
        write(*,*) '1: elapsed by FFTPACK: ',                           &
     &            etime_fft(iflag_FFTPACK)
        if(etime_fft(iflag_FFTW) .gt. zero) then
          write(*,*) '2: elapsed by FFTW3:   ', etime_fft(iflag_FFTW)
        end if
!
      end subroutine s_select_fourier_transform
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_vector(ncomp, Nstacksmp, etime_fft)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: Nstacksmp(0:np_smp)
      real(kind = kreal), intent(inout) :: etime_fft
!
      real(kind = kreal) :: stime, etime
!
!
      call initialize_FFT_select(my_rank, np_smp, Nstacksmp,            &
     &    nidx_rtp(3))
!
      stime = MPI_WTIME()
      call backward_FFT_select(np_smp, Nstacksmp, ncomp, nidx_rtp(3),   &
     &    vr_rtp)
      call forward_FFT_select(np_smp, Nstacksmp, ncomp, nidx_rtp(3),    &
     &    vr_rtp)
      etime = MPI_WTIME() - stime
!
      call finalize_FFT_select(np_smp)
!
      call MPI_allREDUCE (etime, etime_fft, ione,                       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      end subroutine test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      end module select_fourier_transform
