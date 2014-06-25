!>@file   select_fourier_transform.F90
!!@brief  module select_fourier_transform
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine sel_fourier_transform_4_sph(ncomp, Nstacksmp)
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
      real(kind = kreal) :: etime_shortest = -1.0e10
      integer(kind = kint) :: iflag_seelcted
!
      private :: etime_shortest, iflag_seelcted
      private :: s_select_fourier_transform, test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine sel_fourier_transform_4_sph(ncomp, Nstacksmp)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: Nstacksmp(0:np_smp)
!
!
      if(iflag_FFT .eq. iflag_UNDEFINED_FFT) then
        call s_select_fourier_transform(ncomp, Nstacksmp)
        iflag_FFT = iflag_seelcted
      end if
!
      call initialize_FFT_select(my_rank, np_smp, Nstacksmp,            &
     &    nidx_rtp(3))
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
      end subroutine sel_fourier_transform_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine s_select_fourier_transform(ncomp, Nstacksmp)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: Nstacksmp(0:np_smp)
!
      real(kind = kreal) :: etime_fft(1:4)
!
!
      iflag_FFT = iflag_FFTPACK
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFT) )
!
!
#ifdef FFTW3
      iflag_FFT = iflag_FFTW
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFT))
!
      iflag_FFT = iflag_FFTW_SINGLE
      call test_fourier_trans_vector(ncomp, Nstacksmp,                  &
     &    etime_fft(iflag_FFTW))
#endif
!
      if(my_rank .gt. 0) return
        write(*,*) '1: elapsed by FFTPACK: ',                           &
     &            etime_fft(iflag_FFTPACK)
        if(etime_fft(iflag_FFTW) .gt. zero) then
          write(*,*) '2: elapsed by FFTW3:          ',                  &
     &            etime_fft(iflag_FFTW)
        end if
        if(etime_fft(iflag_FFTW_SINGLE) .gt. zero) then
          write(*,*) '3: elapsed by single FFTW3:   ',                  &
     &            etime_fft(iflag_FFTW_SINGLE)
        end if
!
      end subroutine s_select_fourier_transform
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_vector(ncomp, Nstacksmp, etime_fft)
!
      use calypso_mpi
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
      call MPI_allREDUCE (etime, etime_fft, ione,                       &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_fft = etime_fft / dble(nprocs)
!
      call finalize_FFT_select(np_smp, Nstacksmp)
!
      if(etime_fft .lt. etime_shortest                                  &
      &        .or. etime_shortest.lt.0.0d0) then
        iflag_seelcted = iflag_FFTW
        etime_shortest = etime_fft
      end if
!
      end subroutine test_fourier_trans_vector
!
! -----------------------------------------------------------------------
!
      end module select_fourier_transform
