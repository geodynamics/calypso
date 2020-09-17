!>@file   init_FFT_4_MHD.F90
!!@brief  module init_FFT_4_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine init_fourier_transform_4_MHD(ncomp_tot,              &
!!     &          sph_rtp, comm_rtp, trns_MHD, WK_sph, iflag_FFT)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
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
!
      use t_spheric_rtp_data
      use t_sph_transforms
      use t_sph_trans_comm_tbl
      use t_sph_trans_arrays_MHD
      use t_sph_FFT_selector
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
      subroutine init_fourier_transform_4_MHD(ncomp_tot,                &
     &          sph_rtp, comm_rtp, trns_MHD, WK_sph, iflag_FFT)
!
      use m_solver_SR
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_tot
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      integer(kind = kint), intent(inout) :: iflag_FFT
!
!
      if(iflag_FFT .eq. iflag_UNDEFINED_FFT) then
        call compare_FFT_4_MHD(ncomp_tot, sph_rtp, comm_rtp,            &
     &      SR_r1%n_WS, SR_r1%n_WR, SR_r1%WS, SR_r1%WR,                 &
     &      trns_MHD, WK_sph%WK_FFTs)
        iflag_FFT = iflag_selected
      end if
!
      if(my_rank .eq. 0) then
        write(*,'(a,i4)', advance='no') 'Selected Fourier transform: ', &
     &                          iflag_FFT
!
        if     (iflag_FFT .eq. iflag_FFTPACK) then
          write(*,'(a)') ' (FFTPACK) '
        else if(iflag_FFT .eq. iflag_FFTW) then
          write(*,'(a)') ' (FFTW) '
!        else if(iflag_FFT .eq. iflag_ISPACK1) then
!          write(*,'(a)') ' (ISPACK) '
        else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
          write(*,'(a)') ' (FFTW_SINGLE) '
        end if
      end if
!
      call init_sph_FFT_select                                          &
     &   (my_rank, iflag_FFT, sph_rtp, ncomp_tot, WK_sph%WK_FFTs)
!
      end subroutine init_fourier_transform_4_MHD
!
! -----------------------------------------------------------------------
!
      subroutine compare_FFT_4_MHD(ncomp_tot, sph_rtp, comm_rtp,        &
     &          n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp_tot
      integer(kind = kint), intent(in) :: n_WS, n_WR
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal) :: etime_fft(5) = 10000.0
!
!
      call test_fourier_trans_4_MHD(iflag_FFTPACK,                      &
     &    ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTPACK))
!
#ifdef FFTW3
      call test_fourier_trans_4_MHD(iflag_FFTW,                         &
     &    ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTW))
!
      call test_fourier_trans_4_MHD(iflag_FFTW_SINGLE,                  &
     &    ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTW_SINGLE))
#endif
!
#ifdef FFTW3_C
      call test_fourier_trans_4_MHD(iflag_FFTW,                         &
     &    ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTW))
#endif
!
!      call test_fourier_trans_4_MHD(iflag_ISPACK1,                     &
!     &    ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,  &
!     &    WK_FFTs, etime_fft(iflag_ISPACK1))
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
!        if(etime_fft(iflag_ISPACK1) .gt. zero) then
!          write(*,*) '5: elapsed by ISPACK:         ',                 &
!     &            etime_fft(iflag_ISPACK1)
!        end if
!
      end subroutine compare_FFT_4_MHD
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_4_MHD                               &
     &         (iflag_FFT, ncomp, sph_rtp, comm_rtp,                    &
     &          n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs, etime_fft)
!
      use calypso_mpi
      use t_sph_FFT_selector
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WS, n_WR
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: etime_fft
!
      real(kind = kreal) :: starttime, endtime
      type(work_for_sgl_FFTW) :: TEST_mul_FFTW
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_sph_FFT_select'
      call init_sph_FFT_select                                          &
     &   (my_rank, iflag_FFT, sph_rtp, ncomp, WK_FFTs)
!
      if(iflag_debug .gt. 0) write(*,*) 'back_FFT_select_from_recv'
      starttime = MPI_WTIME()
      call back_FFT_select_from_recv(iflag_FFT, sph_rtp, comm_rtp,      &
     &    trns_MHD%backward%ncomp, n_WR, WR, trns_MHD%backward%fld_rtp, &
     &    WK_FFTs)
      call fwd_FFT_select_to_send(iflag_FFT, sph_rtp, comm_rtp,         &
     &    trns_MHD%forward%ncomp, n_WS, trns_MHD%forward%fld_rtp, WS,   &
     &    WK_FFTs)
      endtime = MPI_WTIME() - starttime
!
      if(iflag_debug .gt. 0) write(*,*) 'finalize_sph_FFT_select'
      call finalize_sph_FFT_select(iflag_FFT, WK_FFTs)
!
      call MPI_allREDUCE (endtime, etime_fft, 1,                        &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      etime_fft = etime_fft / dble(nprocs)
!
      end subroutine test_fourier_trans_4_MHD
!
! -----------------------------------------------------------------------
!
      end module init_FFT_4_MHD
