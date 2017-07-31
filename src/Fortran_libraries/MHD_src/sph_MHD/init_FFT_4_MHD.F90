!>@file   init_FFT_4_MHD.F90
!!@brief  module init_FFT_4_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine init_fourier_transform_4_MHD(iflag_SGS, ncomp_tot,   &
!!     &          sph_rtp, comm_rtp, trns_MHD, trns_SGS, WK_FFTs,       &
!!     &          MHD_mul_FFTW, SGS_mul_FFTW)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD, trns_SGS
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
!!        type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
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
      use MHD_FFT_selector
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_addresses_sph_transform
      use t_sph_multi_FFTW
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
      subroutine init_fourier_transform_4_MHD(iflag_SGS, ncomp_tot,     &
     &          sph_rtp, comm_rtp, trns_MHD, trns_SGS, WK_FFTs,         &
     &          MHD_mul_FFTW, SGS_mul_FFTW)
!
      use m_solver_SR
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: iflag_SGS
      integer(kind = kint), intent(in) :: ncomp_tot
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD, trns_SGS
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
!
!
      if(iflag_FFT .eq. iflag_UNDEFINED_FFT) then
        call compare_FFT_4_MHD(ncomp_tot, sph_rtp, comm_rtp,            &
     &      n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs)
        iflag_FFT = iflag_selected
      end if
!
      call init_sph_FFT_select(my_rank, sph_rtp, ncomp_tot, WK_FFTs)
      call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,             &
     &    trns_MHD%ncomp_rtp_2_rj, trns_MHD%ncomp_rj_2_rtp,             &
     &    MHD_mul_FFTW)
      if(iflag_SGS .gt. 0) then
        call init_MHD_FFT_select(my_rank, sph_rtp, ncomp_tot,           &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%ncomp_rj_2_rtp,           &
     &      SGS_mul_FFTW)
      end if
!
      if(my_rank .gt. 0) return
      write(*,'(a,i4)', advance='no') 'Selected Fourier transform: ',   &
     &                          iflag_FFT
!
      if     (iflag_FFT .eq. iflag_FFTPACK) then
        write(*,'(a)') ' (FFTPACK) '
      else if(iflag_FFT .eq. iflag_FFTW) then
        write(*,'(a)') ' (FFTW) '
!      else if(iflag_FFT .eq. iflag_ISPACK) then
!        write(*,'(a)') ' (ISPACK) '
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
      real(kind = kreal) :: etime_fft(5)
!
!
      iflag_FFT = iflag_FFTPACK
      call test_fourier_trans_4_MHD                                     &
     &   (ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFT))
!
!
#ifdef FFTW3
      iflag_FFT = iflag_FFTW
      call test_fourier_trans_4_MHD                                     &
     &   (ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTW))
!
      iflag_FFT = iflag_FFTW_FIELD
      call test_fourier_trans_4_MHD                                     &
     &   (ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTW_FIELD))
!
      iflag_FFT = iflag_FFTW_SINGLE
      call test_fourier_trans_4_MHD                                     &
     &   (ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,   &
     &    WK_FFTs, etime_fft(iflag_FFTW_SINGLE))
#endif
!
!#ifdef FFTW3_C
!      iflag_FFT = iflag_FFTW
!      call test_fourier_trans_4_MHD                                    &
!     &   (ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,  &
!     &    WK_FFTs, etime_fft(iflag_FFTW))
!#endif
!
!      iflag_FFT = iflag_ISPACK
!      call test_fourier_trans_4_MHD                                    &
!     &   (ncomp_tot, sph_rtp, comm_rtp, n_WS, n_WR, WS, WR, trns_MHD,  &
!     &    WK_FFTs, etime_fft(iflag_ISPACK))
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
      subroutine test_fourier_trans_4_MHD(ncomp, sph_rtp, comm_rtp,     &
     &          n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs, etime_fft)
!
      use calypso_mpi
!
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
      if(iflag_debug .gt. 0) write(*,*) 'init_MHD_FFT_select'
      call init_sph_FFT_select(my_rank, sph_rtp, ncomp, WK_FFTs)
      call init_MHD_FFT_select(my_rank, sph_rtp, ncomp,                 &
     &    trns_MHD%ncomp_rtp_2_rj, trns_MHD%ncomp_rj_2_rtp,             &
     &    TEST_mul_FFTW)
!
      if(iflag_debug .gt. 0) write(*,*) 'back_MHD_FFT_sel_from_recv'
      starttime = MPI_WTIME()
      call back_MHD_FFT_sel_from_recv(sph_rtp, comm_rtp,                &
     &    trns_MHD%ncomp_rj_2_rtp, n_WR, WR, trns_MHD%fld_rtp,          &
     &    WK_FFTs, TEST_mul_FFTW)
      call fwd_MHD_FFT_sel_to_send(sph_rtp, comm_rtp,                   &
     &    trns_MHD%ncomp_rtp_2_rj, n_WS, trns_MHD%frc_rtp, WS,          &
     &    WK_FFTs, TEST_mul_FFTW)
      endtime = MPI_WTIME() - starttime
!
      if(iflag_debug .gt. 0) write(*,*) 'finalize_sph_FFT_select'
      call finalize_MHD_FFT_select(TEST_mul_FFTW)
      call finalize_sph_FFT_select(WK_FFTs)
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
