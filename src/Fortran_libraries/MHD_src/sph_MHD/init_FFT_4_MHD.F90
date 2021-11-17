!>@file   init_FFT_4_MHD.F90
!!@brief  module init_FFT_4_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief Select Fourier transform routine by elapsed time
!!
!!@verbatim
!!      subroutine init_fourier_transform_4_MHD(sph_rtp, comm_rtp,      &
!!     &          trns_MHD, WK_FFTs, SR_r, iflag_FFT_MHD)
!!      integer(kind = kint) function                                   &
!!     &                    set_FFT_mode_4_snapshot(iflag_FFT_MHD)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(work_for_FFTs), intent(inout) :: WK_FFTs
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      use m_FFT_selector
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
      use t_sph_trans_arrays_MHD
      use t_sph_FFT_selector
!
      implicit none
!
#ifdef FFTW3
      integer(kind = kint), parameter :: num_test =   8
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_FFTPACK_ONCE,                                   &
     &            iflag_FFTPACK_SINGLE,                                 &
     &            iflag_FFTPACK_COMPONENT,                              &
     &            iflag_FFTPACK_DOMAIN,                                 &
     &            iflag_FFTW_ONCE,                                      &
     &            iflag_FFTW_SINGLE,                                    &
     &            iflag_FFTW_COMPONENT,                                 &
     &            iflag_FFTW_DOMAIN/)
#else
      integer(kind = kint), parameter :: num_test =   4
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_FFTPACK_ONCE,                                   &
     &            iflag_FFTPACK_COMPONENT,                              &
     &            iflag_FFTPACK_SINGLE,                                 &
     &            iflag_FFTPACK_DOMAIN/)
#endif
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
      subroutine init_fourier_transform_4_MHD(sph_rtp, comm_rtp,        &
     &          trns_MHD, WK_FFTs, SR_r, iflag_FFT_MHD)
!
      use t_solver_SR
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      type(send_recv_real_buffer), intent(inout) :: SR_r
      integer(kind = kint), intent(inout) :: iflag_FFT_MHD
!
!
      if(iflag_FFT_MHD .eq. iflag_SEARCH_FASTEST_FFT) then
        call compare_FFT_4_MHD(sph_rtp, comm_rtp,                       &
     &      SR_r%n_WS, SR_r%n_WR, SR_r%WS, SR_r%WR, trns_MHD, WK_FFTs)
        iflag_FFT_MHD = iflag_selected
      end if
!
      call init_sph_FFT_select                                          &
     &   (my_rank, iflag_FFT_MHD, sph_rtp, comm_rtp,                    &
     &    trns_MHD%backward%ncomp, trns_MHD%forward%ncomp, WK_FFTs)
!
      if(my_rank .ne. 0) return
      write(*,'(a,a,a,i3,a)') 'Selected Fourier transform: ',           &
     &   trim(chosen_fft_name(iflag_FFT_MHD)), ' (', iflag_FFT_MHD, ')'
!
      end subroutine init_fourier_transform_4_MHD
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    set_FFT_mode_4_snapshot(iflag_FFT_MHD)
!
      integer(kind = kint), intent(in) :: iflag_FFT_MHD
!
!
      if(iflag_FFT_MHD .eq. iflag_FFTW_COMPONENT) then
        set_FFT_mode_4_snapshot = iflag_FFTW_SINGLE
      else if(iflag_FFT_MHD .eq. iflag_FFTW_ONCE) then
        set_FFT_mode_4_snapshot = iflag_FFTW_DOMAIN
!
      else
        set_FFT_mode_4_snapshot = iflag_FFT_MHD
      end if
!
      end function set_FFT_mode_4_snapshot
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine compare_FFT_4_MHD(sph_rtp, comm_rtp,                   &
     &          n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: n_WS, n_WR
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal) :: etime_fft(num_test) = 10000.0
      integer(kind = kint) :: i
!
!
      do i = 1, num_test
        call test_fourier_trans_4_MHD(list_test(i), sph_rtp, comm_rtp,  &
     &      n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs, etime_fft(i))
      end do
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
      end subroutine compare_FFT_4_MHD
!
! -----------------------------------------------------------------------
!
      subroutine test_fourier_trans_4_MHD(iflag_FFT, sph_rtp, comm_rtp, &
     &          n_WS, n_WR, WS, WR, trns_MHD, WK_FFTs, etime_fft)
!
      use calypso_mpi
      use t_sph_FFT_selector
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
      integer(kind = kint), intent(in) :: n_WS, n_WR
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(work_for_FFTs), intent(inout) :: WK_FFTs
      real (kind=kreal), intent(inout):: WS(n_WS)
      real (kind=kreal), intent(inout):: WR(n_WR)
      real(kind = kreal), intent(inout) :: etime_fft
!
      real(kind = kreal) :: starttime, endtime
!
!
      if(iflag_debug .gt. 0) write(*,*) 'init_sph_FFT_select'
      call init_sph_FFT_select(my_rank, iflag_FFT, sph_rtp, comm_rtp,   &
     &    trns_MHD%backward%ncomp, trns_MHD%forward%ncomp, WK_FFTs)
!
      if(iflag_debug .gt. 0) write(*,*) 'back_FFT_select_from_recv'
      starttime = MPI_WTIME()
      call back_FFT_select_from_recv(sph_rtp, comm_rtp,                 &
     &    trns_MHD%backward%ncomp, n_WR, WR, trns_MHD%backward%fld_rtp, &
     &    WK_FFTs)
      call fwd_FFT_select_to_send(sph_rtp, comm_rtp,                    &
     &    trns_MHD%forward%ncomp, n_WS, trns_MHD%forward%fld_rtp, WS,   &
     &    WK_FFTs)
      endtime = MPI_WTIME() - starttime
!
      if(iflag_debug .gt. 0) write(*,*) 'finalize_sph_FFT_select'
      call finalize_sph_FFT_select(sph_rtp, WK_FFTs)
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
