!>@file   t_sph_single_FFTPACK5.f90
!!@brief  module t_sph_single_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_single_FFTPACK5(sph_rtp, fftpack_t)
!!      subroutine finalize_sph_single_FFTPACK5(fftpack_t)
!!      subroutine verify_sph_single_FFTPACK5(sph_rtp, fftpack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_single_RFFTMF_to_send(sph_rtp, comm_rtp,         &
!!     &          ncomp_fwd, n_WS, X_rtp, WS, fftpack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTPACK5
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sph_single_RFFTMB_from_recv(sph_rtp, comm_rtp,       &
!!     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_comm_tbl), intent(in) :: comm_rtp
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTPACK5
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module t_sph_single_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
!
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>      Structure to use ISPACK
      type work_for_sgl_fftpack
!>        Size of work constant for FFTPACK
        integer(kind = kint) :: NSV
!>        Work constatnts for FFTPACK
        real(kind = 8), allocatable :: WSV(:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
!
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:,:)
!>        Work area for FFTPACK
        real(kind = 8), allocatable :: WK(:,:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_sgl_fftpack
!
      private :: alloc_work_sgl_FFTPACK, alloc_const_sgl_FFTPACK
      private :: dealloc_work_sgl_FFTPACK, dealloc_const_sgl_FFTPACK
      private :: copy_single_RFFTMF_to_send
      private :: copy_single_RFFTMB_from_recv
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_single_FFTPACK5(sph_rtp, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      call alloc_const_sgl_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,    &
     &            ierr)
!
      call alloc_work_sgl_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
!
      allocate(fftpack_t%t_omp(np_smp,0:3))
      fftpack_t%t_omp = 0.0d0
!
      end subroutine init_sph_single_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_single_FFTPACK5(fftpack_t)
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
!
      call dealloc_const_sgl_FFTPACK(fftpack_t)
      call dealloc_work_sgl_FFTPACK(fftpack_t)
      deallocate(fftpack_t%t_omp)
!
      end subroutine finalize_sph_single_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_single_FFTPACK5(sph_rtp, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      if(fftpack_t%iflag_fft_len .ne. sph_rtp%nidx_rtp(3)) then
!
        if(fftpack_t%iflag_fft_len .lt. 0) then
          call alloc_const_sgl_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        else if(sph_rtp%nidx_rtp(3)                                     &
     &             .gt. size(fftpack_t%WK,1) ) then
          call dealloc_const_sgl_FFTPACK(fftpack_t)
          call alloc_const_sgl_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        end if
!
        call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV, &
     &              ierr)
      end if
!
      if(allocated(fftpack_t%WK) .eqv. .false.) then
        call alloc_work_sgl_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      else if(sph_rtp%nidx_rtp(3) .gt. size(fftpack_t%WK,1)) then
        call dealloc_work_sgl_FFTPACK(fftpack_t)
        call alloc_work_sgl_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      end if
!
      end subroutine verify_sph_single_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_single_RFFTMF_to_send(sph_rtp, comm_rtp,           &
     &          ncomp_fwd, n_WS, X_rtp, WS, fftpack_t)
!
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &                   :: X_rtp(sph_rtp%nnod_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: j, ip, ist, ied, nd, ierr
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        fftpack_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,j,nd,ist,ied)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        do j = ist, ied
          do nd = 1, ncomp_fwd
!
            if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
            call sel_copy_single_rtp_to_FFT                             &
     &         (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),              &
     &          sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3), &
     &          X_rtp(1,nd), fftpack_t%X(1,ip))
            if(iflag_FFT_time) fftpack_t%t_omp(ip,1)                    &
     &                      = fftpack_t%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
            if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
            call RFFTMF(ione, ione, sph_rtp%nidx_rtp(3), ione,          &
     &          fftpack_t%X(1,ip),  sph_rtp%nidx_rtp(3),                &
     &          fftpack_t%WSV, fftpack_t%NSV, fftpack_t%WK(1,ip),       &
     &          sph_rtp%nidx_rtp(3), ierr)
            if(iflag_FFT_time) fftpack_t%t_omp(ip,2)                    &
     &                      = fftpack_t%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
            if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
            call copy_single_RFFTMF_to_send                             &
     &         (nd, j, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),           &
     &          sph_rtp%istep_rtp, comm_rtp%irev_sr,                    &
     &          ncomp_fwd, fftpack_t%X(1,ip), n_WS, WS)
            if(iflag_FFT_time) fftpack_t%t_omp(ip,3)                    &
     &                      = fftpack_t%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
          end do
        end do
!
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, fftpack_t%t_omp(1,1),        &
     &      elps1%elapsed(ist_elapsed_FFT+4))
      end if
!
      end subroutine sph_single_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_single_RFFTMB_from_recv(sph_rtp, comm_rtp,         &
     &          ncomp_bwd, n_WR, WR, X_rtp, fftpack_t)
!
      use copy_single_FFT_and_rtp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_comm_tbl), intent(in) :: comm_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: j, ip, ist, ied, nd, ierr
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        fftpack_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,j,nd,ist,ied)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        do j = ist, ied
          do nd = 1, ncomp_bwd
!   normalization
            if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
            call copy_single_RFFTMB_from_recv                           &
     &         (nd, j, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(3),           &
     &          sph_rtp%istep_rtp, comm_rtp%irev_sr,                    &
     &          ncomp_bwd, n_WR, WR, fftpack_t%X(1,ip))
            if(iflag_FFT_time) fftpack_t%t_omp(ip,1)                    &
     &                      = fftpack_t%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
            if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
            call RFFTMB(ione, ione, sph_rtp%nidx_rtp(3), ione,          &
     &          fftpack_t%X(1,ip), sph_rtp%nidx_rtp(3),                 &
     &          fftpack_t%WSV, fftpack_t%NSV, fftpack_t%WK(1,ip),       &
     &          sph_rtp%nidx_rtp(3), ierr)
            if(iflag_FFT_time) fftpack_t%t_omp(ip,2)                    &
     &                      = fftpack_t%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
            if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
            call sel_copy_single_FFT_to_rtp                             &
     &         (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),              &
     &          sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3), &
     &          fftpack_t%X(1,ip), X_rtp(1,nd))
            if(iflag_FFT_time) fftpack_t%t_omp(ip,3)                    &
     &                      = fftpack_t%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
          end do
        end do
!
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, fftpack_t%t_omp(1,1),        &
     &      elps1%elapsed(ist_elapsed_FFT+1))
      end if
!
      end subroutine sph_single_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_sgl_FFTPACK(Nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
!
      allocate( fftpack_t%X(Nfft,np_smp) )
      allocate( fftpack_t%WK(Nfft,np_smp) )

!$omp parallel workshare
      fftpack_t%X =  0.0d0
      fftpack_t%WK = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_work_sgl_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_sgl_FFTPACK(nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: itmp
!
      fftpack_t%iflag_fft_len = nfft
      itmp = int(log(real(Nfft)) / log(two),KIND(itmp))
      fftpack_t%NSV = nfft + itmp + ifour
      allocate(fftpack_t%WSV(fftpack_t%NSV) )
      fftpack_t%WSV = 0.0d0
!
      end subroutine alloc_const_sgl_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_sgl_FFTPACK(fftpack_t)
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
!
      deallocate(fftpack_t%X, fftpack_t%WK)
!
      end subroutine dealloc_work_sgl_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_sgl_FFTPACK(fftpack_t)
!
      type(work_for_sgl_fftpack), intent(inout) :: fftpack_t
!
      deallocate( fftpack_t%WSV )
      fftpack_t%iflag_fft_len = 0
!
      end subroutine dealloc_const_sgl_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_single_RFFTMF_to_send                             &
     &         (nd, j, nnod_rtp, mphi_rtp, istep_rtp, irev_sr_rtp,      &
     &          ncomp_fwd, X_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd, j
      integer(kind = kint), intent(in) :: nnod_rtp, mphi_rtp
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_fft(mphi_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, j0_rtp
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      is_rtp = j0_rtp + istep_rtp(3)
      ic_send = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_fwd
      is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
      WS(ic_send) = X_fft(1)
      WS(is_send) = X_fft(mphi_rtp)
      do m = 1, mphi_rtp/2 - 1
        ic_rtp = j0_rtp + (2*m  ) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m+1) * istep_rtp(3)
        ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send) = X_fft(2*m  )
        WS(is_send) = X_fft(2*m+1)
      end do
!
      end subroutine copy_single_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_single_RFFTMB_from_recv                           &
     &         (nd, j, nnod_rtp, mphi_rtp, istep_rtp, irev_sr_rtp,      &
     &          ncomp_bwd, n_WR, WR, X_fft)
!
      integer(kind = kint), intent(in) :: nd, j
      integer(kind = kint), intent(in) :: nnod_rtp, mphi_rtp
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_fft(mphi_rtp)
!
      integer(kind = kint) :: m, j0_rtp
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!   normalization
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      is_rtp = j0_rtp + istep_rtp(3)
      ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
      is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
      X_fft(1) =           WR(ic_recv)
      X_fft(mphi_rtp) = WR(is_recv)
      do m = 1, mphi_rtp/2 - 1
        ic_rtp = j0_rtp + (2*m  ) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m+1) * istep_rtp(3)
        ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        X_fft(2*m  ) = WR(ic_recv)
        X_fft(2*m+1) = WR(is_recv)
      end do
!
      end subroutine copy_single_RFFTMB_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_single_FFTPACK5
