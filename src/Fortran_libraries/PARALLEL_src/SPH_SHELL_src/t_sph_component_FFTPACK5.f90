!>@file   t_sph_component_FFTPACK5.f90
!!@brief  module t_sph_component_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_comp_FFTPACK5                               &
!!     &         (sph_rtp, ncomp_bwd, ncomp_fwd, fftpack_t)
!!      subroutine finalize_sph_comp_FFTPACK5(fftpack_t)
!!      subroutine verify_sph_comp_FFTPACK5                             &
!!     &         (sph_rtp, ncomp_bwd, ncomp_fwd, fftpack_t)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_comp_RFFTMF_to_send(sph_rtp, comm_rtp,           &
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
!!      subroutine sph_comp_RFFTMB_from_recv(sph_rtp, comm_rtp,         &
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
      module t_sph_component_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>      Structure to use ISPACK
      type work_for_comp_fftpack
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
      end type work_for_comp_fftpack
!
      private :: alloc_work_comp_FFTPACK, alloc_const_comp_FFTPACK
      private :: dealloc_work_comp_FFTPACK, dealloc_const_comp_FFTPACK
      private :: copy_comp_RFFTMF_to_send, copy_comp_RFFTMB_from_recv
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_comp_FFTPACK5                                 &
     &         (sph_rtp, ncomp_bwd, ncomp_fwd, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      call alloc_const_comp_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
      call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,    &
     &    ierr)
!
      call alloc_work_comp_FFTPACK                                      &
     &   (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd,fftpack_t)
!
      allocate(fftpack_t%t_omp(np_smp,0:3))
      fftpack_t%t_omp = 0.0d0
!
      end subroutine init_sph_comp_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_comp_FFTPACK5(fftpack_t)
!
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
!
      call dealloc_const_comp_FFTPACK(fftpack_t)
      call dealloc_work_comp_FFTPACK(fftpack_t)
      deallocate(fftpack_t%t_omp)
!
      end subroutine finalize_sph_comp_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_comp_FFTPACK5                               &
     &         (sph_rtp, ncomp_bwd, ncomp_fwd, fftpack_t)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr, npoint
!
!
      npoint = sph_rtp%nidx_rtp(3) * max(ncomp_bwd, ncomp_fwd)
!
      if(fftpack_t%iflag_fft_len .ne. sph_rtp%nidx_rtp(3)) then
!
        if(fftpack_t%iflag_fft_len .lt. 0) then
          call alloc_const_comp_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        else if(npoint .gt. size(fftpack_t%WK,1) ) then
          call dealloc_const_comp_FFTPACK(fftpack_t)
          call alloc_const_comp_FFTPACK(sph_rtp%nidx_rtp(3), fftpack_t)
        end if
!
        call RFFTMI(sph_rtp%nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV,  &
     &              ierr)
      end if
!
      if(allocated(fftpack_t%WK) .eqv. .false.) then
        call alloc_work_comp_FFTPACK                                    &
     &     (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd, fftpack_t)
      else if(npoint .gt. size(fftpack_t%WK,1)) then
        call dealloc_work_comp_FFTPACK(fftpack_t)
        call alloc_work_comp_FFTPACK                                    &
     &     (sph_rtp%nidx_rtp(3), ncomp_bwd, ncomp_fwd, fftpack_t)
      end if
!
      end subroutine verify_sph_comp_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_comp_RFFTMF_to_send(sph_rtp, comm_rtp,             &
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
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: j, ip, nsize, ist, ied, ierr
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        fftpack_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,j,ist,ied,nsize)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        nsize = ncomp_fwd*sph_rtp%nidx_rtp(3)
        do j = ist, ied
!
          if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
          call sel_copy_comp_rtp_to_FFT                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_fwd, X_rtp(1,1), fftpack_t%X(1,ip))
          if(iflag_FFT_time) fftpack_t%t_omp(ip,1)                      &
     &                      = fftpack_t%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
          if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
          call RFFTMF(ncomp_fwd, ione, sph_rtp%nidx_rtp(3), ncomp_fwd,  &
     &        fftpack_t%X(1,ip), nsize, fftpack_t%WSV, fftpack_t%NSV,   &
     &        fftpack_t%WK(1,ip), nsize, ierr)
          if(iflag_FFT_time) fftpack_t%t_omp(ip,2)                      &
     &                      = fftpack_t%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
          if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
          call copy_comp_RFFTMF_to_send(j, sph_rtp%nnod_rtp,            &
     &        sph_rtp%nidx_rtp(3), sph_rtp%istep_rtp, comm_rtp%irev_sr, &
     &        ncomp_fwd, fftpack_t%X(1,ip), n_WS, WS)
          if(iflag_FFT_time) fftpack_t%t_omp(ip,3)                      &
     &                      = fftpack_t%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, fftpack_t%t_omp(1,1),        &
     &      elps1%elapsed(ist_elapsed_FFT+4))
      end if
!
      end subroutine sph_comp_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_comp_RFFTMB_from_recv(sph_rtp, comm_rtp,           &
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
     &                    :: X_rtp(sph_rtp%nnod_rtp,ncomp_bwd)
!
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) ::  j, ip, inum, nsize, ist, ied, ierr
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        fftpack_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,j,ist,ied,inum,nsize)
      do ip = 1, np_smp
        ist = sph_rtp%istack_rtp_rt_smp(ip-1) + 1
        ied = sph_rtp%istack_rtp_rt_smp(ip)
        nsize = ncomp_bwd*sph_rtp%nidx_rtp(3)
        do j = ist, ied
!
!   normalization
          if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
          call copy_comp_RFFTMB_from_recv(j, sph_rtp%nnod_rtp,          &
     &        sph_rtp%nidx_rtp(3), sph_rtp%istep_rtp, comm_rtp%irev_sr, &
     &        ncomp_bwd, n_WR, WR, fftpack_t%X(1,ip))
          if(iflag_FFT_time) fftpack_t%t_omp(ip,1)                      &
     &                      = fftpack_t%t_omp(ip,1)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
          if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
          call RFFTMB(ncomp_bwd, ione, sph_rtp%nidx_rtp(3), ncomp_bwd,  &
     &        fftpack_t%X(1,ip), nsize, fftpack_t%WSV, fftpack_t%NSV,   &
     &        fftpack_t%WK(1,ip), nsize, ierr)
          if(iflag_FFT_time) fftpack_t%t_omp(ip,2)                      &
     &                      = fftpack_t%t_omp(ip,2)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
!
          if(iflag_FFT_time) fftpack_t%t_omp(ip,0) = MPI_WTIME()
          call sel_copy_comp_FFT_to_rtp                                 &
     &       (j, sph_rtp%nnod_rtp, sph_rtp%istep_rtp(3),                &
     &        sph_rtp%istack_rtp_rt_smp(np_smp), sph_rtp%nidx_rtp(3),   &
     &        ncomp_bwd, fftpack_t%X(1,ip), X_rtp(1,1))
          if(iflag_FFT_time) fftpack_t%t_omp(ip,3)                      &
     &                      = fftpack_t%t_omp(ip,3)                     &
     &                       + MPI_WTIME() - fftpack_t%t_omp(ip,0)
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, fftpack_t%t_omp(1,1),        &
     &      elps1%elapsed(ist_elapsed_FFT+1))
      end if
!
      end subroutine sph_comp_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_comp_FFTPACK                                &
     &         (Nfft, ncomp_bwd, ncomp_fwd, fftpack_t)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: npoint
!
!
      npoint = Nfft * max(ncomp_bwd, ncomp_fwd)
      allocate( fftpack_t%X(npoint,np_smp) )
      allocate( fftpack_t%WK(npoint,np_smp) )
      fftpack_t%WK = 0.0d0
!
      end subroutine alloc_work_comp_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_comp_FFTPACK(nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: itmp
!
      fftpack_t%iflag_fft_len = nfft
      itmp = int(log(real(Nfft)) / log(two),KIND(itmp))
      fftpack_t%NSV = nfft + itmp + ifour
      allocate(fftpack_t%WSV(fftpack_t%NSV) )
      fftpack_t%WSV = 0.0d0
!
      end subroutine alloc_const_comp_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_comp_FFTPACK(fftpack_t)
!
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
!
      deallocate(fftpack_t%X, fftpack_t%WK)
!
      end subroutine dealloc_work_comp_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_comp_FFTPACK(fftpack_t)
!
      type(work_for_comp_fftpack), intent(inout) :: fftpack_t
!
      deallocate( fftpack_t%WSV )
      fftpack_t%iflag_fft_len = 0
!
      end subroutine dealloc_const_comp_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_comp_RFFTMF_to_send                               &
     &         (j, nnod_rtp, mphi_rtp, istep_rtp, irev_sr_rtp,          &
     &          ncomp_fwd, X_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_fft(ncomp_fwd*mphi_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: j0_rtp, inod_s, inod_c
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_send, is_send
!
!
      inod_s = (mphi_rtp-1) * ncomp_fwd
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      ic_rtp = j0_rtp
      is_rtp = j0_rtp + istep_rtp(3)
      ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
      is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
      WS(ic_send+1:ic_send+ncomp_fwd) = X_fft(1:ncomp_fwd)
      WS(is_send+1:is_send+ncomp_fwd)                                   &
     &            = X_fft(inod_s+1:inod_s+ncomp_fwd)
      do m = 1, (mphi_rtp+1)/2 - 1
        inod_c = (2*m-1) * ncomp_fwd
        inod_s = (2*m  ) * ncomp_fwd
        ic_rtp = j0_rtp + (2*m  ) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m+1) * istep_rtp(3)
        ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &            = X_fft(inod_c+1:inod_c+ncomp_fwd)
        WS(is_send+1:is_send+ncomp_fwd)                                 &
     &            = X_fft(inod_s+1:inod_s+ncomp_fwd)
      end do
!
      end subroutine copy_comp_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_comp_RFFTMB_from_recv                             &
     &         (j, nnod_rtp, mphi_rtp, istep_rtp, irev_sr_rtp,          &
     &          ncomp_bwd, n_WR, WR, X_fft)
!
      integer(kind = kint), intent(in) :: j
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: mphi_rtp, istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_fft(ncomp_bwd*mphi_rtp)
!
      integer(kind = kint) :: j0_rtp, inod_s, inod_c
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_recv, is_recv
!
!
      inod_s = (mphi_rtp-1) * ncomp_bwd
      j0_rtp = 1 + (j-1) * istep_rtp(1)
      ic_rtp = j0_rtp
      is_rtp = j0_rtp + istep_rtp(3)
      ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
      is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
      X_fft(1:ncomp_bwd) = WR(ic_recv+1:ic_recv+ncomp_bwd)
      X_fft(inod_s+1:inod_s+ncomp_bwd)                                  &
     &            = WR(is_recv+1:is_recv+ncomp_bwd)
      do m = 1, (mphi_rtp+1)/2 - 1
        inod_c = (2*m-1) * ncomp_bwd
        inod_s = (2*m  ) * ncomp_bwd
        ic_rtp = j0_rtp + (2*m  ) * istep_rtp(3)
        is_rtp = j0_rtp + (2*m+1) * istep_rtp(3)
        ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
        X_fft(inod_c+1:inod_c+ncomp_bwd)                                &
     &              = WR(ic_recv+1:ic_recv+ncomp_bwd)
        X_fft(inod_s+1:inod_s+ncomp_bwd)                                &
     &              = WR(is_recv+1:is_recv+ncomp_bwd)
      end do
!
      end subroutine copy_comp_RFFTMB_from_recv
!
! ------------------------------------------------------------------
!
      end module t_sph_component_FFTPACK5
