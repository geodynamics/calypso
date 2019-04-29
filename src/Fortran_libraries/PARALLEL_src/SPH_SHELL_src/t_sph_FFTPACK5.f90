!>@file   t_sph_FFTPACK5.f90
!!@brief  module t_sph_FFTPACK5
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTPACK5
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_FFTPACK5                                    &
!!     &         (nidx_rtp, maxirt_rtp_smp, ncomp, fftpack_t)
!!      subroutine finalize_sph_FFTPACK5(fftpack_t)
!!      subroutine verify_sph_FFTPACK5                                  &
!!     &         (nidx_rtp, maxirt_rtp_smp, ncomp, fftpack_t)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!!      subroutine sph_RFFTMF_to_send(nnod_rtp, nidx_rtp,               &
!!     &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,          &
!!     &          X_rtp, WS, fftpack_t)
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
!!      subroutine sph_RFFTMB_from_recv(nnod_rtp, nidx_rtp,             &
!!     &          irt_rtp_smp_stack, ncomp, n_WR, irev_sr_rtp,          &
!!     &          WR, X_rtp, fftpack_t)
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
      module t_sph_FFTPACK5
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!>      Structure for each FFTPACK
      type work_each_fftpack
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for FFTPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_each_fftpack
!
!>      Structure to use ISPACK
      type work_for_fftpack
!>      Structure for each thread
        type(work_each_fftpack), allocatable :: smp(:)
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Size of work constant for FFTPACK
        integer(kind = kint) :: NSV
!>        Work constatnts for FFTPACK
        real(kind = 8), allocatable :: WSV(:)
!>        flag for length of Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
      end type work_for_fftpack
!
      private :: alloc_work_4_FFTPACK, alloc_const_4_FFTPACK
      private :: dealloc_work_4_FFTPACK, dealloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_FFTPACK5                                      &
     &         (nidx_rtp, maxirt_rtp_smp, ncomp, fftpack_t)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3), maxirt_rtp_smp
!
      integer(kind = kint), intent(in) :: ncomp
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
      fftpack_t%Mmax_smp = ncomp*maxirt_rtp_smp
!
      call alloc_const_4_FFTPACK(nidx_rtp(3), fftpack_t)
      call RFFTMI(nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV, ierr)
!
      call alloc_work_4_FFTPACK(nidx_rtp(3), fftpack_t)
!
      end subroutine init_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_FFTPACK5(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
!
      call dealloc_const_4_FFTPACK(fftpack_t)
      call dealloc_work_4_FFTPACK(fftpack_t)
!
      end subroutine finalize_sph_FFTPACK5
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_FFTPACK5                                    &
     &         (nidx_rtp, maxirt_rtp_smp, ncomp, fftpack_t)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3), maxirt_rtp_smp
!
      integer(kind = kint), intent(in) ::  ncomp
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: ierr
!
!
      fftpack_t%Mmax_smp = ncomp*maxirt_rtp_smp
!
      if(fftpack_t%iflag_fft_len .ne. nidx_rtp(3)) then
!
        if(fftpack_t%iflag_fft_len .lt. 0) then
          call alloc_const_4_FFTPACK(nidx_rtp(3), fftpack_t)
        else if((fftpack_t%Mmax_smp*nidx_rtp(3))                        &
     &          .gt. size(fftpack_t%smp(1)%WK) ) then
          call dealloc_const_4_FFTPACK(fftpack_t)
          call alloc_const_4_FFTPACK(nidx_rtp(3), fftpack_t)
        end if
!
        call RFFTMI(nidx_rtp(3), fftpack_t%WSV, fftpack_t%NSV, ierr)
      end if
!
      if(allocated(fftpack_t%smp(1)%WK) .eqv. .false.) then
        call alloc_work_4_FFTPACK(nidx_rtp(3), fftpack_t)
      else if( (fftpack_t%Mmax_smp*nidx_rtp(3))                         &
     &      .gt. size(fftpack_t%smp(1)%WK,1)  ) then
        call dealloc_work_4_FFTPACK(fftpack_t)
        call alloc_work_4_FFTPACK(nidx_rtp(3), fftpack_t)
      end if
!
      end subroutine verify_sph_FFTPACK5
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMF_to_send(nnod_rtp, nidx_rtp,                 &
     &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,            &
     &          X_rtp, WS, fftpack_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) ::  m, j, ip, ist, num, inum, nsize, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c, ierr
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(m,j,nd,ist,num,inum,nsize,inod_s,inod_c,      &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
        nsize = num*nidx_rtp(3)
!
        do m = 1, nidx_rtp(3)
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (m-1) * num
            fftpack_t%smp(ip)%X(inod_c) = X_rtp(j,m,nd)
          end do
        end do
!
        call RFFTMF(num, ione, nidx_rtp(3), num, fftpack_t%smp(ip)%X,   &
     &      nsize, fftpack_t%WSV, fftpack_t%NSV, fftpack_t%smp(ip)%WK,  &
     &      nsize, ierr)
!
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          inod_s = inum + (nidx_rtp(3)-1) * num
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          WS(ic_send) = fftpack_t%smp(ip)%X(inum)
          WS(is_send) = fftpack_t%smp(ip)%X(inod_s)
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-1) * num
            inod_s = inum + (2*m  ) * num
            ic_rtp = j + (2*m  ) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m+1) * irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            WS(ic_send) = fftpack_t%smp(ip)%X(inod_c)
            WS(is_send) = fftpack_t%smp(ip)%X(inod_s)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine sph_RFFTMF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_RFFTMB_from_recv(nnod_rtp, nidx_rtp,               &
     &          irt_rtp_smp_stack, ncomp, n_WR, irev_sr_rtp,            &
     &          WR, X_rtp, fftpack_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) ::  m, j, ip, ist, inum, num, nsize, nd
      integer(kind = kint) :: inod_s, inod_c, ierr
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(m,j,nd,ist,num,inum,nsize,inod_s,inod_c,      &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
        nsize = num*nidx_rtp(3)
!
!   normalization
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          inod_s = inum + (nidx_rtp(3)-1) * num
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          fftpack_t%smp(ip)%X(inum) =   WR(ic_recv)
          fftpack_t%smp(ip)%X(inod_s) = WR(is_recv)
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-1) * num
            inod_s = inum + (2*m  ) * num
            ic_rtp = j + (2*m  ) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m+1) * irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            fftpack_t%smp(ip)%X(inod_c) = WR(ic_recv)
            fftpack_t%smp(ip)%X(inod_s) = WR(is_recv)
          end do
        end do
!
        call RFFTMB (num, ione, nidx_rtp(3), num, fftpack_t%smp(ip)%X,  &
     &      nsize, fftpack_t%WSV, fftpack_t%NSV, fftpack_t%smp(ip)%WK,  &
     &      nsize, ierr)
!
        do m = 1, nidx_rtp(3)
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (m-1) * num
            X_rtp(j,m,nd) = fftpack_t%smp(ip)%X(inod_c)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_RFFTMB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTPACK(Nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: iflag_fft_comp, ip
!
!
      allocate( fftpack_t%smp(np_smp) )
!
      iflag_fft_comp = fftpack_t%Mmax_smp*Nfft
      do ip = 1, np_smp
        allocate( fftpack_t%smp(ip)%X(iflag_fft_comp) )
        allocate( fftpack_t%smp(ip)%WK(iflag_fft_comp) )
        fftpack_t%smp(ip)%WK = 0.0d0
      end do
!
      end subroutine alloc_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_FFTPACK(nfft, fftpack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      integer(kind = kint) :: itmp
!
      fftpack_t%iflag_fft_len = nfft
      itmp = int(log(real(Nfft)) / log(two),KIND(itmp))
      fftpack_t%NSV = nfft + itmp + ifour
      allocate(fftpack_t%WSV(fftpack_t%NSV) )
      fftpack_t%WSV = 0.0d0
!
      end subroutine alloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(fftpack_t%smp(ip)%X, fftpack_t%smp(ip)%WK)
      end do
!
      deallocate(fftpack_t%smp)
!
      end subroutine dealloc_work_4_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_FFTPACK(fftpack_t)
!
      type(work_for_fftpack), intent(inout) :: fftpack_t
!
      deallocate( fftpack_t%WSV )
      fftpack_t%iflag_fft_len = 0
!
      end subroutine dealloc_const_4_FFTPACK
!
! ------------------------------------------------------------------
!
      end module t_sph_FFTPACK5
