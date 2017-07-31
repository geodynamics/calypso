!>@file   t_sph_ISPACK_FFT.f90
!!@brief  module t_sph_ISPACK_FFT
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform for spherical harmonics transform 
!!@n      using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_ISPACK(nidx_rtp, maxirt_rtp_smp, ncomp,     &
!!     &          ispack_t)
!!      subroutine finalize_sph_ISPACK(ispack_t)
!!      subroutine verify_sph_ISPACK(nidx_rtp, maxirt_rtp_smp, ncomp,   &
!!     &          ispack_t)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_FTTRUF_to_send(nnod_rtp, nidx_rtp,               &
!!     &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,          &
!!     &          X_rtp, WS, ispack_t)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sph_FTTRUB_from_recv(nnod_rtp, nidx_rtp,             &
!!     &          irt_rtp_smp_stack, ncomp, n_WR, irev_sr_rtp,          &
!!     &          WR, X_rtp, ispack_t)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!! i = 1:     a_{0}
!! i = 2:     a_{Nfft/2}
!! i = 3:     a_{1}
!! i = 4:     b_{1}
!! ...
!! i = 2*k+1: a_{k}
!! i = 2*k+2: b_{k}
!! ...
!! i = Nfft-1:   a_{Nfft/2-1}
!! i = Nfft:     b_{Nfft/2-1}
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
      module t_sph_ISPACK_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      implicit none
!
!
!>      Structure for each thread
      type work_each_ispack
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for ISPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_each_ispack
!
!>      Structure to use ISPACK
      type work_for_ispack
!>      Structure for each thread
        type(work_each_ispack), pointer :: smp(:)
!>        Maximum nuber of components for each SMP process
        integer(kind = kint) :: Mmax_smp
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT(5)
      end type work_for_ispack
!
      private :: alloc_work_4_ispack, alloc_const_4_ispack
      private :: dealloc_work_4_ispack, dealloc_const_4_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_ISPACK(nidx_rtp, maxirt_rtp_smp, ncomp,       &
     &          ispack_t)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      integer(kind = kint), intent(in) :: ncomp
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      ispack_t%Mmax_smp = ncomp*maxirt_rtp_smp
      call alloc_const_4_ispack(nidx_rtp(3), ispack_t)
      call FTTRUI(nidx_rtp(3), ispack_t%IT, ispack_t%T)
!
      call alloc_work_4_ispack(nidx_rtp(3), ispack_t)
!
      end subroutine init_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_ISPACK(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      call dealloc_const_4_ispack(ispack_t)
      call dealloc_work_4_ispack(ispack_t)
!
      end subroutine finalize_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_ISPACK(nidx_rtp, maxirt_rtp_smp, ncomp,     &
     &          ispack_t)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: maxirt_rtp_smp
!
      integer(kind = kint), intent(in) :: ncomp
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      ispack_t%Mmax_smp = ncomp*maxirt_rtp_smp
!
      if((2*nidx_rtp(3)) .ne. size(ispack_t%T)) then
!
        if(allocated(ispack_t%T) .eqv. .false.) then
          call alloc_const_4_ispack(nidx_rtp(3), ispack_t)
        else if( (2*nidx_rtp(3)) .gt. size(ispack_t%T) ) then
          call dealloc_const_4_ispack(ispack_t)
          call alloc_const_4_ispack(nidx_rtp(3), ispack_t)
        end if
!
        call FTTRUI( nidx_rtp(3), ispack_t%IT, ispack_t%T )
      end if
!
      if(ASSOCIATED(ispack_t%smp) .eqv. .false.) then
        call alloc_work_4_ispack(nidx_rtp(3), ispack_t)
      else if( (ispack_t%Mmax_smp*nidx_rtp(3))                          &
     &       .gt. size(ispack_t%smp(1)%X,1) ) then
        call dealloc_work_4_ispack(ispack_t)
        call alloc_work_4_ispack(nidx_rtp(3), ispack_t)
      end if
!
      end subroutine verify_sph_ISPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUF_to_send(nnod_rtp, nidx_rtp,                 &
     &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,            &
     &          X_rtp, WS, ispack_t)
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
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: m, j, ip, ist, num, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inum, inod_s, inod_c
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,nd,ist,num,inum,inod_s,inod_c,            &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
!
        do m = 1, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            ispack_t%smp(ip)%X(inod_c) = X_rtp(j,2*m-1,nd)
            ispack_t%smp(ip)%X(inod_s) = X_rtp(j,2*m,  nd)
          end do
        end do
!
        call FTTRUF(num, nidx_rtp(3), ispack_t%smp(ip)%X,               &
     &      ispack_t%smp(ip)%WK, ispack_t%IT, ispack_t%T)
!
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          inod_c = inum
          inod_s = inum + num
          WS(ic_send) = ispack_t%smp(ip)%X(inod_c)
          WS(is_send) = ispack_t%smp(ip)%X(inod_s)
        end do
        do m = 2, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            WS(ic_send) =   two * ispack_t%smp(ip)%X(inod_c)
            WS(is_send) = - two * ispack_t%smp(ip)%X(inod_s)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine sph_FTTRUF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUB_from_recv(nnod_rtp, nidx_rtp,               &
     &          irt_rtp_smp_stack, ncomp, n_WR, irev_sr_rtp,            &
     &          WR, X_rtp, ispack_t)
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
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) ::  m, j, ip, ist, num, nd
      integer(kind = kint) :: inum, inod_s, inod_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num,inum,nd,inod_s,inod_c,            &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = ncomp *  irt_rtp_smp_stack(ip-1)
        num = ncomp * (irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1))
!
        do inum = 1, num
          nd = 1 + mod(ist+inum-1,ncomp)
          j =  1 + (ist+inum-nd) / ncomp
          is_rtp = j + irt_rtp_smp_stack(np_smp)
          ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
          inod_c = inum
          inod_s = inum + num
          ispack_t%smp(ip)%X(inod_c) = WR(ic_recv)
          ispack_t%smp(ip)%X(inod_s) = WR(is_recv)
        end do
        do m = 2, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
            ispack_t%smp(ip)%X(inod_c) =  half * WR(ic_recv)
            ispack_t%smp(ip)%X(inod_s) = -half * WR(is_recv)
          end do
        end do
!
        call FTTRUB(num, nidx_rtp(3), ispack_t%smp(ip)%X,               &
     &      ispack_t%smp(ip)%WK, ispack_t%IT, ispack_t%T )
!
        do m = 1, nidx_rtp(3)/2
          do inum = 1, num
            nd = 1 + mod(ist+inum-1,ncomp)
            j =  1 + (ist+inum-nd) / ncomp
            inod_c = inum + (2*m-2) * num
            inod_s = inum + (2*m-1) * num
            X_rtp(j,2*m-1,nd) = ispack_t%smp(ip)%X(inod_c)
            X_rtp(j,2*m,  nd) = ispack_t%smp(ip)%X(inod_s)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sph_FTTRUB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_ispack(Nfft, ispack_t)
!
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: iflag_fft_comp, ip
!
!
      allocate( ispack_t%smp(np_smp) )
!
      iflag_fft_comp = ispack_t%Mmax_smp*Nfft
      do ip = 1, np_smp
        allocate( ispack_t%smp(ip)%X(iflag_fft_comp) )
        allocate( ispack_t%smp(ip)%WK(iflag_fft_comp) )
        ispack_t%smp(ip)%WK = 0.0d0
      end do
!
      end subroutine alloc_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_ispack(nfft, ispack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      allocate( ispack_t%T(2*nfft) )
      ispack_t%T = 0.0d0
!
      end subroutine alloc_const_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_ispack(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        deallocate(ispack_t%smp(ip)%X, ispack_t%smp(ip)%WK)
      end do
!
      deallocate(ispack_t%smp)
!
      end subroutine dealloc_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_ispack(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      deallocate(ispack_t%T)
!
      end subroutine dealloc_const_4_ispack
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK_FFT
