!>@file   copy_sph_FFTW_from_recv.f90
!!@brief  module copy_sph_FFTW_from_recv
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTW
!!
!!@verbatim
!!      subroutine pin_FFTW_fields_from_recv                            &
!!     &         (nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,            &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!        integer(kind = kint), intent(in) :: nnod_rt
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &              :: C_fft(Nfft_c,nnod_rt,ncomp_bwd)
!!      subroutine pout_FFTW_smp_fields_from_recv                       &
!!     &         (nnod_rtp, istep_rtp, irt_rtp_smp_stack, irev_sr_rtp,  &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!        integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        integer(kind = kint), intent(in) :: Nfft_c
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                  :: C_fft(ncomp_bwd*Nfft_c)
!!
!!      subroutine copy_prt_FFTW_comp_from_recv                         &
!!     &         (nd, nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,        &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &              :: C_fft(istep_rtp(3)*Nfft_c)
!!      subroutine copy_rtp_FFTW_comp_from_recv                         &
!!     &         (nd, nnod_rtp, irev_sr_rtp, irt_rtp_smp_stack,         &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!        integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
!!        integer(kind = kint), intent(in) :: istep_rtp(3)
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: ncomp_bwd
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real(kind = kreal), intent(in) :: WR(n_WR)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &              :: C_fft(irt_rtp_smp_stack(np_smp)*Nfft_c)
!!@endverbatim
!!
      module copy_sph_FFTW_from_recv
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_comm_table_from_FFTW
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine pin_FFTW_fields_from_recv                              &
     &         (nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,              &
     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: Nfft_c
      integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
      integer(kind = kint), intent(in) :: nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_fft(Nfft_c,nnod_rt,ncomp_bwd)
!
      integer(kind = kint) :: nd, j, j0_rtp, m
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp  parallel do                                                      &
!$omp& private(nd,j,m,j0_rtp,ic_rtp,is_rtp,ic_recv,is_recv)
      do j = 1, nnod_rt
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        do nd = 1, ncomp_bwd
          ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
          C_fft(1,j,nd) = cmplx(WR(ic_recv), zero, kind(0d0))
          do m = 2, Nfft_c-1
            ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            C_fft(m,j,nd)                                               &
     &              = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
          end do
          ic_rtp = j0_rtp + istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          C_fft(Nfft_c,j,nd) = cmplx(WR(ic_recv), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine pin_FFTW_fields_from_recv
!
! ------------------------------------------------------------------
!
      subroutine pout_FFTW_smp_fields_from_recv                         &
     &         (nnod_rtp, istep_rtp, irt_rtp_smp_stack, irev_sr_rtp,    &
     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nnod_rtp, istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: Nfft_c
      complex(kind = fftw_complex), intent(inout)                       &
     &                  :: C_fft(ncomp_bwd*Nfft_c)
!
      integer(kind = kint) :: ip, j, m, ms, ist, num, j0_rtp
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do                                                       &
!$omp&  private(j,ip,ist,ms,num,j0_rtp,ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          ic_recv = ncomp_bwd * (irev_sr_rtp(j0_rtp) - 1)
          ms = ((j-1) + ist*Nfft_c) * ncomp_bwd
          C_fft(ms+1:ms+ncomp_bwd)                                      &
     &        = cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd), zero, kind(0d0))
          do m = 2, Nfft_c-1
            ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
            ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
            is_recv = ncomp_bwd * (irev_sr_rtp(is_rtp) - 1)
            ms = ((j-1) + (m-1)*num + ist*Nfft_c) * ncomp_bwd
            C_fft(ms+1:ms+ncomp_bwd)                                    &
     &        = half * cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),           &
     &                      -WR(is_recv+1:is_recv+ncomp_bwd),kind(0d0))
          end do
          ic_rtp = j0_rtp + istep_rtp(3)
          ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
          ms = ((j-1) + (Nfft_c-1)*num + ist*Nfft_c) * ncomp_bwd
          C_fft(ms+1:ms+ncomp_bwd)                                      &
     &        = cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine pout_FFTW_smp_fields_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_prt_FFTW_comp_from_recv                           &
     &         (nd, nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,          &
     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: Nfft_c
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_fft(istep_rtp(3)*Nfft_c)
!
      integer(kind = kint) :: j, m, j0_rtp, ist_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp  parallel do                                                      &
!$omp& private(j,m,j0_rtp,ist_c,ic_rtp,is_rtp,ic_recv,is_recv)
      do j = 1, nnod_rt
        ist_c = 1 + Nfft_c * (j-1)
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
        C_fft(ist_c) = cmplx(WR(ic_recv), zero, kind(0d0))
        do m = 2, Nfft_c-1
          ist_c = m + Nfft_c * (j-1)
          ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
          is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          C_fft(ist_c)                                                  &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
        end do
        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j0_rtp + istep_rtp(3)
        ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        C_fft(ist_c) = cmplx(WR(ic_recv), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine copy_prt_FFTW_comp_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_FFTW_comp_from_recv                           &
     &         (nd, nnod_rtp, istep_rtp, irt_rtp_smp_stack,             &
     &          irev_sr_rtp, ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: Nfft_c
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_fft(irt_rtp_smp_stack(np_smp)*Nfft_c)
!
      integer(kind = kint) ::  ip, ist, num, i, m, j, j0_rtp
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(ip,ist,num,i,m,j,j0_rtp,ic_rtp,is_rtp,        &
!$omp&                    ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
          i = j + (1-1)*num + Nfft_c*ist
          C_fft(i) = cmplx(WR(ic_recv), zero, kind(0d0))
        end do
!
        do m = 2, Nfft_c-1
          do j = 1, num
            j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
            ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            i = j + (m-1)*num + Nfft_c*ist
            C_fft(i)                                                   &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
          end do
        end do
!
        do j = 1, num
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          ic_rtp = j0_rtp + istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          i = j + (Nfft_c-1)*num + Nfft_c*ist
          C_fft(i) = cmplx(WR(ic_recv), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_FFTW_comp_from_recv
!
! ------------------------------------------------------------------
!
      end module copy_sph_FFTW_from_recv
