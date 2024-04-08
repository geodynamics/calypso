!>@file   set_comm_table_prt_FFTW.f90
!!@brief  module set_comm_table_prt_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTW
!!
!!@verbatim
!!      subroutine set_comm_item_prt_4_FFTW                             &
!!     &        (nnod_rtp, istep_rtp, nnod_rt, ntot_sr_rtp, irev_sr_rtp,&
!!     &         Nfft_c, aNfft, comm_sph_FFTW)
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!!
!!      subroutine copy_prt_field_FFTW_to_send                          &
!!     &         (nnod_rtp, irev_sr_rtp, nnod_rt,                       &
!!     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!!      subroutine copy_prt_comp_FFTW_to_send                           &
!!     &         (nd, nnod_rtp, irev_sr_rtp, nnod_rt,                   &
!!     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!!
!!      subroutine copy_FFTW_field_from_recv                            &
!!     &         (nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,            &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!      subroutine copy_FFTW_field_from_recv_smp                        &
!!     &         (nnod_rtp, istep_rtp, irt_rtp_smp_stack, irev_sr_rtp,  &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!
!!      subroutine copy_FFTW_comp_from_recv                             &
!!     &         (nd, nnod_rtp, istep_rtp, nnod_rt, irev_sr_rtp,        &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!      subroutine copy_FFTW_comp_from_recv_smp                         &
!!     &         (nd, nnod_rtp, irev_sr_rtp, irt_rtp_smp_stack,         &
!!     &          ncomp_bwd, n_WR, WR, Nfft_c, C_fft)
!!@endverbatim
!!
      module set_comm_table_prt_FFTW
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
      subroutine set_comm_item_prt_4_FFTW                               &
     &        (nnod_rtp, istep_rtp, nnod_rt, ntot_sr_rtp, irev_sr_rtp,  &
     &         Nfft_c, aNfft, comm_sph_FFTW)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
!
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!
      integer(kind = kint) ::  m, j, j0_rtp, ist_c
      integer(kind = kint) ::  ic_rtp, is_rtp, ic_send, is_send
!      integer(kind = kint) ::  i
!
!
!$omp  parallel do                                                      &
!$omp& private(j,m,j0_rtp,ist_c,ic_rtp,is_rtp,ic_send,is_send)
      do j = 1, nnod_rt
        ist_c = 1 + Nfft_c * (j-1)
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        ic_send = irev_sr_rtp(j0_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFTW%kl_fftw(ic_send) = j
          comm_sph_FFTW%m_fftw(ic_send) =  1
          comm_sph_FFTW%cnrm_sr_rtp(ic_send) = aNfft * ru
        end if
!        WS(ic_send) = aNfft * real(C_fft(ist_c))
!
        do m = 2, Nfft_c-1
!          ist_c = m + Nfft_c * (j-1)
          ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
          is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
          ic_send = irev_sr_rtp(ic_rtp)
          is_send = irev_sr_rtp(is_rtp)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%kl_fftw(ic_send) = j
            comm_sph_FFTW%m_fftw(ic_send) =  m
            comm_sph_FFTW%cnrm_sr_rtp(ic_send) = two * aNfft * ru
          end if
!          WS(ic_send) = two*aNfft * real(C_fft(ist_c))
!
          if(is_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%kl_fftw(is_send) = j
            comm_sph_FFTW%m_fftw(is_send) =  m
            comm_sph_FFTW%cnrm_sr_rtp(is_send) = two * aNfft * iu
          end if
!          WS(is_send) = two*aNfft * real(C_fft(ist_c)*iu)
        end do 
!        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j0_rtp + istep_rtp(3)
        ic_send = irev_sr_rtp(ic_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFTW%kl_fftw(ic_send) = j
          comm_sph_FFTW%m_fftw(ic_send) =  m
          comm_sph_FFTW%cnrm_sr_rtp(ic_send) = two * aNfft * ru
        end if
!        WS(ic_send) = two*aNfft * real(C_fft(ist_c))
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_prt_4_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_prt_field_FFTW_to_send                            &
     &         (nnod_rtp, irev_sr_rtp, nnod_rt,                         &
     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(nnod_rt*Nfft_c,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: j, m, ist_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do  private(j,m,ist_c,ic_rtp,is_rtp,ic_send,is_send)
      do j = 1, nnod_rt
        ist_c = 1 + Nfft_c * (j-1)
        ic_send = (irev_sr_rtp(j) - 1) * ncomp_fwd
        WS(ic_send+1:is_send+ncomp_fwd)                                 &
     &          = aNfft * real(C_fft(ist_c,1:ncomp_fwd))
        do m = 2, Nfft_c-1
          ist_c = m + Nfft_c * (j-1)
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send+1:is_send+ncomp_fwd)                               &
     &          = two*aNfft * real(C_fft(ist_c,1:ncomp_fwd))
          WS(is_send+1:is_send+ncomp_fwd)                               &
     &          = two*aNfft * real(C_fft(ist_c,1:ncomp_fwd)*iu)
        end do 
        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j + nnod_rt
        ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &          = two*aNfft * real(C_fft(ist_c,1:ncomp_fwd))
      end do
!$omp end parallel do
!
      end subroutine copy_prt_field_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_comp_FFTW_to_send                             &
     &         (nd, nnod_rtp, irev_sr_rtp, nnod_rt,                     &
     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(nnod_rt*Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: j, m, ist_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do  private(j,m,ist_c,ic_rtp,is_rtp,ic_send,is_send)
      do j = 1, nnod_rt
        ist_c = 1 + Nfft_c * (j-1)
        ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
        WS(ic_send) = aNfft * real(C_fft(ist_c))
        do m = 2, Nfft_c-1
          ist_c = m + Nfft_c * (j-1)
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send) = two*aNfft * real(C_fft(ist_c))
          WS(is_send) = two*aNfft * real(C_fft(ist_c)*iu)
        end do 
        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j + nnod_rt
        ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        WS(ic_send) = two*aNfft * real(C_fft(ist_c))
      end do
!$omp end parallel do
!
      end subroutine copy_prt_comp_FFTW_to_send
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_FFTW_field_from_recv                              &
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
     &              :: C_fft(nnod_rt*Nfft_c,ncomp_bwd)
!
      integer(kind = kint) :: nd, j, j0_rtp, m, ist_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp  parallel do                                                      &
!$omp& private(nd,j,m,j0_rtp,ist_c,ic_rtp,is_rtp,ic_recv,is_recv)
      do j = 1, nnod_rt
        j0_rtp = 1 + (j-1) * istep_rtp(1)
        do nd = 1, ncomp_bwd
          ist_c = 1 + Nfft_c * (j-1)
          ic_recv = nd + (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
          C_fft(ist_c,nd) = cmplx(WR(ic_recv), zero, kind(0d0))
          do m = 2, Nfft_c-1
            ist_c = m + Nfft_c * (j-1)
            ic_rtp = j0_rtp + (2*m-2) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m-1) * istep_rtp(3)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            C_fft(ist_c,nd)                                             &
     &              = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
          end do
          ist_c = Nfft_c + Nfft_c * (j-1)
          ic_rtp = j0_rtp + istep_rtp(3)
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          C_fft(ist_c,nd) = half * cmplx(WR(ic_recv), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTW_field_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTW_field_from_recv_smp                          &
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
     &        = half*cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),             &
     &                    -WR(is_recv+1:is_recv+ncomp_bwd),kind(0d0))
          end do
          ic_rtp = j0_rtp + istep_rtp(3)
          ic_recv = ncomp_bwd * (irev_sr_rtp(ic_rtp) - 1)
          ms = ((j-1) + (Nfft_c-1)*num + ist*Nfft_c) * ncomp_bwd
          C_fft(ms+1:ms+ncomp_bwd)                                      &
     &        = half * cmplx(WR(ic_recv+1:ic_recv+ncomp_bwd),           &
     &                       zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTW_field_from_recv_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_FFTW_comp_from_recv                               &
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
        C_fft(ist_c) = half * cmplx(WR(ic_recv), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine copy_FFTW_comp_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTW_comp_from_recv_smp                           &
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
      real(kind=kreal), intent(in):: WR(n_WR)
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
          C_fft(i) = half * cmplx(WR(ic_recv), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTW_comp_from_recv_smp
!
! ------------------------------------------------------------------
!
      end module set_comm_table_prt_FFTW
