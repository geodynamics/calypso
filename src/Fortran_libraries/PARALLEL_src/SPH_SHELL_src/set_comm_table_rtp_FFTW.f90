!>@file   set_comm_table_rtp_FFTW.f90
!!@brief  module set_comm_table_rtp_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTW
!!
!!@verbatim
!!      subroutine set_comm_item_rtp_4_FFTW                             &
!!     &         (nnod_rtp, ntot_sr_rtp, irev_sr_rtp,                   &
!!     &          irt_rtp_smp_stack, Nfft_c, aNfft, comm_sph_FFTW)
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!!
!!      subroutine copy_rtp_field_FFTW_to_send                          &
!!     &         (nnod_rtp, irev_sr_rtp, irt_rtp_smp_stack,             &
!!     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!!      subroutine copy_rtp_comp_FFTW_to_send                           &
!!     &         (nd, nnod_rtp, irev_sr_rtp, irt_rtp_smp_stack,         &
!!     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!!@endverbatim
!!
      module set_comm_table_rtp_FFTW
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
      subroutine set_comm_item_rtp_4_FFTW                               &
     &         (nnod_rtp, ntot_sr_rtp, irev_sr_rtp,                     &
     &          irt_rtp_smp_stack, Nfft_c, aNfft, comm_sph_FFTW)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
!
      type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!
      integer(kind = kint) ::  ip, ist, num
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!      integer(kind = kint) ::  i
!
!
!$omp parallel do private(ip,ist,num,m,j,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
!          i = j + (1-1)*num + Nfft_c*ist
          ic_rtp = j+ist
          ic_send = irev_sr_rtp(j+ist)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%ip_smp_fftw(ic_send) = ip
            comm_sph_FFTW%kl_fftw(ic_send) = j
            comm_sph_FFTW%m_fftw(ic_send) =  1
            comm_sph_FFTW%cnrm_sr_rtp(ic_send) = aNfft * ru
          end if
!          WS(ic_send) = aNfft * real(C_fft(i))
        end do
!
        do m = 2, Nfft_c-1
          do j = 1, num
!            i = j + (m-1)*num + Nfft_c*ist
            ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
            ic_send = irev_sr_rtp(ic_rtp)
            if(ic_send .le. ntot_sr_rtp) then
              comm_sph_FFTW%ip_smp_fftw(ic_send) = ip
              comm_sph_FFTW%kl_fftw(ic_send) = j
              comm_sph_FFTW%m_fftw(ic_send) =  m
              comm_sph_FFTW%cnrm_sr_rtp(ic_send) = two * aNfft * ru
            end if
!            WS(ic_send) = two * aNfft * real(C_fft(i)*ru)
!
            is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
            is_send = irev_sr_rtp(is_rtp)
            if(is_send .le. ntot_sr_rtp) then
              comm_sph_FFTW%ip_smp_fftw(is_send) = ip
              comm_sph_FFTW%kl_fftw(is_send) = j
              comm_sph_FFTW%m_fftw(is_send) =  m
              comm_sph_FFTW%cnrm_sr_rtp(is_send) = two * aNfft *iu
            end if
!            WS(is_send) = two * aNfft * real(C_fft(i)*iu)
          end do 
        end do
!
        do j = 1, num
!          i = j + (Nfft_c-1)*num + Nfft_c*ist
          ic_rtp = j+ist + irt_rtp_smp_stack(np_smp)
          ic_send = irev_sr_rtp(ic_rtp)
          if(ic_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%ip_smp_fftw(ic_send) = ip
            comm_sph_FFTW%kl_fftw(ic_send) = j
            comm_sph_FFTW%m_fftw(ic_send) =  Nfft_c
            comm_sph_FFTW%cnrm_sr_rtp(ic_send) = two * aNfft * ru
          end if
!          WS(ic_send) = two * aNfft * real(C_fft(i)*ru)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_rtp_4_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_field_FFTW_to_send                            &
     &         (nnod_rtp, irev_sr_rtp, irt_rtp_smp_stack,               &
     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &            :: C_fft(irt_rtp_smp_stack(np_smp)*Nfft_c*ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  ip, ist, num, ms
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do                                                       &
!$omp&  private(j,ip,ist,ms,num,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist = irt_rtp_smp_stack(ip-1)
        do j = 1, num
          ic_send = ncomp_fwd * (irev_sr_rtp(j+ist) - 1)
          ms = ((j-1) + ist*Nfft_c) * ncomp_fwd
          WS(ic_send+1:ic_send+ncomp_fwd)                               &
     &          = aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
          do m = 2, Nfft_c-1
            ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_send = ncomp_fwd * (irev_sr_rtp(ic_rtp) - 1)
            is_send = ncomp_fwd * (irev_sr_rtp(is_rtp) - 1)
            ms = ((j-1) + (m-1)*num + ist*Nfft_c) * ncomp_fwd
            WS(ic_send+1:ic_send+ncomp_fwd)                             &
     &          = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
            WS(is_send+1:is_send+ncomp_fwd)                             &
     &          = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd)*iu)
          end do 
          ic_rtp = j+ist + irt_rtp_smp_stack(np_smp)
          ic_send = ncomp_fwd * (irev_sr_rtp(ic_rtp)-1)
          ms = ((j-1) + (Nfft_c-1)*num + ist*Nfft_c) * ncomp_fwd
          WS(ic_send+1:ic_send+ncomp_fwd)                               &
     &        = two*aNfft * real(C_fft(ms+1:ms+ncomp_fwd))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_field_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_comp_FFTW_to_send                             &
     &         (nd, nnod_rtp, irev_sr_rtp, irt_rtp_smp_stack,           &
     &          ncomp_fwd, Nfft_c, aNfft, C_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(irt_rtp_smp_stack(np_smp)*Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  ip, ist, num, i
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(ip,ist,num,i,m,j,ic_rtp,is_rtp,               &
!$omp&                    ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          ic_send = nd + (irev_sr_rtp(j+ist) - 1) * ncomp_fwd
          i = j + (1-1)*num + Nfft_c*ist
          WS(ic_send) = aNfft * real(C_fft(i))
        end do
!
        do m = 2, Nfft_c-1
          do j = 1, num
            ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            i = j + (m-1)*num + Nfft_c*ist
            WS(ic_send) = two * aNfft * real(C_fft(i))
            WS(is_send) = two * aNfft * real(C_fft(i)*iu)
          end do 
        end do
!
        do j = 1, num
          ic_rtp = j+ist + irt_rtp_smp_stack(np_smp)
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          i = j + (Nfft_c-1)*num + Nfft_c*ist
          WS(ic_send) = two * aNfft * real(C_fft(i))
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_comp_FFTW_to_send
!
! ------------------------------------------------------------------
!
      end module set_comm_table_rtp_FFTW
