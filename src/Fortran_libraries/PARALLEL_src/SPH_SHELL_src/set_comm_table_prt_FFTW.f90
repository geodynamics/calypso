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
!!     &         Nfft_c, comm_sph_FFTW)
!!        integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
!!        integer(kind = kint), intent(in) :: istep_rtp(3)
!!        integer(kind = kint), intent(in) :: ntot_sr_rtp
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        integer(kind = kint), intent(in) :: Nfft_c
!!        type(comm_tbl_from_FFTW), intent(inout) :: comm_sph_FFTW
!!
!!      subroutine pin_FFTW_fields_to_send                              &
!!     &         (nnod_rtp, irev_sr_rtp, nnod_rt,                       &
!!     &          ncomp_fwd, Nfft_c, C_fft, n_WS, WS)
!!        complex(kind = fftw_complex), intent(in)                      &
!!     &              :: C_fft(nnod_rt*Nfft_c,ncomp_fwd)
!!      subroutine copy_prt_comp_FFTW_to_send                           &
!!     &         (nd, nnod_rtp, irev_sr_rtp, nnod_rt,                   &
!!     &          ncomp_fwd, Nfft_c, C_fft, n_WS, WS)
!!        integer(kind = kint), intent(in) :: nd
!!        integer(kind = kint), intent(in) :: nnod_rtp
!!        integer(kind = kint), intent(in) :: nnod_rt
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        integer(kind = kint), intent(in) :: Nfft_c
!!        complex(kind = fftw_complex), intent(in)                      &
!!     &              :: C_fft(nnod_rt*Nfft_c)
!!        integer(kind = kint), intent(in) :: n_WS
!!        real(kind = kreal), intent(inout) :: WS(n_WS)
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
     &         Nfft_c, comm_sph_FFTW)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rt
      integer(kind = kint), intent(in) :: istep_rtp(3)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
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
          comm_sph_FFTW%cnrm_sr_rtp(ic_send) = ru
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
            comm_sph_FFTW%cnrm_sr_rtp(ic_send) = two * ru
          end if
!          WS(ic_send) = two*aNfft * real(C_fft(ist_c))
!
          if(is_send .le. ntot_sr_rtp) then
            comm_sph_FFTW%kl_fftw(is_send) = j
            comm_sph_FFTW%m_fftw(is_send) =  m
            comm_sph_FFTW%cnrm_sr_rtp(is_send) = two * iu
          end if
!          WS(is_send) = two*aNfft * real(C_fft(ist_c)*iu)
        end do 
!        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j0_rtp + istep_rtp(3)
        ic_send = irev_sr_rtp(ic_rtp)
        if(ic_send .le. ntot_sr_rtp) then
          comm_sph_FFTW%kl_fftw(ic_send) = j
          comm_sph_FFTW%m_fftw(ic_send) =  Nfft_c
          comm_sph_FFTW%cnrm_sr_rtp(ic_send) = ru
        end if
!        WS(ic_send) = aNfft * real(C_fft(ist_c))
      end do
!$omp end parallel do
!
      end subroutine set_comm_item_prt_4_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pin_FFTW_fields_to_send                                &
     &         (nnod_rtp, irev_sr_rtp, nnod_rt,                         &
     &          ncomp_fwd, Nfft_c, C_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(nnod_rt*Nfft_c,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
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
     &          = real(C_fft(ist_c,1:ncomp_fwd))
        do m = 2, Nfft_c-1
          ist_c = m + Nfft_c * (j-1)
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send+1:is_send+ncomp_fwd)                               &
     &          = two * real(C_fft(ist_c,1:ncomp_fwd))
          WS(is_send+1:is_send+ncomp_fwd)                               &
     &          = two * real(C_fft(ist_c,1:ncomp_fwd)*iu)
        end do 
        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j + nnod_rt
        ic_send = (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        WS(ic_send+1:ic_send+ncomp_fwd)                                 &
     &          = real(C_fft(ist_c,1:ncomp_fwd))
      end do
!$omp end parallel do
!
      end subroutine pin_FFTW_fields_to_send
!
! ------------------------------------------------------------------
!
      subroutine copy_prt_comp_FFTW_to_send                             &
     &         (nd, nnod_rtp, irev_sr_rtp, nnod_rt,                     &
     &          ncomp_fwd, Nfft_c, C_fft, n_WS, WS)
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
      complex(kind = fftw_complex), intent(in)                          &
     &              :: C_fft(nnod_rt*Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      real(kind = kreal), intent(inout) :: WS(n_WS)
!
      integer(kind = kint) :: j, m, ist_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do  private(j,m,ist_c,ic_rtp,is_rtp,ic_send,is_send)
      do j = 1, nnod_rt
        ist_c = 1 + Nfft_c * (j-1)
        ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
        WS(ic_send) = real(C_fft(ist_c))
        do m = 2, Nfft_c-1
          ist_c = m + Nfft_c * (j-1)
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send) = two * real(C_fft(ist_c))
          WS(is_send) = two * real(C_fft(ist_c)*iu)
        end do 
        ist_c = Nfft_c + Nfft_c * (j-1)
        ic_rtp = j + nnod_rt
        ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        WS(ic_send) = real(C_fft(ist_c))
      end do
!$omp end parallel do
!
      end subroutine copy_prt_comp_FFTW_to_send
!
! ------------------------------------------------------------------
!
      end module set_comm_table_prt_FFTW
