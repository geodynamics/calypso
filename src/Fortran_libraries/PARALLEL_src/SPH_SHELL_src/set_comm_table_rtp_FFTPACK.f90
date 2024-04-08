!>@file   set_comm_table_rtp_FFTPACK.f90
!!@brief  module set_comm_table_rtp_FFTPACK
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  communication table from FFTPACK5
!!
!!@verbatim
!!      subroutine set_comm_item_rtp_4_FFTPACK                          &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                &
!!     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!!
!!      subroutine copy_FFTPACK_field_from_recv(nnod_rtp, nidx_rtp,     &
!!     &          istep_rtp, irt_rtp_smp_stack, ncomp_bwd, irev_sr_rtp, &
!!     &          n_WR, WR, X_FFT)
!!      subroutine copy_FFTPACK_comp_from_recv(nd, nnod_rtp, nidx_rtp,  &
!!     &          istep_rtp, irt_rtp_smp_stack, ncomp_bwd, irev_sr_rtp, &
!!     &          n_WR, WR, X_FFT)
!!        type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!!@endverbatim
!!
      module set_comm_table_rtp_FFTPACK
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_sph_comm_table_from_FFT
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine set_comm_item_rtp_4_FFTPACK                            &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                  &
     &          ntot_sr_rtp, irev_sr_rtp, comm_sph_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ntot_sr_rtp
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      type(comm_tbl_from_FFT), intent(inout) :: comm_sph_FFT
!
      integer(kind = kint) :: ip, m, j, ist, num
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(m,j,ist,num,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        do j = 1, num
            ic_rtp = j+ist
            ic_send = irev_sr_rtp(ic_rtp)
            if(ic_send .le. ntot_sr_rtp) then
              comm_sph_FFT%ip_smp_fft(ic_send) = ip
              comm_sph_FFT%kl_fft(ic_send) = j
              comm_sph_FFT%m_fft(ic_send) =  1
              comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
            end if
!
            is_rtp = j+ist + nidx_rtp(1)*nidx_rtp(2)
            is_send = irev_sr_rtp(is_rtp)
            if(is_send .le. ntot_sr_rtp) then
              comm_sph_FFT%ip_smp_fft(is_send) = ip
              comm_sph_FFT%kl_fft(is_send) = j
              comm_sph_FFT%m_fft(is_send) =  nidx_rtp(3)
              comm_sph_FFT%rnorm_sr_rtp(is_send) = one
            end if
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
!              inod_c = j + (2*m-1)*num
              ic_rtp = j+ist + (2*m  ) * irt_rtp_smp_stack(np_smp)
              ic_send = irev_sr_rtp(ic_rtp)
              if(ic_send .le. ntot_sr_rtp) then
                comm_sph_FFT%ip_smp_fft(ic_send) = ip
                comm_sph_FFT%kl_fft(ic_send) = j
                comm_sph_FFT%m_fft(ic_send) = 2*m
                comm_sph_FFT%rnorm_sr_rtp(ic_send) = one
              end if
!
!              inod_s = j + (2*m  )*num
              is_rtp = j+ist + (2*m+1) * irt_rtp_smp_stack(np_smp)
              is_send = irev_sr_rtp(is_rtp)
              if(is_send .le. ntot_sr_rtp) then
                comm_sph_FFT%ip_smp_fft(is_send) = ip
                comm_sph_FFT%kl_fft(is_send) = j
                comm_sph_FFT%m_fft(is_send) = 2*m+1
                comm_sph_FFT%rnorm_sr_rtp(is_send) = one
              end if
            end do
          end do
        end do
!$omp end parallel do
!
      end subroutine set_comm_item_rtp_4_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_field_from_recv(nnod_rtp, nidx_rtp,       &
     &          istep_rtp, irt_rtp_smp_stack, ncomp_bwd, irev_sr_rtp,   &
     &          n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3), istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(ncomp_bwd*nnod_rtp)
!
      integer(kind = kint) :: m, j, ip, ist, num, j0_rtp
      integer(kind = kint) :: inod_s, inod_c, ist_fft
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(m,j,ist,num,inod_s,inod_c,j0_rtp,             &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
!
!   normalization
        do j = 1, num
          inod_c = ((j-1) + ist_fft) * ncomp_bwd
          inod_s = ((j-1) + (nidx_rtp(3)-1)*num + ist_fft) * ncomp_bwd
!
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          is_rtp = j0_rtp + istep_rtp(3)
          ic_recv = (irev_sr_rtp(j0_rtp) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_FFT(inod_c+1:inod_c+ncomp_bwd)                              &
     &            = WR(ic_recv+1:ic_recv+ncomp_bwd)
          X_FFT(inod_s+1:inod_s+ncomp_bwd)                              &
     &            = WR(is_recv+1:is_recv+ncomp_bwd)
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
            inod_c = ((j-1) + (2*m-1)*num + ist_fft) * ncomp_bwd
            inod_s = ((j-1) + (2*m  )*num + ist_fft) * ncomp_bwd
!!
            j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
            ic_rtp = j0_rtp + (2*m  ) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m+1) * istep_rtp(3)
            ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
      !
            X_FFT(inod_c+1:inod_c+ncomp_bwd)                            &
     &              = WR(ic_recv+1:ic_recv+ncomp_bwd)
            X_FFT(inod_s+1:inod_s+ncomp_bwd)                            &
     &              = WR(is_recv+1:is_recv+ncomp_bwd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTPACK_field_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_FFTPACK_comp_from_recv(nd, nnod_rtp, nidx_rtp,    &
     &          istep_rtp, irt_rtp_smp_stack, ncomp_bwd, irev_sr_rtp,   &
     &          n_WR, WR, X_FFT)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3), istep_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rtp)
!
      integer(kind = kint) :: m, j, ip, ist, num
      integer(kind = kint) :: inod_s, inod_c, ist_fft, j0_rtp
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!$omp parallel do private(m,j,ist,num,j0_rtp,inod_s,inod_c,             &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ist_fft = irt_rtp_smp_stack(ip-1) * nidx_rtp(3)
!
!   normalization
        do j = 1, num
          inod_c = j + ist_fft
          inod_s = j + (nidx_rtp(3)-1)*num + ist_fft
!
          j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
          ic_rtp = j0_rtp
          is_rtp = j0_rtp + istep_rtp(3)
          ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          X_FFT(inod_c) = WR(ic_recv+nd)
          X_FFT(inod_s) = WR(is_recv+nd)
        end do
        do m = 1, (nidx_rtp(3)+1)/2 - 1
          do j = 1, num
            inod_c = j + (2*m-1)*num + ist_fft
            inod_s = j + (2*m  )*num + ist_fft
!
            j0_rtp = 1 + (j+ist-1) * istep_rtp(1)
            ic_rtp = j0_rtp + (2*m  ) * istep_rtp(3)
            is_rtp = j0_rtp + (2*m+1) * istep_rtp(3)
            ic_recv = (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
      !
            X_FFT(inod_c) = WR(ic_recv+nd)
            X_FFT(inod_s) = WR(is_recv+nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_FFTPACK_comp_from_recv
!
! ------------------------------------------------------------------
!
      end module set_comm_table_rtp_FFTPACK
