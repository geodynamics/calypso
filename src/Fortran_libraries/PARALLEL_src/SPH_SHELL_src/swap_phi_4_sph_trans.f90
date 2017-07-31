!>@file   swap_phi_4_sph_trans.f90
!!@brief  module swap_phi_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2014
!
!>@brief  Swap array for phi componene
!!
!!@verbatim
!!      subroutine swap_phi_from_trans(numdir, nnod_rtp, nidx_rtp, d_sph)
!!      subroutine swap_phi_to_trans(numdir, nnod_rtp, nidx_rtp, v_prt)
!!@endverbatim
!
      module swap_phi_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
! 
      real(kind = kreal), allocatable, private :: v_tmp(:)
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine swap_phi_from_trans(numdir, nnod_rtp, nidx_rtp, d_sph)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir, nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(inout) :: d_sph(nnod_rtp,numdir)
!
      integer(kind = kint) :: i_mkl, i_klm, kr_lt, mphi, nd
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
!
      allocate(v_tmp(nnod_rtp))
!
      do nd = 1, numdir
!$omp parallel workshare
        v_tmp(1:nnod_rtp) = d_sph(1:nnod_rtp,nd)
!$omp end parallel workshare
!
!$omp parallel do private(i_mkl,i_klm,mphi,kr_lt)
        do mphi = 1, nidx_rtp(3)
          do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            i_mkl = mphi + (kr_lt-1)*nidx_rtp(3)
            d_sph(i_klm,nd) = v_tmp(i_mkl)
          end do
        end do
!$omp end parallel do
      end do
!
      deallocate(v_tmp)
!
      end subroutine swap_phi_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine swap_phi_to_trans(numdir, nnod_rtp, nidx_rtp, v_prt)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: numdir, nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
!
      real(kind = kreal), intent(inout) :: v_prt(nnod_rtp,numdir)
!
      integer(kind = kint) :: i_mkl, i_klm, kr_lt, mphi, nd
!
!
      if(iflag_FFT .ne. iflag_FFTW) return
!
      allocate(v_tmp(nnod_rtp))
!
      do nd = 1, numdir
!$omp parallel workshare
        v_tmp(1:nnod_rtp) = v_prt(1:nnod_rtp,nd)
!$omp end parallel workshare
!
!$omp parallel do private(i_mkl,i_klm,mphi,kr_lt)
        do kr_lt = 1, nidx_rtp(1)*nidx_rtp(2)
          do mphi = 1, nidx_rtp(3)
            i_klm = kr_lt + (mphi-1)*nidx_rtp(1)*nidx_rtp(2)
            i_mkl = mphi + (kr_lt-1)*nidx_rtp(3)
            v_prt(i_mkl,nd) = v_tmp(i_klm)
          end do
        end do
!$omp end parallel do
      end do
!
      deallocate(v_tmp)
!
      end subroutine swap_phi_to_trans
!
!-----------------------------------------------------------------------
!
      end module swap_phi_4_sph_trans
