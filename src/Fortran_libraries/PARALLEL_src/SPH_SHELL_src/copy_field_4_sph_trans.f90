!>@file   copy_field_4_sph_trans.f90
!!@brief  module copy_field_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical harmonics data
!!
!!@verbatim
!!      subroutine copy_scalar_from_trans_smp(nnod_rtp, m_folding,      &
!!     &         inod_rtp_smp_stack, nnod, v_rtp, d_sph)
!!      subroutine copy_vector_from_trans_smp(nnod_rtp, m_folding,      &
!!     &         inod_rtp_smp_stack, nnod, v_rtp, d_sph)
!!      subroutine copy_tensor_from_trans_smp(nnod_rtp, m_folding,      &
!!     &         inod_rtp_smp_stack, nnod, v_rtp, d_sph)
!!
!!      subroutine copy_scalar_to_trans_smp                             &
!!     &         (nnod_rtp, nnod, d_sph, v_rtp)
!!      subroutine copy_vector_to_trans_smp                             &
!!     &         (nnod_rtp, nnod, d_sph, v_rtp)
!!      subroutine copy_tensor_to_trans_smp                             &
!!     &         (nnod_rtp, nnod, d_sph, v_rtp)
!!@endverbatim
!
      module copy_field_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
! 
! -------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_trans_smp(nnod_rtp, m_folding,        &
     &         inod_rtp_smp_stack, nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod, m_folding
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp)
      real(kind = kreal), intent(inout) :: d_sph(nnod)
!
      integer(kind = kint) :: ist, ied, m_sym, mst, ip
!
!
!$omp do private(ist,ied,m_sym,mst)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do m_sym = 1, m_folding
          mst = (m_sym-1) * inod_rtp_smp_stack(np_smp)
          d_sph(ist+mst:ied+mst) = v_rtp(ist:ied)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_scalar_from_trans_smp
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_trans_smp(nnod_rtp, m_folding,        &
     &         inod_rtp_smp_stack, nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod, m_folding
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp,3)
      real(kind = kreal), intent(inout) :: d_sph(nnod,3)
!
      integer(kind = kint) :: ist, ied, m_sym, mst, ip
!
!
!$omp do private(ist,ied,m_sym,mst)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do m_sym = 1, m_folding
          mst = (m_sym-1) * inod_rtp_smp_stack(np_smp)
          d_sph(ist+mst:ied+mst,1) = v_rtp(ist:ied,1)
          d_sph(ist+mst:ied+mst,2) = v_rtp(ist:ied,2)
          d_sph(ist+mst:ied+mst,3) = v_rtp(ist:ied,3)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_vector_from_trans_smp
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_trans_smp(nnod_rtp, m_folding,        &
     &         inod_rtp_smp_stack, nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod, m_folding
      integer(kind = kint), intent(in) :: inod_rtp_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp,6)
      real(kind = kreal), intent(inout) :: d_sph(nnod,6)
!
      integer(kind = kint) :: ist, ied, m_sym, mst, ip
!
!
!$omp do private(ist,ied,m_sym,mst)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do m_sym = 1, m_folding
          mst = (m_sym-1) * inod_rtp_smp_stack(np_smp)
          d_sph(ist+mst:ied+mst,1) = v_rtp(ist:ied,1)
          d_sph(ist+mst:ied+mst,2) = v_rtp(ist:ied,2)
          d_sph(ist+mst:ied+mst,3) = v_rtp(ist:ied,3)
          d_sph(ist+mst:ied+mst,4) = v_rtp(ist:ied,4)
          d_sph(ist+mst:ied+mst,5) = v_rtp(ist:ied,5)
          d_sph(ist+mst:ied+mst,6) = v_rtp(ist:ied,6)
        end do
      end do
!$omp end do nowait
!
      end subroutine copy_tensor_from_trans_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_to_trans_smp                               &
     &         (nnod_rtp, nnod, d_sph, v_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod
      real(kind = kreal), intent(in) :: d_sph(nnod)
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp)
!
!
!$omp workshare
      v_rtp(1:nnod_rtp) = d_sph(1:nnod_rtp)
!$omp end workshare nowait
!
      end subroutine copy_scalar_to_trans_smp
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_to_trans_smp                               &
     &         (nnod_rtp, nnod, d_sph, v_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod
      real(kind = kreal), intent(in) :: d_sph(nnod,3)
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp,3)
!
!
!$omp workshare
      v_rtp(1:nnod_rtp,1) = d_sph(1:nnod_rtp,1)
      v_rtp(1:nnod_rtp,2) = d_sph(1:nnod_rtp,2)
      v_rtp(1:nnod_rtp,3) = d_sph(1:nnod_rtp,3)
!$omp end workshare nowait
!
      end subroutine copy_vector_to_trans_smp
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_to_trans_smp                               &
     &         (nnod_rtp, nnod, d_sph, v_rtp)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod
      real(kind = kreal), intent(in) :: d_sph(nnod,6)
      real(kind = kreal), intent(inout) :: v_rtp(nnod_rtp,6)
!
!
!$omp workshare
      v_rtp(1:nnod_rtp,1) = d_sph(1:nnod_rtp,1)
      v_rtp(1:nnod_rtp,2) = d_sph(1:nnod_rtp,2)
      v_rtp(1:nnod_rtp,3) = d_sph(1:nnod_rtp,3)
      v_rtp(1:nnod_rtp,4) = d_sph(1:nnod_rtp,4)
      v_rtp(1:nnod_rtp,5) = d_sph(1:nnod_rtp,5)
      v_rtp(1:nnod_rtp,6) = d_sph(1:nnod_rtp,6)
!$omp end workshare nowait
!
      end subroutine copy_tensor_to_trans_smp
!
!-----------------------------------------------------------------------
!
      end module copy_field_4_sph_trans
