!>@file   copy_field_4_sph_trans.f90
!!@brief  module copy_field_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2012
!
!>@brief  Copy spherical harmonics data
!!
!!@verbatim
!!      subroutine copy_scalar_from_trans_smp                           &
!!     &         (nnod_rtp, m_folding, nnod, v_rtp, d_sph)
!!      subroutine copy_vector_from_trans_smp                           &
!!     &         (nnod_rtp, m_folding, nnod, v_rtp, d_sph)
!!      subroutine copy_tensor_from_trans_smp                           &
!!     &         (nnod_rtp, m_folding, nnod, v_rtp, d_sph)
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_trans_smp                             &
     &         (nnod_rtp, m_folding, nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod, m_folding
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp)
      real(kind = kreal), intent(inout) :: d_sph(nnod)
!
      integer(kind = kint) :: m_sym, mst
!
!
      do m_sym = 1, m_folding
        mst = (m_sym-1) * nnod_rtp
!$omp workshare
        d_sph(1+mst:nnod_rtp+mst) = v_rtp(1:nnod_rtp)
!$omp end workshare
      end do
!
      end subroutine copy_scalar_from_trans_smp
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_trans_smp                             &
     &         (nnod_rtp, m_folding, nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod, m_folding
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp,3)
      real(kind = kreal), intent(inout) :: d_sph(nnod,3)
!
      integer(kind = kint) :: m_sym, mst
!
!
      do m_sym = 1, m_folding
        mst = (m_sym-1) * nnod_rtp
!$omp workshare
        d_sph(1+mst:nnod_rtp+mst,1) = v_rtp(1:nnod_rtp,1)
        d_sph(1+mst:nnod_rtp+mst,2) = v_rtp(1:nnod_rtp,2)
        d_sph(1+mst:nnod_rtp+mst,3) = v_rtp(1:nnod_rtp,3)
!$omp end workshare
      end do
!
      end subroutine copy_vector_from_trans_smp
!
!-----------------------------------------------------------------------
!
      subroutine copy_tensor_from_trans_smp                             &
     &         (nnod_rtp, m_folding, nnod, v_rtp, d_sph)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod, m_folding
      real(kind = kreal), intent(in) :: v_rtp(nnod_rtp,6)
      real(kind = kreal), intent(inout) :: d_sph(nnod,6)
!
      integer(kind = kint) :: m_sym, mst
!
!
      do m_sym = 1, m_folding
        mst = (m_sym-1) * nnod_rtp
!$omp workshare
        d_sph(1+mst:nnod_rtp+mst,1) = v_rtp(1:nnod_rtp,1)
        d_sph(1+mst:nnod_rtp+mst,2) = v_rtp(1:nnod_rtp,2)
        d_sph(1+mst:nnod_rtp+mst,3) = v_rtp(1:nnod_rtp,3)
        d_sph(1+mst:nnod_rtp+mst,4) = v_rtp(1:nnod_rtp,4)
        d_sph(1+mst:nnod_rtp+mst,5) = v_rtp(1:nnod_rtp,5)
        d_sph(1+mst:nnod_rtp+mst,6) = v_rtp(1:nnod_rtp,6)
!$omp end workshare
      end do
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
