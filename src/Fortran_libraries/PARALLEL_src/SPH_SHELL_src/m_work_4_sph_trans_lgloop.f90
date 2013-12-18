!>@file   m_work_4_sph_trans_lgloop.f90
!!@brief  module m_work_4_sph_trans_lgloop
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform for longer loop
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans_long(ncomp_sph_trans)
!!      subroutine deallocate_work_sph_trans_long
!!
!!      subroutine clear_b_trans_vector_long(nfld)
!!      subroutine clear_b_trans_scalar_long(nfld)
!!
!!      subroutine clear_f_trans_vector_long(nfld)
!!      subroutine clear_f_trans_scalar_long(nfld)
!!
!!      subroutine check_vr_rtm_long(my_rank, nfld)
!!      subroutine check_sp_rlm_long(my_rank, nfld)
!!@endverbatim
!!
      module m_work_4_sph_trans_lgloop
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>      Spectr data for Legendre transform  @f$ f(r,l,m) @f$ 
!>@n      Order: sp_rlm_long(i_comp,i_fld,j_rlm,k_rtm)
!>@n      size: sp_rlm_long(3*nfld*nidx_rlm(2)*nidx_rtm(1))
      real(kind = kreal), allocatable :: sp_rlm_long(:)
!>      field data for Legendre transform  @f$ f(r,\theta,m) @f$ 
!!@n     Order: vr_rtm_long(i_comp,i_fld,l_rtm,k_rtm,m_rtm)
!!@n     size:  vr_rtm_long(3*nfld*nidx_rtm(2)*nidx_rtm(1)*nidx_rtm(3))
      real(kind = kreal), allocatable :: vr_rtm_long(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans_long(ncomp_sph_trans)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: ncomp_sph_trans
!
!
      allocate(sp_rlm_long(ncomp_sph_trans*nnod_rlm))
      allocate(vr_rtm_long(ncomp_sph_trans*nnod_rtm))
!
      sp_rlm_long = 0.0d0
      vr_rtm_long = 0.0d0
!
      end subroutine allocate_work_sph_trans_long
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans_long
!
!
      deallocate(sp_rlm_long, vr_rtm_long)
!
      end subroutine deallocate_work_sph_trans_long
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine clear_b_trans_vector_long(nfld)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nfld
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rtm*nfld
        vr_rtm_long(3*inod-2) = zero
        vr_rtm_long(3*inod-1) = zero
        vr_rtm_long(3*inod  ) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_scalar_long(nfld)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nfld
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rtm*nfld
        vr_rtm_long(inod) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_scalar_long
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_vector_long(nfld)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nfld
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rlm*nfld
        sp_rlm_long(3*inod-2) = zero
        sp_rlm_long(3*inod-1) = zero
        sp_rlm_long(3*inod  ) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_scalar_long(nfld)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nfld
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rlm*nfld
        sp_rlm_long(inod) = zero
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      subroutine check_vr_rtm_long(my_rank, nfld)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nfld
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'vr_rtm_long', nfld
      do inod = 1, nnod_rtm
        ist = (inod-1) * nfld + 1
        ied = (inod-1) * nfld + nfld
        write(50+my_rank,'(4i10,1p200e20.12)') inod,                    &
     &        idx_global_rtm(inod,1:3), vr_rtm_long(ist:ied)
      end do
!
      end subroutine check_vr_rtm_long
!
! ----------------------------------------------------------------------
!
      subroutine check_sp_rlm_long(my_rank, nfld)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nfld
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'sp_rlm_long', nfld
      do inod = 1, nnod_rlm
        ist = (inod-1) * nfld + 1
        ied = (inod-1) * nfld + nfld
        write(50+my_rank,'(3i10,1p200e20.12)') inod,                    &
     &        idx_global_rlm(inod,1:2), sp_rlm_long(ist:ied)
      end do
!
      end subroutine check_sp_rlm_long
!
! ----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_lgloop
