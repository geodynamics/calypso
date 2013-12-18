!>@file   legendre_transform_krin.f90
!!@brief  module legendre_transform_lgloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Legendre transforms
!!       (longest loop version)
!!
!!
!!@verbatim
!!    Backward transforms
!!      subroutine leg_bwd_trans_vector_long(ncomp, nvector)
!!      subroutine leg_bwd_trans_scalar_long(ncomp, nvector, nscalar)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!
!!    Forward transforms
!!      subroutine leg_fwd_trans_vector_long(ncomp, nvector)
!!      subroutine leg_fwd_trans_scalar_long(ncomp, nvector, nscalar)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
!
      module legendre_transform_lgloop
!
      use m_precision
      use m_work_4_sph_trans_lgloop
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_vector_long(ncomp, nvector)
!
      use legendre_bwd_trans_lgloop
      use copy_4_schmidt_trans_long
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call copy_b_trans_vector_long(ncomp, nvector)
      call clear_b_trans_vector_long(nvector)
!
      call legendre_b_trans_vector_long(nvector)
!
      call back_b_trans_vector_long(ncomp, nvector)
!
      end subroutine leg_bwd_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine leg_bwd_trans_scalar_long(ncomp, nvector, nscalar)
!
      use legendre_bwd_trans_lgloop
      use copy_4_schmidt_trans_long
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call copy_b_trans_scalar_long(ncomp, nvector, nscalar)
      call clear_b_trans_scalar_long(nscalar)
!
      call legendre_b_trans_scalar_long(nscalar)
!
      call back_b_trans_scalar_long(ncomp, nvector, nscalar)
!
      end subroutine leg_bwd_trans_scalar_long
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_vector_long(ncomp, nvector)
!
      use legendre_fwd_trans_lgloop
      use copy_4_schmidt_trans_long
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
!
      call copy_f_trans_vector_long(ncomp, nvector)
      call clear_f_trans_vector_long(nvector)
!
      call legendre_f_trans_vector_long(nvector)
!
      call back_f_trans_vector_long(ncomp, nvector)
!
      end subroutine leg_fwd_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine leg_fwd_trans_scalar_long(ncomp, nvector, nscalar)
!
      use legendre_fwd_trans_lgloop
      use copy_4_schmidt_trans_long
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
!
      call copy_f_trans_scalar_long(ncomp, nvector, nscalar)
      call clear_f_trans_scalar_long(nscalar)
!
      call legendre_f_trans_scalar_long(nscalar)
!
      call back_f_trans_scalar_long(ncomp, nvector, nscalar)
!
      end subroutine leg_fwd_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      end module legendre_transform_lgloop
