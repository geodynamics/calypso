!>@file   m_work_4_sph_trans_spin.f90
!!@brief  module m_work_4_sph_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans(ncomp)
!!      subroutine deallocate_work_sph_trans
!!
!!    Data for single vector field
!!      radial component:      vr_rtp(3*i_rtp-2)
!!      elevetional component: vr_rtp(3*i_rtp-1)
!!      azimuthal component:   vr_rtp(2*i_rtp  )
!!
!!    Data for single vector spectrum
!!      Poloidal component:          sp_rj(3*i_rj-2)
!!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!!      Toroidal component:          sp_rj(3*i_rj  )
!!
!!    Data for single scalar
!!      field: vr_rtp(i_rtp)
!!      spectr: sp_rj(i_rj)
!!
!!@endverbatim
!!
!!@n @param  ncomp  number of components for Legendre transform
!!@n @param  icomp_st  start component to clear
!!@n @param  icomp_ed  end component to clear
!
      module m_work_4_sph_trans_spin
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>     Work field data for Legendre transform
      real(kind = kreal), allocatable :: vr_rtm_wk(:)
!
!>     Work spectr data for Legendre transform
      real(kind = kreal), allocatable :: sp_rlm_wk(:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans(ncomp)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: ncomp
!
!
      allocate(sp_rlm_wk(nnod_rlm*ncomp))
      allocate(vr_rtm_wk(nnod_rtm*ncomp))
!
      sp_rlm_wk = 0.0d0
      vr_rtm_wk = 0.0d0
!
      end subroutine allocate_work_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans
!
      deallocate(vr_rtm_wk, sp_rlm_wk)
!
      end subroutine deallocate_work_sph_trans
!
! ----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_spin
