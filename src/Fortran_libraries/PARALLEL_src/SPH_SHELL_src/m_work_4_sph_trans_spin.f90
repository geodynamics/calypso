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
!!      subroutine allocate_work_sph_trans_spin(nb_sph_trans)
!!      subroutine deallocate_work_sph_trans_spin
!!
!!      subroutine clear_b_trans_vector_spin(nb)
!!      subroutine clear_b_trans_scalar_spin(nb)
!!
!!      subroutine clear_f_trans_vector_spin(nb)
!!      subroutine clear_f_trans_scalar_spin(nb)
!!
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
!!@n @param  nb_sph_trans
!!             maximum number of fields for Legendre transform
!!@n @param  nb  number of fields to be transformed
!
      module m_work_4_sph_trans_spin
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>     field data for Legendre transform
!!@n       original layout: vr_rtm_spin(l_rtm,m_rtm,k_rtm,i_fld,nb,3)
!!@n       size: vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),nidx_rtm(1)*nb,3)
      real(kind = kreal), allocatable :: vr_rtm_spin(:,:,:,:)
!
!>     spectr data for Legendre transform
!!@n      original layout: sp_rlm_spin(j_rlm,k_rtm,i_fld,nb,3)
!!@n        size: sp_rlm_spin(nidx_rlm(2),nidx_rtm(1)*nb,3)
      real(kind = kreal), allocatable :: sp_rlm_spin(:,:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans_spin(nb_sph_trans)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb_sph_trans
      integer(kind = kint) :: num1
!
!
      num1 = nb_sph_trans*nidx_rlm(1)
      allocate(sp_rlm_spin(nidx_rlm(2),num1,3))
      sp_rlm_spin = 0.0d0
!
      num1 = nb_sph_trans*nidx_rlm(1)
      allocate(vr_rtm_spin(nidx_rtm(2),nidx_rtm(3),num1,3))
      vr_rtm_spin = 0.0d0
!
      end subroutine allocate_work_sph_trans_spin
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans_spin
!
      deallocate(vr_rtm_spin, sp_rlm_spin)
!
      end subroutine deallocate_work_sph_trans_spin
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine clear_b_trans_vector_spin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
!
!
!$omp parallel do private(k_rtm,l_rtm)
      do m_rtm = 1, nidx_rtm(3)
        do l_rtm = 1, nidx_rtm(2)
          do k_rtm = 1, nidx_rtm(1)*nb
            vr_rtm_spin(l_rtm,m_rtm,k_rtm,1) = zero
            vr_rtm_spin(l_rtm,m_rtm,k_rtm,2) = zero
            vr_rtm_spin(l_rtm,m_rtm,k_rtm,3) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_scalar_spin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
!
!
!$omp parallel do private(m_rtm,l_rtm)
      do k_rtm = 1, nidx_rtm(1)*nb
        do m_rtm = 1, nidx_rtm(3)
          do l_rtm = 1, nidx_rtm(2)
            vr_rtm_spin(l_rtm,m_rtm,k_rtm,1) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_vector_spin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: j_rlm, k_rtm
!
!
!$omp parallel do private(j_rlm)
      do k_rtm = 1, nidx_rtm(1)*nb
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_spin(j_rlm,k_rtm,1) = zero
            sp_rlm_spin(j_rlm,k_rtm,2) = zero
            sp_rlm_spin(j_rlm,k_rtm,3) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_scalar_spin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: j_rlm, k_rtm
!
!
!$omp parallel do private(j_rlm)
      do k_rtm = 1, nidx_rtm(1)*nb
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_spin(j_rlm,k_rtm,1) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_spin
