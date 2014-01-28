!>@file   m_work_4_sph_trans_krin.f90
!!@brief  module m_work_4_sph_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Field data for Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine allocate_work_sph_trans_krin(nb_sph_trans)
!!      subroutine deallocate_work_sph_trans_krin
!!
!!      subroutine clear_b_trans_vector_krin(nb)
!!      subroutine clear_b_trans_scalar_krin(nb)
!!
!!      subroutine clear_f_trans_vector_krin(nb)
!!      subroutine clear_f_trans_scalar_krin(nb)
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
      module m_work_4_sph_trans_krin
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>     field data for Legendre transform
!!@n       original layout: vr_rtm_krin(i_fld,k_rtm,l_rtm,m_rtm,nd)
!!@n       size: vr_rtm_krin(nb*nidx_rtm(1),nidx_rtm(2),nidx_rtm(3),3)
      real(kind = kreal), allocatable :: vr_rtm_krin(:,:,:)
!
!>     spectr data for Legendre transform
!!@n      original layout: sp_rlm_krin(i_fld,k_rtm,j_rlm,nd)
!!@n      size: sp_rlm_krin(nb*nidx_rtm(1),nidx_rlm(2),3)
      real(kind = kreal), allocatable :: sp_rlm_krin(:,:,:)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_work_sph_trans_krin(nb_sph_trans)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb_sph_trans
      integer(kind = kint) :: num1, num2
!
!
      num1 = nb_sph_trans*nidx_rlm(1)
      allocate(sp_rlm_krin(num1,nidx_rlm(2),3))
      sp_rlm_krin = 0.0d0
!
      num2 = nidx_rtm(2)*nidx_rtm(3)
      allocate(vr_rtm_krin(num1,num2,3))
      vr_rtm_krin = 0.0d0
!
      end subroutine allocate_work_sph_trans_krin
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_sph_trans_krin
!
      deallocate(vr_rtm_krin, sp_rlm_krin)
!
      end subroutine deallocate_work_sph_trans_krin
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_vector_krin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm, i_rtm_1
!
!
!$omp parallel do private(k_rtm,l_rtm,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do l_rtm = 1, nidx_rtm(2)
          do k_rtm = 1, nidx_rtm(1)*nb
            i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
            vr_rtm_krin(k_rtm,i_rtm_1,1) = zero
            vr_rtm_krin(k_rtm,i_rtm_1,2) = zero
            vr_rtm_krin(k_rtm,i_rtm_1,3) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_scalar_krin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm, i_rtm_1
!
!
!$omp parallel do private(k_rtm,l_rtm,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do l_rtm = 1, nidx_rtm(2)
          do k_rtm = 1, nidx_rtm(1)*nb
            i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
            vr_rtm_krin(k_rtm,i_rtm_1,1) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_vector_krin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: j_rlm, k_rtm
!
!
!$omp parallel do private(j_rlm)
      do k_rtm = 1, nidx_rtm(1)*nb
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_krin(k_rtm,j_rlm,1) = zero
            sp_rlm_krin(k_rtm,j_rlm,2) = zero
            sp_rlm_krin(k_rtm,j_rlm,3) = zero
          end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_scalar_krin(nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) ::  j_rlm, k_rtm
!
!
!$omp parallel do private(j_rlm)
      do k_rtm = 1, nidx_rtm(1)*nb
          do j_rlm = 1, nidx_rlm(2)
            sp_rlm_krin(k_rtm,j_rlm,1) = zero
          end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module m_work_4_sph_trans_krin
