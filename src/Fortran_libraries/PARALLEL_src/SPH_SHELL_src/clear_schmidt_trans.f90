!>@file   clear_schmidt_trans.f90
!!@brief  module clear_schmidt_trans
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Clear data for Legendre transform
!!
!!@verbatim
!!      subroutine clear_b_trans_vector(ncomp, nvector)
!!      subroutine clear_b_trans_scalar(ncomp, nvector, nscalar)
!!
!!      subroutine clear_f_trans_vector(ncomp, nvector)
!!      subroutine clear_f_trans_scalar(ncomp, nvector, nscalar)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module clear_schmidt_trans
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_vector(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, ist, ied, i_rtm, ip_rtm, nd
!
!
!$omp parallel do private(ip,i_rtm,ip_rtm,ist,ied,nd)
      do ip = 1, np_smp
        ist = inod_rtm_smp_stack(ip-1) + 1
        ied = inod_rtm_smp_stack(ip)
        do nd = 1, nvector
          do i_rtm = ist, ied
            ip_rtm = 3*nd + (i_rtm-1) * ncomp
            vr_rtm(ip_rtm-2) = zero
            vr_rtm(ip_rtm-1) = zero
            vr_rtm(ip_rtm  ) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_scalar(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: ip, ist, ied, i_rtm, ip_rtm, nd
!
!
!$omp parallel do private(ip,i_rtm,ip_rtm,ist,ied,nd)
      do ip = 1, np_smp
        ist = inod_rtm_smp_stack(ip-1) + 1
        ied = inod_rtm_smp_stack(ip)
        do nd = 1, nscalar
          do i_rtm = ist, ied
            ip_rtm = nd + 3*nvector + (i_rtm-1) * ncomp
            vr_rtm(ip_rtm) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_vector(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: ip, ist, ied, i_rlm, ip_rlm, nd
!
!
!$omp parallel do private(ip,i_rlm,ip_rlm,ist,ied,nd)
      do ip = 1, np_smp
        ist = inod_rlm_smp_stack(ip-1) + 1
        ied = inod_rlm_smp_stack(ip)
        do nd = 1, nvector
          do i_rlm = ist, ied
            ip_rlm = 3*nd + (i_rlm-1) * ncomp
            sp_rlm(ip_rlm-2) = zero
            sp_rlm(ip_rlm-1) = zero
            sp_rlm(ip_rlm  ) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_scalar(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: ip, ist, ied, i_rlm, ip_rlm, nd
!
!
!$omp parallel do private(ip,i_rlm,ip_rlm,ist,ied,nd)
      do ip = 1, np_smp
        ist = inod_rlm_smp_stack(ip-1) + 1
        ied = inod_rlm_smp_stack(ip)
        do nd = 1, nscalar
          do i_rlm = ist, ied
            ip_rlm = nd + 3*nvector + (i_rlm-1) * ncomp
            sp_rlm(i_rlm) = zero
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      end module clear_schmidt_trans

