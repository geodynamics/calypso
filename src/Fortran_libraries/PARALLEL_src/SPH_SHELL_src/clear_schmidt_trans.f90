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
!!      subroutine clear_b_trans_vector(nb)
!!      subroutine clear_b_trans_scalar(nb)
!!
!!      subroutine clear_f_trans_vector(nb)
!!      subroutine clear_f_trans_scalar(nb)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
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
      subroutine clear_b_trans_vector(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, i_rtm
!
!
!$omp parallel do private(ip,i_rtm,ist,ied)
      do ip = 1, np_smp
        ist = nb*inod_rtm_smp_stack(ip-1) + 1
        ied = nb*inod_rtm_smp_stack(ip)
        do i_rtm = ist, ied
            vr_rtm(3*i_rtm-2) = zero
            vr_rtm(3*i_rtm-1) = zero
            vr_rtm(3*i_rtm  ) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine clear_b_trans_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, i_rtm
!
!
!$omp parallel do private(ip,i_rtm,ist,ied)
      do ip = 1, np_smp
        ist = nb*inod_rtm_smp_stack(ip-1) + 1
        ied = nb*inod_rtm_smp_stack(ip)
        do i_rtm = ist, ied
            vr_rtm(i_rtm) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_b_trans_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_vector(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, i_rlm
!
!
!$omp parallel do private(ip,i_rlm,ist,ied)
      do ip = 1, np_smp
        ist = nb*inod_rlm_smp_stack(ip-1) + 1
        ied = nb*inod_rlm_smp_stack(ip)
        do i_rlm = ist, ied
            sp_rlm(3*i_rlm-2) = zero
            sp_rlm(3*i_rlm-1) = zero
            sp_rlm(3*i_rlm  ) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_vector
!
! -----------------------------------------------------------------------
!
      subroutine clear_f_trans_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: ip, ist, ied, i_rlm
!
!
!$omp parallel do private(ip,i_rlm,ist,ied)
      do ip = 1, np_smp
        ist = nb*inod_rlm_smp_stack(ip-1) + 1
        ied = nb*inod_rlm_smp_stack(ip)
        do i_rlm = ist, ied
            sp_rlm(i_rlm) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine clear_f_trans_scalar
!
! -----------------------------------------------------------------------
!
      end module clear_schmidt_trans

