!>@file   ordering_lag_trans_fldout.f90
!!@brief  module ordering_lag_trans_fldout
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Copy data for Legendre transform for longer loop
!!
!!@verbatim
!!      subroutine order_b_trans_vector_fldout(ncomp, nvector)
!!      subroutine order_b_trans_scalar_fldout(ncomp, nvector, nscalar)
!!      subroutine order_f_trans_vector_fldout(ncomp, nvector)
!!      subroutine order_f_trans_scalar_fldout(ncomp, nvector, nscalar)
!!
!!      subroutine back_f_trans_vector_fldout(ncomp, nvector)
!!      subroutine back_f_trans_scalar_fldout(ncomp, nvector, nscalar)
!!      subroutine back_b_trans_vector_fldout(ncomp, nvector)
!!      subroutine back_b_trans_scalar_fldout(ncomp, nvector, nscalar)
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module ordering_lag_trans_fldout
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_4_sph_trans_fldout
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_vector_fldout(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm_0, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rtm, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm_0,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nvector
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 =  3*i_fld + (j_rlm-1) * ncomp                      &
     &                         + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm_fdout(i_rlm_1,3*i_fld-2) = sp_rlm(i_rlm_0-2)
            sp_rlm_fdout(i_rlm_1,3*i_fld-1) = sp_rlm(i_rlm_0-1)
            sp_rlm_fdout(i_rlm_1,3*i_fld  ) = sp_rlm(i_rlm_0  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_scalar_fldout(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm_0, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rtm, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm_0,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nscalar
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = 3*nvector + i_fld                                 &
     &             + (j_rlm-1) * ncomp                                  &
     &             + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm_fdout(i_rlm_1,i_fld) = sp_rlm(i_rlm_0)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_scalar_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_vector_fldout(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nvector
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 =  3*i_fld + (l_rtm-1) * ncomp                    &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (k_rtm-1) * nidx_rtm(2)                 &
     &                        + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm_fdout(i_rtm_1,3*i_fld-2) = vr_rtm(i_rtm_0-2)       &
     &                  * radius_1d_rtp_r(k_rtm)*radius_1d_rtp_r(k_rtm)
              vr_rtm_fdout(i_rtm_1,3*i_fld-1) = vr_rtm(i_rtm_0-1)       &
     &                  * radius_1d_rtp_r(k_rtm)
              vr_rtm_fdout(i_rtm_1,3*i_fld  ) = vr_rtm(i_rtm_0  )       &
     &                  * radius_1d_rtp_r(k_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_scalar_fldout(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nscalar
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 = 3*nvector + i_fld                               &
     &               + (l_rtm-1) * ncomp                                &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (k_rtm-1) * nidx_rtm(2)                 &
     &                        + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm_fdout(i_rtm_1,i_fld) = vr_rtm(i_rtm_0)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_scalar_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_fldout(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm_0, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm_0,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nvector
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = 3*i_fld + (j_rlm-1) * ncomp                       &
     &                      + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm(i_rlm_0-2) = sp_rlm_fdout(i_rlm_1,3*i_fld-2)
            sp_rlm(i_rlm_0-1) = sp_rlm_fdout(i_rlm_1,3*i_fld-1)
            sp_rlm(i_rlm_0  ) = sp_rlm_fdout(i_rlm_1,3*i_fld  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_fldout(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm_0, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1, i_fld
!
!
!$omp parallel do private(j_rlm,i_fld,i_rlm_0,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do i_fld = 1, nscalar
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = 3*nvector + i_fld                                 &
     &             + (j_rlm-1) * ncomp                                  &
     &             + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = j_rlm + (k_rtm-1) * nidx_rlm(2)
!
            sp_rlm(i_rlm_0  ) = sp_rlm_fdout(i_rlm_1,i_fld)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_fldout
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_fldout(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nvector
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 =  3*i_fld + (l_rtm-1) * ncomp                    &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (k_rtm-1) * nidx_rtm(2)                 &
     &                        + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm(i_rtm_0-2)  = vr_rtm_fdout(i_rtm_1,3*i_fld-2)      &
     &                       * a_r_1d_rtm_r(k_rtm)*a_r_1d_rtm_r(k_rtm)
              vr_rtm(i_rtm_0-1)  = vr_rtm_fdout(i_rtm_1,3*i_fld-1)      &
     &                       * a_r_1d_rtm_r(k_rtm)
              vr_rtm(i_rtm_0  )  = vr_rtm_fdout(i_rtm_1,3*i_fld  )      &
     &                       * a_r_1d_rtm_r(k_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_fldout
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_fldout(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1, i_fld
!
!
!$omp parallel do private(k_rtm,l_rtm,i_fld,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do i_fld = 1, nscalar
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 = 3*nvector + i_fld                               &
     &               + (l_rtm-1) * ncomp                                &
     &               + (k_rtm-1) * ncomp * nidx_rtm(2)                  &
     &               + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm-1 + (k_rtm-1) * nidx_rtm(2)               &
     &                          + (m_rtm-1) * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm(i_rtm_0)  = vr_rtm_fdout(i_rtm_1,i_fld)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_fldout
!
! -----------------------------------------------------------------------
!
      end module ordering_lag_trans_fldout

