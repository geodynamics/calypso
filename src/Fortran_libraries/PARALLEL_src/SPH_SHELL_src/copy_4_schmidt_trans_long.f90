!>@file   copy_4_schmidt_trans_long.f90
!!@brief  module copy_4_schmidt_trans_long
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2013
!
!>@brief  Copy data for Legendre transform for longer loop
!!
!!@verbatim
!!      subroutine copy_b_trans_vector_long(ncomp, nvector)
!!      subroutine copy_b_trans_scalar_long(ncomp, nvector, nscalar)
!!      subroutine copy_f_trans_vector_long(ncomp, nvector)
!!      subroutine copy_f_trans_scalar_long(ncomp, nvector, nscalar)
!!
!!      subroutine back_f_trans_vector_long(ncomp, nvector)
!!      subroutine back_f_trans_scalar_long(ncomp, nvector, nscalar)
!!      subroutine back_b_trans_vector_long(ncomp, nvector)
!!      subroutine back_b_trans_scalar_long(ncomp, nvector, nscalar)
!!@endverbatim
!!
!!@param   ncomp       Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module copy_4_schmidt_trans_long
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
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
      subroutine copy_b_trans_vector_long(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm_0, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rlm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm_0,i_rlm_1)
      do k_rlm = 1, nidx_rtm(1)
        do nd = 1, nvector
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = 3*nd + (j_rlm-1) * ncomp                          &
     &                 + (k_rlm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = nd + (j_rlm-1) * nvector                          &
     &                 + (k_rlm-1) * nvector * nidx_rlm(2)
!
            sp_rlm_long(3*i_rlm_1-2) = sp_rlm(i_rlm_0-2)
            sp_rlm_long(3*i_rlm_1-1) = sp_rlm(i_rlm_0-1)
            sp_rlm_long(3*i_rlm_1  ) = sp_rlm(i_rlm_0  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_b_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine copy_b_trans_scalar_long(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm_0, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rlm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm_0,i_rlm_1)
      do k_rlm = 1, nidx_rtm(1)
        do nd = 1, ncomp
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = nd + 3*nvector + (j_rlm-1) * ncomp                &
     &                   + (k_rlm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = nd + (j_rlm-1) * nscalar                          &
     &                   + (k_rlm-1) * nscalar * nidx_rlm(2)
!
            sp_rlm_long(i_rlm_1) = sp_rlm(i_rlm_0)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_b_trans_scalar_long
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_f_trans_vector_long(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nvector
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 =   3*nd + (l_rtm-1) * ncomp                      &
     &                  + (k_rtm-1) * ncomp * nidx_rtm(2)               &
     &                  + (m_rtm-1) * ncomp * nidx_rtm(1)*nidx_rtm(2)
              i_rtm_1 =  nd + (l_rtm-1) * nvector                       &
     &                  + (k_rtm-1) * nvector * nidx_rtm(2)             &
     &                  + (m_rtm-1) * nvector * nidx_rtm(1)*nidx_rtm(2)
!
              vr_rtm_long(3*i_rtm_1-2) = vr_rtm(i_rtm_0-2)              &
     &                  * radius_1d_rtp_r(k_rtm)*radius_1d_rtp_r(k_rtm)
              vr_rtm_long(3*i_rtm_1-1) = vr_rtm(i_rtm_0-1)              &
     &                  * radius_1d_rtp_r(k_rtm)
              vr_rtm_long(3*i_rtm_1  ) = vr_rtm(i_rtm_0  )              &
     &                  * radius_1d_rtp_r(k_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_f_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine copy_f_trans_scalar_long(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nscalar
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 =   nd + 3*nvector + (l_rtm-1) * ncomp            &
     &                 + (k_rtm-1) * ncomp * nidx_rtm(2)                &
     &                 + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 =   nd + (l_rtm-1) * nscalar                      &
     &                 + (k_rtm-1) * nscalar * nidx_rtm(2)              &
     &                 + (m_rtm-1) * nscalar * nidx_rtm(1)*nidx_rtm(2)
!
              vr_rtm_long(i_rtm_1) = vr_rtm(i_rtm_0)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_f_trans_scalar_long
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_long(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm_0, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm_0,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nvector
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = 3*nd + (j_rlm-1) * ncomp                          &
     &                     + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 =   nd + (j_rlm-1) * nvector                        &
     &                     + (k_rtm-1) * nvector * nidx_rlm(2)
!
            sp_rlm(i_rlm_0-2) = sp_rlm_long(3*i_rlm_1-2)
            sp_rlm(i_rlm_0-1) = sp_rlm_long(3*i_rlm_1-1)
            sp_rlm(i_rlm_0  ) = sp_rlm_long(3*i_rlm_1  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_long(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm_0, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm_0,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nscalar
          do j_rlm = 1, nidx_rlm(2)
            i_rlm_0 = nd + 3*nvector + (j_rlm-1) * ncomp                &
     &                               + (k_rtm-1) * ncomp * nidx_rlm(2)
            i_rlm_1 = nd + (j_rlm-1) * nscalar                          &
     &                   + (k_rtm-1) * nscalar * nidx_rlm(2)
!
            sp_rlm(i_rlm_0) = sp_rlm_long(i_rlm_1)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_long
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_long(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nvector
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 = 3*nd + (l_rtm-1) * ncomp                        &
     &                 + (k_rtm-1) * ncomp * nidx_rtm(2)                &
     &                 + (m_rtm-1) * ncomp * nidx_rtm(1)*nidx_rtm(2)
              i_rtm_1 = nd + (l_rtm-1) * nvector                        &
     &                 + (k_rtm-1) * nvector * nidx_rtm(2)              &
     &                 + (m_rtm-1) * nvector * nidx_rtm(1)*nidx_rtm(2)
!
              vr_rtm(i_rtm_0-2)  = vr_rtm_long(3*i_rtm_1-2)             &
     &                       * a_r_1d_rtm_r(k_rtm)*a_r_1d_rtm_r(k_rtm)
              vr_rtm(i_rtm_0-1)  = vr_rtm_long(3*i_rtm_1-1)             &
     &                       * a_r_1d_rtm_r(k_rtm)
              vr_rtm(i_rtm_0  )  = vr_rtm_long(3*i_rtm_1  )             &
     &                       * a_r_1d_rtm_r(k_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_long(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm_0, i_rtm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm_0,i_rtm_1)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nscalar
            do l_rtm = 1, nidx_rtm(2)
              i_rtm_0 =   nd + 3*nvector + (l_rtm-1) * ncomp            &
     &                 + (k_rtm-1) * ncomp * nidx_rtm(2)                &
     &                 + (m_rtm-1) * ncomp * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 =   nd + (l_rtm-1) * nscalar                      &
     &                 + (k_rtm-1) * nscalar * nidx_rtm(2)              &
     &                 + (m_rtm-1) * nscalar * nidx_rtm(1)*nidx_rtm(2)
!
              vr_rtm(i_rtm_0)  = vr_rtm_long(i_rtm_1)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      end module copy_4_schmidt_trans_long

