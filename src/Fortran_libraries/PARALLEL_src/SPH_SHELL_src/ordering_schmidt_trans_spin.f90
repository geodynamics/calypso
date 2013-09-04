!>@file   ordering_schmidt_trans_spin.f90
!!@brief  module ordering_schmidt_trans_spin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  Copy data for Legendre transform
!!       (innermost loop is spherical harmonics)
!!
!!@verbatim
!!      subroutine order_b_trans_vector_spin(nb)
!!      subroutine order_b_trans_scalar_spin(nb)
!!      subroutine order_f_trans_vector_spin(nb)
!!      subroutine order_f_trans_scalar_spin(nb)
!!
!!      subroutine back_f_trans_vector_spin(nb)
!!      subroutine back_f_trans_scalar_spin(nb)
!!      subroutine back_b_trans_vector_spin(nb)
!!      subroutine back_b_trans_scalar_spin(nb)
!!@endverbatim
!!
!!@n @param  nb  number of fields to be transformed
!
      module ordering_schmidt_trans_spin
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_4_sph_trans_spin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_vector_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rlm_2, k_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_2)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rtm-1) * nb * nidx_rlm(2)
            i_rlm_2 = k_rtm + (nd-1) * nidx_rtm(1)
!
            sp_rlm_spin(j_rlm,i_rlm_2,1) = sp_rlm(3*i_rlm-2)
            sp_rlm_spin(j_rlm,i_rlm_2,2) = sp_rlm(3*i_rlm-1)
            sp_rlm_spin(j_rlm,i_rlm_2,3) = sp_rlm(3*i_rlm  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rlm_2, k_rtm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_2)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rtm-1) * nb * nidx_rlm(2)
            i_rlm_2 = k_rtm + (nd-1) * nidx_rtm(1)
!
            sp_rlm_spin(j_rlm,i_rlm_2,1) = sp_rlm(i_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_vector_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = k_rtm + (nd-1) * nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm_spin(l_rtm,m_rtm,kr_nd,1) = vr_rtm(3*i_rtm-2)
              vr_rtm_spin(l_rtm,m_rtm,kr_nd,2) = vr_rtm(3*i_rtm-1)
              vr_rtm_spin(l_rtm,m_rtm,kr_nd,3) = vr_rtm(3*i_rtm  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = k_rtm + (nd-1) * nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm_spin(l_rtm,m_rtm,kr_nd,1) = vr_rtm(i_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_2
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_2)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rtm-1) * nb * nidx_rlm(2)
            i_rlm_2 = k_rtm + (nd-1) * nidx_rtm(1)
!
            sp_rlm(3*i_rlm-2) = sp_rlm_spin(j_rlm,i_rlm_2,1)
            sp_rlm(3*i_rlm-1) = sp_rlm_spin(j_rlm,i_rlm_2,2)
            sp_rlm(3*i_rlm  ) = sp_rlm_spin(j_rlm,i_rlm_2,3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_2
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_2)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rtm-1) * nb * nidx_rlm(2)
            i_rlm_2 = k_rtm + (nd-1) * nidx_rtm(1)
!
            sp_rlm(i_rlm) = sp_rlm_spin(j_rlm,i_rlm_2,1)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_spin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = k_rtm + (nd-1) * nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm(3*i_rtm-2) = vr_rtm_spin(l_rtm,m_rtm,kr_nd,1)
              vr_rtm(3*i_rtm-1) = vr_rtm_spin(l_rtm,m_rtm,kr_nd,2)
              vr_rtm(3*i_rtm  ) = vr_rtm_spin(l_rtm,m_rtm,kr_nd,3)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_spin
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_spin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = k_rtm + (nd-1) * nidx_rtm(1)
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
!
              vr_rtm(i_rtm) = vr_rtm_spin(l_rtm,m_rtm,kr_nd,1)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_spin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_spin

