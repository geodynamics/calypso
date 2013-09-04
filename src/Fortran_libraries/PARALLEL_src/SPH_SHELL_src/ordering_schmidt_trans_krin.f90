!ordering_schmidt_trans_krin.f90
!      module ordering_schmidt_trans_krin
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine order_b_trans_vector_krin(nb)
!      subroutine order_b_trans_scalar_krin(nb)
!      subroutine order_f_trans_vector_krin(nb)
!      subroutine order_f_trans_scalar_krin(nb)
!
!      subroutine back_f_trans_vector_krin(nb)
!      subroutine back_f_trans_scalar_krin(nb)
!      subroutine back_b_trans_vector_krin(nb)
!      subroutine back_b_trans_scalar_krin(nb)
!
      module ordering_schmidt_trans_krin
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
      use m_work_4_sph_trans_krin
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_vector_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rlm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_1)
      do k_rlm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rlm-1) * nb * nidx_rlm(2)
            i_rlm_1 = nd + (k_rlm-1) * nb
!
            sp_rlm_krin(i_rlm_1,j_rlm,1) = sp_rlm(3*i_rlm-2)
            sp_rlm_krin(i_rlm_1,j_rlm,2) = sp_rlm(3*i_rlm-1)
            sp_rlm_krin(i_rlm_1,j_rlm,3) = sp_rlm(3*i_rlm  )
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine order_b_trans_scalar_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: i_rlm_1, k_rlm
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_1)
      do k_rlm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rlm-1) * nb * nidx_rlm(2)
            i_rlm_1 = nd + (k_rlm-1) * nb
!
            sp_rlm_krin(i_rlm_1,j_rlm,1) = sp_rlm(i_rlm)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_vector_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,i_rtm_1,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = nd + (k_rtm-1) * nb
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
!
              vr_rtm_krin(kr_nd,i_rtm_1,1) = vr_rtm(3*i_rtm-2)
              vr_rtm_krin(kr_nd,i_rtm_1,2) = vr_rtm(3*i_rtm-1)
              vr_rtm_krin(kr_nd,i_rtm_1,3) = vr_rtm(3*i_rtm  )
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine order_f_trans_scalar_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,i_rtm_1,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = nd + (k_rtm-1) * nb
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
!
              vr_rtm_krin(kr_nd,i_rtm_1,1) = vr_rtm(i_rtm)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine order_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_vector_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rtm-1) * nb * nidx_rlm(2)
            i_rlm_1 = nd + (k_rtm-1) * nb
!
            sp_rlm(3*i_rlm-2) = sp_rlm_krin(i_rlm_1,j_rlm,1)
            sp_rlm(3*i_rlm-1) = sp_rlm_krin(i_rlm_1,j_rlm,2)
            sp_rlm(3*i_rlm  ) = sp_rlm_krin(i_rlm_1,j_rlm,3)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine back_f_trans_scalar_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rtm
      integer(kind = kint) :: i_rlm_1
      integer(kind = kint) :: nd
!
!
!$omp parallel do private(j_rlm,nd,i_rlm,i_rlm_1)
      do k_rtm = 1, nidx_rtm(1)
        do nd = 1, nb
          do j_rlm = 1, nidx_rlm(2)
            i_rlm = nd + (j_rlm-1) * nb                                 &
     &                 + (k_rtm-1) * nb * nidx_rlm(2)
            i_rlm_1 = nd + (k_rtm-1) * nb
!
            sp_rlm(i_rlm  ) = sp_rlm_krin(i_rlm_1,j_rlm,1)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_vector_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,i_rtm_1,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = nd + (k_rtm-1) * nb
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
!
              vr_rtm(3*i_rtm-2)  = vr_rtm_krin(kr_nd,i_rtm_1,1)
              vr_rtm(3*i_rtm-1)  = vr_rtm_krin(kr_nd,i_rtm_1,2)
              vr_rtm(3*i_rtm  )  = vr_rtm_krin(kr_nd,i_rtm_1,3)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine back_b_trans_scalar_krin(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: k_rtm, l_rtm, m_rtm
      integer(kind = kint) :: i_rtm, i_rtm_1
      integer(kind = kint) :: nd, kr_nd
!
!
!$omp parallel do private(k_rtm,l_rtm,nd,i_rtm,i_rtm_1,kr_nd)
      do m_rtm = 1, nidx_rtm(3)
        do k_rtm = 1, nidx_rtm(1)
          do nd = 1, nb
            kr_nd = nd + (k_rtm-1) * nb
            do l_rtm = 1, nidx_rtm(2)
              i_rtm =   nd + (l_rtm-1) * nb                             &
     &                     + (k_rtm-1) * nb * nidx_rtm(2)               &
     &                     + (m_rtm-1) * nb * nidx_rtm(1) * nidx_rtm(2)
              i_rtm_1 = l_rtm + (m_rtm-1) * nidx_rtm(2)
!
              vr_rtm(i_rtm)  = vr_rtm_krin(kr_nd,i_rtm_1,1)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine back_b_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module ordering_schmidt_trans_krin

