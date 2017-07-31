!>@file   legendre_fwd_trans_krin.f90
!!@brief  module legendre_fwd_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  forward Legendre transform
!!       (innermost loop is field and radius)
!!
!!@verbatim
!!      subroutine legendre_f_trans_vector_krin                         &
!!     &         (ncomp, nvector, sph_rtm, sph_rlm, idx_trns,           &
!!     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,             &
!!     &          P_rtm, dPdt_rtm, vr_rtm_krin, sp_rlm_spin)
!!      subroutine legendre_f_trans_scalar_krin                         &
!!     &         (ncomp, nvector, nscalar, sph_rtm, sph_rlm, idx_trns,  &
!!     &          g_sph_rlm, weight_rtm, P_rtm,                         &
!!     &          vr_rtm_krin, sp_rlm_spin)
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!
!!        Input:  vr_rtm_krin
!!        Output: sp_rlm_spin
!!
!!     field data for Legendre transform
!!         original layout: vr_rtm_krin(i_fld,k_rtm,l_rtm,m_rtm,nd)
!!         size: vr_rtm_krin(nidx_rtm(1)*nidx_rtm(2)*nidx_rtm(3),nb)
!!      real(kind = kreal), allocatable :: vr_rtm_krin(:,:)
!!
!!     spectr data for Legendre transform
!!        original layout: sp_rlm_krin(k_rlm,j_rlm,i_fld)
!!        size: sp_rlm_krin(nidx_rlm(1)*nidx_rlm(2),nb)
!!      real(kind = kreal), allocatable :: sp_rlm_krin(:,:)
!!@endverbatim
!!
!!@n @param  ncomp    number of components to be transformed
!!@n @param  nvector  number of vector to be transformed
!!@n @param  nscalar  number of scalar to be transformed
!
      module legendre_fwd_trans_krin
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_work_4_sph_trans
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_vector_krin                           &
     &         (ncomp, nvector, sph_rtm, sph_rlm, idx_trns,             &
     &          asin_theta_1d_rtm, g_sph_rlm, weight_rtm,               &
     &          P_rtm, dPdt_rtm, vr_rtm_krin, sp_rlm_spin)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: weight_rtm(sph_rtm%nidx_rtm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(sph_rtm%nidx_rtm(1)*ncomp,                   &
     &                     sph_rtm%nidx_rtm(2),sph_rtm%nidx_rtm(3))
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(sph_rtm%nidx_rtm(1)*ncomp,                   &
     &                     sph_rlm%nidx_rlm(2))
!
      integer(kind = kint) :: ip, jst, jed, lp, lst, led
      integer(kind = kint) :: j_rlm, mp_rlm, mn_rlm, l_rtm
      integer(kind = kint) :: kr_nd, k_rlm, nb_nri
      real(kind = kreal) :: sp1(nvector*sph_rlm%nidx_rlm(1))
      real(kind = kreal) :: sp2(nvector*sph_rlm%nidx_rlm(1))
      real(kind = kreal) :: sp3(nvector*sph_rlm%nidx_rlm(1))
      real(kind = kreal) :: Pvw_l(sph_rtm%nidx_rtm(2))
      real(kind = kreal) :: dPvw_l(sph_rtm%nidx_rtm(2))
      real(kind = kreal) :: Pgvw_l(sph_rtm%nidx_rtm(2))
!
!
      nb_nri = nvector*sph_rlm%nidx_rlm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,jst,jed,lp,lst,led,j_rlm,l_rtm,kr_nd,      &
!$omp&                    mp_rlm,mn_rlm,k_rlm,Pvw_l,dPvw_l,Pgvw_l,      &
!$omp&                    sp1,sp2,sp3)
      do ip = 1, np_smp
        jst = sph_rlm%istack_rlm_j_smp(ip-1) + 1
        jed = sph_rlm%istack_rlm_j_smp(ip  )
        do j_rlm = jst, jed
          do kr_nd = 1, nb_nri
            sp_rlm_spin(kr_nd,         j_rlm) = 0.0d0
            sp_rlm_spin(kr_nd+nb_nri,  j_rlm) = 0.0d0
            sp_rlm_spin(kr_nd+2*nb_nri,j_rlm) = 0.0d0
          end do
        end do
!
        do lp = 1, idx_trns%nblock_l_rtm
          lst = idx_trns%lstack_block_rtm(lp-1) + 1
          led = idx_trns%lstack_block_rtm(lp  )
!
          do j_rlm = jst, jed
            mp_rlm = idx_trns%mdx_p_rlm_rtm(j_rlm)
            mn_rlm = idx_trns%mdx_n_rlm_rtm(j_rlm)
!
            do l_rtm = lst, led
              Pvw_l(l_rtm) = P_rtm(l_rtm,j_rlm)                         &
     &                       * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
              dPvw_l(l_rtm) = dPdt_rtm(l_rtm,j_rlm)                     &
     &                       * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
              Pgvw_l(l_rtm) = P_rtm(l_rtm,j_rlm)                        &
     &                       * dble(sph_rlm%idx_gl_1d_rlm_j(j_rlm,3))   &
     &                        * asin_theta_1d_rtm(l_rtm)                &
     &                        * g_sph_rlm(j_rlm,7)* weight_rtm(l_rtm)
            end do
!
            sp1(1:nb_nri) = 0.0d0
            sp2(1:nb_nri) = 0.0d0
            sp3(1:nb_nri) = 0.0d0
            do l_rtm = lst, led
              do kr_nd = 1, nb_nri
                sp1(kr_nd) = sp1(kr_nd)                                 &
     &                    +  vr_rtm_krin(kr_nd,         l_rtm,mp_rlm)   &
     &                      * Pvw_l(l_rtm)
!
                sp2(kr_nd) = sp2(kr_nd)                                 &
     &                    + (vr_rtm_krin(kr_nd+nb_nri,  l_rtm,mp_rlm)   &
     &                      * dPvw_l(l_rtm)                             &
     &                     - vr_rtm_krin(kr_nd+2*nb_nri,l_rtm,mn_rlm)   &
     &                      * Pgvw_l(l_rtm))
!
                sp3(kr_nd) = sp3(kr_nd)                                 &
     &                    - (vr_rtm_krin(kr_nd+nb_nri,  l_rtm,mn_rlm)   &
     &                      * Pgvw_l(l_rtm)                             &
     &                     + vr_rtm_krin(kr_nd+2*nb_nri,l_rtm,mp_rlm)   &
     &                      * dPvw_l(l_rtm))
              end do
            end do
!
            do kr_nd = 1, nb_nri
              k_rlm = 1 + mod((kr_nd-1),sph_rlm%nidx_rlm(1))
              sp_rlm_spin(kr_nd,         j_rlm)                         &
     &             = sp_rlm_spin(kr_nd,         j_rlm)                  &
     &              + sp1(kr_nd) * sph_rlm%radius_1d_rlm_r(k_rlm)**2
              sp_rlm_spin(kr_nd+nb_nri,  j_rlm)                         &
     &             =  sp_rlm_spin(kr_nd+nb_nri,  j_rlm)                 &
     &              + sp2(kr_nd) * sph_rlm%radius_1d_rlm_r(k_rlm)
              sp_rlm_spin(kr_nd+2*nb_nri,j_rlm)                         &
     &             = sp_rlm_spin(kr_nd+2*nb_nri,j_rlm)                  &
     &              + sp3(kr_nd) * sph_rlm%radius_1d_rlm_r(k_rlm)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_vector_krin
!
! -----------------------------------------------------------------------
!
      subroutine legendre_f_trans_scalar_krin                           &
     &         (ncomp, nvector, nscalar, sph_rtm, sph_rlm, idx_trns,    &
     &          g_sph_rlm, weight_rtm, P_rtm,                           &
     &          vr_rtm_krin, sp_rlm_spin)
!
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind = kreal), intent(in) :: weight_rtm(sph_rtm%nidx_rtm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in)                                    &
     &      :: vr_rtm_krin(sph_rtm%nidx_rtm(1)*ncomp,                   &
     &                     sph_rtm%nidx_rtm(2),sph_rtm%nidx_rtm(3))
      real(kind = kreal), intent(inout)                                 &
     &      :: sp_rlm_spin(sph_rtm%nidx_rtm(1)*ncomp,                   &
     &                     sph_rlm%nidx_rlm(2))
!
      integer(kind = kint) :: ip, jst, jed, lp, lst, led
      integer(kind = kint) :: j_rlm, l_rtm, mp_rlm
      integer(kind = kint) :: kst, nb_nri, kr_nd
      real(kind = kreal) :: Pws_l(sph_rtm%nidx_rtm(2))
      real(kind = kreal) :: sp1(nscalar*sph_rlm%nidx_rlm(1))
!
!
      kst = 3*nvector * sph_rlm%nidx_rlm(1)
      nb_nri = nscalar*sph_rlm%nidx_rlm(1)
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,jst,jed,lp,lst,led,j_rlm,l_rtm,mp_rlm,     &
!$omp&                    kr_nd,Pws_l,sp1)
      do ip = 1, np_smp
        jst = sph_rlm%istack_rlm_j_smp(ip-1) + 1
        jed = sph_rlm%istack_rlm_j_smp(ip  )
        do j_rlm = jst, jed
          do kr_nd = 1, nb_nri
              sp_rlm_spin(kr_nd+kst,j_rlm) = 0.0d0
          end do
        end do
!
        do lp = 1, idx_trns%nblock_l_rtm
          lst = idx_trns%lstack_block_rtm(lp-1) + 1
          led = idx_trns%lstack_block_rtm(lp  )
!
          do j_rlm = jst, jed
            mp_rlm = idx_trns%mdx_p_rlm_rtm(j_rlm)
!
            do l_rtm = lst, led
              Pws_l(l_rtm) = P_rtm(l_rtm,j_rlm)                         &
     &                 * g_sph_rlm(j_rlm,6)*weight_rtm(l_rtm)
            end do
!
            do l_rtm = lst, led
              sp1(1:nscalar*sph_rtm%nidx_rtm(1)) = 0.0d0
!
              do kr_nd = 1, nb_nri
                sp1(kr_nd) = sp1(kr_nd)                                 &
     &                      + vr_rtm_krin(kr_nd+kst,l_rtm,mp_rlm)       &
     &                       * Pws_l(l_rtm)
              end do
            end do
!
            do kr_nd = 1, nb_nri
              sp_rlm_spin(kr_nd+kst,j_rlm)                              &
     &              = sp_rlm_spin(kr_nd+kst,j_rlm) + sp1(kr_nd)
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_f_trans_scalar_krin
!
! -----------------------------------------------------------------------
!
      end module legendre_fwd_trans_krin
