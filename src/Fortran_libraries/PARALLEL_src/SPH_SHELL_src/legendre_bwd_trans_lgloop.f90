!>@file   legendre_bwd_trans_lgloop.f90
!!@brief  module legendre_bwd_trans_lgloop
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (longest loop version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_long                         &
!!     &         (ncomp, nvector, sph_rlm, sph_rtm, idx_trns,           &
!!     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,        &
!!     &          sp_rlm, vr_rtm)
!!        Input:  sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!        Output: vr_rtm   (Order: radius,theta,phi)
!!      subroutine legendre_b_trans_scalar_long                         &
!!     &         (ncomp, nvector, nscalar, sph_rlm, sph_rtm, idx_trns,  &
!!     &          P_rtm, sp_rlm, vr_rtm)
!!        Input:  sp_rlm
!!        Output: vr_rtm
!!
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_lgloop
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
      subroutine legendre_b_trans_vector_long                           &
     &         (ncomp, nvector, sph_rlm, sph_rtm, idx_trns,             &
     &          asin_theta_1d_rtm, g_sph_rlm, P_rtm, dPdt_rtm,          &
     &          sp_rlm, vr_rtm)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind = kreal), intent(in)                                    &
     &           :: asin_theta_1d_rtm(sph_rtm%nidx_rtm(2))
      real(kind = kreal), intent(in)                                    &
     &           :: g_sph_rlm(sph_rlm%nidx_rlm(2),17)
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &           :: sp_rlm(ncomp*sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &           :: vr_rtm(ncomp*sph_rtm%nnod_rtm)
!
      integer(kind = kint) :: ip, kst, ked, lp, lst, led
      integer(kind = kint) :: i_rlm, j_rlm
      integer(kind = kint) :: k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, in_rtm
      integer(kind = kint) :: nd, kr_nd
      real(kind = kreal) :: Pg3_l, dPdt_l, Pgv_l
!
!
!$omp parallel do schedule(static)                                      &
!$omp&             private(ip,kst,ked,lp,lst,led,nd,kr_nd,i_rlm,l_rtm,  &
!$omp&                     k_rlm,j_rlm,ip_rtm,in_rtm,Pg3_l,dPdt_l,Pgv_l)
      do ip = 1, np_smp
        kst = nvector*sph_rlm%istack_rlm_kr_smp(ip-1) + 1
        ked = nvector*sph_rlm%istack_rlm_kr_smp(ip  )
        do j_rlm = 1, sph_rlm%nidx_rlm(2)
          do kr_nd = kst, ked
            nd =    1 + mod( (kr_nd-1),nvector)
            k_rlm = 1 + (kr_nd - nd) / nvector
!
            i_rlm = 3*nd + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)    &
     &                            + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
            sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2)                           &
     &                       * sph_rlm%a_r_1d_rlm_r(k_rlm)**2
            sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                           &
     &                       * sph_rlm%a_r_1d_rlm_r(k_rlm)
            sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                           &
     &                       * sph_rlm%a_r_1d_rlm_r(k_rlm)
          end do
        end do
!
        do lp = 1, idx_trns%nblock_l_rtm
          lst = idx_trns%lstack_block_rtm(lp-1) + 1
          led = idx_trns%lstack_block_rtm(lp  )
!
          do l_rtm = lst, led
            do j_rlm = 1, sph_rlm%nidx_rlm(2)
              do kr_nd = kst, ked
                nd =    1 + mod( (kr_nd-1),nvector)
                k_rlm = 1 + (kr_nd - nd) / nvector
!
                Pg3_l = P_rtm(l_rtm,j_rlm) * g_sph_rlm(j_rlm,3)
                dPdt_l = dPdt_rtm(l_rtm,j_rlm)
!
                ip_rtm = 3*nd                                           &
     &                  + ncomp*((l_rtm-1) * sph_rtm%istep_rtm(2)       &
     &                         + (k_rlm-1) * sph_rtm%istep_rtm(1)       &
     &                  + (idx_trns%mdx_p_rlm_rtm(j_rlm)-1)             &
     &                                     * sph_rtm%istep_rtm(3))
!
                i_rlm = 3*nd                                            &
     &                 + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)      &
     &                 + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
                vr_rtm(ip_rtm-2) = vr_rtm(ip_rtm-2)                     &
     &                            + sp_rlm(i_rlm-2) * Pg3_l
                vr_rtm(ip_rtm-1) = vr_rtm(ip_rtm-1)                     &
     &                            + sp_rlm(i_rlm-1) * dPdt_l
                vr_rtm(ip_rtm  ) = vr_rtm(ip_rtm  )                     &
     &                            - sp_rlm(i_rlm  ) * dPdt_l
              end do
            end do
!
            do j_rlm = 1, sph_rlm%nidx_rlm(2)
              do kr_nd = kst, ked
                nd =    1 + mod( (kr_nd-1),nvector)
                k_rlm = 1 + (kr_nd - nd) / nvector
!
                Pgv_l = -P_rtm(l_rtm,j_rlm)                             &
     &                 * dble(sph_rlm%idx_gl_1d_rlm_j(j_rlm,3))         &
     &                 * asin_theta_1d_rtm(l_rtm)
!
                in_rtm = 3*nd                                           &
     &                  + ncomp*((l_rtm-1) * sph_rtm%istep_rtm(2)       &
     &                         + (k_rlm-1) * sph_rtm%istep_rtm(1)       &
     &                  + (idx_trns%mdx_n_rlm_rtm(j_rlm)-1)             &
     &                   * sph_rtm%istep_rtm(3))
!
                i_rlm = 3*nd                                            &
     &                 + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)      &
     &                 + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
                vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1)                     &
     &                            + sp_rlm(i_rlm  ) * Pgv_l
                vr_rtm(in_rtm  ) = vr_rtm(in_rtm  )                     &
     &                            + sp_rlm(i_rlm-1) * Pgv_l
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_long
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_long                           &
     &         (ncomp, nvector, nscalar, sph_rlm, sph_rtm, idx_trns,    &
     &          P_rtm,  sp_rlm, vr_rtm)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind= kreal), intent(in)                                     &
     &           :: P_rtm(sph_rtm%nidx_rtm(2),sph_rlm%nidx_rlm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(inout)                                 &
     &           :: sp_rlm(ncomp*sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &           :: vr_rtm(ncomp*sph_rtm%nnod_rtm)
!
      integer(kind = kint) :: ip, kst, ked, lp, lst, led
      integer(kind = kint) :: i_rlm, k_rlm, j_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, nd, kr_nd
!
!
!$omp parallel do schedule(static)                                      &
!$omp&             private(ip,kst,ked,lp,lst,led,k_rlm,j_rlm,nd,kr_nd,  &
!$omp&                     i_rlm,l_rtm,ip_rtm)
      do ip = 1, np_smp
        kst = nscalar*sph_rlm%istack_rlm_kr_smp(ip-1) + 1
        ked = nscalar*sph_rlm%istack_rlm_kr_smp(ip  )
        do lp = 1, idx_trns%nblock_l_rtm
          lst = idx_trns%lstack_block_rtm(lp-1) + 1
          led = idx_trns%lstack_block_rtm(lp  )
!
          do j_rlm = 1, sph_rlm%nidx_rlm(2)
            do kr_nd = kst, ked
              nd =    1 + mod( (kr_nd-1),nscalar)
              k_rlm = 1 + (kr_nd - nd) / nscalar
!
              do l_rtm = lst, led
                ip_rtm = nd + 3*nvector                                 &
     &                      + ncomp*((l_rtm-1) * sph_rtm%istep_rtm(2)   &
     &                             + (k_rlm-1) * sph_rtm%istep_rtm(1)   &
     &                      + (idx_trns%mdx_p_rlm_rtm(j_rlm)-1)         &
     &                                         * sph_rtm%istep_rtm(3))
!
                i_rlm = nd + 3*nvector                                  &
     &                     + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)  &
     &                     + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
                vr_rtm(ip_rtm) = vr_rtm(ip_rtm)                         &
     &                        + sp_rlm(i_rlm) * P_rtm(l_rtm,j_rlm)
              end do
            end do
          end do
!
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_long
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_lgloop
