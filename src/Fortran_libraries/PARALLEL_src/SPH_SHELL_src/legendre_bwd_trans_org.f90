!>@file   legendre_bwd_trans_org.f90
!!@brief  module legendre_bwd_trans_org
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief  backward Legendre transform
!!       (Original version)
!!
!!@verbatim
!!      subroutine legendre_b_trans_vector_org                          &
!!     &         (ncomp, nvector, sph_rlm, sph_rtm, idx_trns,           &
!!     &          asin_theta_1d_rtm, g_sph_rlm, P_jl, dPdt_jl,          &
!!     &          sp_rlm, vr_rtm)
!!        Input:  vr_rtm   (Order: radius,theta,phi)
!!        Output: sp_rlm   (Order: poloidal,diff_poloidal,toroidal)
!!      subroutine legendre_b_trans_scalar_org                          &
!!     &         (ncomp, nvector, nscalar, sph_rlm, sph_rtm, idx_trns,  &
!!     &          P_jl, sp_rlm, vr_rtm)
!!        Input:  vr_rtm
!!        Output: sp_rlm
!!
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(index_4_sph_trans), intent(in) :: idx_trns
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_org
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
      subroutine legendre_b_trans_vector_org                            &
     &         (ncomp, nvector, sph_rlm, sph_rtm, idx_trns,             &
     &          asin_theta_1d_rtm, g_sph_rlm, P_jl, dPdt_jl,            &
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
     &           :: P_jl(sph_rlm%nidx_rlm(2),sph_rtm%nidx_rtm(2))
      real(kind= kreal), intent(in)                                     &
     &           :: dPdt_jl(sph_rlm%nidx_rlm(2),sph_rtm%nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout)                                 &
     &           :: sp_rlm(ncomp*sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &           :: vr_rtm(ncomp*sph_rtm%nnod_rtm)
!
      integer(kind = kint) :: ip_rtm, in_rtm, i_rlm, j_rlm
      integer(kind = kint) :: k_rlm, l_rtm, nd
      integer(kind = kint) :: ip, kst, ked
      integer(kind = kint) :: mp_rlm, mn_rlm, jst, jed
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: vr1, vr2, vr3
      real(kind = kreal) :: Pg3_j(sph_rlm%nidx_rlm(2))
      real(kind = kreal) :: dPdt_j(sph_rlm%nidx_rlm(2))
      real(kind = kreal) :: Pgv_j(sph_rlm%nidx_rlm(2))
!
!
      if(nvector .le. 0) return
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,jst,jed,                           &
!$omp&                    j_rlm,l_rtm,nd,i_rlm,ip_rtm,in_rtm,k_rlm,     &
!$omp&                    mp_rlm,mn_rlm,vr1,vr2,vr3,Pg3_j,dPdt_j,Pgv_j, &
!$omp&                    a1r_1d_rlm_r,a2r_1d_rlm_r)
      do ip = 1, np_smp
        kst = sph_rtm%istack_rtm_kr_smp(ip-1) + 1
        ked = sph_rtm%istack_rtm_kr_smp(ip  )
        do k_rlm = kst, ked
          a1r_1d_rlm_r = sph_rlm%a_r_1d_rlm_r(k_rlm)
          a2r_1d_rlm_r = a1r_1d_rlm_r * a1r_1d_rlm_r
          do j_rlm = 1, sph_rlm%nidx_rlm(2)
            do nd = 1, nvector
              i_rlm = 3*nd                                              &
     &               + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)        &
     &               + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
              sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) * a2r_1d_rlm_r
              sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1) * a1r_1d_rlm_r
              sp_rlm(i_rlm  ) = sp_rlm(i_rlm  ) * a1r_1d_rlm_r
            end do
          end do
        end do
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst = idx_trns%lstack_rlm(mp_rlm-1) + 1
          jed = idx_trns%lstack_rlm(mp_rlm)
          do k_rlm = kst, ked
            do l_rtm = 1, sph_rtm%nidx_rtm(2)
              do j_rlm = jst, jed
                Pg3_j(j_rlm) = P_jl(j_rlm,l_rtm) * g_sph_rlm(j_rlm,3)
                dPdt_j(j_rlm) = dPdt_jl(j_rlm,l_rtm)
              end do
!
              do nd = 1, nvector
                vr1 = 0.0d0
                vr2 = 0.0d0
                vr3 = 0.0d0
                do j_rlm = jst, jed
                  i_rlm = 3*nd                                          &
     &                   + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)    &
     &                   + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
                  vr1 = vr1 + sp_rlm(i_rlm-2) * Pg3_j(j_rlm)
                  vr2 = vr2 + sp_rlm(i_rlm-1) * dPdt_j(j_rlm)
                  vr3 = vr3 - sp_rlm(i_rlm  ) * dPdt_j(j_rlm)
                end do
                ip_rtm = 3*nd                                           &
     &                  + ncomp*((l_rtm-1) *  sph_rtm%istep_rtm(2)      &
     &                         + (k_rlm-1) *  sph_rtm%istep_rtm(1)      &
     &                         + (mp_rlm-1) * sph_rtm%istep_rtm(3))
                vr_rtm(ip_rtm-2) = vr_rtm(ip_rtm-2) + vr1
                vr_rtm(ip_rtm-1) = vr_rtm(ip_rtm-1) + vr2
                vr_rtm(ip_rtm  ) = vr_rtm(ip_rtm  ) + vr3
              end do
            end do
          end do
        end do
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          mn_rlm = sph_rtm%nidx_rtm(3) - mp_rlm + 1
          jst = idx_trns%lstack_rlm(mp_rlm-1) + 1
          jed = idx_trns%lstack_rlm(mp_rlm)
          do k_rlm = kst, ked
            do l_rtm = 1, sph_rtm%nidx_rtm(2)
              do j_rlm = jst, jed
                Pgv_j(j_rlm) = -P_jl(j_rlm,l_rtm)                       &
     &                        * dble(sph_rlm%idx_gl_1d_rlm_j(j_rlm,3))  &
     &                        * asin_theta_1d_rtm(l_rtm)
              end do
!
              do nd = 1, nvector
                vr2 = 0.0d0
                vr3 = 0.0d0
                do j_rlm = jst, jed
                  i_rlm = 3*nd                                          &
     &                   + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)    &
     &                   + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
                  vr2 = vr2 + sp_rlm(i_rlm  ) * Pgv_j(j_rlm)
                  vr3 = vr3 + sp_rlm(i_rlm-1) * Pgv_j(j_rlm)
                end do
                in_rtm = 3*nd                                           &
     &                  + ncomp*((l_rtm-1) *  sph_rtm%istep_rtm(2)      &
     &                         + (k_rlm-1) *  sph_rtm%istep_rtm(1)      &
     &                         + (mn_rlm-1) * sph_rtm%istep_rtm(3))
                vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1) + vr2
                vr_rtm(in_rtm  ) = vr_rtm(in_rtm  ) + vr3
              end do
            end do
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_vector_org
!
! -----------------------------------------------------------------------
!
      subroutine legendre_b_trans_scalar_org                            &
     &         (ncomp, nvector, nscalar, sph_rlm, sph_rtm, idx_trns,    &
     &          P_jl, sp_rlm, vr_rtm)
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(index_4_sph_trans), intent(in) :: idx_trns
      real(kind= kreal), intent(in)                                     &
     &          :: P_jl(sph_rlm%nidx_rlm(2),sph_rtm%nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
      real(kind = kreal), intent(in) :: sp_rlm(ncomp*sph_rlm%nnod_rlm)
      real(kind = kreal), intent(inout)                                 &
     &           :: vr_rtm(ncomp*sph_rtm%nnod_rtm)
!
      integer(kind = kint) :: i_rlm, j_rlm, k_rlm, l_rtm
      integer(kind = kint) :: ip_rtm, nd, ip, kst, ked
      integer(kind = kint) :: mp_rlm, jst, jed
      real(kind = kreal) :: vr1
      real(kind = kreal) :: P_j(sph_rlm%nidx_rlm(2))
!
!
      if(nscalar .le. 0) return
!
!$omp parallel do schedule(static)                                      &
!$omp&            private(ip,kst,ked,j_rlm,l_rtm,nd,k_rlm,              &
!$omp&                    ip_rtm,i_rlm,mp_rlm,jst,jed,vr1,P_j)
      do ip = 1, np_smp
        kst = sph_rtm%istack_rtm_kr_smp(ip-1) + 1
        ked = sph_rtm%istack_rtm_kr_smp(ip  )
!
        do mp_rlm = 1, sph_rtm%nidx_rtm(3)
          jst = idx_trns%lstack_rlm(mp_rlm-1) + 1
          jed = idx_trns%lstack_rlm(mp_rlm)
          do k_rlm = kst, ked
!
            do l_rtm = 1, sph_rtm%nidx_rtm(2)
              P_j(jst:jed) = P_jl(jst:jed,l_rtm)
              do nd = 1, nscalar
                vr1 = 0.0d0
                do j_rlm = jst, jed
                  i_rlm = nd + 3*nvector                                &
     &                   + ncomp * ((j_rlm-1) * sph_rlm%istep_rlm(2)    &
     &                   + (k_rlm-1) * sph_rlm%istep_rlm(1))
!
                  vr1 = vr1 + sp_rlm(i_rlm) * P_j(j_rlm)
                end do
                ip_rtm = nd + 3*nvector                                 &
     &                  + ncomp*((l_rtm-1) *  sph_rtm%istep_rtm(2)      &
     &                         + (k_rlm-1) *  sph_rtm%istep_rtm(1)      &
     &                         + (mp_rlm-1) * sph_rtm%istep_rtm(3))
                vr_rtm(ip_rtm) = vr1
              end do
            end do
!
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine legendre_b_trans_scalar_org
!
! -----------------------------------------------------------------------
!
      end module legendre_bwd_trans_org
