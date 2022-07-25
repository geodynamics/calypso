!>@file   interact_coriolis_rlm.f90
!!@brief  module interact_coriolis_rlm
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2013
!
!>@brief Evaluate Adams-Gaunt integrals for Coriolis term
!!       and coefficients for Coriolis term on f(r,l,m)
!!
!!@verbatim
!!      subroutine cal_gaunt_coriolis_rlm(l_truncation,                 &
!!     &         (l_truncation, jmax_rlm, idx_gl_1d_rlm_j,              &
!!     &          jgi_cor_rlm, jei_cor_rlm, gi_cor_rlm, ei_cor_rlm)
!!      subroutine interact_rot_coriolis_rlm                            &
!!     &         (jmax_rlm, g_sph_rlm, gi_cor_rlm, ei_cor_rlm,          &
!!     &          sw_rlm, tw_rlm, sd_rlm, td_rlm, sr_rlm, tr_rlm,       &
!!     &          sh_rlm, hh_rlm, th_rlm)
!!
!!        tr_rlm(i,j3) = -(a(a+1) + b(b+1) - l(l+1)) * G_abl            &
!!                        / (2l(l+1) N_l)
!!        sr_rlm(i,j3) = E_abl / (l(l+1) N_l)
!!
!!        th_rlm(i,j3) = a(a+1) * (-a(a+1) + b(b+1) + l(l+1))           &
!!                      * G_abl / (2l(l+1) N_l)
!!        sh_rlm(i,j3) = b(b+1) * E_abl / (l(l+1) N_l)
!!        hh_rlm(i,j3) = a(a+1) * E_abl / (l(l+1) N_l)
!!
!!        sw_rlm(i,1,j3) = -a(a+1) * (-a(a+1) + b(b+1) + l(l+1))        &
!!                        * G_abl / (2l(l+1) N_l)
!!        sw_rlm(i,2,j3) =  b(b+1) * ( a(a+1) - b(b+1) + l(l+1))        &
!!                         * G_abl / (2l(l+1) N_l)
!!        sw_rlm(i,3,j3) =  a(a+1) * E_abl / (l(l+1) N_l)
!!
!!        tw_rlm(i,0,j3) = -(a(a+1) + b(b+1) - l(l+1))                  &
!!                        * E_abl / (l(l+1) N_l)                        &
!!                       = - hh_rlm(i,j3) - sh_rlm(i,j3)                &
!!                        + l(l+1) * sr_rlm(i,j3)
!!        tw_rlm(i,1,j3) = -a(a+1) * E_abl / (l(l+1) N_l)               &
!!                       = - hh_rlm(i,j3)
!!        tw_rlm(i,2,j3) = -l(l+1) * E_abl / (l(l+1) N_l)               &
!!                       = - l(l+1) * sr_rlm(i,j3)
!!        tw_rlm(i,3,j3) = -(l(l+1)*(a(a+1) + b(b+1) - l(l+1))          &
!!                          + a(a+1)*(-a(a+1) + b(b+1) + l(l+1)))       &
!!                        * G_abl / (2l(l+1) N_l)                       &
!!                       = l(l+1)*tr_rlm(i,j3) - th_rlm(i,j3))
!!        tw_rlm(i,4,j3) = -a(a+1) * (-a(a+1) + b(b+1) + l(l+1))        &
!!                        * G_abl/ (2l(l+1) N_l)                        &
!!                       = -th_rlm(i,j3)
!!
!!        sd_rlm(i,1,j3) = l(l+1) * G_abl / (l(l+1) N_l)
!!        sd_rlm(i,2,j3) = (a(a+1) + b(b+1) - l(l+1)) * G_abl           &
!!                        / (2l(l+1) N_l)                               &
!!                       = - tr_rlm(i,j3)
!!        td_rlm(i,1,j3) = E_abl / (l(l+1) N_l)                         &
!!                       = sr_rlm(i,j3)
!!@endverbatim
!!
!!@param   l_truncation   Truncation level
!!@param   jmax_rlm       Number of local hermonics modes
!!@param   idx_gl_1d_rlm_j(jmax_rlm,3)  Spherical harmonics modes list
!
      module interact_coriolis_rlm
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_gaunt_coriolis_rlm
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_gaunt_coriolis_rlm                                 &
     &         (l_truncation, jmax_rlm, idx_gl_1d_rlm_j,                &
     &          jgi_cor_rlm, jei_cor_rlm, gi_cor_rlm, ei_cor_rlm)
!
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint), intent(in) :: jmax_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(jmax_rlm,3)
!
      integer(kind = kint), intent(inout) :: jgi_cor_rlm(jmax_rlm,2)
      integer(kind = kint), intent(inout) :: jei_cor_rlm(jmax_rlm,1)
      real(kind = kreal), intent(inout) :: gi_cor_rlm(jmax_rlm,2)
      real(kind = kreal), intent(inout) :: ei_cor_rlm(jmax_rlm,1)
!
      integer(kind = kint) :: l3, m3, j3, l2, m2, j_rlm
      integer(kind = kint) :: l2_gl_k1, l2_gl_k2, l2_gl_l1
      integer(kind = kint) :: m2_gl_k1, m2_gl_k2, m2_gl_l1
!
!
      do j_rlm = 1, jmax_rlm
        j3 = idx_gl_1d_rlm_j(j_rlm,1)
        l3 = idx_gl_1d_rlm_j(j_rlm,2)
        m3 = idx_gl_1d_rlm_j(j_rlm,3)
!
        l2 = l3 - 1
        if(l2.ge.1 .and. l2.le.l_truncation .and. abs(m3).le.l2) then
          gi_cor_rlm(j_rlm,1) = leadki(ione, izero, l2, m3, l3, m3)
          l2_gl_k1 = l2
          m2_gl_k1 = m3
        else
          gi_cor_rlm(j_rlm,1) = zero
          l2_gl_k1 = l3
          m2_gl_k1 = m3
        end if
!
        l2 = l3 + 1
        if(l2.ge.1 .and. l2.le.l_truncation .and. abs(m3).le.l2) then
          gi_cor_rlm(j_rlm,2) = leadki(ione, izero, l2, m3, l3, m3)
          l2_gl_k2 = l2
          m2_gl_k2 = m3
        else
          gi_cor_rlm(j_rlm,2) = zero
          l2_gl_k2 = l3
          m2_gl_k2 = m3
        end if
!
        if(l3 .ge. 1) then
          m2 = -m3
          ei_cor_rlm(j_rlm,1) = leadli(ione, izero, l3, m2, l3, m3)
          l2_gl_l1 = l3
          m2_gl_l1 = m2
        end if
!
        if(j3 .eq. 0) then
          l2_gl_k1 = izero
          m2_gl_k1 = izero
          l2_gl_k2 = izero
          m2_gl_k2 = izero
          l2_gl_l1 = izero
          m2_gl_l1 = izero
          gi_cor_rlm(j_rlm,2) = zero
          ei_cor_rlm(j_rlm,1) = zero
        end if
!
        jgi_cor_rlm(j_rlm,1) = find_local_sph_rlm_address(jmax_rlm,     &
     &                          idx_gl_1d_rlm_j, l2_gl_k1, m2_gl_k1)
        jgi_cor_rlm(j_rlm,2) = find_local_sph_rlm_address(jmax_rlm,     &
     &                          idx_gl_1d_rlm_j, l2_gl_k2, m2_gl_k2)
        jei_cor_rlm(j_rlm,1) = find_local_sph_rlm_address(jmax_rlm,     &
     &                          idx_gl_1d_rlm_j, l2_gl_l1, m2_gl_l1)
      end do
!
      end subroutine cal_gaunt_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      subroutine interact_rot_coriolis_rlm                              &
     &         (jmax_rlm, g_sph_rlm, gi_cor_rlm, ei_cor_rlm,            &
     &          sw_rlm, tw_rlm, sd_rlm, td_rlm, sr_rlm, tr_rlm,         &
     &          sh_rlm, hh_rlm, th_rlm)
!
      integer(kind = kint), intent(in) :: jmax_rlm
      real(kind = kreal), intent(in):: g_sph_rlm(jmax_rlm,17)
!
      real(kind = kreal), intent(in) :: gi_cor_rlm(jmax_rlm,2)
      real(kind = kreal), intent(in) :: ei_cor_rlm(jmax_rlm,1)
!
      real(kind = kreal), intent(inout) :: sw_rlm(2,3,jmax_rlm)
      real(kind = kreal), intent(inout) :: tw_rlm(2,4,jmax_rlm)
      real(kind = kreal), intent(inout) :: sd_rlm(2,2,jmax_rlm)
      real(kind = kreal), intent(inout) :: td_rlm(2,jmax_rlm)
      real(kind = kreal), intent(inout) :: sr_rlm(2,jmax_rlm)
      real(kind = kreal), intent(inout) :: tr_rlm(2,jmax_rlm)
      real(kind = kreal), intent(inout) :: sh_rlm(2,jmax_rlm)
      real(kind = kreal), intent(inout) :: hh_rlm(2,jmax_rlm)
      real(kind = kreal), intent(inout) :: th_rlm(2,jmax_rlm)
!
      integer(kind = kint) :: j3
!
!
!$omp parallel do private(j3)
      do j3 = 1, jmax_rlm
        sw_rlm(1,1,j3) = ( two-g_sph_rlm(j3,4)-g_sph_rlm(j3,3) )        &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        sw_rlm(2,1,j3) = ( two-g_sph_rlm(j3,5)-g_sph_rlm(j3,3) )        &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17) 
!*
        sw_rlm(1,2,j3) = g_sph_rlm(j3,4)                                &
     &               * ( two-g_sph_rlm(j3,4)+g_sph_rlm(j3,3) )          &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17) * half
        sw_rlm(2,2,j3) =  g_sph_rlm(j3,5)                               &
     &               *( two-g_sph_rlm(j3,5)+g_sph_rlm(j3,3) )           &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17) * half
!*
        sw_rlm(1,3,j3) =  two * ei_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        sw_rlm(2,3,j3) =  zero
!*
        tw_rlm(1,3,j3) =-( g_sph_rlm(j3,3)                              &
     &               * ( two+g_sph_rlm(j3,4)-g_sph_rlm(j3,3) )          &
     &               + two*( -two+g_sph_rlm(j3,4)+g_sph_rlm(j3,3) ) )   &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17) * half
        tw_rlm(2,3,j3) =-( g_sph_rlm(j3,3)                              &
     &               * ( two+g_sph_rlm(j3,5)-g_sph_rlm(j3,3) )          &
     &               + two*( -two+g_sph_rlm(j3,5)+g_sph_rlm(j3,3) ) )   &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17) * half
!*
        tw_rlm(1,4,j3) =-( -two+g_sph_rlm(j3,4)+g_sph_rlm(j3,3) )       &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        tw_rlm(2,4,j3) =-( -two+g_sph_rlm(j3,5)+g_sph_rlm(j3,3) )       &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17)
!*
        tw_rlm(1,1,j3) =-two * ei_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        tw_rlm(2,1,j3) = zero
        tw_rlm(1,2,j3) =-g_sph_rlm(j3,3) * ei_cor_rlm(j3,1)             &
     &               * g_sph_rlm(j3,17)
        tw_rlm(2,2,j3) = zero
!
!
        sd_rlm(1,1,j3) = g_sph_rlm(j3,4) * gi_cor_rlm(j3,1)             &
     &               * g_sph_rlm(j3,17)
        sd_rlm(2,1,j3) = g_sph_rlm(j3,5) * gi_cor_rlm(j3,2)             &
     &               * g_sph_rlm(j3,17)
!*
        sd_rlm(1,2,j3) = ( two+g_sph_rlm(j3,4)-g_sph_rlm(j3,3) )        &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17) * half
        sd_rlm(2,2,j3) = ( two-g_sph_rlm(j3,5)+g_sph_rlm(j3,3) )        &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17) * half
!*
        td_rlm(1,j3) = ei_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        td_rlm(2,j3) = zero
!*
!
        tr_rlm(1,j3) = -(two + g_sph_rlm(j3,4) - g_sph_rlm(j3,3))       &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17) * half
        tr_rlm(2,j3) = -(two - g_sph_rlm(j3,5) + g_sph_rlm(j3,3))       &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17) * half
!*
        sr_rlm(1,j3) = ei_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        sr_rlm(2,j3) = zero
!*
        th_rlm(1,j3) = (-two + g_sph_rlm(j3,4) + g_sph_rlm(j3,3))       &
     &                * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        th_rlm(2,j3) = (-two + g_sph_rlm(j3,5) + g_sph_rlm(j3,3))       &
     &                * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17)
!
        sh_rlm(1,j3) = g_sph_rlm(j3,3) * ei_cor_rlm(j3,1)               &
     &                * g_sph_rlm(j3,17)
        sh_rlm(2,j3) = zero
!
        hh_rlm(1,j3) = two * ei_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        hh_rlm(2,j3) = zero
      end do
!$omp end parallel do
!*
      end subroutine interact_rot_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      end module interact_coriolis_rlm
