!>@file   m_coriolis_coefs_tri_rlm.f90
!!@brief  module m_coriolis_coefs_tri_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1994
!!@n Modified in 2010
!
!>@brief  Coefficients for Coriolis term on f(r,l,m)
!!
!!@verbatim
!!      subroutine alloc_coriolis_coef_tri_rlm(jmax_rlm)
!!      subroutine dealloc_coriolis_coef_tri_rlm
!!      subroutine interact_rot_coriolis_rlm(jmax_rlm)
!!
!!*************************************************
!!*
!!*  Rotation of the Coriolos term
!!*     (wss) = wss(jc,1,j3)*w*dyb/r**2
!!*            + wss(jc,2,j3)*dw*yb/r**2
!!*
!!*     (wts) = wts(j3)*w*yb/r**2
!!*
!!*     (wst) = wst(1,j3)*( dw*dyb/r**2 + w*d2yb/r**2 - 2*w*dyb/r**3 )
!!*            + wst(2,j3)*( d2w/r**2 - 2*dw/r**3 )*yb
!!*
!!*     (wtt) = wtt(jc,1,j3)*dw*yb/r**2
!!*            + wtt(jc,2,j3)*w*( dyb/r**2 - 2*yb/r**3 )
!!*
!!*   Divergence of the Coriolis term
!!*     (wsd) = wsd(jc,1,j3)*w*wsb/r**4
!!*            + wsd(jc,2,j3)*dw*dwsb/r**2
!!*     (wtd) = wtd(j3)*dw*dwtb/r**2
!!*
!!*  Radial componenet of the Coriolis term
!!*     (wsr) = wsr(jc,1,j3)*dw*dusb/r**2
!!*     (wtr) = wtr(j3)*dw*wtb/r**2
!!
!!*************************************************
!!*
!!*************************************************
!!*
!!*     wss(jc,1,j3) = sw_rlm(jc,1,j3)
!!*     wss(jc,2,j3) = sw_rlm(jc,2,j3)
!!*     wts(jc,j3)   = sw_rlm(jc,3,j3)
!!*     wst(jc,1,j3) = tw_rlm(jc,1,j3)
!!*     wst(jc,2,j3) = tw_rlm(jc,2,j3)
!!*     wtt(jc,1,j3) = tw_rlm(jc,3,j3)
!!*     wtt(jc,2,j3) = tw_rlm(jc,4,j3)
!!*
!!*     wsd(jc,1,j3) = sd_rlm(jc,1,j3)
!!*     wsd(jc,2,j3) = sd_rlm(jc,2,j3)
!!*     wtd(jc,j3)   = td_rlm(jc,j3)
!!*
!!*     wsr(jc,j3) =   sr_rlm(jc,j3)
!!*     wtr(jc,j3) =   tr_rlm(jc,j3)
!!*
!!*************************************************
!!*
!!*************************************************************
!!*
!!*      gk_cor(j3,jk,2) : gaunt integral for Coriolis term
!!*              jgi_cor_rlm(j3,1) : index for gi_cor_rlm(j3,1)
!!*              jgi_cor_rlm(j3,2) : index for gi_cor_rlm(j3,2)
!!*      ei_cor_rlm(j3,1) : elsasser integral for Coriolis term
!!*              jei_cor_rlm(j3,1) : index for ei_cor_rlm(j3,1)
!!*
!!*************************************************************
!!*
!!*******************************************************************
!!*                                                                 *
!!*  Adams - Gaunt integrals                                        *
!!*                                                                 *
!!*  (m1,m2,m3)  //  (m1)   (m2)   (m3)                             *
!!* Ki         = || Y    * Y    * Y     sin(theta) d(theta)d(phi)   *
!!*  (l1,l2,l3)  //  (l1)   (l2)   (l3)                             *
!!*                                                                 *
!!*                            (m2)        (m3)                     *
!!*  (m1,m2,m3)  //  (m1)    dy(l2)      dy(l3)                     *
!!* Li         = || Y    *[ -------- * ----------                   *
!!*  (l1,l2,l3)  //  (l1)   d(theta)    d(phi)                      *
!!*                                                                 *
!!*                    (m2)        (m3)                             *
!!*                  dy(l2)      dy(l3)                             *
!!*              - ---------- * -------- ] d(theta)d(phi)           *
!!*                  d(phi)     d(theta)                            *
!!*                                                                 *
!!*  where                                                          *
!!*                   (m)   (m)  | sin(m*phi) |                     *
!!*                  Y   = P   * |   1        |                     *
!!*                   (l)   (l)  | cos(m*phi) |                     *
!!*                                                                 *
!!*                         (m)     2(l-m)!     1                   *
!!*                        P   = [----------]**--- * P(l,m)         *
!!*                         (l)      (l+m)!     2                   *
!!*                         (0)                                     *
!!*                        P   =           P(l,0)                   *
!!*                         (l)                                     *
!!*                                                                 *
!!*******************************************************************
!!
!!@endverbatim
!!
      module m_coriolis_coefs_tri_rlm
!*
      use m_precision
      use m_constants
!
      implicit none
!
!>      Coefficients for curl of Coriolis force for poloidal vorticity
      real(kind = kreal), allocatable :: sw_rlm(:,:,:)
!>      Coefficients for curl of Coriolis force for toroidal vorticity
      real(kind = kreal), allocatable :: tw_rlm(:,:,:)
!>      Coefficients for divergence of Coriolis force 
!!       for poloidal vorticity by poloidal velocity
      real(kind = kreal), allocatable :: sd_rlm(:,:,:)
!>      Coefficients for divergence of Coriolis force 
!!       for poloidal vorticity by Toroidal velocity
      real(kind = kreal), allocatable :: td_rlm(:,:)
!>      Coefficients for radial compoonent of Coriolis force 
!!       for poloidal vorticity by poloidal velocity
      real(kind = kreal), allocatable :: sr_rlm(:,:)
!>      Coefficients for radial compoennt of Coriolis force 
!!       for poloidal vorticity by Toroidal velocity
      real(kind = kreal), allocatable :: tr_rlm(:,:)
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine alloc_coriolis_coef_tri_rlm(jmax_rlm)
!
      integer(kind = kint), intent(in) :: jmax_rlm
!
!
      allocate( sw_rlm(2,3,jmax_rlm) )
      allocate( tw_rlm(2,4,jmax_rlm) )
!
      allocate( sd_rlm(2,2,jmax_rlm) )
      allocate( td_rlm(2,jmax_rlm) )
!
      allocate( sr_rlm(2,jmax_rlm) )
      allocate( tr_rlm(2,jmax_rlm) )
!
      sw_rlm = zero
      tw_rlm = zero
!
      sd_rlm = zero
      td_rlm = zero
!
      sr_rlm = zero
      tr_rlm = zero
!
      end subroutine alloc_coriolis_coef_tri_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_coriolis_coef_tri_rlm
!
      deallocate(sw_rlm, tw_rlm)
      deallocate(sd_rlm, td_rlm)
      deallocate(sr_rlm, tr_rlm)
!
      end subroutine dealloc_coriolis_coef_tri_rlm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine interact_rot_coriolis_rlm(jmax_rlm)
!
      use m_schmidt_poly_on_rtm
      use m_gaunt_coriolis_rlm
!
      integer(kind = kint), intent(in) :: jmax_rlm
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
        tr_rlm(1,j3) = -( two+g_sph_rlm(j3,4)-g_sph_rlm(j3,3) )         &
     &               * gi_cor_rlm(j3,1) * g_sph_rlm(j3,17) * half
        tr_rlm(2,j3) = -( two-g_sph_rlm(j3,5)+g_sph_rlm(j3,3) )         &
     &               * gi_cor_rlm(j3,2) * g_sph_rlm(j3,17) * half
!*
        sr_rlm(1,j3) = ei_cor_rlm(j3,1) * g_sph_rlm(j3,17)
        sr_rlm(2,j3) = zero
      end do
!$omp end parallel do
!*
      end subroutine interact_rot_coriolis_rlm
!
! -----------------------------------------------------------------------
!
      end module m_coriolis_coefs_tri_rlm
