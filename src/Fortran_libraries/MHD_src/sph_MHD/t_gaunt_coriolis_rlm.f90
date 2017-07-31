!>@file   t_gaunt_coriolis_rlm.f90
!!@brief  module t_gaunt_coriolis_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1994
!@n     Modified in Dec., 2013
!
!>@brief Adams-Gaunt integrals for Coriolis term
!!       and coefficients for Coriolis term on f(r,l,m)
!!
!!@verbatim
!!      subroutine alloc_gaunt_coriolis_rlm(jmax_rlm, gt_cor)
!!      subroutine alloc_coriolis_coef_tri_rlm(jmax_rlm, gt_cor)
!!      subroutine dealloacte_gaunt_coriolis_rlm
!!      subroutine dealloc_coriolis_coef_tri_rlm
!!
!!      integer function find_local_sph_rlm_address(jmax_rlm,           &
!!     &       idx_gl_1d_rlm_j, l_gl, m_gl)
!!      integer function find_local_radius_rlm_address(nri_rlm,         &
!!     &       idx_gl_1d_rlm_r, kr_gl)
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
!!*              jgi_rlm(j3,1) : index for gi_rlm(j3,1)
!!*              jgi_rlm(j3,2) : index for gi_rlm(j3,2)
!!*      ei_rlm(j3,1) : elsasser integral for Coriolis term
!!*              jei_rlm(j3,1) : index for ei_rlm(j3,1)
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
!!@endverbatim
!!
      module t_gaunt_coriolis_rlm
!*
      use m_precision
      use m_constants
!
      implicit none
!
!>        Gunat integrals of Coriolis term
      type gaunt_coriolis_rlm
!>        Local address for Coriolis term using Gaunt integral
        integer(kind = kint), allocatable :: jgi_rlm(:,:)
!>        Local address for Coriolis term using Elsasser integral
        integer(kind = kint), allocatable :: jei_rlm(:,:)
!
!>        Gaunt integral for Coriolis term
        real(kind = kreal), allocatable :: gi_rlm(:,:)
!>        Elsasser integral for Coriolis term
        real(kind = kreal), allocatable :: ei_rlm(:,:)
!
!
!>        Coefficients for curl of Coriolis force for poloidal vorticity
        real(kind = kreal), allocatable :: sw_rlm(:,:,:)
!>        Coefficients for curl of Coriolis force for toroidal vorticity
        real(kind = kreal), allocatable :: tw_rlm(:,:,:)
!>        Coefficients for divergence of Coriolis force 
!!         for poloidal vorticity by poloidal velocity
        real(kind = kreal), allocatable :: sd_rlm(:,:,:)
!>        Coefficients for divergence of Coriolis force 
!!         for poloidal vorticity by Toroidal velocity
        real(kind = kreal), allocatable :: td_rlm(:,:)
!>        Coefficients for radial compoonent of Coriolis force 
!!         for poloidal vorticity by poloidal velocity
        real(kind = kreal), allocatable :: sr_rlm(:,:)
!>        Coefficients for radial compoennt of Coriolis force 
!!         for poloidal vorticity by Toroidal velocity
        real(kind = kreal), allocatable :: tr_rlm(:,:)
      end type gaunt_coriolis_rlm
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine alloc_gaunt_coriolis_rlm(jmax_rlm, gt_cor)
!
      integer(kind = kint), intent(in) :: jmax_rlm
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!
!
      allocate( gt_cor%jgi_rlm(jmax_rlm,2) )
      allocate( gt_cor%jei_rlm(jmax_rlm,1) )
      allocate( gt_cor%gi_rlm(jmax_rlm,2) )
      allocate( gt_cor%ei_rlm(jmax_rlm,1) )
!
      if(jmax_rlm .le. 0) return
      gt_cor%jgi_rlm = 0
      gt_cor%jei_rlm = 0
      gt_cor%gi_rlm = 0.0d0
      gt_cor%ei_rlm = 0.0d0
!
      end subroutine alloc_gaunt_coriolis_rlm
!
!-----------------------------------------------------------------------
!
      subroutine alloc_coriolis_coef_tri_rlm(jmax_rlm, gt_cor)
!
      integer(kind = kint), intent(in) :: jmax_rlm
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!
!
      allocate( gt_cor%sw_rlm(2,3,jmax_rlm) )
      allocate( gt_cor%tw_rlm(2,4,jmax_rlm) )
!
      allocate( gt_cor%sd_rlm(2,2,jmax_rlm) )
      allocate( gt_cor%td_rlm(2,jmax_rlm) )
!
      allocate( gt_cor%sr_rlm(2,jmax_rlm) )
      allocate( gt_cor%tr_rlm(2,jmax_rlm) )
!
      if(jmax_rlm .le. 0) return
      gt_cor%sw_rlm = zero
      gt_cor%tw_rlm = zero
!
      gt_cor%sd_rlm = zero
      gt_cor%td_rlm = zero
!
      gt_cor%sr_rlm = zero
      gt_cor%tr_rlm = zero
!
      end subroutine alloc_coriolis_coef_tri_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloacte_gaunt_coriolis_rlm(gt_cor)
!
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!
!
      deallocate(gt_cor%jgi_rlm, gt_cor%gi_rlm)
      deallocate(gt_cor%jei_rlm, gt_cor%ei_rlm)
!
      end subroutine dealloacte_gaunt_coriolis_rlm
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_coriolis_coef_tri_rlm(gt_cor)
!
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
!
      deallocate(gt_cor%sw_rlm, gt_cor%tw_rlm)
      deallocate(gt_cor%sd_rlm, gt_cor%td_rlm)
      deallocate(gt_cor%sr_rlm, gt_cor%tr_rlm)
!
      end subroutine dealloc_coriolis_coef_tri_rlm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer function find_local_sph_rlm_address(jmax_rlm,             &
     &       idx_gl_1d_rlm_j, l_gl, m_gl)
!
      integer(kind = kint), intent(in) :: jmax_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(jmax_rlm,3)
      integer(kind = kint), intent(in) :: l_gl, m_gl
!
      integer(kind = kint) :: j
!
!
      find_local_sph_rlm_address = 0
      do j = 1, jmax_rlm
        if (   idx_gl_1d_rlm_j(j,2) .eq. l_gl                           &
     &   .and. idx_gl_1d_rlm_j(j,3) .eq. m_gl) then
          find_local_sph_rlm_address = j
          return
        end if
      end do
!
      end function find_local_sph_rlm_address
!
!-----------------------------------------------------------------------
!
      integer function find_local_radius_rlm_address(nri_rlm,           &
     &       idx_gl_1d_rlm_r, kr_gl)
!
      integer(kind = kint), intent(in) :: nri_rlm
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_r(nri_rlm)
      integer(kind = kint), intent(in) :: kr_gl
!
      integer(kind = kint) :: k
!
!
      find_local_radius_rlm_address = 0
      do k = 1, nri_rlm
        if (idx_gl_1d_rlm_r(k) .eq. kr_gl) then
          find_local_radius_rlm_address = k
          return
        end if
      end do
!
      end function find_local_radius_rlm_address
!
!-----------------------------------------------------------------------
!
      end module t_gaunt_coriolis_rlm
