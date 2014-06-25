!>@file   m_coriolis_terms_rlm.f90
!!@brief  module m_coriolis_terms_rlm
!!
!!@author H. Matsui
!!@date Programmed in 1995
!@n     Modified in Dec., 2013
!
!>@brief  Coriolis terms array
!!
!!@verbatim
!!************************************************
!!
!!      subroutine allocate_d_coriolis_rlm
!!      subroutine deallocate_d_coriolis_rlm
!!
!!************************************************
!!
!!  Rotation of the Coriolos term
!!     (wss) = wss(jc,1,j3)*w*dyb/r**2
!!            + wss(jc,2,j3)*dw*yb/r**2
!!
!!     (wts) = wts(j3)*w*yb/r**2
!!
!!     (wst) = wst(1,j3)*( dw*dyb/r**2 + w*d2yb/r**2 - 2*w*dyb/r**3 )
!!            + wst(2,j3)*( d2w/r**2 - 2*dw/r**3 )*yb
!!
!!     (wtt) = wtt(jc,1,j3)*dw*yb/r**2
!!            + wtt(jc,2,j3)*w*( dyb/r**2 - 2*yb/r**3 )
!!
!!   Divergence of the Coriolis term
!!     (wsd) = wsd(jc,1,j3)*w*wsb/r**4
!!            + wsd(jc,2,j3)*dw*dwsb/r**2
!!     (wtd) = wtd(j3)*dw*dwtb/r**2
!!
!!  Radial componenet of the Coriolis term
!!     (wsr) = wsr(jc,1,j3)*dw*dusb/r**2
!!     (wtr) = wtr(j3)*dw*wtb/r**2
!!
!!************************************************
!!
!!************************************************
!!
!!     wss(jc,1,j3) = sw_rj(jc,1,j3)
!!     wss(jc,2,j3) = sw_rj(jc,2,j3)
!!     wts(jc,j3)   = sw_rj(jc,3,j3)
!!     wst(jc,1,j3) = tw_rj(jc,1,j3)
!!     wst(jc,2,j3) = tw_rj(jc,2,j3)
!!     wtt(jc,1,j3) = tw_rj(jc,3,j3)
!!     wtt(jc,2,j3) = tw_rj(jc,4,j3)
!!
!!     wsd(jc,1,j3) = sd_rj(jc,1,j3)
!!     wsd(jc,2,j3) = sd_rj(jc,2,j3)
!!     wtd(jc,j3)   = td_rj(jc,j3)
!!
!!     wsr(jc,j3) =   sr_rj(jc,j3)
!!     wtr(jc,j3) =   tr_rj(jc,j3)
!!
!!************************************************
!!@endverbatim
!!
!!@n @param coef_cor  Coefficient for the Coriolis term
!
!
      module m_coriolis_terms_rlm
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      implicit none
!
!
!>        local spectr index for ICB and  @f$ m = -1, 0, 1@f$.
!!        for @f$ f(r,l,m) @f$
!!@n        If spectr data do not exist in subdomain,
!!@n        idx_rlm_degree_one(m) = 0.
      integer(kind = kint) :: idx_rlm_ICB = 0
!
!>        local spectr index for @f$ l = m = 0@f$ for @f$ f(r,l,m) @f$
!!@n        If spectr data do not exist in subdomain,
!!@n        idx_rlm_degree_one(m) = 0.
      integer (kind=kint) :: idx_rlm_degree_zero = 0
!>        local spectr index for @f$ l = 1@f$ and  @f$ m = -1, 0, 1@f$.
!!        for @f$ f(r,l,m) @f$
!!@n        If spectr data do not exist in subdomain,
!!@n        idx_rlm_degree_one(m) = 0.
      integer (kind=kint) :: idx_rlm_degree_one(-1:1) = (/0,0,0/)
!
      real(kind = kreal), allocatable :: d_cor_rlm(:,:)
!
      real(kind = kreal), allocatable :: d_cor_in_rlm(:)
      real(kind = kreal), allocatable :: d_cor_out_rlm(:)
!
!
      integer(kind = kint) :: ncomp_coriolis_rlm = 3
      integer(kind = kint) :: ip_rlm_rot_cor = 1
      integer(kind = kint) :: it_rlm_rot_cor = 2
      integer(kind = kint) :: ip_rlm_div_cor = 3
!
      integer(kind = kint) :: kr_in_U_rlm =  0
      integer(kind = kint) :: kr_out_U_rlm = 0
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine allocate_d_coriolis_rlm
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      ip_rlm_rot_cor = 1
      it_rlm_rot_cor = 2
      ip_rlm_div_cor = 3
!
      ncomp_coriolis_rlm = 3
      allocate( d_cor_rlm(ncomp_coriolis_rlm,nnod_rlm) )
!
      num = nidx_rlm(2)
      allocate( d_cor_in_rlm(num) )
      allocate( d_cor_out_rlm(num) )
!
      d_cor_rlm = 0.0d0
      d_cor_in_rlm =  0.0d0
      d_cor_out_rlm = 0.0d0
!
      end subroutine allocate_d_coriolis_rlm
!
!   ------------------------------------------------------------------
!   ------------------------------------------------------------------
!
      subroutine deallocate_d_coriolis_rlm
!
!
      deallocate(d_cor_rlm, d_cor_in_rlm, d_cor_out_rlm)
!
      end subroutine deallocate_d_coriolis_rlm
!
!   ------------------------------------------------------------------
!
      end module m_coriolis_terms_rlm
