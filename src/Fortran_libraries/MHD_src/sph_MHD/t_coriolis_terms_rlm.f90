!>@file   t_coriolis_terms_rlm.f90
!!@brief  module t_coriolis_terms_rlm
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
!!      subroutine init_sum_coriolis_rlm                                &
!!     &         (l_truncation, sph_rlm, sph_bc_U, leg, gt_cor, cor_rlm)
!!        type(phys_address), intent(in) :: b_trns
!!
!!      subroutine alloc_d_coriolis_rlm(nnod_rlm, jmax_rlm, cor_rlm)
!!      subroutine dealloc_d_coriolis_rlm(cor_rlm)
!!        integer(kind = kint), intent(in) :: nnod_rlm, jmax_rlm
!!        type(coriolis_rlm_data), intent(inout) :: cor_rlm
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
!
!
      module t_coriolis_terms_rlm
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
!
      use t_spheric_rlm_data
      use t_poloidal_rotation
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_boundary_params_sph_MHD
      use t_gaunt_coriolis_rlm
!
      implicit none
!
!
!>      Structure of Coriolis terms in@f$ f(r,l,m) @f$.
      type coriolis_rlm_data
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
!>        Number of compoennt of coriolis term data
        integer(kind = kint) :: ncomp_coriolis_rlm = 6
!>        Coriolis term
        real(kind = kreal), allocatable :: d_cor_rlm(:,:)
!>        Coriolis term at inner boundary
        real(kind = kreal), allocatable :: d_cor_in_rlm(:)
!>        Coriolis term at outer boundary
        real(kind = kreal), allocatable :: d_cor_out_rlm(:)
!
!        integer(kind = kint) :: kr_in_U_rlm =  0
!        integer(kind = kint) :: kr_out_U_rlm = 0
      end type coriolis_rlm_data
!
      private :: alloc_d_coriolis_rlm
!
!   ------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_sum_coriolis_rlm                                  &
     &         (l_truncation, sph_rlm, sph_bc_U, leg, gt_cor, cor_rlm)
!
      use calypso_mpi
      use interact_coriolis_rlm
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_boundary_type), intent(in) :: sph_bc_U
      type(legendre_4_sph_trans), intent(in) :: leg
      integer(kind = kint), intent(in) :: l_truncation
!
      type(gaunt_coriolis_rlm), intent(inout) :: gt_cor
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
      integer(kind = kint) :: m
!
!
      call alloc_gaunt_coriolis_rlm(sph_rlm%nidx_rlm(2), gt_cor)
      call alloc_coriolis_coef_tri_rlm(sph_rlm%nidx_rlm(2), gt_cor)
      call alloc_d_coriolis_rlm                                         &
     &   (sph_rlm%nnod_rlm, sph_rlm%nidx_rlm(2), cor_rlm)
!
!
      cor_rlm%idx_rlm_ICB                                               &
     &            = find_local_radius_rlm_address(sph_rlm%nidx_rlm(1),  &
     &             sph_rlm%idx_gl_1d_rlm_r, sph_bc_U%kr_in)
      cor_rlm%idx_rlm_degree_zero                                       &
     &           = find_local_sph_rlm_address(sph_rlm%nidx_rlm(2),      &
     &                      sph_rlm%idx_gl_1d_rlm_j, izero, izero)
      do m = -1, 1
        cor_rlm%idx_rlm_degree_one(m)                                   &
     &           = find_local_sph_rlm_address(sph_rlm%nidx_rlm(2),      &
     &                         sph_rlm%idx_gl_1d_rlm_j, ione, m)
      end do
!
!
      if(iflag_debug.eq.1) write(*,*) 'cal_gaunt_coriolis_rlm'
      call cal_gaunt_coriolis_rlm                                       &
     &   (l_truncation, sph_rlm%nidx_rlm(2), sph_rlm%idx_gl_1d_rlm_j,   &
     &    gt_cor%jgi_rlm, gt_cor%jei_rlm,                               &
     &    gt_cor%gi_rlm, gt_cor%ei_rlm)
!
      if(iflag_debug.eq.1) write(*,*) 'interact_rot_coriolis_rlm'
      call interact_rot_coriolis_rlm                                    &
     &   (sph_rlm%nidx_rlm(2), leg%g_sph_rlm,                           &
     &    gt_cor%gi_rlm, gt_cor%ei_rlm,                                 &
     &    gt_cor%sw_rlm, gt_cor%tw_rlm, gt_cor%sd_rlm,                  &
     &    gt_cor%td_rlm, gt_cor%sr_rlm, gt_cor%tr_rlm,                  &
     &    gt_cor%sh_rlm, gt_cor%hh_rlm, gt_cor%th_rlm)
!
      end subroutine init_sum_coriolis_rlm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_d_coriolis_rlm(nnod_rlm, jmax_rlm, cor_rlm)
!
      integer(kind = kint), intent(in) :: nnod_rlm, jmax_rlm
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      allocate(cor_rlm%d_cor_rlm(nnod_rlm,cor_rlm%ncomp_coriolis_rlm))
!
      allocate(cor_rlm%d_cor_in_rlm(jmax_rlm))
      allocate(cor_rlm%d_cor_out_rlm(jmax_rlm))
!
      if(nnod_rlm .le. 0) return
      cor_rlm%d_cor_rlm = 0.0d0
      cor_rlm%d_cor_in_rlm =  0.0d0
      cor_rlm%d_cor_out_rlm = 0.0d0
!
      end subroutine alloc_d_coriolis_rlm
!
!   ------------------------------------------------------------------
!   ------------------------------------------------------------------
!
      subroutine dealloc_d_coriolis_rlm(cor_rlm)
!
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
!
!
      deallocate(cor_rlm%d_cor_rlm)
      deallocate(cor_rlm%d_cor_in_rlm, cor_rlm%d_cor_out_rlm)
!
      end subroutine dealloc_d_coriolis_rlm
!
!   ------------------------------------------------------------------
!
      end module t_coriolis_terms_rlm
