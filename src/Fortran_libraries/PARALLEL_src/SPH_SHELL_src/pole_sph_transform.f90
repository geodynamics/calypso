!>@file   pole_sph_transform.f90
!!@brief  module pole_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Spherical transform for poles
!!
!!@verbatim
!!      subroutine init_pole_transform(sph_rtp)
!!
!!      subroutine pole_backward_transforms(ncomp, nvector, nscalar,    &
!!     &          sph_params, sph_rtp, sph_rtm, sph_rlm, comm_rlm, leg, &
!!     &          n_WR, WR, v_pl_local)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_comm_tbl), intent(in) :: comm_rlm
!!        type(legendre_4_sph_trans), intent(inout) :: leg
!!
!!------------------------------------------------------------------
!!
!!      vr =  l*(l+1)*Y(l,0)* S(l,0) / r**2
!!      vt =  (dYdt(l,1s)*dSdr(l,1s)
!!           + dYdt(l,1c)*dSdr(l,1c))  / r
!!         + cos(theta) * (d2Ydtdp(l,1s)*T(l,1s) 
!!                       + d2Ydtdp(l,1c)*T(l,1c))  / r
!!      vp = cos(theta) * (d2Ydtdp(l,1s)*dSdr(l,1s)
!!                       + d2Ydtdp(l,1c)*dSdr(l,1c))  / r
!!           -(dYdt(l,1s)*T(l,1s)
!!           + dYdt(l,1c)*T(l,1c))  / r
!!
!!  if phi = 0
!!
!!      vr =  l*(l+1)*P(l,0)* S(l,0) / r**2
!!      vt =  dPdt(l,1)*dSdr(l,1c)  / r
!!         + cos(theta) * dPdt(l,1)*T(l,1s) / r
!!      vp = cos(theta) * dPdt(l,1)*dSdr(l,1s) / r
!!           - dPdt(l,1)*T(l,1c)  / r
!!
!! if z > 0 (North pole)
!!
!!      vx = vt
!!      vy = vp
!!      vz = vr
!!
!! if z < 0 (South pole)
!!
!!      vx = -vt
!!      vy =  vp
!!      vz = -vr
!!
!!------------------------------------------------------------------
!!------------------------------------------------------------------
!!
!! if r= 0 (Center)
!!
!!      vz =  2 * P(1,0) * S(1,0) / r_c**2
!!         =  2 * S(1,0) / r_c**2
!!      vx =  2 * dPdt(l,1) * S(1,1c) / r_c**2
!!         = - 2 * S(1,1c) / r_c**2
!!      vy =  2 * dPdt(l,1) * S(1,1s) / r_c**2
!!         = - 2 * S(1,1s) / r_c**2
!!------------------------------------------------------------------
!!@endverbatim
!!
!!@param ncomp Number of components for transform
!
      module pole_sph_transform
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use m_spheric_constants
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_schmidt_poly_on_rtm
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_pole_transform(sph_rtp)
!
      use cal_minmax_and_stacks
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      integer(kind = kint) :: max_npole_smp
!
!
      call alloc_num_pole_sph_trans(sph_rtp)
!
      call count_number_4_smp(np_smp, ione, sph_rtp%nnod_pole,          &
     &    sph_rtp%istack_npole_smp, max_npole_smp)
!
      end subroutine init_pole_transform
!
! -----------------------------------------------------------------------
!
      subroutine pole_backward_transforms(ncomp, nvector, nscalar,      &
     &          sph_params, sph_rtp, sph_rtm, sph_rlm, comm_rlm, leg,   &
     &          n_WR, WR, v_pl_local)
!
      use calypso_mpi
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(legendre_4_sph_trans), intent(in) :: leg
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
!
      real(kind = kreal), intent(in) :: WR(n_WR)
      real(kind = kreal), intent(inout)                                 &
     &           :: v_pl_local(sph_rtp%nnod_pole,ncomp)
!
!
      if     (sph_params%iflag_shell_mode.eq.iflag_no_FEMMESH           &
        .or.  sph_params%iflag_shell_mode.eq.iflag_MESH_same) return
!
      if (iflag_debug.gt.0)  write(*,*) 'schmidt_b_trans_pole_vect'
      call schmidt_b_trans_pole_vect(ncomp, nvector,                    &
     &   sph_params%l_truncation, sph_rtm%ist_rtm_order_zero,           &
     &   sph_rtm%ist_rtm_order_1s, sph_rtm%ist_rtm_order_1c,            &
     &   sph_rlm%nnod_rlm, sph_rlm%nidx_rlm(1), sph_rlm%nidx_rlm(2),    &
     &   sph_rtp%nnod_pole, sph_rlm%istep_rlm,                          &
     &   sph_rtp%nidx_global_rtp, sph_rlm%idx_gl_1d_rlm_r,              &
     &   sph_rlm%a_r_1d_rlm_r, comm_rlm%irev_sr,                        &
     &   leg%g_sph_rlm, leg%P_pole_rtm, leg%dPdt_pole_rtm,              &
     &    n_WR, WR, v_pl_local)
      call schmidt_b_trans_pole_scalar(ncomp, nvector, nscalar,         &
     &    sph_params%l_truncation, sph_rtm%ist_rtm_order_zero,          &
     &    sph_rlm%nnod_rlm, sph_rlm%nidx_rlm(1), sph_rlm%nidx_rlm(2),   &
     &    sph_rtp%nnod_pole, sph_rlm%istep_rlm,                         &
     &    sph_rtp%nidx_global_rtp, sph_rlm%idx_gl_1d_rlm_r,             &
     &    comm_rlm%irev_sr, leg%P_pole_rtm, n_WR, WR, v_pl_local)
!
      if(sph_params%iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_vect                                &
     &    (ncomp, nvector, sph_rtm%ist_rtm_order_zero,                  &
     &     sph_rtm%ist_rtm_order_1s, sph_rtm%ist_rtm_order_1c,          &
     &     sph_rlm%nnod_rlm, sph_rtp%nnod_pole,                         &
     &     sph_rtm%idx_gl_1d_rtm_r(1), sph_rtm%a_r_1d_rtm_r(1),         &
     &     comm_rlm%irev_sr, n_WR, WR, v_pl_local)
        call schmidt_b_trans_center_scalar                              &
     &     (ncomp, nvector, nscalar, sph_rtp%nnod_pole, v_pl_local)
      end if
!
      end subroutine pole_backward_transforms
!
! -----------------------------------------------------------------------
!
      end module pole_sph_transform
