!>@file   pole_sph_transform.f90
!!@brief  module pole_sph_transform
!!
!!@author H. Matsui
!!@date Programmed in June, 2012
!
!>@brief  Spherical transform for poles
!!
!!@verbatim
!!      subroutine init_pole_transform(numnod)
!!
!!      subroutine pole_backward_transforms(ncomp, nvector, nscalar,    &
!!     &          n_WR, WR, v_pl_local)
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
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_pole_transform(numnod)
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_phys_constants
!
      use m_work_pole_sph_trans
!
      use m_spheric_constants
!
      integer(kind = kint), intent(in) :: numnod
!
!
      if(iflag_shell_mode .ne. iflag_MESH_same) then
        nnod_rtp_pole = numnod
      end if
!
      call init_num_pole_sph_trans
!
      end subroutine init_pole_transform
!
! -----------------------------------------------------------------------
!
      subroutine pole_backward_transforms(ncomp, nvector, nscalar,      &
     &          n_WR, WR, v_pl_local)
!
      use calypso_mpi
      use m_sph_trans_comm_table
      use m_work_pole_sph_trans
      use schmidt_b_trans_at_pole
      use schmidt_b_trans_at_center
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nvector, nscalar
      integer(kind = kint), intent(in) :: n_WR
!
      real(kind = kreal), intent(in) :: WR(n_WR)
      real(kind = kreal), intent(inout) :: v_pl_local(nnod_pole,ncomp)
!
!
      if     (iflag_shell_mode.eq.iflag_no_FEMMESH                      &
        .or.  iflag_shell_mode.eq.iflag_MESH_same) return
!
      if (iflag_debug.gt.0)  write(*,*) 'schmidt_b_trans_pole_vect'
      call schmidt_b_trans_pole_vect(ncomp, nvector,                    &
     &    irev_sr_rlm, n_WR, WR, v_pl_local)
      call schmidt_b_trans_pole_scalar(ncomp, nvector, nscalar,         &
     &    irev_sr_rlm, n_WR, WR, v_pl_local)
!
      if(iflag_shell_mode .eq. iflag_MESH_w_center) then
        call schmidt_b_trans_center_vect(ncomp, nvector,                &
     &      irev_sr_rlm, n_WR, WR, v_pl_local)
        call schmidt_b_trans_center_scalar                              &
     &     (ncomp, nvector, nscalar, v_pl_local)
      end if
!
      end subroutine pole_backward_transforms
!
! -----------------------------------------------------------------------
!
      end module pole_sph_transform
