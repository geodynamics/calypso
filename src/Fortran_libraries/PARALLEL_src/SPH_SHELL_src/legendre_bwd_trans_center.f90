!>@file   legendre_bwd_trans_center.f90
!!@brief  module legendre_bwd_trans_center
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!!@n    Modified in Apr. 2013
!
!>@brief Legandre harmonics transform at center
!!
!!@verbatim
!!      subroutine leg_b_trans_center_vector(ncomp, nvector)
!!      subroutine leg_b_trans_center_scalar(ncomp, nvector, nscalar)
!!
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
!!
!!------------------------------------------------------------------
!!@endverbatim
!!
!!@param   ncomp    Total number of components for spherical transform
!!@param   nvector  Number of vector for spherical transform
!!@param   nscalar  Number of scalar (including tensor components)
!!                  for spherical transform
!
      module legendre_bwd_trans_center
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
!
      use m_work_4_sph_trans
      use m_work_pole_sph_trans
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine leg_b_trans_center_vector(ncomp, nvector)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
!
      integer(kind = kint) :: i_rlm, nd
!
!
      v_ct_local(1:3*nvector) = zero
!
      if(ist_rtm_order_zero.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nvector
          i_rlm = 3*nd + ist_rtm_order_zero * ncomp
          v_ct_local(3*nd-2) =  two * sp_rlm(i_rlm-2)                   &
     &                  * a_r_1d_rtm_r(1)*a_r_1d_rtm_r(1)
        end do
      end if
!
      if(ist_rtm_order_1s.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nvector
          i_rlm = 3*nd + (ist_rtm_order_1s-1) * ncomp
          v_ct_local(3*nd  ) = -two * sp_rlm(i_rlm-2)                   &
     &                  * a_r_1d_rtm_r(1)*a_r_1d_rtm_r(1)
        end do
      end if
!
      if(ist_rtm_order_1c.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nvector
          i_rlm = 3*nd + (ist_rtm_order_1c-1) * ncomp
          v_ct_local(3*nd-1) = -two * sp_rlm(i_rlm-2)                   &
     &                  * a_r_1d_rtm_r(1)*a_r_1d_rtm_r(1)
        end do
      end if
!
      end subroutine leg_b_trans_center_vector
!
!------------------------------------------------------------------
!
      subroutine leg_b_trans_center_scalar(ncomp, nvector, nscalar)
!
      integer(kind = kint), intent(in) :: ncomp, nvector, nscalar
!
      integer(kind = kint) :: i_rlm, nd
!
!
      v_ct_local(1+3*nvector:ncomp) = zero
!
      if(ist_rtm_order_zero.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nscalar
          i_rlm = nd + 3*nvector + ist_rtm_order_zero * ncomp
          v_ct_local(nd) =  two * sp_rlm(i_rlm)
        end do
      end if
!
      end subroutine leg_b_trans_center_scalar
!
!------------------------------------------------------------------
!
      end module legendre_bwd_trans_center
