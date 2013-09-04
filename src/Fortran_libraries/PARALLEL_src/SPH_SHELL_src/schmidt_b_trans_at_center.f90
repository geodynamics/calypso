!schmidt_b_trans_at_center.f90
!      module schmidt_b_trans_at_center
!
!     Written by H. Matsui on July, 2007
!
!
!      subroutine schmidt_b_trans_center_scalar(nb)
!      subroutine schmidt_b_trans_center_vect(nb)
!
!------------------------------------------------------------------
!
! if r= 0 (Center)
!
!      vz =  2 * P(1,0) * S(1,0) / r_c**2
!         =  2 * S(1,0) / r_c**2
!      vx =  2 * dPdt(l,1) * S(1,1c) / r_c**2
!         = - 2 * S(1,1c) / r_c**2
!      vy =  2 * dPdt(l,1) * S(1,1s) / r_c**2
!         = - 2 * S(1,1s) / r_c**2
!
!------------------------------------------------------------------
!
      module schmidt_b_trans_at_center
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
      subroutine schmidt_b_trans_center_scalar(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, nd
!
!
      v_center(nb) = zero
!
      if(ist_rtm_order_zero.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nb
          i_rlm = nd + ist_rtm_order_zero * nb
          v_ct_local(nd) =  two * sp_rlm(i_rlm)
        end do
      end if
!
      end subroutine schmidt_b_trans_center_scalar
!
!------------------------------------------------------------------
!
      subroutine schmidt_b_trans_center_vect(nb)
!
      integer(kind = kint), intent(in) :: nb
!
      integer(kind = kint) :: i_rlm, nd
!
!
      v_ct_local(1:3*nb) = zero
!
      if(ist_rtm_order_zero.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nb
          i_rlm = nd + ist_rtm_order_zero * nb
          v_ct_local(3*nd-2) =  two * sp_rlm(3*i_rlm-2)                 &
     &                  * a_r_1d_rtm_r(1)*a_r_1d_rtm_r(1)
        end do
      end if
!
      if(ist_rtm_order_1s.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nb
          i_rlm = nd + (ist_rtm_order_1s-1) * nb
          v_ct_local(3*nd  ) = -two * sp_rlm(3*i_rlm-2)                 &
     &                  * a_r_1d_rtm_r(1)*a_r_1d_rtm_r(1)
        end do
      end if
!
      if(ist_rtm_order_1c.gt.0 .and. idx_gl_1d_rlm_r(1).eq.1) then
        do nd = 1, nb
          i_rlm = nd + (ist_rtm_order_1c-1) * nb
          v_ct_local(3*nd-1) = -two * sp_rlm(3*i_rlm-2)                 &
     &                  * a_r_1d_rtm_r(1)*a_r_1d_rtm_r(1)
        end do
      end if
!
      end subroutine schmidt_b_trans_center_vect
!
!------------------------------------------------------------------
!
      end module schmidt_b_trans_at_center
