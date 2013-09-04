!
!      module boundary_radius_func
!
!      Programmed by H. Matsui on June., 1994
!      modified by H. Matsui on Apr., 2009
!
!      subroutine set_equi_dr_ICB
!      subroutine set_equi_dr_CMB
!      subroutine set_non_equi_dr_center
!      subroutine set_equi_dr_outside
!*
!***********************************************************************
!
!*      inner core boundary ( k = ICB )
!*        dr_1d_rj(k,0) =  r(ICB+1) - r(ICB)
!*        dr_1d_rj(k,1) =  r(ICB+1) - r(ICB)
!*        dr_1d_rj(k,2) =  1 / (r(ICB+1) - r(ICB))
!*
!*      outer core boundary ( k = CMB )
!*        dr_1d_rj(nri,0) = r(CMB) - r(CMB-1)
!*        dr_1d_rj(nri,1) = r(CMB) - r(CMB-1)
!*        dr_1d_rj(nri,2) = 1 / (r(CMB) - r(CMB-1)) 
!*
!*      center ( k = 1 )
!*        dr_1d_rj(1,0) = r(2) - r(1)
!*        dr_1d_rj(1,1) = r(1)
!*        dr_1d_rj(k,2) = 1 / ( (r(2) - r(1)) * r(1)*r(2) )
!*
!*      outer core boundary ( k = nri )
!*        dr_1d_rj(nri,0) = r(nri) - r(nri-1)
!*        dr_1d_rj(nri,1) = r(nri) - r(nri-1)
!*        dr_1d_rj(nri,2) = 1 / (r(nri) - r(nri-1)) 
!*
!*
!***********************************************************************
!
      module boundary_radius_func
!
      use m_precision
      use m_constants
!
      use m_spheric_parameter
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_equi_dr_ICB
!
!
        dr_1d_rj(nlayer_ICB,0) = radius_1d_rj_r(nlayer_ICB+1)           &
     &                          - radius_1d_rj_r(nlayer_ICB)
        dr_1d_rj(nlayer_ICB,1) = dr_1d_rj(nlayer_ICB,0)
        dr_1d_rj(nlayer_ICB,2) = one /  dr_1d_rj(nlayer_ICB,0)
!
      end subroutine set_equi_dr_ICB
!
!  -------------------------------------------------------------------
!
      subroutine set_equi_dr_CMB
!
!
        dr_1d_rj(nlayer_CMB,0) = radius_1d_rj_r(nlayer_CMB)             &
     &                          - radius_1d_rj_r(nlayer_CMB-1)
        dr_1d_rj(nlayer_CMB,1) = radius_1d_rj_r(nlayer_CMB)             &
     &                          - radius_1d_rj_r(nlayer_CMB-1)
        dr_1d_rj(nlayer_CMB,2) = one /  dr_1d_rj(nlayer_CMB,1)
!
      end subroutine set_equi_dr_CMB
!
!  -------------------------------------------------------------------
!
      subroutine set_non_equi_dr_center
!
!
        dr_1d_rj(1,0) = radius_1d_rj_r(2) - radius_1d_rj_r(1)
        dr_1d_rj(1,1) = radius_1d_rj_r(1)
        dr_1d_rj(1,2) = one / ( dr_1d_rj(1,0)                           &
     &                 * radius_1d_rj_r(2) * radius_1d_rj_r(1))
!
      end subroutine set_non_equi_dr_center
!
!  -------------------------------------------------------------------
!
      subroutine set_equi_dr_outside
!
      integer(kind = kint) :: k
!
        k = nidx_rj(1)
        dr_1d_rj(k,0) = radius_1d_rj_r(k) - radius_1d_rj_r(k-1)
        dr_1d_rj(k,1) = dr_1d_rj(k,0)
        dr_1d_rj(k,2) = one /  dr_1d_rj(k,0)
!
      end subroutine set_equi_dr_outside
!
!  -------------------------------------------------------------------
!
      end module boundary_radius_func
