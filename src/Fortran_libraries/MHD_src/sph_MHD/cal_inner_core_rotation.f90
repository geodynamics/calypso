!> @file  cal_inner_core_rotation.f90
!!      module cal_inner_core_rotation
!!
!! @author  H. Matsui
!! @date Programmed in Nov., 2012
!
!> @brief Evaluate torques for inner core rotation
!!
!!@verbatim
!!      subroutine set_inner_core_rotation(kr_in)
!!      subroutine set_icore_viscous_matrix(kr_in)
!!      subroutine cal_icore_viscous_drag_explicit(kr_in, coef_d,       &
!!     &          it_velo, it_viscous)
!!      subroutine copy_icore_rot_to_tor_coriolis(kr_in)
!!      subroutine inner_core_coriolis_rj(kr_in)
!!      subroutine int_icore_toroidal_lorentz(kr_in)
!!@endverbatim
!!
!!@n @param coef_d  Coefficient for diffusion term
!!@n @param it_velo       Field address for toroidal velocity
!!@n @param it_viscous    Field address for toroidal viscous dissipation
!
      module cal_inner_core_rotation
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_phys_address
      use m_sph_spectr_data
!
      implicit  none
!
      private :: int_icore_tor_lorentz_l1, cal_icore_viscous_drag_l1
      private :: set_rotate_icb_vt_sph_mat
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_inner_core_rotation(kr_in)
!
      integer(kind = kint), intent(in) :: kr_in
!
!
      call set_inner_core_rot_l1(idx_rj_degree_one(-1), kr_in)
      call set_inner_core_rot_l1(idx_rj_degree_one( 0), kr_in)
      call set_inner_core_rot_l1(idx_rj_degree_one( 1), kr_in)
!
      end subroutine set_inner_core_rotation
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_icore_viscous_matrix(kr_in)
!
      use m_t_int_parameter
      use m_physical_property
!
      integer(kind = kint), intent(in) :: kr_in
!
!
      call set_rotate_icb_vt_sph_mat(idx_rj_degree_one(-1), kr_in,      &
     &     coef_imp_v, coef_d_velo)
      call set_rotate_icb_vt_sph_mat(idx_rj_degree_one( 0), kr_in,      &
     &     coef_imp_v, coef_d_velo)
      call set_rotate_icb_vt_sph_mat(idx_rj_degree_one( 1), kr_in,      &
     &     coef_imp_v, coef_d_velo)
!!
      end subroutine set_icore_viscous_matrix
!
! ----------------------------------------------------------------------
!
      subroutine cal_icore_viscous_drag_explicit(kr_in, coef_d,         &
     &          it_velo, it_viscous)
!
      integer(kind = kint), intent(in) :: kr_in
      integer(kind = kint), intent(in) :: it_velo, it_viscous
      real(kind = kreal), intent(in) :: coef_d
!
!
      call cal_icore_viscous_drag_l1(idx_rj_degree_one(-1), kr_in,      &
     &    coef_d, it_velo, it_viscous)
      call cal_icore_viscous_drag_l1(idx_rj_degree_one( 0), kr_in,      &
     &    coef_d, it_velo, it_viscous)
      call cal_icore_viscous_drag_l1(idx_rj_degree_one( 1), kr_in,      &
     &    coef_d, it_velo, it_viscous)
!
      end subroutine cal_icore_viscous_drag_explicit
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_icore_rot_to_tor_coriolis(kr_in)
!
      integer(kind = kint), intent(in) :: kr_in
!
      integer(kind = kint) :: m, i1
!
!
      do m = -1, 1
        if(idx_rj_degree_one(m) .gt. 0) then
          i1 = idx_rj_degree_one(m) + (kr_in-1)*nidx_rj(2)
          d_rj(i1,itor%i_coriolis) = d_rj(i1,ipol%i_rot_Coriolis)
        end if
      end do
!
      end subroutine copy_icore_rot_to_tor_coriolis
!
! ----------------------------------------------------------------------
!
      subroutine inner_core_coriolis_rj(kr_in)
!
      use m_physical_property
      use m_poloidal_rotation
!
      integer(kind = kint), intent(in) :: kr_in
!
      integer(kind = kint) :: i11s, i10c, i11c
!
!
      if(idx_rj_degree_one( 1) .le. 0) return
!
!
      i11s = idx_rj_degree_one(-1) + (kr_in-1)*nidx_rj(2)
      i10c = idx_rj_degree_one( 0) + (kr_in-1)*nidx_rj(2)
      i11c = idx_rj_degree_one( 1) + (kr_in-1)*nidx_rj(2)
!
      d_rj(i11s,ipol%i_rot_Coriolis)                                    &
     &       =  omega_rj(kr_in,0,2)*d_rj(i11c,ipol%i_vort)              &
     &        - omega_rj(kr_in,0,3)*d_rj(i10c,ipol%i_vort)
      d_rj(i11c,ipol%i_rot_Coriolis)                                    &
     &       =  omega_rj(kr_in,0,1)*d_rj(i10c,ipol%i_vort)              &
     &        - omega_rj(kr_in,0,2)*d_rj(i11s,ipol%i_vort)
      d_rj(i10c,ipol%i_rot_Coriolis)                                    &
     &       =  omega_rj(kr_in,0,3)*d_rj(i11s,ipol%i_vort)              &
     &        - omega_rj(kr_in,0,1)*d_rj(i11c,ipol%i_vort)
!
      d_rj(i11s,ipol%i_rot_Coriolis)                                    &
     &       = -two*coef_cor*radius_1d_rj_r(kr_in)                      &
     &        * d_rj(i11s,ipol%i_rot_Coriolis)
      d_rj(i11c,ipol%i_rot_Coriolis)                                    &
     &       = -two*coef_cor*radius_1d_rj_r(kr_in)                      &
     &        * d_rj(i11c,ipol%i_rot_Coriolis)
      d_rj(i10c,ipol%i_rot_Coriolis)                                    &
     &       = -two*coef_cor*radius_1d_rj_r(kr_in)                      &
     &        * d_rj(i10c,ipol%i_rot_Coriolis)
!
      end subroutine inner_core_coriolis_rj
!
! ----------------------------------------------------------------------
!
      subroutine int_icore_toroidal_lorentz(kr_in)
!
      integer(kind = kint), intent(in) :: kr_in
!
!
      call int_icore_tor_lorentz_l1(idx_rj_degree_one(-1), kr_in)
      call int_icore_tor_lorentz_l1(idx_rj_degree_one( 0), kr_in)
      call int_icore_tor_lorentz_l1(idx_rj_degree_one( 1), kr_in)
!
      end subroutine int_icore_toroidal_lorentz
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rotate_icb_vt_sph_mat(idx_rj_l0, kr_in,            &
     &          coef_imp, coef_d)
!
      use m_t_int_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
      use m_fdm_coefs
!
      integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
!
      if(idx_rj_l0 .le. 0) return
!
!       vt_evo_mat(3,kr_in-1,idx_rj_l0) = zero
        vt_evo_mat(2,kr_in,  idx_rj_l0)                                 &
     &          = one + coef_imp*dt*coef_d * five                       &
     &           * (+ dr_1d_rj(kr_in, 2)                                &
     &              + two*ar_1d_rj(kr_in,1) )                           &
     &           * a_r_1d_rj_r(kr_in)
        vt_evo_mat(1,kr_in+1,idx_rj_l0)                                 &
     &          = - coef_imp*dt*coef_d * five                           &
     &             * dr_1d_rj(kr_in, 2)                                 &
     &             * a_r_1d_rj_r(kr_in)
!
!        vt_evo_mat(2,kr_in,  idx_rj_l0)                                &
!     &          = one + coef_imp*dt*coef_d * five                      &
!     &           * ( -d1nod_mat_fdm_2(kr_in,-1)                        &
!     &                * radius_1d_rj_r(kr_in-1)**2                     &
!     &                * ar_1d_rj(kr_in,2)                              &
!     &              - d1nod_mat_fdm_2(kr_in, 0)                        &
!     &              + two*ar_1d_rj(kr_in,1) )
!        vt_evo_mat(1,kr_in+1,idx_rj_l0)                                &
!     &          = - coef_imp*dt*coef_d * five                          &
!     &             * d1nod_mat_fdm_2(kr_in, 1)
!
      end subroutine set_rotate_icb_vt_sph_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_inner_core_rot_l1(idx_rj_l0, kr_in)
!
      integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
!
      integer(kind = kint) :: i10c, i10c_ri
      integer(kind = kint) :: k
      real(kind = kreal) :: ratio
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_ri = idx_rj_l0 + (kr_in-1)*nidx_rj(2)
!$omp parallel do private(k,i10c,ratio)
      do k = 1, kr_in-1
        i10c = idx_rj_l0 + (k-1)*nidx_rj(2)
!
        ratio = radius_1d_rj_r(k)*radius_1d_rj_r(k) * ar_1d_rj(kr_in,2)
!
        d_rj(i10c,itor%i_velo) =   ratio * d_rj(i10c_ri,itor%i_velo)
        d_rj(i10c,ipol%i_vort) =   ratio * d_rj(i10c_ri,ipol%i_vort)
        d_rj(i10c,ipol%i_vort+1) = two *   d_rj(i10c_ri,ipol%i_vort)    &
     &                            * radius_1d_rj_r(k)*ar_1d_rj(kr_in,2)
      end do
!$omp end parallel do
!
      i10c = idx_rj_l0 + (kr_in-1)*nidx_rj(2)
      d_rj(i10c,ipol%i_vort+1) = two *   d_rj(i10c_ri,ipol%i_vort)      &
     &                          * ar_1d_rj(kr_in,1)
!
      end subroutine set_inner_core_rot_l1
!
! ----------------------------------------------------------------------
!
      subroutine cal_icore_viscous_drag_l1(idx_rj_l0, kr_in, coef_d,    &
     &          it_velo, it_viscous)
!
      use m_fdm_coefs
!
      real(kind = kreal), intent(in) :: coef_d
      integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
      integer(kind = kint), intent(in) :: it_velo, it_viscous
!
      integer(kind = kint) ::  i10c_ri, i10c_r1
      real(kind = kreal) :: mat_1, mat_0
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_ri = idx_rj_l0 + (kr_in-1)*nidx_rj(2)
      i10c_r1 = idx_rj_l0 +  kr_in * nidx_rj(2)
!
!      mat_0 = d1nod_mat_fdm_2(kr_in,-1)                                &
!     &       * radius_1d_rj_r(kr_in-1)**2 * ar_1d_rj(kr_in,2)          &
!     &      + d1nod_mat_fdm_2(kr_in, 0) - two*ar_1d_rj(kr_in,1)
!      mat_1 = d1nod_mat_fdm_2(kr_in, 1)
!
      mat_0 = - dr_1d_rj(kr_in, 2) - two*ar_1d_rj(kr_in,1)
      mat_1 =   dr_1d_rj(kr_in, 2)
!
      d_rj(i10c_ri,it_viscous)                                          &
     &                   =  five  * coef_d * a_r_1d_rj_r(kr_in)         &
     &                          * (mat_0 * d_rj(i10c_ri,it_velo)        &
     &                           + mat_1 * d_rj(i10c_r1,it_velo))
!
      end subroutine cal_icore_viscous_drag_l1
!
! ----------------------------------------------------------------------
!
      subroutine int_icore_tor_lorentz_l1(idx_rj_l0, kr_in)
!
      integer(kind = kint), intent(in) :: kr_in, idx_rj_l0
!
      integer(kind = kint) :: k
      integer(kind = kint) :: i10c_i, i10c_o
      real(kind = kreal) :: sk_10c
!
!
      if(idx_rj_l0 .le. 0) return
!
      i10c_o = idx_rj_l0
      sk_10c = d_rj(i10c_o,itor%i_lorentz)                              &
     &          * radius_1d_rj_r(1)*radius_1d_rj_r(1) * dr_1d_rj(1,0)
!
!$omp parallel do reduction(+:sk_10c) private(i10c_i,i10c_o)
      do k = 1, kr_in-1
        i10c_i = idx_rj_l0 + (k-1)*nidx_rj(2)
        i10c_o = idx_rj_l0 + (k  )*nidx_rj(2)
!
        sk_10c = sk_10c                                                 &
     &        + (d_rj(i10c_i,itor%i_lorentz) * radius_1d_rj_r(k  )**2   &
     &         + d_rj(i10c_o,itor%i_lorentz) * radius_1d_rj_r(k+1)**2)  &
     &        * dr_1d_rj(k,0)
      end do
!$omp end parallel do
!
      i10c_o = idx_rj_l0 + (kr_in-1)*nidx_rj(2)
      d_rj(i10c_o,itor%i_lorentz) = half * five * sk_10c                &
     &                           * a_r_1d_rj_r(kr_in)**3
      d_rj(i10c_o,ipol%i_rot_Lorentz) = d_rj(i10c_o,itor%i_lorentz)
!
      end subroutine int_icore_tor_lorentz_l1
!
! ----------------------------------------------------------------------
!
      end module cal_inner_core_rotation
