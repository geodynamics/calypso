!>@file   cal_vorticity_terms_adams.f90
!!@brief  module cal_vorticity_terms_adams
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evoluve the vorticity equation by explicit scheme 
!!
!!@verbatim
!!      subroutine cal_vorticity_eq_adams(kr_in, kr_out)
!!      subroutine cal_vorticity_eq_euler(kr_in, kr_out)
!!
!!      subroutine set_MHD_terms_to_force(it_rot_buo)
!!      subroutine set_rot_cv_terms_to_force(it_rot_buo)
!!
!!      subroutine set_rot_advection_to_force
!!      subroutine add_coriolis_to_vort_force
!!      subroutine add_buoyancy_to_vort_force(it_rot_buo)
!!      subroutine add_lorentz_to_vort_force
!!
!!      subroutine set_ini_adams_inertia
!!@endverbatim
!!
!!@n @param kr_in       Radial ID for inner boundary
!!@n @param kr_out      Radial ID for outer boundary
!!
!!@n @param it_rot_buo  Spectr field address
!!                       for toroidal curl of buodyancy
!
      module cal_vorticity_terms_adams
!
      use m_precision
!
      use m_t_int_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_adams(kr_in, kr_out)
!
      use m_physical_property
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_vort) = d_rj(inod,ipol%i_vort)                 &
     &                 + dt * (coef_exp_v * d_rj(inod,ipol%i_w_diffuse) &
     &                           + adam_0 * d_rj(inod,ipol%i_forces)    &
     &                           + adam_1 * d_rj(inod,ipol%i_pre_mom))
        d_rj(inod,itor%i_vort) = d_rj(inod,itor%i_vort)                 &
     &                 + dt * (coef_exp_v * d_rj(inod,itor%i_w_diffuse) &
     &                           + adam_0 * d_rj(inod,itor%i_forces)    &
     &                           + adam_1 * d_rj(inod,itor%i_pre_mom))
!
        d_rj(inod,ipol%i_pre_mom) = d_rj(inod,ipol%i_forces)
        d_rj(inod,itor%i_pre_mom) = d_rj(inod,itor%i_forces)
      end do
!$omp end do nowait
!
      end subroutine cal_vorticity_eq_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_vorticity_eq_euler(kr_in, kr_out)
!
      use m_physical_property
!
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      integer(kind = kint) :: inod, ist, ied
!
!
      ist = (kr_in-1)*nidx_rj(2) + 1
      ied = kr_out * nidx_rj(2)
!$omp do private (inod)
      do inod = ist, ied
        d_rj(inod,ipol%i_vort) = d_rj(inod,ipol%i_vort)                 &
     &               + dt * (coef_exp_v *  d_rj(inod,ipol%i_w_diffuse)  &
     &                                   + d_rj(inod,ipol%i_forces) )
!
        d_rj(inod,itor%i_vort) = d_rj(inod,itor%i_vort)                 &
     &               + dt * (coef_exp_v *  d_rj(inod,itor%i_w_diffuse)  &
     &                                   + d_rj(inod,itor%i_forces) )
       end do
!$omp end do
!
      end subroutine cal_vorticity_eq_euler
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_MHD_terms_to_force(it_rot_buo)
!
      integer(kind = kint), intent(in) :: it_rot_buo
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = - d_rj(inod,ipol%i_rot_inertia)      &
     &                             + d_rj(inod,ipol%i_rot_Coriolis)     &
     &                             + d_rj(inod,ipol%i_rot_Lorentz)
        d_rj(inod,itor%i_forces) = - d_rj(inod,itor%i_rot_inertia)      &
     &                             + d_rj(inod,itor%i_rot_Coriolis)     &
     &                             + d_rj(inod,itor%i_rot_Lorentz)      &
     &                             + d_rj(inod,it_rot_buo)
      end do
!$omp end do nowait
!
      end subroutine set_MHD_terms_to_force
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_cv_terms_to_force(it_rot_buo)
!
      integer(kind = kint), intent(in) :: it_rot_buo
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = - d_rj(inod,ipol%i_rot_inertia)      &
     &                             + d_rj(inod,ipol%i_rot_Coriolis)
        d_rj(inod,itor%i_forces) = - d_rj(inod,itor%i_rot_inertia)      &
     &                             + d_rj(inod,itor%i_rot_Coriolis)     &
     &                             + d_rj(inod,it_rot_buo)
      end do
!$omp end do nowait
!
      end subroutine set_rot_cv_terms_to_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_advection_to_force
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = - d_rj(inod,ipol%i_rot_inertia)
        d_rj(inod,itor%i_forces) = - d_rj(inod,itor%i_rot_inertia)
      end do
!$omp end do nowait
!
      end subroutine set_rot_advection_to_force
!
! ----------------------------------------------------------------------
!
      subroutine add_coriolis_to_vort_force
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = d_rj(inod,ipol%i_forces)             &
     &                           + d_rj(inod,ipol%i_rot_Coriolis)
        d_rj(inod,itor%i_forces) = d_rj(inod,itor%i_forces)             &
     &                           + d_rj(inod,itor%i_rot_Coriolis)
      end do
!$omp end do nowait
!
      end subroutine add_coriolis_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_buoyancy_to_vort_force(it_rot_buo)
!
      integer(kind = kint), intent(in) :: it_rot_buo
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,itor%i_forces) = d_rj(inod,itor%i_forces)             &
     &                           + d_rj(inod,it_rot_buo)
       end do
!$omp end do nowait
!
      end subroutine add_buoyancy_to_vort_force
!
! ----------------------------------------------------------------------
!
      subroutine add_lorentz_to_vort_force
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_forces) = d_rj(inod,ipol%i_forces)             &
     &                           + d_rj(inod,ipol%i_rot_Lorentz)
        d_rj(inod,itor%i_forces) = d_rj(inod,itor%i_forces)             &
     &                           + d_rj(inod,itor%i_rot_Lorentz)
       end do
!$omp end do nowait
!
      end subroutine add_lorentz_to_vort_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ini_adams_inertia
!
      integer(kind = kint) :: inod
!
!
!$omp do private (inod)
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_pre_mom) = d_rj(inod,ipol%i_forces)
        d_rj(inod,itor%i_pre_mom) = d_rj(inod,itor%i_forces)
      end do
!$omp end do nowait
!
      end subroutine set_ini_adams_inertia
!
! ----------------------------------------------------------------------
!
      end module cal_vorticity_terms_adams
