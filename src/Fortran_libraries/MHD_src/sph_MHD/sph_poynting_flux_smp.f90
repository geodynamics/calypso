!>@file   sph_poynting_flux_smp.f90
!!@brief  module sph_poynting_flux_smp
!!
!!@author H. Matsui
!!@date Programmed...May., 2009
!
!>@brief Evaluate poynting flux for nodal field
!!@n     $omp parallel is required to use these routines
!!
!!@verbatim
!!      subroutine cal_rtp_electric_field_smp
!!      subroutine cal_rtp_poynting_flux_smp
!!      subroutine cal_rtp_magnetic_streaching
!!@endverbatim
!
      module sph_poynting_flux_smp
!
      use m_precision
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_physical_property
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_electric_field_smp
!
      use poynting_flux_smp
!
!
      call cal_electric_field_smp(np_smp, nnod_rtp, inod_rtp_smp_stack, &
     &    coef_d_magne, d_rtp(1,irtp%i_current),                        &
     &    d_rtp(1,irtp%i_vp_induct), d_rtp(1,irtp%i_electric))
!
      end subroutine cal_rtp_electric_field_smp
!
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_poynting_flux_smp
!
      use poynting_flux_smp
!
!
      call cal_poynting_flux_smp(np_smp, nnod_rtp, inod_rtp_smp_stack,  &
     &     coef_d_magne, d_rtp(1,irtp%i_current),                       &
     &     d_rtp(1,irtp%i_vp_induct), d_rtp(1,irtp%i_magne),            &
     &     d_rtp(1,irtp%i_poynting))
!
      end subroutine cal_rtp_poynting_flux_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_rtp_magnetic_streaching
!
      use poynting_flux_smp
      use m_work_4_sph_trans
!
!
      if(irtp%i_mag_stretch .eq. 0) return
!
!$omp parallel
      call cal_rtp_magnetic_streach(np_smp, nnod_rtp,                   &
     &   inod_rtp_smp_stack, nidx_rtp(1), nidx_rtp(2), a_r_1d_rtp_r,    &
     &   cot_theta_1d_rtp, d_rtp(1,irtp%i_magne), d_rtp(1,irtp%i_velo), &
     &   d_rtp(1,irtp%i_grad_vx), d_rtp(1,irtp%i_grad_vy),              &
     &   d_rtp(1,irtp%i_grad_vz), d_rtp(1,irtp%i_mag_stretch) )
!$omp end parallel
!
      end subroutine cal_rtp_magnetic_streaching
!
! -----------------------------------------------------------------------
!
      subroutine copy_velo_to_grad_v_rtp
!
      integer(kind = kint) :: ip, ist, ied, inod
!
!
      if(irtp%i_mag_stretch .eq. 0) return
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_rtp_smp_stack(ip-1) + 1
        ied = inod_rtp_smp_stack(ip)
        do inod = ist, ied
          d_rtp(inod,irtp%i_grad_vx) = d_rtp(inod,irtp%i_velo  )
          d_rtp(inod,irtp%i_grad_vy) = d_rtp(inod,irtp%i_velo+1)
          d_rtp(inod,irtp%i_grad_vz) = d_rtp(inod,irtp%i_velo+2)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_velo_to_grad_v_rtp
!
! -----------------------------------------------------------------------
!
      subroutine cal_grad_of_velocities_sph
!
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
!
      integer(kind = kint) :: ip, ist, ied, inod
!
      if(ipol%i_mag_stretch .eq. 0) return
!
!
!$omp parallel do private(ip,ist,ied,inod)
      do ip = 1, np_smp
        ist = inod_rj_smp_stack(ip-1) + 1
        ied = inod_rj_smp_stack(ip)
        do inod = ist, ied
          d_rj(inod,ipol%i_mag_stretch  ) = d_rj(inod,ipol%i_grad_vx)
          d_rj(inod,ipol%i_mag_stretch+1) = d_rj(inod,ipol%i_grad_vy)
          d_rj(inod,ipol%i_mag_stretch+2) = d_rj(inod,ipol%i_grad_vz)
        end do
      end do
!$omp end parallel do
!
      call const_sph_gradient_no_bc                                     &
     &     (sph_bc_U, (ipol%i_mag_stretch  ), ipol%i_grad_vx)
      call const_sph_gradient_no_bc                                     &
     &     (sph_bc_U, (ipol%i_mag_stretch+1), ipol%i_grad_vy)
      call const_sph_gradient_no_bc                                     &
     &     (sph_bc_U, (ipol%i_mag_stretch+2), ipol%i_grad_vz)
!
      end subroutine cal_grad_of_velocities_sph
!
! -----------------------------------------------------------------------
!
      end module sph_poynting_flux_smp
