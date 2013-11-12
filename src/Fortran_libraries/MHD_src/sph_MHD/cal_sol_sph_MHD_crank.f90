!>@file   cal_sol_sph_MHD_crank.f90
!!@brief  module cal_sol_sph_MHD_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update fields for MHD dynamo model
!!
!!@verbatim
!!      subroutine s_cal_sol_sph_MHD_crank
!!      subroutine set_sph_field_to_start
!!
!!      subroutine check_ws_spectr
!!@endverbatim
!
      module cal_sol_sph_MHD_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_radial_matrices_sph
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_physical_property
      use const_sph_radial_grad
      use const_sph_rotation
      use const_sph_diffusion
!
      implicit none
!
      private :: update_after_vorticity_sph, update_after_magne_sph
      private :: update_after_heat_sph, update_after_composit_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_sol_sph_MHD_crank
!
      use cal_rot_buoyancies_sph_MHD
      use cal_sol_sph_fluid_crank
!
!      integer(kind = kint) :: j, k, inod
!
!*-----  time evolution   -------------
!*
!      call check_ws_spectr
!
      if(iflag_t_evo_4_velo .gt. id_no_evolution) then
!         Input:    ipol%i_vort, itor%i_vort
!         Solution: ipol%i_velo, itor%i_velo, idpdr%i_velo
        if (iflag_debug .gt. 0)                                         &
     &       write(*,*) 'cal_sol_velo_by_vort_sph_crank'
        call cal_sol_velo_by_vort_sph_crank
      end if
!
!  Input: ipol%i_temp,  Solution: ipol%i_temp
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_temperature_sph_crank'
      if(iflag_t_evo_4_temp .gt. id_no_evolution) then
        call cal_sol_temperature_sph_crank
      end if
!
!  Input: ipol%i_light,  Solution: ipol%i_light
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_composition_sph_crank'
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        call cal_sol_composition_sph_crank
      end if
!
!  Input: ipol%i_magne, itor%i_magne
!  Solution: ipol%i_magne, itor%i_magne, idpdr%i_magne
      if(iflag_debug.gt.0) write(*,*) 'cal_sol_magne_sph_crank'
      if(iflag_t_evo_4_magne .gt. id_no_evolution) then
        call cal_sol_magne_sph_crank
      end if
!
!*  ---- update after evolution ------------------
!      call check_vs_spectr
!
      if(iflag_t_evo_4_velo .gt. id_no_evolution) then
        call update_after_vorticity_sph
        call cal_rot_radial_self_gravity
      end if
!
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call update_after_heat_sph
      end if
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        call update_after_composit_sph
      end if
      if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call update_after_magne_sph
      end if
!
      end subroutine s_cal_sol_sph_MHD_crank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_field_to_start
!
      use const_sph_radial_grad
      use cal_rot_buoyancies_sph_MHD
!
!
      if(ipol%i_velo*ipol%i_vort .gt. 0) then
        call const_grad_vp_and_vorticity
      end if
!
      if(iflag_debug.gt.0) write(*,*) 'update_after_vorticity_sph'
      call update_after_vorticity_sph
      if(iflag_debug.gt.0) write(*,*) 'cal_rot_radial_self_gravity'
      call cal_rot_radial_self_gravity
!
      if(iflag_debug.gt.0) write(*,*) 'update_after_heat_sph'
      call update_after_heat_sph
      if(iflag_debug.gt.0) write(*,*) 'update_after_composit_sph'
      call update_after_composit_sph
!
      if(ipol%i_magne*ipol%i_current .gt. 0) then
        call const_grad_bp_and_current
      end if
!        Input:    ipol%i_magne, itor%i_magne
!        Solution: idpdr%i_magne, 
!                  ipol%i_current, itor%i_current, idpdr%i_current
!
      call update_after_magne_sph
!
      end subroutine set_sph_field_to_start
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine update_after_vorticity_sph
!
      use m_boundary_params_sph_MHD
      use cal_inner_core_rotation
!
!
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic                     &
     &  .and. idx_rj_degree_one(1) .gt. 0) then
        call set_inner_core_rotation
      end if
!
!       Input: ipol%i_vort, itor%i_vort
!       Solution: ipol%i_v_diffuse, itor%i_v_diffuse, idpdr%i_v_diffuse
      if(iflag_debug.gt.0) write(*,*) 'const_sph_viscous_by_vort2'
      if(ipol%i_v_diffuse .gt. 0) call const_sph_viscous_by_vort2
!       Input:    ipol%i_vort, itor%i_vort
!       Solution: ipol%i_w_diffuse, itor%i_w_diffuse, idpdr%i_w_diffuse
      if(iflag_debug.gt.0) write(*,*)'const_sph_vorticirty_diffusion'
      if(ipol%i_w_diffuse .gt. 0) call const_sph_vorticirty_diffusion
!
      end subroutine update_after_vorticity_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine update_after_magne_sph
!
!
!       Input:    ipol%i_current, itor%i_current
!       Solution: ipol%i_b_diffuse, itor%i_b_diffuse, idpdr%i_b_diffuse
      if(iflag_debug .gt. 0) write(*,*) 'const_sph_mag_diffuse_by_j'
      if(ipol%i_b_diffuse .gt. 0) call const_sph_mag_diffuse_by_j
!
      end subroutine update_after_magne_sph
!
! -----------------------------------------------------------------------
!
      subroutine update_after_heat_sph
!
!
!         Input: ipol%i_temp,  Solution: ipol%i_grad_t
      if(iflag_debug .gt. 0)  write(*,*)                                &
     &           'const_radial_grad_temp', ipol%i_grad_t
      if(ipol%i_grad_t .gt. 0)   call const_radial_grad_temp
!
!         Input: ipol%i_temp,  Solution: ipol%i_t_diffuse
      if(iflag_debug .gt. 0)  write(*,*)                                &
     &           'const_sph_thermal_diffusion', ipol%i_t_diffuse
      if(ipol%i_t_diffuse .gt. 0) call const_sph_thermal_diffusion
!
      end subroutine update_after_heat_sph
!
! -----------------------------------------------------------------------
!
      subroutine update_after_composit_sph
!
!
!         Input: ipol%i_light,  Solution: ipol%i_grad_composit
      if(ipol%i_grad_composit .gt. 0) call const_radial_grad_composit
!
      if (iflag_debug .gt. 0) write(*,*) 'const_sph_composit_diffusion'
!         Input: ipol%i_light,  Solution: ipol%i_c_diffuse
      if(ipol%i_c_diffuse .gt. 0) call const_sph_composit_diffusion
!
      end subroutine update_after_composit_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vs_spectr
!
      integer(kind = kint) :: j, k, inod
!
      write(150+my_rank,*) 'j, k, s_velo, ds_velo, t_velo, w_diffuse'
      do j = 1, nidx_rj(2)
         do k = 1, nidx_rj(1)
          inod = j + (k-1) * nidx_rj(2)
          write(150+my_rank,'(2i10,1p20E25.15e3)') j, k,                &
     &        d_rj(inod,ipol%i_velo),d_rj(inod,idpdr%i_velo),           &
     &        d_rj(inod,itor%i_velo)
        end do
      end do
!
      end subroutine check_vs_spectr
!
! -----------------------------------------------------------------------
!
      subroutine check_ws_spectr
!
      integer(kind = kint) :: j, k, inod
!
      write(150+my_rank,*) 'j, k, s_vort, ds_vort, t_vort'
      do j = 1, nidx_rj(2)
         do k = 1, nidx_rj(1)
          inod = j + (k-1) * nidx_rj(2)
          write(150+my_rank,'(2i10,1p20E25.15e3)') j, k,                &
     &        d_rj(inod,ipol%i_vort), d_rj(inod,idpdr%i_vort),          &
     &        d_rj(inod,itor%i_vort)
        end do
      end do
!
      end subroutine check_ws_spectr
!
! -----------------------------------------------------------------------
!
      end module cal_sol_sph_MHD_crank
