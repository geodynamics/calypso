!>@file   cal_sol_sph_fluid_crank.f90
!!@brief  module cal_sol_sph_fluid_crank
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Oct., 2009
!
!>@brief  Update each field for MHD dynamo model
!!
!!@verbatim
!!      subroutine cal_sol_velo_by_vort_sph_crank
!!        Input address:    ipol%i_vort, itor%i_vort
!!        Solution address: ipol%i_velo, itor%i_velo, idpdr%i_velo
!!
!!      subroutine cal_sol_pressure_by_div_v
!!        Solution address: ipol%i_press
!!
!!
!!      subroutine cal_sol_magne_sph_crank
!!        Input address:    ipol%i_magne, itor%i_magne
!!        Solution address: ipol%i_magne, itor%i_magne, idpdr%i_magne
!!
!!      subroutine cal_sol_temperature_sph_crank
!!        Input address:    ipol%i_temp
!!        Solution address: ipol%i_temp
!!      subroutine cal_sol_composition_sph_crank
!!        Input address:    ipol%i_light
!!        Solution address: ipol%i_light
!!@endverbatim
!
      module cal_sol_sph_fluid_crank
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_control_params_sph_MHD
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_radial_matrices_sph
      use set_reference_sph_mhd
!
      use lubksb_357band_mul
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_velo_by_vort_sph_crank
!
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
!
      integer(kind = kint) :: inod
!       integer(kind = kint) :: k, j
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_velo) = d_rj(inod,itor%i_vort)
        d_rj(inod,itor%i_velo) = d_rj(inod,ipol%i_vort)
      end do
!$omp end parallel do
!
      call delete_zero_degree_comp(ipol%i_velo)
!
      if     (sph_bc_U%iflag_icb .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2(ipol%i_velo)
      else if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2(ipol%i_velo)
      else
        call cal_sph_nod_icb_rigid_velo2(ipol%i_velo)
      end if
!
      if(sph_bc_U%iflag_cmb .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2(ipol%i_velo)
      else
        call cal_sph_nod_cmb_rigid_velo2(ipol%i_velo)
      end if
!
!      write(my_rank+70,*) 'k, j, inod, vp_rhs, vt_rhs'
!      do j = 1, nidx_rj(2)
!        j = 3
!        do k = 1, nlayer_CMB
!          inod = (k-1)*nidx_rj(2) + j
!          write(my_rank+70,*) k, j, inod,                              &
!     &                 d_rj(inod,ipol%i_velo), d_rj(inod,itor%i_velo)
!        end do
!      end do
!
      call lubksb_5band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), vp_evo_lu,                            &
     &    i_vp_pivot, d_rj(1,ipol%i_velo) )
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), vt_evo_lu,                            &
     &    i_vt_pivot, d_rj(1,itor%i_velo) )
!
      call const_grad_vp_and_vorticity
!
!      write(my_rank+170,*) 'k, j, vt2, wp2, dwp2'
!      do j = 1, nidx_rj(2)
!         j = 6
!        do k = 1, nlayer_CMB
!            inod = (k-1)*nidx_rj(2) + j
!            write(my_rank+170,'(2i10,1p8E25.15e3)') k, j,              &
!     &              d_rj(inod,ipol%i_velo),d_rj(inod,itor%i_velo)
!     &          d_rj(inod,ipol%i_velo), d_rj(inod,idpdr%i_velo),       &
!     &          d_rj(inod,itor%i_velo), d_rj(inod,itor%i_vort)
!        end do
!      end do
!
      end subroutine cal_sol_velo_by_vort_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_pressure_by_div_v
!
      use set_reference_sph_mhd
!
!      integer(kind = kint) :: k, inod, icmb
!
!
!      if (idx_rj_degree_zero .gt. 0) then
!        write(170,*) 'k, j=0, inod, div_v'
!        do k = 1, nlayer_CMB
!            inod = (k-1)*nidx_rj(2) + idx_rj_degree_zero
!            write(170,'(2i10,1p3E25.15e3)') k,                         &
!     &              idx_rj_degree_zero, d_rj(inod,ipol%i_press)
!        end do
!      end if
!      if (idx_rj_degree_one(0).gt.0) then
!        write(171,*) 'k, j=2, inod, div_v'
!        do k = 1, nlayer_CMB
!            inod = (k-1)*nidx_rj(2) + idx_rj_degree_one(0)
!            write(171,'(2i10,1p3E25.15e3)') k,                         &
!     &              idx_rj_degree_one(0), d_rj(inod,ipol%i_press)
!        end do
!      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), p_poisson_lu, i_p_pivot,              &
     &    d_rj(1,ipol%i_press) )
!
      call adjust_by_ave_pressure_on_CMB
!
!      if (idx_rj_degree_zero .gt. 0) then
!        icmb = (nlayer_CMB-1)*nidx_rj(2) + idx_rj_degree_zero
!        do k = 1, nlayer_CMB
!          inod = (k-1)*nidx_rj(2) + idx_rj_degree_zero
!          d_rj(inod,ipol%i_press) = d_rj(inod,ipol%i_press)            &
!     &                         - d_rj(icmb,ipol%i_press)
!        end do
!      end if
!
!      if (idx_rj_degree_zero .gt. 0) then
!        write(170,*) 'k, j=0, inod, press'
!        do k = 1, nlayer_CMB
!            inod = (k-1)*nidx_rj(2) + idx_rj_degree_zero
!            write(170,'(2i10,1p3E25.15e3)') k,                         &
!     &              idx_rj_degree_zero, d_rj(inod,ipol%i_press)
!        end do
!      end if
!      if (idx_rj_degree_one(0).gt.0) then
!        write(171,*) 'k, j=2, inod, press'
!        do k = 1, nlayer_CMB
!            inod = (k-1)*nidx_rj(2) + idx_rj_degree_one(0)
!            write(171,'(2i10,1p3E25.15e3)') k,                         &
!     &              idx_rj_degree_one(0), d_rj(inod,ipol%i_press)
!        end do
!      end if
!
      end subroutine cal_sol_pressure_by_div_v
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_magne_sph_crank
!
      use m_boundary_params_sph_MHD
      use const_sph_radial_grad
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
!
!
      call delete_zero_degree_comp(ipol%i_magne)
!
      if(sph_bc_B%iflag_icb .eq. iflag_sph_insulator) then
        call cal_sph_nod_icb_ins_mag2(ipol%i_magne)
      else if(sph_bc_B%iflag_icb .eq. iflag_radial_magne) then
        call cal_sph_nod_icb_qvc_mag2(ipol%i_magne)
      end if
!
      if(sph_bc_B%iflag_cmb .eq. iflag_radial_magne) then
        call cal_sph_nod_cmb_qvc_mag2(ipol%i_magne)
      else
        call cal_sph_nod_cmb_ins_mag2(ipol%i_magne)
      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), bs_evo_lu, i_bs_pivot,                &
     &    d_rj(1,ipol%i_magne) )
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), bt_evo_lu, i_bt_pivot,                &
     &    d_rj(1,itor%i_magne) )
!
      call const_grad_bp_and_current
!
      end subroutine cal_sol_magne_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_temperature_sph_crank
!
      use m_boundary_params_sph_MHD
      use set_scalar_boundary_sph
      use adjust_fixed_flux_sph
!
      if (sph_bc_T%iflag_icb .eq. iflag_fixed_flux) then
        call adjust_icb_fix_h_flux_sph
      else
        call set_fixed_scalar_sph(nidx_rj(2), ione, nlayer_ICB,         &
     &      ipol%i_temp, sph_bc_T%ICB_fld)
      end if
!
      if (sph_bc_T%iflag_cmb .eq. iflag_fixed_flux) then
        call adjust_cmb_fix_h_flux_sph
      else
        call set_fixed_scalar_sph(nidx_rj(2), nlayer_CMB, nidx_rj(1),   &
     &      ipol%i_temp, sph_bc_T%CMB_fld)
      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), temp_evo_lu,                          &
     &    i_temp_pivot, d_rj(1,ipol%i_temp) )
!
      end subroutine cal_sol_temperature_sph_crank
!
! -----------------------------------------------------------------------
!
      subroutine cal_sol_composition_sph_crank
!
      use m_boundary_params_sph_MHD
      use set_scalar_boundary_sph
      use adjust_fixed_flux_sph
!
!
      if (sph_bc_C%iflag_icb .eq. iflag_fixed_flux) then
        call adjust_icb_fix_c_flux_sph
      else
        call set_fixed_scalar_sph(nidx_rj(2), ione, nlayer_ICB,         &
     &      ipol%i_light, sph_bc_C%ICB_fld)
      end if
!
      if (sph_bc_C%iflag_cmb .eq. iflag_fixed_flux) then
        call adjust_cmb_fix_c_flux_sph
      else
        call set_fixed_scalar_sph(nidx_rj(2), nlayer_CMB, nidx_rj(1),   &
     &      ipol%i_light, sph_bc_C%CMB_fld)
      end if
!
      call lubksb_3band_mul(np_smp, idx_rj_smp_stack(0,2),              &
     &    nidx_rj(2), nidx_rj(1), composit_evo_lu, i_composit_pivot,    &
     &    d_rj(1,ipol%i_light) )
!
      end subroutine cal_sol_composition_sph_crank
!
! -----------------------------------------------------------------------
!
      end module cal_sol_sph_fluid_crank
