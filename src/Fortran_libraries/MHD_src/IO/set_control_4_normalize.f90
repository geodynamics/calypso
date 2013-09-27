!>@file   set_control_4_normalize.f90
!!@brief  module set_control_4_normalize
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2001
!!@n    Mmodified by H. Matsui in Aug., 2007
!
!> @brief set normalizatios for MHD simulation from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_normalize
!!@endverbatim
!
      module set_control_4_normalize
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_normalize
!
      use calypso_mpi
      use m_parallel_var_dof
      use m_machine_parameter
      use m_control_parameter
      use m_normalize_parameter
      use m_node_phys_address
      use m_ctl_data_momentum_norm
      use m_ctl_data_termal_norm
      use m_ctl_data_induct_norm
      use m_ctl_data_composite_norm
      use m_ctl_data_mhd_normalize
!
      integer (kind = kint) :: i
!
!
!   set normalization
!
      if (i_num_dimless .eq. 0) then
          e_message =                                                   &
     &     'Set dimensionless numbers'
          call calypso_MPI_abort(90, e_message)
      else
        num_dimless = num_dimless_ctl
      end if
!
      if (num_dimless/=0) then
        allocate(name_dimless(num_dimless))
        allocate(dimless(num_dimless))
        name_dimless(1:num_dimless) =  name_dimless_ctl(1:num_dimless)
        dimless(1:num_dimless) =       dimless_ctl(1:num_dimless)
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'num_dimless ',num_dimless
        do i = 1, num_dimless
          write(*,*) i, trim(name_dimless(i)), ': ', dimless(i)
        end do
      end if
!
!    set normalization for thermal
!
      if (iflag_t_evo_4_temp .eq. id_no_evolution) then
        num_coef_4_termal =    0
        num_coef_4_t_diffuse = 0
        num_coef_4_h_source =  0
      else
!
        if (i_n_thermal.eq.0) then
          e_message =                                                   &
     &     'Set coefficients for time stepping for temperature'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_termal = num_coef_4_termal_ctl
        end if
!
        if (i_n_t_diff.eq.0) then
          e_message =                                                   &
     &     'Set coefficients for time stepping for thermal diffusion'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_t_diffuse = num_coef_4_t_diffuse_ctl
        end if
!
        if (i_n_h_src .eq. 0) then
          num_coef_4_h_source = num_coef_4_h_source_ctl
        end if
      end if
!
      if (num_coef_4_termal.gt.0) then
        call allocate_coef_4_termal
        coef_4_termal_name = coef_4_termal_name_ctl
        coef_4_termal_power = coef_4_termal_power_ctl
        call deallocate_coef_4_termal_ctl
      end if
!
      if (num_coef_4_t_diffuse.gt.0) then
        call allocate_coef_4_t_diffuse
        coef_4_t_diffuse_name = coef_4_t_diffuse_name_ctl
        coef_4_t_diffuse_power = coef_4_t_diffuse_power_ctl
        call deallocate_coef_4_t_diffuse_ctl
      end if
!
      if (num_coef_4_h_source.gt.0) then
        call allocate_coef_4_h_source
        coef_4_h_source_name = coef_4_h_src_name_ctl
        coef_4_h_source_power = coef_4_h_src_power_ctl
        call deallocate_coef_4_h_source_ctl
      end if
!
!    set coefficients for momentum equation
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        num_coef_4_velocity =  0
        num_coef_4_press =     0
        num_coef_4_v_diffuse = 0
        num_coef_4_buoyancy =  0
        num_coef_4_Coriolis =  0
        num_coef_4_Lorentz =   0
!
      else
!
        if (i_n_mom.eq.0) then
          e_message =                                                   &
     &     'Set coefficients for time stepping for velocity'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_velocity = num_coef_4_velocity_ctl
        end if
!
        if (i_n_press.eq.0) then
          e_message =                                                   &
     &     'Set coefficients for pressure gradient'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_press = num_coef_4_press_ctl
        end if
!
        if (i_n_v_diff.eq.0) then
          e_message =                                                   &
     &     'Set coefficients for viscosity'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_v_diffuse = num_coef_4_v_diffuse_ctl
        end if
!
        if(iflag_4_gravity .eq. id_turn_OFF                             &
     &      .and. iflag_4_filter_gravity .eq. id_turn_OFF) then
          num_coef_4_buoyancy = 0
        else
          if (i_n_buo.eq.0) then
            e_message = 'Set coefficients for buoyancy'
            call calypso_MPI_abort(90, e_message)
          else
            num_coef_4_buoyancy = num_coef_4_buoyancy_ctl
          end if
        end if
        if (iflag_debug .ge. iflag_routine_msg) then
          write(*,*) 'i_n_c_buo', i_n_c_buo
          write(*,*) 'num_coef_4_buoyancy', num_coef_4_buoyancy
        end if
!
        if (iflag_4_composit_buo .eq. id_turn_OFF) then
          num_coef_4_comp_buo = 0
        else
          if (i_n_c_buo.eq.0) then
            e_message = 'Set coefficients for compiositional buoyancy'
            call calypso_MPI_abort(90, e_message)
          else
            num_coef_4_comp_buo = num_coef_4_comp_buo_ctl
          end if
        end if
!
        if (iflag_4_coriolis .eq. id_turn_OFF) then
          num_coef_4_Coriolis = 0
        else
          if (i_n_cor.eq.0) then
            e_message = 'Set coefficients for Coriolis force'
            call calypso_MPI_abort(90, e_message)
          else
            num_coef_4_Coriolis = num_coef_4_Coriolis_ctl
          end if
        end if
!
        if (iflag_4_lorentz .eq. id_turn_OFF) then
          num_coef_4_Lorentz = 0
        else
          if (i_n_lor.eq.0) then
            e_message = 'Set coefficients for Lorentz force'
            call calypso_MPI_abort(90, e_message)
          else
            num_coef_4_Lorentz = num_coef_4_Lorentz_ctl
          end if
        end if
      end if
!
      if (num_coef_4_velocity.gt.0) then
        call allocate_coef_4_velocity
        coef_4_velocity_name = coef_4_velocity_name_ctl
        coef_4_velocity_power = coef_4_velocity_power_ctl
        call deallocate_coef_4_velocity_ctl
      end if
!
      if (num_coef_4_press.gt.0) then
        call allocate_coef_4_press
        coef_4_press_name = coef_4_press_name_ctl
        coef_4_press_power = coef_4_press_power_ctl
        call deallocate_coef_4_press_ctl
      end if
!
      if (num_coef_4_v_diffuse.gt.0) then
        call allocate_coef_4_v_diffuse
        coef_4_v_diffuse_name = coef_4_v_diffuse_name_ctl
        coef_4_v_diffuse_power = coef_4_v_diffuse_power_ctl
        call deallocate_coef_4_v_diffuse_ctl
      end if
!
      if (num_coef_4_buoyancy.gt.0) then
        call allocate_coef_4_buoyancy
        coef_4_buoyancy_name = coef_4_buoyancy_name_ctl
        coef_4_buoyancy_power = coef_4_buoyancy_power_ctl
        call deallocate_coef_4_buoyancy_ctl
      end if
!
      if (num_coef_4_comp_buo.gt.0) then
        call allocate_coef_4_comp_buo
        coef_4_comp_buo_name =  coef_4_comp_buo_name_ctl
        coef_4_comp_buo_power = coef_4_comp_buo_power_ctl
        call deallocate_coef_4_comp_buo_ctl
      end if
!
      if (num_coef_4_Coriolis.gt.0) then
        call allocate_coef_4_coriolis
        coef_4_Coriolis_name = coef_4_Coriolis_name_ctl
        coef_4_Coriolis_power = coef_4_Coriolis_power_ctl
        call deallocate_coef_4_coriolis_ctl
      end if
!
      if (num_coef_4_Lorentz.gt.0) then
        call allocate_coef_4_lorentz
        coef_4_Lorentz_name = coef_4_Lorentz_name_ctl
        coef_4_Lorentz_power = coef_4_Lorentz_power_ctl
        call deallocate_coef_4_lorentz_ctl
      end if
!
!    coefficients for inducition equation
!
      if (iflag_t_evo_4_magne .eq. id_no_evolution                      &
     &  .and. iflag_t_evo_4_vect_p .eq. id_no_evolution) then
        num_coef_4_magnetic = 0
        num_coef_4_mag_p = 0
        num_coef_4_m_diffuse = 0
        num_coef_4_induction = 0
!
      else
!
        if (i_n_magne.eq.0) then
          e_message =                                                   &
     &     'Set coefficients for integration for magnetic field'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_magnetic = num_coef_4_magnetic_ctl
        end if
!
        if (i_n_mag_p.eq.0                                              &
     &       .and. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          e_message =                                                   &
     &     'Set coefficients for integration for magnetic potential'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_mag_p = num_coef_4_mag_p_ctl
        end if
!
        if (i_n_m_diff.eq.0) then
          e_message = 'Set coefficients for magnetic diffusion'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_m_diffuse = num_coef_4_m_diffuse_ctl
        end if
!
        if (i_n_induct.eq.0) then
          e_message = 'Set coefficients for induction term'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_induction = num_coef_4_induction_ctl
        end if
!
      end if
!
      if (num_coef_4_magnetic.gt.0) then
        call allocate_coef_4_magne
        coef_4_magnetic_name = coef_4_magnetic_name_ctl
        coef_4_magnetic_power = coef_4_magnetic_power_ctl
        call deallocate_coef_4_magne_ctl
      end if
!
      if (num_coef_4_mag_p.gt.0) then
        call allocate_coef_4_mag_p
        coef_4_mag_p_name = coef_4_mag_p_name_ctl
        coef_4_mag_p_power = coef_4_mag_p_power_ctl
        call deallocate_coef_4_mag_p_ctl
      end if
!
      if (num_coef_4_m_diffuse.gt.0) then
        call allocate_coef_4_m_diffuse
        coef_4_m_diffuse_name = coef_4_m_diffuse_name_ctl
        coef_4_m_diffuse_power = coef_4_m_diffuse_power_ctl
        call deallocate_coef_4_m_diffuse_ctl
      end if
!
      if (num_coef_4_induction.gt.0) then
        call allocate_coef_4_induction
        coef_4_induction_name = coef_4_induction_name_ctl
        coef_4_induction_power = coef_4_induction_power_ctl
        call deallocate_coef_4_induction_ctl
      end if
!
!    set normalization for dummy scalar
!
      if (iflag_t_evo_4_composit .eq. id_no_evolution) then
        num_coef_4_composition =  0
        num_coef_4_c_diffuse =    0
        num_coef_4_c_source =     0
      else
!
        if (i_n_dscalar .eq. 0) then
          e_message =                                                   &
     &     'Set coefficients for time stepping for composition scalar'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_composition = num_coef_4_composit_ctl
        end if
!
        if (i_n_dsc_diff .eq. 0) then
          e_message =                                                   &
     &     'Set coefficients for time stepping for scalar diffusion'
          call calypso_MPI_abort(90, e_message)
        else
          num_coef_4_c_diffuse = num_coef_4_c_diffuse_ctl
        end if
!
        if (i_n_dsc_src .gt. 0) then
          num_coef_4_c_source = num_coef_4_c_src_ctl
        end if
      end if
!
      if (num_coef_4_composition .gt. 0) then
        call allocate_coef_4_composition
        coef_4_composit_name =  coef_4_composit_name_ctl
        coef_4_composit_power = coef_4_composit_power_ctl
        call deallocate_coef_4_dscalar_ctl
      end if
!
      if (num_coef_4_c_diffuse .gt. 0) then
        call allocate_coef_4_c_diffuse
        coef_4_c_diffuse_name =  coef_4_c_diff_name_ctl
        coef_4_c_diffuse_power = coef_4_c_diff_power_ctl
        call deallocate_coef_4_dsc_diff_ctl
      end if
!
      if (num_coef_4_c_source .gt. 0) then
        call allocate_coef_4_c_source
        coef_4_c_source_name =  coef_4_c_src_name_ctl
        coef_4_c_source_power = coef_4_c_src_power_ctl
        call deallocate_coef_4_dsc_src_ctl
      end if
!
      end subroutine s_set_control_4_normalize
!
! -----------------------------------------------------------------------
!
      end module set_control_4_normalize
