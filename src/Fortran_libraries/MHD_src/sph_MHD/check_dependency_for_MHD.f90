!>@file   check_dependency_for_MHD.f90
!!@brief  module check_dependency_for_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine check_dependencies
!!@endverbatim
!
      module check_dependency_for_MHD
!
      use m_precision
!
      implicit none
!
!>      list of required field name
      character(len=kchara), allocatable :: phys_check_name(:)
      private :: phys_check_name
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_dependencies(num_nod_phys, phys_nod_name)
!
      use calypso_mpi
      use m_control_parameter
      use m_phys_labels
!
      use check_field_dependency
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      character(len=kchara) :: target_name
      integer (kind = kint) :: iflag, i, num_check
!
!
      allocate( phys_check_name(num_nod_phys) )
!
!  check dependencies
!
!
      do i = 1, num_nod_phys
          iflag = 0
!
          if (    phys_nod_name(i) .eq. fhd_velo                        &
     &       .or. phys_nod_name(i) .eq. fhd_filter_v                    &
     &       .or. phys_nod_name(i) .eq. fhd_vort                        &
     &       .or. phys_nod_name(i) .eq. fhd_magne                       &
     &       .or. phys_nod_name(i) .eq. fhd_press                       &
     &       .or. phys_nod_name(i) .eq. fhd_temp                        &
     &       .or. phys_nod_name(i) .eq. fhd_light                       &
     &       .or. phys_nod_name(i) .eq. fhd_kinetic_helicity            &
     &       .or. phys_nod_name(i) .eq. fhd_viscous                     &
     &       .or. phys_nod_name(i) .eq. fhd_mom_flux                    &
     &       .or. phys_nod_name(i) .eq. fhd_inertia                     &
     &       .or. phys_nod_name(i) .eq. fhd_Coriolis                    &
     &       .or. phys_nod_name(i) .eq. fhd_SGS_m_flux                  &
     &       .or. phys_nod_name(i) .eq. fhd_grad_v_1                    &
     &       .or. phys_nod_name(i) .eq. fhd_grad_v_2                    &
     &       .or. phys_nod_name(i) .eq. fhd_grad_v_3                    &
     &       ) then
!
           num_check = 1
           phys_check_name(1) = fhd_velo
!
          else if (phys_nod_name(i) .eq. fhd_press_grad                 &
     &       ) then
            num_check = 1
            phys_check_name(1) = fhd_press
!
          else if ( phys_nod_name(i) .eq. fhd_filter_b                  &
     &         .or. phys_nod_name(i) .eq. fhd_current                   &
     &         .or. phys_nod_name(i) .eq. fhd_mag_potential             &
     &         .or. phys_nod_name(i) .eq. fhd_mag_diffuse               &
     &         .or. phys_nod_name(i) .eq. fhd_mag_tension               &
     &         .or. phys_nod_name(i) .eq. fhd_Lorentz                   &
     &         .or. phys_nod_name(i) .eq. fhd_maxwell_t                 &
     &         .or. phys_nod_name(i) .eq. fhd_SGS_maxwell_t             &
     &       ) then 
           num_check = 1
           phys_check_name(1) = fhd_magne
!
          else if ( phys_nod_name(i) .eq. fhd_filter_a                  &
     &         .or. phys_nod_name(i) .eq. fhd_scalar_potential          &
     &         .or. phys_nod_name(i) .eq. fhd_magnetic_helicity         &
     &         .or. phys_nod_name(i) .eq. fhd_vecp_diffuse              &
     &       ) then
           num_check = 1
           phys_check_name(1) = fhd_vecp
!
          else if ( phys_nod_name(i) .eq. fhd_thermal_diffusion         &
     &         .or. phys_nod_name(i) .eq. fhd_part_temp                 &
     &         .or. phys_nod_name(i) .eq. fhd_filter_temp               &
     &         .or. phys_nod_name(i) .eq. fhd_buoyancy                  &
     &         .or. phys_nod_name(i) .eq. fhd_heat_source               &
     &         .or. phys_nod_name(i) .eq. fhd_grad_temp                 &
     &       ) then
           num_check = 1
           phys_check_name(1) = fhd_temp
!
          else if (phys_nod_name(i) .eq. fhd_SGS_m_ene_gen              &
     &       ) then
            num_check = 1
            phys_check_name(1) = fhd_SGS_induction
!
          else if  (phys_nod_name(i) .eq. fhd_grad_composit             &
     &         .or. phys_nod_name(i) .eq. fhd_filter_comp               &
     &         .or. phys_nod_name(i) .eq. fhd_comp_buo                  &
     &         .or. phys_nod_name(i) .eq. fhd_c_diffuse                 &
     &         .or. phys_nod_name(i) .eq. fhd_light_source              &
     &       ) then
            num_check = 1
            phys_check_name(1) = fhd_light
!
          else if ( phys_nod_name(i) .eq. fhd_filter_buo                &
     &       ) then
           num_check = 1
           phys_check_name(1) = fhd_filter_temp
!
          else if (phys_nod_name(i) .eq. fhd_temp_scale                 &
     &       ) then
            num_check = 2
            phys_check_name(1) = fhd_temp
            phys_check_name(2) = fhd_thermal_diffusion
!
          else if (phys_nod_name(i) .eq. fhd_composition_scale          &
     &       ) then
            num_check = 2
            phys_check_name(1) = fhd_light
            phys_check_name(2) = fhd_c_diffuse
!
!
          else if ( phys_nod_name(i) .eq. fhd_mag_induct                &
     &       ) then
!
           if (iflag_t_evo_4_magne .gt. id_no_evolution) then
            num_check = 2
            phys_check_name(1) = fhd_velo
            phys_check_name(2) = fhd_magne
           else
            num_check = 1
            phys_check_name(1) = fhd_vp_induct
           end if
!
          else if ( phys_nod_name(i) .eq. fhd_mag_stretch               &
     &       ) then
            num_check = 4
            phys_check_name(1) = fhd_magne
            phys_check_name(2) = fhd_grad_v_1
            phys_check_name(3) = fhd_grad_v_2
            phys_check_name(4) = fhd_grad_v_3

!
          else if ( phys_nod_name(i) .eq. fhd_div_SGS_m_flux            &
     &       ) then
            num_check = 1
            phys_check_name(1) = fhd_SGS_m_flux
!
          else if ( phys_nod_name(i) .eq. fhd_SGS_Lorentz               &
     &       ) then
            num_check = 1
            phys_check_name(1) = fhd_SGS_maxwell_t
!
          else if ( phys_nod_name(i) .eq. fhd_SGS_induction             &
     &       ) then
           if (iflag_t_evo_4_magne .gt. id_no_evolution) then
            num_check = 2
            phys_check_name(1) = fhd_velo
            phys_check_name(2) = fhd_magne
           else
            num_check = 1
            phys_check_name(1) = fhd_SGS_vp_induct
           end if
!
          else if ( phys_nod_name(i) .eq. fhd_cross_helicity            &
     &         .or. phys_nod_name(i) .eq. fhd_vp_induct                 &
     &         .or. phys_nod_name(i) .eq. fhd_induct_t                  &
     &         .or. phys_nod_name(i) .eq. fhd_SGS_vp_induct             &
     &         .or. phys_nod_name(i) .eq. fhd_SGS_induct_t              &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_magne
!
          else if (phys_nod_name(i) .eq. fhd_buoyancy_flux              &
     &        .or. phys_nod_name(i) .eq. fhd_h_flux                     &
     &        .or. phys_nod_name(i) .eq. fhd_ph_flux                    &
     &        .or. phys_nod_name(i) .eq. fhd_SGS_h_flux                 &
     &        .or. phys_nod_name(i) .eq. fhd_heat_advect                &
     &        .or. phys_nod_name(i) .eq. fhd_part_h_advect              &
     &        .or. phys_nod_name(i) .eq. fhd_div_SGS_h_flux             &
     &        .or. phys_nod_name(i) .eq. fhd_SGS_temp_gen               &
     &        .or. phys_nod_name(i) .eq. fhd_entropy                    &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_temp
!
          else if (phys_nod_name(i) .eq. fhd_SGS_buoyancy               &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_SGS_h_flux
!
          else if (phys_nod_name(i) .eq. fhd_SGS_comp_buo               &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_SGS_h_flux
!
          else if (phys_nod_name(i) .eq. fhd_SGS_buo_flux               &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_SGS_h_flux
!
          else if (phys_nod_name(i) .eq. fhd_SGS_comp_buo_flux          &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_SGS_h_flux
!
          else if (phys_nod_name(i) .eq. fhd_composit_advect            &
     &        .or. phys_nod_name(i) .eq. fhd_c_flux                     &
     &        .or. phys_nod_name(i) .eq. fhd_comp_buo_flux              &
     &        .or. phys_nod_name(i) .eq. fhd_SGS_c_flux                 &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_light
!
          else if (phys_nod_name(i) .eq. fhd_filter_buo_flux            &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_filter_temp
!
!
          else if (phys_nod_name(i) .eq. fhd_temp_generation            &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_heat_advect
           phys_check_name(2) = fhd_temp
!
          else if (phys_nod_name(i) .eq. fhd_part_temp_gen              &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_part_h_advect
           phys_check_name(2) = fhd_temp
!
          else if (phys_nod_name(i) .eq. fhd_kinetic_helicity           &
     &        .or. phys_nod_name(i) .eq. fhd_velocity_scale             &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_vort
!
          else if (phys_nod_name(i) .eq. fhd_current_helicity           &
     &        .or. phys_nod_name(i) .eq. fhd_magnetic_scale             &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_magne
           phys_check_name(2) = fhd_current
!
          else if (phys_nod_name(i) .eq. fhd_mag_ene_gen                &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_magne
           phys_check_name(2) = fhd_mag_induct
!
          else if (phys_nod_name(i) .eq. fhd_mag_tension_work           &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_mag_tension
!
          else if (phys_nod_name(i) .eq. fhd_vis_ene_diffuse            &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_viscous
!
          else if (phys_nod_name(i) .eq. fhd_mag_ene_diffuse            &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_magne
           phys_check_name(2) = fhd_mag_diffuse
!
          else if (phys_nod_name(i) .eq. fhd_Reynolds_work              &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_div_SGS_m_flux
!
!
          else if ( phys_nod_name(i) .eq. fhd_vecp                      &
     &       ) then
           num_check = 3
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_magne
           phys_check_name(3) = fhd_mag_potential
!
          else if (phys_nod_name(i) .eq. fhd_Lorentz_work               &
     &        .or. phys_nod_name(i) .eq. fhd_work_agst_Lorentz          &
     &        .or. phys_nod_name(i) .eq. fhd_SGS_Lorentz_work           &
     &       ) then
           num_check = 3
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_magne
           phys_check_name(3) = fhd_current
!
          else if (phys_nod_name(i) .eq. fhd_density                    &
     &       ) then
           num_check = 3
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_temp
           phys_check_name(3) = fhd_light
!
          else if (phys_nod_name(i) .eq. fhd_per_density                &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_density
           phys_check_name(2) = fhd_ref_density
!
          else if (phys_nod_name(i) .eq. fhd_entropy_source             &
     &       ) then
           num_check = 1
           phys_check_name(1) = fhd_entropy
!
          else if (phys_nod_name(i) .eq. fhd_per_entropy                &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_entropy
           phys_check_name(2) = fhd_ref_entropy
!
          else if (phys_nod_name(i) .eq. fhd_e_field                    &
     &       ) then
            num_check = 2
            phys_check_name(1) = fhd_vp_induct
            phys_check_name(2) = fhd_current
!
          else if (phys_nod_name(i) .eq. fhd_poynting                   &
     &       ) then
             num_check = 2
             phys_check_name(1) = fhd_current
             phys_check_name(2) = fhd_vp_induct
!
          else if (phys_nod_name(i) .eq. fhd_div_m_flux                 &
     &         ) then
           num_check = 1
           phys_check_name(1) = fhd_mom_flux
!
          else if (phys_nod_name(i) .eq. fhd_div_maxwell_t              &
     &         ) then
           num_check = 1
           phys_check_name(1) = fhd_maxwell_t
!
          else if (phys_nod_name(i) .eq. fhd_div_induct_t               &
     &         ) then
           num_check = 1
           phys_check_name(1) = fhd_induct_t
!
          else if (phys_nod_name(i) .eq. fhd_div_h_flux                 &
     &         ) then
           num_check = 3
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_temp
           phys_check_name(3) = fhd_h_flux
!
          else if (phys_nod_name(i) .eq. fhd_div_ph_flux                &
     &         ) then
           num_check = 3
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_temp
           phys_check_name(3) = fhd_ph_flux
!
          else if (phys_nod_name(i) .eq. fhd_SGS_div_h_flux_true        &
     &         ) then
           num_check = 3
           phys_check_name(1) = fhd_filter_v
           phys_check_name(2) = fhd_filter_temp
           phys_check_name(3) = fhd_div_h_flux
!
          else if (phys_nod_name(i) .eq. fhd_SGS_div_m_flux_true        &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_filter_v
           phys_check_name(2) = fhd_div_m_flux
!
          else if (phys_nod_name(i) .eq. fhd_SGS_Lorentz_true           &
     &         ) then
           num_check = 2
           phys_check_name(1) = fhd_filter_b
           phys_check_name(2) = fhd_div_maxwell_t
!
          else if (phys_nod_name(i) .eq. fhd_SGS_mag_induct_true        &
     &         ) then
           num_check = 3
           phys_check_name(1) = fhd_filter_v
           phys_check_name(2) = fhd_filter_b
           phys_check_name(3) = fhd_div_induct_t
!
          else if (phys_nod_name(i) .eq. fhd_SGS_Lorentz_wk_true        &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_SGS_Lorentz_true
!
          else if (phys_nod_name(i) .eq. fhd_Reynolds_work_true         &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_SGS_div_m_flux_true
!
          else if (phys_nod_name(i) .eq. fhd_SGS_temp_gen_true          &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_temp
           phys_check_name(2) = fhd_SGS_div_h_flux_true
!
          else if (phys_nod_name(i) .eq. fhd_SGS_m_ene_gen_true         &
     &       ) then
           num_check = 2
           phys_check_name(1) = fhd_magne
           phys_check_name(2) = fhd_SGS_mag_induct_true
!
          else if (phys_nod_name(i) .eq. fhd_div_Lorentz                &
     &        .or. phys_nod_name(i) .eq. fhd_rot_Lorentz                &
     &       ) then
           num_check = 1
           phys_check_name(1) = fhd_Lorentz
!
!
!   Old field label... Should be deleted later!!
          else if (phys_nod_name(i) .eq. fhd_buoyancy_work) then
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_temp
!
!   If there is no field on list....
          else
            num_check = 0
          end if
!
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           phys_nod_name(i), phys_nod_name, phys_check_name)
      end do
!
!
!   check dependencies for time evolution
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution                     &
     &     .and. iflag_t_evo_4_magne .gt. id_no_evolution) then
         call calypso_MPI_abort(402,'You should choose vector potential &
     &  OR magnetic field for time evolution')
      end if
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
           target_name = 'time integration for velocity'
           num_check = 2
           phys_check_name(1) = fhd_velo
           phys_check_name(2) = fhd_press
           call check_dependence_phys(num_nod_phys, num_check,          &
     &           target_name, phys_nod_name, phys_check_name)
      end if
!
      if (iflag_t_evo_4_temp .gt. id_no_evolution) then
           target_name = 'time integration for temperature'
           num_check = 2
           phys_check_name(1) = fhd_temp
           phys_check_name(2) = fhd_velo
           call check_dependence_phys(num_nod_phys, num_check,          &
     &           target_name, phys_nod_name, phys_check_name)
      end if
!
      if (iflag_t_evo_4_composit .ne. id_no_evolution) then
           target_name = 'time integration for dummy scalar'
           num_check = 2
           phys_check_name(1) = fhd_light
           phys_check_name(2) = fhd_velo
           call check_dependence_phys(num_nod_phys, num_check,          &
     &           target_name, phys_nod_name, phys_check_name)
      end if
!
      if (iflag_t_evo_4_magne .ne. id_no_evolution) then
           target_name = 'time integration for magnetic field'
           num_check = 2
           phys_check_name(1) = fhd_magne
           phys_check_name(2) = fhd_velo
           call check_dependence_phys(num_nod_phys, num_check,          &
     &           target_name, phys_nod_name, phys_check_name)
      end if
!
      if (iflag_t_evo_4_vect_p .gt. id_no_evolution) then
           target_name = 'time integration for vector potential'
           num_check = 2
           phys_check_name(1) = fhd_vecp
           phys_check_name(2) = fhd_velo
           call check_dependence_phys(num_nod_phys, num_check,          &
     &           target_name, phys_nod_name, phys_check_name)
      end if
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        if ( iflag_SGS_inertia .ne. id_SGS_none) then
          target_name = 'solving SGS momentum flux'
          num_check = 1
          phys_check_name(1) = fhd_SGS_m_flux
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
!
        if (iflag_SGS_lorentz .ne. id_SGS_none) then
          target_name = 'solving SGS lorentz term'
          num_check = 1
          phys_check_name(1) = fhd_SGS_maxwell_t
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if ( iflag_SGS_heat .ne. id_SGS_none) then
          target_name = 'solving SGS heat flux'
          num_check = 1
          phys_check_name(1) = fhd_SGS_h_flux
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_magne .gt. id_no_evolution) then
        if ( iflag_SGS_induction .ne. id_SGS_none) then
          target_name = 'solving SGS magnetic induction'
          num_check = 1
          phys_check_name(1) = fhd_SGS_induct_t
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if ( iflag_SGS_induction .ne. id_SGS_none) then
          target_name = 'solving SGS induction 4 vector potential'
          num_check = 1
          phys_check_name(1) = fhd_SGS_vp_induct
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_composit .gt. id_no_evolution) then
        if (iflag_SGS_comp_flux .ne. id_SGS_none) then
          target_name = 'solving SGS compsition flux'
          num_check = 1
          phys_check_name(1) = fhd_SGS_c_flux
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        if ( iflag_4_gravity .gt. id_turn_OFF) then
          target_name = 'temperature is required for buoyancy'
          num_check = 1
          phys_check_name(1) = fhd_temp
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
!
        if ( iflag_4_composit_buo .gt. id_turn_OFF) then
          target_name                                                   &
     &        = 'composition is required for compositional buoyancy'
          num_check = 1
          phys_check_name(1) = fhd_light
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
!
        if ( iflag_4_filter_gravity .gt. id_turn_OFF) then
          target_name                                                   &
     &      = 'filtered temperature is required for filtered buoyancy'
          num_check = 1
          phys_check_name(1) = fhd_filter_temp
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
!
        if ( iflag_4_lorentz .gt. id_turn_OFF) then
          target_name = 'magnetic field is required for Lorentz force'
          num_check = 1
          phys_check_name(1) = fhd_magne
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
!
!
        if ( iflag_SGS_inertia .eq. id_SGS_similarity                   &
     &     .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          target_name = 'filterd u is required for SGS momentum flux'
          num_check = 1
          phys_check_name(1) = fhd_filter_v
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
!
        if (    iflag_SGS_lorentz .eq. id_SGS_similarity                &
     &     .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          target_name = 'filterd B is required for SGS Lorentz term'
          num_check = 1
          phys_check_name(1) = fhd_filter_b
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
      if(iflag_SGS_gravity .gt. id_SGS_none) then
        if(iflag_4_gravity .eq. id_turn_OFF                             &
     &     .and. iflag_4_composit_buo .eq. id_turn_OFF) then
          call calypso_MPI_abort(100, 'set one of buoyancy sources')
        end if
        if(iflag_4_gravity .gt. id_turn_OFF) then
          if(iflag_SGS_inertia.eq.id_SGS_none                           &
     &       .or. iflag_SGS_heat.eq.id_SGS_none) then
            call calypso_MPI_abort(100,                                 &
     &          'Turn on SGS momentum flux and heat flux')
          end if
        end if
        if(iflag_4_composit_buo .gt. id_turn_OFF) then
          if(iflag_SGS_inertia.eq.id_SGS_none                           &
     &       .or. iflag_SGS_comp_flux.eq.id_SGS_none) then
              call calypso_MPI_abort(100,                               &
     &          'Turn on SGS momentum flux and composition flux')
          end if
        end if
      end if
!
      if ( iflag_t_evo_4_temp .gt. id_no_evolution) then
        if (iflag_SGS_heat .eq. id_SGS_similarity                       &
     &          .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          target_name = 'filterd temp. is required for SGS heat flux'
          num_check = 1
          phys_check_name(1) = fhd_filter_temp
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_magne .gt. id_no_evolution) then
        if ( iflag_SGS_induction .eq. id_SGS_similarity                 &
     &      .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          target_name = 'filterd u and B is required for SGS induction'
          num_check = 2
          phys_check_name(1) = fhd_filter_v
          phys_check_name(2) = fhd_filter_b
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if ( iflag_SGS_induction .eq. id_SGS_similarity                 &
     &      .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          target_name = 'filterd u and B is required for SGS induction'
          num_check = 2
          phys_check_name(1) = fhd_filter_v
          phys_check_name(2) = fhd_filter_b
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
      if ( iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        if ( iflag_commute_correction .gt. id_SGS_commute_OFF           &
     &       .and. iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
          target_name = 'filterd A is required for dynamic model'
          num_check = 1
          phys_check_name(1) = fhd_filter_a
          call check_dependence_phys(num_nod_phys, num_check,           &
     &           target_name, phys_nod_name, phys_check_name)
        end if
      end if
!
      deallocate (phys_check_name)
!
      end subroutine check_dependencies
!
! -----------------------------------------------------------------------
!
      end module check_dependency_for_MHD
