!>@file   set_control_4_force.f90
!!@brief  module set_control_4_force
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for forces from control data
!!
!!@verbatim
!!     subroutine s_set_control_4_force
!!@endverbatim
!
      module set_control_4_force
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_force
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_mhd_forces
      use m_physical_property
      use skip_comment_f
!
      integer (kind = kint) :: i, iflag
!
!
      iflag_4_gravity =        id_turn_OFF
      iflag_4_coriolis =       id_turn_OFF
      iflag_4_lorentz =        id_turn_OFF
      iflag_4_rotate =         id_turn_OFF
      iflag_4_composit_buo =   id_turn_OFF
      iflag_4_filter_gravity = id_turn_OFF
!
      if (iflag_t_evo_4_velo .eq. id_no_evolution) then
        num_force = 0
      else
        if (force_names_ctl%icou .gt. 0) then
          num_force = force_names_ctl%num
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &      write(*,*) 'num_force ', num_force
        end if
      end if
!
      if (num_force .gt. 0) then
!
        call allocate_force_list
        name_force(1:num_force) = force_names_ctl%c_tbl(1:num_force)
        call deallocate_name_force_ctl
!
        do i = 1, num_force
          if(    cmp_no_case(name_force(i), 'Gravity')                  &
     &      .or. cmp_no_case(name_force(i), 'Gravity_ele')              &
     &      .or. cmp_no_case(name_force(i), 'Gravity_element')          &
     &      .or. cmp_no_case(name_force(i), 'Buoyancy')                 &
     &      .or. cmp_no_case(name_force(i), 'Buoyancy_ele')             &
     &      .or. cmp_no_case(name_force(i), 'Buoyancy_element')         &
     &      .or. cmp_no_case(name_force(i), 'Thermal_buoyancy')         &
     &      .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_ele')     &
     &      .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_element') &
     &      .or. cmp_no_case(name_force(i), 'Thermal_gravity')          &
     &      .or. cmp_no_case(name_force(i), 'Thermal_gravity_ele')      &
     &      .or. cmp_no_case(name_force(i), 'Thermal_gravity_element')  &
     &      ) iflag_4_gravity =  id_FORCE_ele_int
!
          if(     cmp_no_case(name_force(i), 'Gravity_nod')             &
     &       .or. cmp_no_case(name_force(i), 'Buoyancy_nod')            &
     &       .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_nod')    &
     &       .or. cmp_no_case(name_force(i), 'Thermal_gravity_nod')     &
     &       .or. cmp_no_case(name_force(i), 'Gravity_node')            &
     &       .or. cmp_no_case(name_force(i), 'Buoyancy_node')           &
     &       .or. cmp_no_case(name_force(i), 'Thermal_buoyancy_node')   &
     &       .or. cmp_no_case(name_force(i), 'Thermal_gravity_node')    &
     &      ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then
              iflag_4_gravity = id_FORCE_ele_int
            else
              iflag_4_gravity = id_FORCE_at_node
            end if
          end if
!
          if(  cmp_no_case(name_force(i), 'Composite_buoyancy')         &
     &    .or. cmp_no_case(name_force(i), 'Composite_buoyancy_ele')     &
     &    .or. cmp_no_case(name_force(i), 'Composite_buoyancy_element') &
     &    .or. cmp_no_case(name_force(i), 'Composite_gravity')          &
     &    .or. cmp_no_case(name_force(i), 'Composite_gravity_ele')      &
     &    .or. cmp_no_case(name_force(i), 'Composite_gravity_element')  &
     &       ) iflag_4_composit_buo =  id_FORCE_ele_int
!
          if(     cmp_no_case(name_force(i), 'Composite_buoyancy_nod')  &
     &       .or. cmp_no_case(name_force(i), 'Composite_gravity_nod')   &
     &       .or. cmp_no_case(name_force(i), 'Composite_buoyancy_node') &
     &       .or. cmp_no_case(name_force(i), 'Composite_gravity_node')  &
     &       ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then
              iflag_4_composit_buo = id_FORCE_ele_int
            else
              iflag_4_composit_buo = id_FORCE_at_node
            end if
          end if
!
          if(     cmp_no_case(name_force(i), 'Filtered_gravity')        &
     &       .or. cmp_no_case(name_force(i), 'Filtered_buoyancy')       &
     &       ) iflag_4_filter_gravity =  id_FORCE_ele_int
!
          if (cmp_no_case(name_force(i), 'Coriolis')                    &
     &        )  iflag_4_coriolis = id_FORCE_ele_int
!
          if (cmp_no_case(name_force(i), 'Coriolis_node')) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then
              iflag_4_coriolis = id_FORCE_ele_int
            else
              iflag_4_coriolis = id_FORCE_at_node
            end if
          end if
!
          if(cmp_no_case(name_force(i), 'Coriolis_imp')) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson) then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass)   &
     &          then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else
              iflag_4_coriolis = id_FORCE_ele_int
            end if
          end if
!
          if(cmp_no_case(name_force(i), 'Coriolis_node_imp')) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson) then
              iflag_4_coriolis = id_Coriolis_nod_imp
            else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass)   &
     &               then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else
              iflag_4_coriolis = id_FORCE_ele_int
            end if
          end if
!
          if(cmp_no_case(name_force(i), 'Lorentz')                      &
     &           )  iflag_4_lorentz = id_turn_ON
          if(cmp_no_case(name_force(i), 'Lorentz_full')                 &
     &           )  iflag_4_lorentz = id_Lorentz_w_Emag
!
          if(cmp_no_case(name_force(i), 'Rotation_form')                &
     &           )  iflag_4_rotate =  id_turn_ON
        end do
      end if
!
!  direction of gravity
!
      i_grav = iflag_no_gravity
      iflag = iflag_4_gravity + iflag_4_composit_buo                    &
     &       + iflag_4_filter_gravity
      if (iflag .gt. 0) then
        if (i_gravity_type .eq. 0) then
          i_grav = iflag_self_r_g
        else
!
          if     (gravity_ctl .eq. 'constant'                           &
     &       .or. gravity_ctl .eq. 'Constant'                           &
     &       .or. gravity_ctl .eq. 'CONSTANT'                           &
     &       .or. gravity_ctl .eq. '0') then
             i_grav = iflag_const_g
          else if(gravity_ctl .eq. 'constant_radial'                    &
     &       .or. gravity_ctl .eq. 'Constant_radial'                    &
     &       .or. gravity_ctl .eq. 'CONSTANT_RADIAL'                    &
     &       .or. gravity_ctl .eq. '1') then
             i_grav = iflag_radial_g
          else if(gravity_ctl .eq. 'radial'                             &
     &       .or. gravity_ctl .eq. 'Radial'                             &
     &       .or. gravity_ctl .eq. 'RADIAL'                             &
     &       .or. gravity_ctl .eq. '2') then
             i_grav = iflag_self_r_g
           end if
        end if
!
        if (i_grav .eq. iflag_const_g) then
          if (gravity_vector_ctl%icou .eq. 0) then
            e_message = 'Set gravity vector'
            call calypso_MPI_abort(ierr_force, e_message)
          else
!
            do i = 1, gravity_vector_ctl%num
              if(cmp_no_case(gravity_vector_ctl%c_tbl(i),'X')           &
     &            ) grav(1) = - gravity_vector_ctl%vect(i)
              if(cmp_no_case(gravity_vector_ctl%c_tbl(i),'Y')           &
     &            ) grav(2) = - gravity_vector_ctl%vect(i)
              if(cmp_no_case(gravity_vector_ctl%c_tbl(i),'Z')           &
     &            ) grav(3) = - gravity_vector_ctl%vect(i)
            end do
            call dealloc_control_array_c_r(gravity_vector_ctl)
          end if
        end if
      end if
      if (iflag_debug .eq. iflag_routine_msg)                           &
     &               write(*,*) 'i_grav ',i_grav
!
!  direction of angular velocity of rotation
!
      angular(1:2) = zero
      angular(3) =   one
!
      if ((iflag_4_coriolis*system_rotation_ctl%icou) .gt. 0) then
        do i = 1, system_rotation_ctl%num
          if(cmp_no_case(system_rotation_ctl%c_tbl(i),'X')              &
     &       )  angular(1) = system_rotation_ctl%vect(i)
          if(cmp_no_case(system_rotation_ctl%c_tbl(i),'Y')              &
     &       )  angular(2) = system_rotation_ctl%vect(i)
          if(cmp_no_case(system_rotation_ctl%c_tbl(i),'Z')              &
     &       )  angular(3) = system_rotation_ctl%vect(i)
        end do
        call dealloc_control_array_c_r(system_rotation_ctl)
      end if
!
!
!  setting for external mangnetic field
!
      if (i_magneto_cv .eq. 0) then
        iflag_magneto_cv = id_turn_OFF
      else
        if(      cmp_no_case(magneto_cv_ctl, 'On')                      &
     &     .or.  cmp_no_case(magneto_cv_ctl, '1')                       &
     &    ) iflag_magneto_cv = id_turn_ON
      end if
!
      ex_magne(1:3) = 0.0d0
!
      if (iflag_magneto_cv .gt. id_turn_OFF) then
        if (ext_magne_ctl%icou .eq. 0) then
          e_message = 'Set external magnetic field'
          call calypso_MPI_abort(ierr_force, e_message)
        else
!
          do i = 1, ext_magne_ctl%num
            if(cmp_no_case(ext_magne_ctl%c_tbl(i),'X')                  &
     &            ) ex_magne(1) = ext_magne_ctl%vect(i)
            if(cmp_no_case(ext_magne_ctl%c_tbl(i),'Y')                  &
     &            ) ex_magne(2) = ext_magne_ctl%vect(i)
            if(cmp_no_case(ext_magne_ctl%c_tbl(i),'Z')                  &
     &            ) ex_magne(3) = ext_magne_ctl%vect(i)
          end do
          call dealloc_control_array_c_r(ext_magne_ctl)
        end if
      end if
!
     if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'name_force '
        do i = 1, num_force
          write(*,*) i, trim(name_force(i))
        end do
!
        if(i_grav .eq. iflag_const_g) then
          write(*,'(a, 1p3E25.15e3)') 'gravity ', grav(1:3)
        end if
!
        write(*,*) 'magneto_cv ',iflag_magneto_cv
        write(*,'(a,1p3E25.15e3)') 'ex_magne ',ex_magne
        write(*,*) 'iflag_4_coriolis', iflag_4_coriolis
        if(iflag_4_coriolis .gt. id_turn_OFF) then
          write(*,'(a, 1p3E25.15e3)') 'rotation ', angular(1:3)
        end if
      end if
!
!
      end subroutine s_set_control_4_force
!
! -----------------------------------------------------------------------
!
      end module set_control_4_force
