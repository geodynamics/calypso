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
      use m_machine_parameter
      use m_control_parameter
      use m_ctl_data_mhd_forces
      use m_physical_property
!
      character(len=kchara) :: tmpchara
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
        if (i_num_forces.gt.0) then
          num_force = num_force_ctl
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &      write(*,*) 'num_force ', num_force
        end if
      end if
!
      if (num_force .gt. 0) then
        allocate(name_force(num_force))
!
        name_force(1:num_force) = name_force_ctl(1:num_force)
!
        do i = 1, num_force
          if (    name_force(i) .eq. 'gravity'                          &
     &      .or.  name_force(i) .eq. 'Gravity'                          &
     &      .or.  name_force(i) .eq. 'GRAVITY'                          &
     &      .or.  name_force(i) .eq. 'gravity_ele'                      &
     &      .or.  name_force(i) .eq. 'Gravity_ele'                      &
     &      .or.  name_force(i) .eq. 'GRAVITY_ELE'                      &
     &      .or.  name_force(i) .eq. 'gravity_element'                  &
     &      .or.  name_force(i) .eq. 'Gravity_element'                  &
     &      .or.  name_force(i) .eq. 'GRAVITY_ELEMENT'                  &
     &      ) then
            iflag_4_gravity =  id_FORCE_ele_int
          else if(name_force(i) .eq. 'gravity_node'                     &
     &      .or.  name_force(i) .eq. 'Gravity_node'                     &
     &      .or.  name_force(i) .eq. 'GRAVITY_NODE'                     &
     &      .or.  name_force(i) .eq. 'gravity_nod'                      &
     &      .or.  name_force(i) .eq. 'Gravity_nod'                      &
     &      .or.  name_force(i) .eq. 'GRAVITY_NOD'                      &
     &      ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then
              iflag_4_gravity = id_FORCE_ele_int
            else
              iflag_4_gravity = id_FORCE_at_node
            end if
!
          else if (name_force(i) .eq. 'composite_gravity'               &
     &      .or.   name_force(i) .eq. 'Composite_gravity'               &
     &      .or.   name_force(i) .eq. 'COMPOSITE_GRAVITY'               &
     &      .or.   name_force(i) .eq. 'composite_gravity_ele'           &
     &      .or.   name_force(i) .eq. 'Composite_gravity_ele'           &
     &      .or.   name_force(i) .eq. 'COMPOSITE_GRAVITY_ELE'           &
     &      .or.   name_force(i) .eq. 'composite_gravity_element'       &
     &      .or.   name_force(i) .eq. 'Composite_gravity_element'       &
     &      .or.   name_force(i) .eq. 'COMPOSITE_GRAVITY_ELEMENT'       &
     &      ) then
            iflag_4_composit_buo =  id_FORCE_ele_int
!
          else if (name_force(i) .eq. 'composite_gravity_nod'           &
     &      .or.   name_force(i) .eq. 'Composite_gravity_nod'           &
     &      .or.   name_force(i) .eq. 'COMPOSITE_GRAVITY_NOD'           &
     &      .or.   name_force(i) .eq. 'composite_gravity_node'          &
     &      .or.   name_force(i) .eq. 'Composite_gravity_node'          &
     &      .or.   name_force(i) .eq. 'COMPOSITE_GRAVITY_NODE'          &
     &      ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then
              iflag_4_composit_buo = id_FORCE_ele_int
            else
              iflag_4_composit_buo = id_FORCE_at_node
            end if
!
          else if(name_force(i) .eq. 'filtered_gravity'                 &
     &      .or.  name_force(i) .eq. 'Filtered_gravity'                 &
     &      .or.  name_force(i) .eq. 'FILTERED_GRAVITY'                 &
     &      ) then
            iflag_4_filter_gravity =  id_FORCE_ele_int
!
          else if ( name_force(i) .eq. 'Coriolis' ) then
              iflag_4_coriolis = id_FORCE_ele_int
          else if ( name_force(i) .eq. 'Coriolis_node' ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass) then
              iflag_4_coriolis = id_FORCE_ele_int
            else
              iflag_4_coriolis = id_FORCE_at_node
            end if
          else if ( name_force(i) .eq. 'Coriolis_imp' ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson) then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass)   &
     &          then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else
              iflag_4_coriolis = id_FORCE_ele_int
            end if
          else if ( name_force(i) .eq. 'Coriolis_node_imp' ) then
            if (iflag_t_evo_4_velo .eq. id_Crank_nicolson) then
              iflag_4_coriolis = id_Coriolis_nod_imp
            else if (iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass)   &
     &               then
              iflag_4_coriolis = id_Coriolis_ele_imp
            else
              iflag_4_coriolis = id_FORCE_ele_int
            end if
!
          else if ( name_force(i) .eq. 'Lorentz' ) then
            iflag_4_lorentz = id_turn_ON
          else if ( name_force(i) .eq. 'Lorentz_full' ) then
            iflag_4_lorentz = id_Lorentz_w_Emag
          else if ( name_force(i) .eq. 'Rotation_form' ) then
            iflag_4_rotate =  id_turn_ON
          end if
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
          if (i_gravity_vect .eq. 0) then
            e_message = 'Set gravity vector'
            call calypso_MPI_abort(90, e_message)
          else
!
            do i = 1, i_gravity_vect
              tmpchara = g_dir_name_ctl(i)
!
              if ( tmpchara .eq. 'x' .or. tmpchara .eq. 'X' ) then
                grav(1) = - g_vect_ctl(i)
              else if ( tmpchara .eq. 'y' .or. tmpchara .eq. 'Y' ) then
                grav(2) = - g_vect_ctl(i)
              else if ( tmpchara .eq. 'z' .or. tmpchara .eq. 'Z' ) then
                grav(3) = - g_vect_ctl(i)
              end if
            end do
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
      if ((iflag_4_coriolis*i_rotation_vec) .gt. 0) then
        do i = 1, i_rotation_vec
          tmpchara = angular_dir_name_ctl(i)
!
          if ( tmpchara .eq. 'x' .or. tmpchara .eq. 'X' ) then
            angular(1) = angular_vect_ctl(i)
          else if ( tmpchara .eq. 'y' .or. tmpchara .eq. 'Y' ) then
            angular(2) = angular_vect_ctl(i)
          else if ( tmpchara .eq. 'z' .or. tmpchara .eq. 'Z' ) then
            angular(3) = angular_vect_ctl(i)
          end if
        end do
      end if
!
!
!  setting for external mangnetic field
!
      if (i_magneto_cv .eq. 0) then
        iflag_magneto_cv = id_turn_OFF
      else
        if(magneto_cv_ctl .eq. 'on' .or. magneto_cv_ctl .eq. 'On'       &
     &    .or. magneto_cv_ctl .eq. 'ON' .or. magneto_cv_ctl .eq. '1')   &
     &    iflag_magneto_cv = id_turn_ON
      end if
!
      ex_magne = 0.0d0
!
      if (iflag_magneto_cv .gt. id_turn_OFF) then
        if (i_magne_vect .eq. 0) then
          e_message = 'Set external magnetic field'
          call calypso_MPI_abort(90, e_message)
        else
!
          do i = 1, i_magne_vect
            tmpchara = magne_dir_name_ctl(i)
!
            if ( tmpchara .eq. 'x' .or. tmpchara .eq. 'X' ) then
              ex_magne(1) = magne_vect_ctl(i)
            else if ( tmpchara .eq. 'y' .or. tmpchara .eq. 'Y' ) then
              ex_magne(2) = magne_vect_ctl(i)
            else if ( tmpchara .eq. 'z' .or. tmpchara .eq. 'Z' ) then
              ex_magne(3) = magne_vect_ctl(i)
            end if
          end do
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
