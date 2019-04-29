!>@file   set_control_4_force.f90
!!@brief  module set_control_4_force
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for forces from control data
!!
!!@verbatim
!!      subroutine s_set_control_4_force                                &
!!     &         (frc_ctl, g_ctl, cor_ctl, mcv_ctl, fl_prop, cd_prop)
!!        type(forces_control), intent(in) :: frc_ctl
!!        type(gravity_control), intent(in) :: g_ctl
!!        type(coriolis_control), intent(in) :: cor_ctl
!!        type(magneto_convection_control), intent(in) :: mcv_ctl
!!        type(fluid_property), intent(inout) :: fl_prop
!!        type(conductive_property), intent(inout) :: cd_prop
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
      subroutine s_set_control_4_force                                  &
     &         (frc_ctl, g_ctl, cor_ctl, mcv_ctl, fl_prop, cd_prop)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
      use t_physical_property
      use t_ctl_data_mhd_forces
      use skip_comment_f
!
      type(forces_control), intent(in) :: frc_ctl
      type(gravity_control), intent(in) :: g_ctl
      type(coriolis_control), intent(in) :: cor_ctl
      type(magneto_convection_control), intent(in) :: mcv_ctl
!
      type(fluid_property), intent(inout) :: fl_prop
      type(conductive_property), intent(inout) :: cd_prop
!
      integer (kind = kint) :: i, iflag
      character(len=kchara) :: tmpchara
!
!
      fl_prop%iflag_4_gravity =        id_turn_OFF
      fl_prop%iflag_4_coriolis =       id_turn_OFF
      fl_prop%iflag_4_lorentz =        id_turn_OFF
      fl_prop%iflag_4_composit_buo =   id_turn_OFF
      fl_prop%iflag_4_filter_gravity = id_turn_OFF
!
      if (fl_prop%iflag_scheme .eq. id_no_evolution) then
        fl_prop%num_force = 0
      else
        if (frc_ctl%force_names%icou .gt. 0) then
          fl_prop%num_force = frc_ctl%force_names%num
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &      write(*,*) 'num_force ', fl_prop%num_force
        end if
      end if
!
      if(fl_prop%num_force .gt. 0) then
!
        call alloc_force_list(fl_prop)
        fl_prop%name_force(1:fl_prop%num_force)                         &
     &          = frc_ctl%force_names%c_tbl(1:fl_prop%num_force)
!
        do i = 1, fl_prop%num_force
          tmpchara = fl_prop%name_force(i)
          if(    cmp_no_case(tmpchara, 'Gravity')                       &
     &      .or. cmp_no_case(tmpchara, 'Gravity_ele')                   &
     &      .or. cmp_no_case(tmpchara, 'Gravity_element')               &
     &      .or. cmp_no_case(tmpchara, 'Buoyancy')                      &
     &      .or. cmp_no_case(tmpchara, 'Buoyancy_ele')                  &
     &      .or. cmp_no_case(tmpchara, 'Buoyancy_element')              &
     &      .or. cmp_no_case(tmpchara, 'Thermal_buoyancy')              &
     &      .or. cmp_no_case(tmpchara, 'Thermal_buoyancy_ele')          &
     &      .or. cmp_no_case(tmpchara, 'Thermal_buoyancy_element')      &
     &      .or. cmp_no_case(tmpchara, 'Thermal_gravity')               &
     &      .or. cmp_no_case(tmpchara, 'Thermal_gravity_ele')           &
     &      .or. cmp_no_case(tmpchara, 'Thermal_gravity_element')       &
     &      ) fl_prop%iflag_4_gravity =  id_FORCE_ele_int
!
          if(     cmp_no_case(tmpchara, 'Gravity_nod')                  &
     &       .or. cmp_no_case(tmpchara, 'Buoyancy_nod')                 &
     &       .or. cmp_no_case(tmpchara, 'Thermal_buoyancy_nod')         &
     &       .or. cmp_no_case(tmpchara, 'Thermal_gravity_nod')          &
     &       .or. cmp_no_case(tmpchara, 'Gravity_node')                 &
     &       .or. cmp_no_case(tmpchara, 'Buoyancy_node')                &
     &       .or. cmp_no_case(tmpchara, 'Thermal_buoyancy_node')        &
     &       .or. cmp_no_case(tmpchara, 'Thermal_gravity_node')         &
     &      ) then
            if(fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass) then
              fl_prop%iflag_4_gravity = id_FORCE_ele_int
            else
              fl_prop%iflag_4_gravity = id_FORCE_at_node
            end if
          end if
!
          if(     cmp_no_case(tmpchara, 'Composite_buoyancy')           &
     &       .or. cmp_no_case(tmpchara, 'Composite_buoyancy_ele')       &
     &       .or. cmp_no_case(tmpchara, 'Composite_buoyancy_element')   &
     &       .or. cmp_no_case(tmpchara, 'Composite_gravity')            &
     &       .or. cmp_no_case(tmpchara, 'Composite_gravity_ele')        &
     &       .or. cmp_no_case(tmpchara, 'Composite_gravity_element')    &
     &       ) fl_prop%iflag_4_composit_buo =  id_FORCE_ele_int
!
          if(     cmp_no_case(tmpchara, 'Composite_buoyancy_nod')       &
     &       .or. cmp_no_case(tmpchara, 'Composite_gravity_nod')        &
     &       .or. cmp_no_case(tmpchara, 'Composite_buoyancy_node')      &
     &       .or. cmp_no_case(tmpchara, 'Composite_gravity_node')       &
     &       ) then
            if(fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass) then
              fl_prop%iflag_4_composit_buo = id_FORCE_ele_int
            else
              fl_prop%iflag_4_composit_buo = id_FORCE_at_node
            end if
          end if
!
          if(     cmp_no_case(tmpchara, 'Filtered_gravity')             &
     &       .or. cmp_no_case(tmpchara, 'Filtered_buoyancy')            &
     &       ) fl_prop%iflag_4_filter_gravity =  id_FORCE_ele_int
!
          if (cmp_no_case(tmpchara, 'Coriolis')                         &
     &        )  fl_prop%iflag_4_coriolis = id_FORCE_ele_int
!
          if (cmp_no_case(tmpchara, 'Coriolis_node')) then
            if(fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass) then
              fl_prop%iflag_4_coriolis = id_FORCE_ele_int
            else
              fl_prop%iflag_4_coriolis = id_FORCE_at_node
            end if
          end if
!
          if(cmp_no_case(tmpchara, 'Coriolis_imp')) then
            if(fl_prop%iflag_scheme .eq. id_Crank_nicolson) then
              fl_prop%iflag_4_coriolis = id_Coriolis_ele_imp
            else if(fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass)  &
     &          then
              fl_prop%iflag_4_coriolis = id_Coriolis_ele_imp
            else
              fl_prop%iflag_4_coriolis = id_FORCE_ele_int
            end if
          end if
!
          if(cmp_no_case(tmpchara, 'Coriolis_node_imp')) then
            if(fl_prop%iflag_scheme .eq. id_Crank_nicolson) then
              fl_prop%iflag_4_coriolis = id_Coriolis_nod_imp
            else if(fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass)  &
     &               then
              fl_prop%iflag_4_coriolis = id_Coriolis_ele_imp
            else
              fl_prop%iflag_4_coriolis = id_FORCE_ele_int
            end if
          end if
!
          if(cmp_no_case(tmpchara, 'Lorentz')                           &
     &           )  fl_prop%iflag_4_lorentz = id_turn_ON
          if(cmp_no_case(tmpchara, 'Lorentz_full')                      &
     &           )  fl_prop%iflag_4_lorentz = id_Lorentz_w_Emag
        end do
      end if
!
!  direction of gravity
!
      fl_prop%i_grav = iflag_no_gravity
      iflag = fl_prop%iflag_4_gravity + fl_prop%iflag_4_composit_buo    &
     &       + fl_prop%iflag_4_filter_gravity
      if (iflag .gt. 0) then
        if (g_ctl%gravity%iflag .eq. 0) then
          fl_prop%i_grav = iflag_self_r_g
        else
          tmpchara = g_ctl%gravity%charavalue
!
          if     (cmp_no_case(tmpchara, 'constant')) then
             fl_prop%i_grav = iflag_const_g
          else if(cmp_no_case(tmpchara, 'constant_radial')) then
             fl_prop%i_grav = iflag_radial_g
          else if(cmp_no_case(tmpchara, 'radial')) then
             fl_prop%i_grav = iflag_self_r_g
           end if
        end if
!
        if (fl_prop%i_grav .eq. iflag_const_g) then
          if (g_ctl%gravity_vector%icou .eq. 0) then
            e_message = 'Set gravity vector'
            call calypso_MPI_abort(ierr_force, e_message)
          else
!
            do i = 1, g_ctl%gravity_vector%num
              if(cmp_no_case(g_ctl%gravity_vector%c_tbl(i),'X')         &
     &            ) fl_prop%grav(1) = - g_ctl%gravity_vector%vect(i)
              if(cmp_no_case(g_ctl%gravity_vector%c_tbl(i),'Y')         &
     &            ) fl_prop%grav(2) = - g_ctl%gravity_vector%vect(i)
              if(cmp_no_case(g_ctl%gravity_vector%c_tbl(i),'Z')         &
     &            ) fl_prop%grav(3) = - g_ctl%gravity_vector%vect(i)
            end do
          end if
        end if
      end if
      if (iflag_debug .eq. iflag_routine_msg)                           &
     &               write(*,*) 'i_grav ',fl_prop%i_grav
!
!  direction of angular velocity of rotation
!
      fl_prop%sys_rot(1:2) = zero
      fl_prop%sys_rot(3) =   one
!
      if ((fl_prop%iflag_4_coriolis * cor_ctl%system_rotation%icou)     &
     &      .gt. 0) then
        do i = 1, cor_ctl%system_rotation%num
          if(cmp_no_case(cor_ctl%system_rotation%c_tbl(i),'X')          &
     &       )  fl_prop%sys_rot(1) = cor_ctl%system_rotation%vect(i)
          if(cmp_no_case(cor_ctl%system_rotation%c_tbl(i),'Y')          &
     &       )  fl_prop%sys_rot(2) = cor_ctl%system_rotation%vect(i)
          if(cmp_no_case(cor_ctl%system_rotation%c_tbl(i),'Z')          &
     &       )  fl_prop%sys_rot(3) = cor_ctl%system_rotation%vect(i)
        end do
      end if
!
!  setting for external mangnetic field
!
      if (mcv_ctl%magneto_cv%iflag .eq. 0) then
        cd_prop%iflag_magneto_cv = id_turn_OFF
      else
        if(yes_flag(mcv_ctl%magneto_cv%charavalue))                     &
     &              cd_prop%iflag_magneto_cv = id_turn_ON
      end if
!
      cd_prop%ex_magne(1:3) = 0.0d0
!
      if (cd_prop%iflag_magneto_cv .gt. id_turn_OFF) then
        if (mcv_ctl%ext_magne%icou .eq. 0) then
          e_message = 'Set external magnetic field'
          call calypso_MPI_abort(ierr_force, e_message)
        else
!
          do i = 1, mcv_ctl%ext_magne%num
            if(cmp_no_case(mcv_ctl%ext_magne%c_tbl(i),'X')              &
     &            ) cd_prop%ex_magne(1) = mcv_ctl%ext_magne%vect(i)
            if(cmp_no_case(mcv_ctl%ext_magne%c_tbl(i),'Y')              &
     &            ) cd_prop%ex_magne(2) = mcv_ctl%ext_magne%vect(i)
            if(cmp_no_case(mcv_ctl%ext_magne%c_tbl(i),'Z')              &
     &            ) cd_prop%ex_magne(3) = mcv_ctl%ext_magne%vect(i)
          end do
        end if
      end if
!
     if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'name_force '
        do i = 1, fl_prop%num_force
          write(*,*) i, trim(fl_prop%name_force(i))
        end do
!
        if(fl_prop%i_grav .eq. iflag_const_g) then
          write(*,'(a, 1p3E25.15e3)') 'gravity ', fl_prop%grav(1:3)
        end if
!
        write(*,*) 'magneto_cv ', cd_prop%iflag_magneto_cv
        write(*,'(a,1p3E25.15e3)') 'ex_magne ',cd_prop%ex_magne
        write(*,*) 'iflag_4_coriolis', fl_prop%iflag_4_coriolis
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          write(*,'(a, 1p3E25.15e3)') 'rotation:', fl_prop%sys_rot(1:3)
        end if
      end if
!
      end subroutine s_set_control_4_force
!
! -----------------------------------------------------------------------
!
      end module set_control_4_force
