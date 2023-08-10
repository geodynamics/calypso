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
!!     &         (frc_ctl, g_ctl, cor_ctl, mcv_ctl, MHD_prop)
!!        type(forces_control), intent(in) :: frc_ctl
!!        type(gravity_control), intent(in) :: g_ctl
!!        type(coriolis_control), intent(in) :: cor_ctl
!!        type(magneto_convection_control), intent(in) :: mcv_ctl
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!@endverbatim
!
      module set_control_4_force
!
      use m_precision
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use m_force_control_labels
!
      implicit  none
!
      private :: set_control_force_flags, set_control_4_gravity
      private :: set_control_4_Coriolis_force, set_control_4_induction
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_control_4_force                                  &
     &         (frc_ctl, g_ctl, cor_ctl, mcv_ctl, MHD_prop)
!
      use t_control_parameter
      use t_ctl_data_mhd_forces
      use t_ctl_data_mhd_magne
      use t_ctl_data_gravity
      use t_ctl_data_coriolis_force
      use skip_comment_f
!
      type(forces_control), intent(in) :: frc_ctl
      type(gravity_control), intent(in) :: g_ctl
      type(coriolis_control), intent(in) :: cor_ctl
      type(magneto_convection_control), intent(in) :: mcv_ctl
!
      type(MHD_evolution_param), intent(inout) :: MHD_prop
!
!
      call set_control_force_flags(frc_ctl, MHD_prop%fl_prop)
!
!  direction of gravity
      call set_control_4_gravity(g_ctl, MHD_prop%fl_prop)
!
!  Set Corilis force settings
      call set_control_4_Coriolis_force(cor_ctl, MHD_prop%fl_prop)
!
!  setting for external mangnetic field
      call set_control_4_induction(mcv_ctl, MHD_prop%cd_prop)
!
      end subroutine s_set_control_4_force
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_force_flags(frc_ctl, fl_prop)
!
      use t_physical_property
      use t_ctl_data_mhd_forces
      use skip_comment_f
!
      type(forces_control), intent(in) :: frc_ctl
!
      type(fluid_property), intent(inout) :: fl_prop
!
      integer (kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!
      fl_prop%iflag_4_inertia =         .TRUE.
      fl_prop%iflag_4_gravity =         .FALSE.
      fl_prop%iflag_4_coriolis =        .FALSE.
      fl_prop%iflag_4_lorentz =         .FALSE.
      fl_prop%iflag_4_composit_buo =    .FALSE.
!
      fl_prop%iflag_4_filter_gravity =  .FALSE.
      fl_prop%iflag_4_filter_comp_buo = .FALSE.
      fl_prop%iflag_4_filter_lorentz =  .FALSE.
!
      if (fl_prop%iflag_scheme .eq. id_no_evolution) then
        fl_prop%num_force = 0
        fl_prop%iflag_4_inertia = .FALSE.
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
          if(    cmp_no_case(tmpchara, gravity_label)                   &
     &      .or. cmp_no_case(tmpchara, gravity_e1)                      &
     &      .or. cmp_no_case(tmpchara, gravity_e2)                      &
     &      .or. cmp_no_case(tmpchara, gravity_e5)                      &
     &      ) fl_prop%iflag_4_gravity =  .TRUE.
!
          if(    cmp_no_case(tmpchara, comp_gravity_label)              &
     &      .or. cmp_no_case(tmpchara, comp_gravity_e1)                 &
     &      .or. cmp_no_case(tmpchara, comp_gravity_e5)                 &
     &      .or. cmp_no_case(tmpchara, comp_gravity_e6)                 &
     &       ) fl_prop%iflag_4_composit_buo =  .TRUE.
!
          if(     cmp_no_case(tmpchara, Filtered_gravity_label)         &
     &       .or. cmp_no_case(tmpchara, Filtered_gravity_e1)            &
     &       ) fl_prop%iflag_4_filter_gravity =  .TRUE.
!
          if(   cmp_no_case(tmpchara, Filtered_comp_gravity_label)      &
     &     .or. cmp_no_case(tmpchara, Filtered_comp_gravity_e1)         &
     &       ) fl_prop%iflag_4_filter_comp_buo = .TRUE.
!
          if (cmp_no_case(tmpchara, coriolis_e1)                        &
     &       ) fl_prop%iflag_4_coriolis = .TRUE.
!
          if(cmp_no_case(tmpchara, hd_filtered_inertia)) then
            fl_prop%iflag_4_filter_inertia = .TRUE.
            fl_prop%iflag_4_inertia = .FALSE.
          end if
!
          if(cmp_no_case(tmpchara, lorentz_label)) then
            fl_prop%iflag_4_lorentz = .TRUE.
          else if(cmp_no_case(tmpchara, hd_filtered_Lorentz)) then
            fl_prop%iflag_4_filter_lorentz = .TRUE.
          end if
!
        end do
      end if
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'name_force '
        do i = 1, fl_prop%num_force
          write(*,*) i, trim(fl_prop%name_force(i))
        end do
      end if
!
      end subroutine set_control_force_flags
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_gravity(g_ctl, fl_prop)
!
      use calypso_mpi
      use t_physical_property
      use t_ctl_data_gravity
      use skip_comment_f
!
      type(gravity_control), intent(in) :: g_ctl
!
      type(fluid_property), intent(inout) :: fl_prop
!
      integer (kind = kint) :: i
      character(len=kchara) :: tmpchara
!
!  direction of gravity
!
      fl_prop%i_grav = iflag_no_gravity
      if(     fl_prop%iflag_4_gravity                                   &
     &   .or. fl_prop%iflag_4_composit_buo                              &
     &   .or. fl_prop%iflag_4_filter_gravity                            &
     &   .or. fl_prop%iflag_4_filter_comp_buo) then
        if(g_ctl%FEM_gravity_model%iflag .gt. 0                         &
     &    .and. cmp_no_case(g_ctl%FEM_gravity_model%charavalue,'node')  &
     &    .and. fl_prop%iflag_scheme .ne. id_Crank_nicolson_cmass) then
          fl_prop%iflag_FEM_gravity = id_FORCE_at_node
        else
          fl_prop%iflag_FEM_gravity = id_FORCE_ele_int
        end if
!
!
        fl_prop%i_grav = iflag_self_r_g
        if (g_ctl%gravity%iflag .gt. 0) then
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
!
      if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'i_grav ',fl_prop%i_grav
        if(fl_prop%i_grav .eq. iflag_const_g) then
          write(*,'(a, 1p3E25.15e3)') 'gravity ', fl_prop%grav(1:3)
        end if
      end if
!
      end subroutine set_control_4_gravity
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_Coriolis_force(cor_ctl, fl_prop)
!
      use t_physical_property
      use t_ctl_data_mhd_forces
      use t_ctl_data_coriolis_force
      use skip_comment_f
!
      type(coriolis_control), intent(in) :: cor_ctl
!
      type(fluid_property), intent(inout) :: fl_prop
!
      integer (kind = kint) :: i
!
!  Set direction of angular velocity of rotation
!
      fl_prop%sys_rot(1:2) = zero
      fl_prop%sys_rot(3) =   one
!
      if(fl_prop%iflag_4_coriolis .eqv. .FALSE.) return
      if(cor_ctl%FEM_coriolis_model%iflag .gt. 0                        &
     &  .and. cmp_no_case(cor_ctl%FEM_coriolis_model%charavalue,'node') &
     &  .and. fl_prop%iflag_scheme .ne. id_Crank_nicolson_cmass) then
        fl_prop%iflag_FEM_coriolis = id_FORCE_at_node
      end if
!
      if(fl_prop%iflag_scheme .gt. id_Crank_nicolson                    &
     &   .and. fl_prop%iflag_4_coriolis                                 &
     &   .and. cor_ctl%FEM_coriolis_implicit%iflag .gt. 0               &
     &   .and. yes_flag(cor_ctl%FEM_coriolis_implicit%charavalue)) then
        fl_prop%iflag_coriolis_implicit = .TRUE.
      end if
!
      if(cor_ctl%system_rotation%icou .gt. 0) then
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
     if (iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'iflag_4_coriolis', fl_prop%iflag_4_coriolis
        if(fl_prop%iflag_4_coriolis) then
          write(*,'(a, 1p3E25.15e3)') 'rotation:', fl_prop%sys_rot(1:3)
        end if
      end if
!
      end subroutine set_control_4_Coriolis_force
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_induction(mcv_ctl, cd_prop)
!
      use calypso_mpi
      use t_physical_property
      use t_ctl_data_mhd_magne
      use skip_comment_f
!
      type(magneto_convection_control), intent(in) :: mcv_ctl
!
      type(conductive_property), intent(inout) :: cd_prop
!
      integer(kind = kint) :: i
!
!  setting for external mangnetic field
!
      call set_filtered_induction_ctl                                   &
     &   (mcv_ctl%filterd_induction_ctl, cd_prop)
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
      if(iflag_debug .ge. iflag_routine_msg) then
        write(*,*) 'Induction mode', cd_prop%iflag_4_induction
        write(*,*) 'Filtered Induction mode',                           &
     &            cd_prop%iflag_4_filter_induction
        write(*,*) 'magneto_cv ', cd_prop%iflag_magneto_cv
        write(*,'(a,1p3E25.15e3)') 'ex_magne ',cd_prop%ex_magne
      end if
!
      end subroutine set_control_4_induction
!
! -----------------------------------------------------------------------
!
      end module set_control_4_force
