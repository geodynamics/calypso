!>@file   m_force_control_labels.f90
!!        module m_force_control_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Force labels
!!
!!@verbatim
!!      subroutine set_force_list_array(array_c)
!!      subroutine set_filter_force_list_array(array_c)
!!      subroutine set_sph_force_list_array(array_c)
!!        type(ctl_array_chara), intent(inout) :: array_c
!!
!! !!!!! Force names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   Advection names  (Other possible expression)
!!
!!     inertia
!!     magnetic_induction   (induction)
!!     heat_advect          (heat)
!!     composition_advect   (comp_flux)
!!
!!   Force names
!!
!!     Coriolis_force       (Coriolis)
!!     Lorentz_force        (Lorentz)
!!
!!     buoyancy             (Thermal_buoyancy, Thermal_gravity, gravity)
!!     composite_buoyancy   (Compositional_buoyancy, composite_gravity
!!                           compositional_gravity)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_force_control_labels
!
      use m_precision
!
      use t_base_force_labels
!
      implicit  none
!
!       Opthional names of force control labels
!
!>        Term label for induction term
      character(len=kchara), parameter :: induction_1 =  'induction'
!>        Term label for heat equation
      character(len=kchara), parameter :: heat_flux_1 =  'heat'
!>        Term label for cpmpositional flux term
      character(len=kchara), parameter :: comp_flux_1 =  'comp_flux'
!
!>       Coriolis force label
      character(len=kchara), parameter :: coriolis_e1 = 'Coriolis'
      character(len=kchara), parameter :: coriolis_n1 = 'Coriolis_node'
      character(len=kchara), parameter :: coriolis_i1 = 'Coriolis_imp'
!
!>       Lorentz force label
      character(len=kchara), parameter :: lorentz_label = 'Lorentz'
!
!>       Thermal buoyancy label
      character(len=kchara), parameter                                  &
     &             :: gravity_label = 'Thermal_buoyancy'
!
      character(len=kchara), parameter :: gravity_e1 =  'gravity'
      character(len=kchara), parameter :: gravity_e2 =  'buoyancy'
      character(len=kchara), parameter                                  &
     &             :: gravity_e5 = 'Thermal_gravity'
!
!
!>       Compositional buoyancy label
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_label = 'Compositional_buoyancy'
!
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e1 = 'Compositional_gravity'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e5 = 'Composite_gravity'
      character(len=kchara), parameter                                  &
     &             :: comp_gravity_e6 = 'Composite_buoyancy'
!
!
!>       Filtered thermal buoyancy label
      character(len=kchara), parameter                                  &
     &             :: Filtered_gravity_label = 'Filtered_buoyancy'
!
      character(len=kchara), parameter                                  &
     &             :: Filtered_gravity_e1 = 'Filtered_gravity'
!
!>       Filtered compositional buoyancy label
      character(len=kchara), parameter                                  &
     &             :: Filtered_comp_gravity_label                       &
     &                        = 'Filtered_compositional_buoyancy'
!
      character(len=kchara), parameter                                  &
     &             :: Filtered_comp_gravity_e1                          &
     &                        = 'Filtered_compositional_gravity'
!
      character(len=kchara), parameter                                  &
     &             :: hd_filtered_inertia = 'filtered_inertia'
      character(len=kchara), parameter                                  &
     &             :: hd_filtered_Lorentz = 'filtered_Lorentz'
!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_force_list_array(array_c)
      use m_base_force_labels
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call set_sph_force_list_array(array_c)
!
      call append_c_to_ctl_array(Coriolis_force%name,     array_c)
      call append_c_to_ctl_array(Lorentz_force%name,      array_c)
      call append_c_to_ctl_array(buoyancy%name,           array_c)
      call append_c_to_ctl_array(composite_buoyancy%name, array_c)
!
      end subroutine set_force_list_array
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_force_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      call set_sph_force_list_array(array_c)
!
      call append_c_to_ctl_array(Filtered_gravity_label, array_c)
      call append_c_to_ctl_array(Filtered_comp_gravity_label, array_c)
      call append_c_to_ctl_array(hd_filtered_inertia, array_c)
      call append_c_to_ctl_array(hd_filtered_Lorentz, array_c)
!
      end subroutine set_filter_force_list_array
!
! ----------------------------------------------------------------------
!
      subroutine set_sph_force_list_array(array_c)
      use t_control_array_character
      type(ctl_array_chara), intent(inout) :: array_c
!
      array_c%array_name = '  '
      array_c%num =         0
      call alloc_control_array_chara(array_c)
!
      call append_c_to_ctl_array(coriolis_e1, array_c)
      call append_c_to_ctl_array(lorentz_label, array_c)
      call append_c_to_ctl_array(gravity_label, array_c)
      call append_c_to_ctl_array(comp_gravity_label, array_c)
!
      call append_c_to_ctl_array(Filtered_gravity_label, array_c)
      call append_c_to_ctl_array(Filtered_comp_gravity_label, array_c)
      call append_c_to_ctl_array(hd_filtered_inertia, array_c)
      call append_c_to_ctl_array(hd_filtered_Lorentz, array_c)
!
      end subroutine set_sph_force_list_array
!
! ----------------------------------------------------------------------
!
      end module m_force_control_labels
