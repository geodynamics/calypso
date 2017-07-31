!>@file   bcast_4_field_ctl.f90
!!@brief  module bcast_4_field_ctl
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control brog for field definition
!!
!!@verbatim
!!      subroutine bcast_phys_data_ctl(hd_block, iflag, fld_ctl)
!!
!! ---------------------------------------------------------------------
!!
!!     Example of control block
!!
!!    begin phys_values_ctl
!!      array nod_value_ctl   12
!!        nod_value_ctl  velocity            Viz_On   Monitor_On
!!        nod_value_ctl  temperature         Viz_On   Monitor_On
!!        nod_value_ctl  pressure            Viz_On   Monitor_Off
!!        nod_value_ctl  vorticity           Viz_On   Monitor_Off
!!        nod_value_ctl  vector_potential    Viz_Off  Monitor_Off
!!        nod_value_ctl  magnetic_field      Viz_On   Monitor_On
!!        nod_value_ctl  current_density     Viz_On   Monitor_Off
!!        nod_value_ctl  magnetic_potential  Viz_Off  Monitor_Off
!!        nod_value_ctl  composition         Viz_Off  Monitor_Off
!!
!!        nod_value_ctl  heat_flux             Viz_Off  Monitor_Off
!!        nod_value_ctl  momentum_flux         Viz_Off  Monitor_Off
!!        nod_value_ctl  maxwell_tensor        Viz_Off  Monitor_Off
!!        nod_value_ctl  vecp_induction        Viz_Off  Monitor_Off
!!      end array nod_value_ctl
!!
!!      array quad_field_name_ctl    5
!!        quad_field_name_ctl  vector_potential
!!        quad_field_name_ctl  heat_flux
!!        quad_field_name_ctl  momentum_flux
!!        quad_field_name_ctl  maxwell_tensor
!!        quad_field_name_ctl  vecp_induction
!!      end array quad_field_name_ctl
!!
!!      array linear_field_name_ctl    7
!!        linear_field_name_ctl  velocity
!!        linear_field_name_ctl  pressure
!!        linear_field_name_ctl  vorticity
!!        linear_field_name_ctl  temperature
!!        linear_field_name_ctl  magnetic_field
!!        linear_field_name_ctl  current_density
!!        linear_field_name_ctl  magnetic_potential
!!        linear_field_name_ctl  composition
!!      end array linear_field_name_ctl
!!    end phys_values_ctl
!!
!! ---------------------------------------------------------------------
!!@endverbatim
!
      module bcast_4_field_ctl
!
      use m_precision
      use t_ctl_data_4_fields
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine bcast_phys_data_ctl(fld_ctl)
!
      use bcast_control_arrays
!
      type(field_control), intent(inout) :: fld_ctl
!
!
      call bcast_ctl_array_c3(fld_ctl%field_ctl)
!
      call bcast_ctl_array_c1(fld_ctl%quad_phys)
      call bcast_ctl_array_c1(fld_ctl%linear_phys)
!
      end subroutine bcast_phys_data_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_4_field_ctl
