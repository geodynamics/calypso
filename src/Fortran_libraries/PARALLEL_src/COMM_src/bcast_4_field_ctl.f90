!>@file   bcast_4_field_ctl.f90
!!@brief  module bcast_4_field_ctl
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control brog for field definition
!!
!!@verbatim
!!      subroutine bcast_phys_data_ctl(fld_ctl)
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
!!      array scalar_field_ctl
!!        scalar_field_ctl  temperature   501
!!        scalar_field_ctl  pressure      601
!!      end array scalar_field_ctl
!!
!!      array vector_field_ctl
!!        vector_field_ctl  velocity            1   2   3
!!        vector_field_ctl  vorticity         201 202 203
!!        vector_field_ctl  magnetic_field    301 302 303
!!      end array vector_field_ctl
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
      call bcast_ctl_array_c1(fld_ctl%quad_phys)
!
      call bcast_ctl_array_ci(fld_ctl%scalar_phys)
      call bcast_ctl_array_ci3(fld_ctl%vector_phys)
!
      call MPI_BCAST(fld_ctl%i_phys_values, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_phys_data_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_4_field_ctl
