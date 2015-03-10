!>@file   m_ctl_data_4_fields.f90
!!@brief  module m_ctl_data_4_fields
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!
!>@brief  Control brog for field definition
!!
!!@verbatim
!!      subroutine deallocate_phys_control
!!      subroutine deallocate_quad_phys_control
!!      subroutine deallocate_linear_phys_control
!!
!!      subroutine read_phys_values
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
      module m_ctl_data_4_fields
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!>      Structure for list of field
!!@n      field_ctl%icou:  Read flag for 'nod_value_ctl'
!!@n      field_ctl%num:   Number of field
!!@n      field_ctl%c1_tbl: Name of field
!!@n      field_ctl%c2_tbl: flag for visualization output
!!@n      field_ctl%c3_tbl: flag for time series output
      type(ctl_array_c3), save :: field_ctl
!
!>      Structure for list of field on quadrature elements
!!@n      quad_phys_ctl%icou:  Read flag for 'quad_field_name_ctl'
!!@n      quad_phys_ctl%num:   Number of field
!!@n      quad_phys_ctl%c_tbl: Name list of field
      type(ctl_array_chara), save :: quad_phys_ctl
!
!>      Structure for list of field on linear elements
!!@n      linear_phys_ctl%icou:  Read flag for 'linear_field_name_ctl'
!!@n      linear_phys_ctl%num:   Number of field
!!@n      linear_phys_ctl%c_tbl: Name list of field
      type(ctl_array_chara), save :: linear_phys_ctl
!
!   label for entry of group
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!>      Number of field
      integer (kind=kint) :: i_phys_values =   0
!
!   4th level for fields
!
      character(len=kchara), parameter                                  &
     &      :: hd_field_list = 'nod_value_ctl'
!
!   4th level for each order
!
      character(len=kchara), parameter                                  &
     &      :: hd_quad_field =   'quad_field_name_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_linear_field = 'linear_field_name_ctl'
!
      private :: hd_phys_values, i_phys_values
      private :: hd_field_list
      private :: hd_quad_field, hd_linear_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_phys_control
!
       call dealloc_control_array_c3(field_ctl)
!
       end subroutine deallocate_phys_control
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_quad_phys_control
!
      call dealloc_control_array_chara(quad_phys_ctl)
!
      end subroutine deallocate_quad_phys_control
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_linear_phys_control
!
      call dealloc_control_array_chara(linear_phys_ctl)
!
      end subroutine deallocate_linear_phys_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_phys_values
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_phys_values) .eq. 0) return
      if (i_phys_values .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_phys_values, i_phys_values)
        if(i_phys_values .gt. 0) exit
!
        call read_control_array_c3(hd_field_list, field_ctl)
!
        call read_control_array_c1(hd_quad_field, quad_phys_ctl)
        call read_control_array_c1(hd_linear_field, linear_phys_ctl)
      end do
!
      end subroutine read_phys_values
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_fields
