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
!
      implicit  none
!
!>      Number of field
      integer (kind=kint)  :: num_nod_phys_ctl = 0
!
!>      Name list of field
      character (len=kchara), allocatable :: phys_nod_name_ctl(:)
!>      flag for visualization output
      character (len=kchara), allocatable :: visualize_ctl(:)
!>      flag for visualization output
      character (len=kchara), allocatable :: monitor_ctl(:)
!
!>      Number of field for the quadrature elements
      integer (kind=kint)  :: num_quad_field_ctl =   0
!>      Name list of field for the quadrature elements
      character (len=kchara), allocatable :: quad_phys_name_ctl(:)
!
!>      Number of field for the linear elements
      integer (kind=kint)  :: num_linear_field_ctl = 0
!>      Name list of field for the linear elements
      character (len=kchara), allocatable :: linear_phys_name_ctl(:)
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
     &      :: hd_num_nod_phys = 'nod_value_ctl'
!>      Read flag for 'nod_value_ctl'
      integer (kind=kint) :: i_num_nod_phys =   0
!
!   4th level for each order
!
      character(len=kchara), parameter                                  &
     &      :: hd_num_quad_field =   'quad_field_name_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_num_linear_field = 'linear_field_name_ctl'
!>      Read flag for 'quad_field_name_ctl'
      integer (kind=kint) :: i_num_quad_field =   0
!>      Read flag for 'linear_field_name_ctl'
      integer (kind=kint) :: i_num_linear_field = 0
!
      private :: hd_phys_values, i_phys_values
      private :: hd_num_nod_phys
      private :: hd_num_quad_field, hd_num_linear_field
!
      private :: allocate_quad_phys_control
      private :: allocate_linear_phys_control
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
       subroutine allocate_phys_control
!
       allocate( phys_nod_name_ctl(num_nod_phys_ctl) )
       allocate( visualize_ctl(num_nod_phys_ctl) )
       allocate( monitor_ctl(num_nod_phys_ctl) )
!
       end subroutine allocate_phys_control
!
! -----------------------------------------------------------------------
!
       subroutine allocate_quad_phys_control
!
       allocate( quad_phys_name_ctl(num_quad_field_ctl) )
!
       end subroutine allocate_quad_phys_control
!
! -----------------------------------------------------------------------
!
       subroutine allocate_linear_phys_control
!
       allocate( linear_phys_name_ctl(num_linear_field_ctl) )
!
       end subroutine allocate_linear_phys_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine deallocate_phys_control
!
       deallocate( phys_nod_name_ctl )
       deallocate( visualize_ctl )
       deallocate( monitor_ctl )
!
       end subroutine deallocate_phys_control
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_quad_phys_control
!
       deallocate( quad_phys_name_ctl )
!
       end subroutine deallocate_quad_phys_control
!
! -----------------------------------------------------------------------
!
       subroutine deallocate_linear_phys_control
!
       deallocate( linear_phys_name_ctl )
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
        call find_control_array_flag(hd_num_nod_phys, num_nod_phys_ctl)
        if(num_nod_phys_ctl.gt.0 .and. i_num_nod_phys.eq.0) then
          call allocate_phys_control
          call read_control_array_chara3_list(hd_num_nod_phys,          &
     &        num_nod_phys_ctl, i_num_nod_phys, phys_nod_name_ctl,      &
     &        visualize_ctl, monitor_ctl)
        end if
!
        call find_control_array_flag(hd_num_quad_field,                 &
     &      num_quad_field_ctl)
        if(num_quad_field_ctl.gt.0 .and. i_num_quad_field.eq.0) then
          call allocate_quad_phys_control
          call read_control_array_chara_list(hd_num_quad_field,         &
     &        num_quad_field_ctl, i_num_quad_field, quad_phys_name_ctl)
        end if
!
        call find_control_array_flag(hd_num_linear_field,               &
     &      num_linear_field_ctl)
        if(num_linear_field_ctl.gt.0                                    &
     &      .and. i_num_linear_field.eq.0) then
          call allocate_linear_phys_control
          call read_control_array_chara_list(hd_num_linear_field,       &
     &        num_linear_field_ctl, i_num_linear_field,                 &
     &        linear_phys_name_ctl)
        end if
      end do
!
      end subroutine read_phys_values
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_4_fields
