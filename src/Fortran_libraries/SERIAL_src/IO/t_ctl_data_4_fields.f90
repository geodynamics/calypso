!>@file   t_ctl_data_4_fields.f90
!!        module t_ctl_data_4_fields
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!!
!>@brief  Control brog for field definition
!!
!!@verbatim
!!      subroutine dealloc_phys_control(fld_ctl)
!!
!!      subroutine read_phys_data_control                               &
!!     &         (id_control, hd_block, fld_ctl, c_buf)
!!      subroutine write_phys_data_control                              &
!!     &         (id_control, hd_block, fld_ctl, level)
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
      module t_ctl_data_4_fields
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_character3
      use t_control_array_charaint
      use t_control_array_charaint3
!
      implicit  none
!
!>      Structure of field information control
      type field_control
!>        Structure for list of field
!!@n       field_ctl%icou:  Read flag for 'nod_value_ctl'
!!@n       field_ctl%num:   Number of field
!!@n       field_ctl%c1_tbl: Name of field
!!@n       field_ctl%c2_tbl: flag for visualization output
!!@n       field_ctl%c3_tbl: flag for time series output
        type(ctl_array_c3) :: field_ctl
!
!>        Structure for list of field on quadrature elements
!!@n       quad_phys%icou:  Read flag for 'quad_field_name_ctl'
!!@n       quad_phys%num:   Number of field
!!@n       quad_phys%c_tbl: Name of field
        type(ctl_array_chara) :: quad_phys
!
!>        Structure for list of scalar field
!!@n       quad_phys%icou:  Read flag for 'quad_field_name_ctl'
!!@n       quad_phys%num:   Number of field
!!@n       quad_phys%c_tbl: Name of field
        type(ctl_array_ci) :: scalar_phys
!
!>        Structure for list of vector field
!!@n       quad_phys%icou:  Read flag for 'quad_field_name_ctl'
!!@n       quad_phys%num:   Number of field
!!@n       quad_phys%c_tbl: Name of field
        type(ctl_array_ci3) :: vector_phys
!
        integer (kind=kint) :: i_phys_values =   0
      end type field_control
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
!
      character(len=kchara), parameter                                  &
     &      :: hd_scalar_field =   'scalar_field_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_vector_field =   'vector_field_ctl'
!
      private :: hd_field_list, hd_quad_field
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_phys_control(fld_ctl)
!
      type(field_control), intent(inout) :: fld_ctl
!
      call dealloc_control_array_c3(fld_ctl%field_ctl)
      call dealloc_control_array_chara(fld_ctl%quad_phys)
!
      call dealloc_control_array_c_i(fld_ctl%scalar_phys)
      call dealloc_control_array_c_i3(fld_ctl%vector_phys)
!
      end subroutine dealloc_phys_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_phys_data_control                                 &
     &         (id_control, hd_block, fld_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(field_control), intent(inout) :: fld_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fld_ctl%i_phys_values .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c3                                      &
     &     (id_control, hd_field_list, fld_ctl%field_ctl, c_buf)
        call read_control_array_c1                                      &
     &     (id_control, hd_quad_field, fld_ctl%quad_phys, c_buf)
!
        call read_control_array_c_i                                     &
     &     (id_control, hd_scalar_field, fld_ctl%scalar_phys, c_buf)
        call read_control_array_c_i3                                    &
     &     (id_control, hd_vector_field, fld_ctl%vector_phys, c_buf)
      end do
      fld_ctl%i_phys_values = 1
!
      end subroutine read_phys_data_control
!
!   --------------------------------------------------------------------
!
      subroutine write_phys_data_control                                &
     &         (id_control, hd_block, fld_ctl, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(field_control), intent(in) :: fld_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c3                                       &
     &   (id_control, level, hd_field_list, fld_ctl%field_ctl)
!
      call write_control_array_c1                                       &
     &   (id_control, level, hd_quad_field, fld_ctl%quad_phys)
!
      call write_control_array_c_i                                      &
     &   (id_control, level, hd_scalar_field, fld_ctl%scalar_phys)
      call write_control_array_c_i3                                     &
     &   (id_control, level, hd_vector_field, fld_ctl%vector_phys)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_phys_data_control
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_4_fields
