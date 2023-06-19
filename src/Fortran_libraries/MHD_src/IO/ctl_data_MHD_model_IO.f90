!>@file   ctl_data_MHD_model_IO.f90
!!@brief  module ctl_data_MHD_model_IO
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_sph_mhd_model                                   &
!!     &         (id_control, hd_block, model_ctl, c_buf)
!!      subroutine read_sph_mhd_model_items(id_control, model_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_mhd_model                                  &
!!     &         (id_control, hd_block, model_ctl, level)
!!      subroutine write_sph_mhd_model_items                            &
!!     &         (id_control, model_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        integer(kind = kint), intent(inout) :: level
!!@endverbatim
!
      module ctl_data_MHD_model_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_mhd_evo_area
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
      use t_ctl_data_mhd_normalize
      use t_ctl_data_mhd_forces
      use t_ctl_data_coriolis_force
      use t_ctl_data_gravity
      use t_ctl_data_mhd_magne
      use t_ctl_data_magnetic_scale
      use t_ctl_data_temp_model
      use t_ctl_data_dimless_numbers
      use t_ctl_data_MHD_model
!
      use skip_comment_f
!
      implicit none
!
!
!    label for entry of group
!
      character(len=kchara), parameter, private                         &
     &      :: hd_phys_values =  'phys_values_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_time_evo =     'time_evolution_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_layers_ctl = 'layers_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_bc_4_node =          'bc_4_node'
      character(len=kchara), parameter, private                         &
     &      :: hd_boundary_condition = 'boundary_condition'
      character(len=kchara), parameter, private                         &
     &      :: hd_bc_4_surf =    'bc_4_surface'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_forces_ctl =   'forces_define'
      character(len=kchara), parameter, private                         &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_gravity_ctl =  'gravity_define'
      character(len=kchara), parameter, private                         &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_induction_ctl =  'magnetic_induciton_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_magneto_cv_ctl =  'Magneto_convection_def'
      character(len=kchara), parameter, private                         &
     &      :: hd_bscale_ctl =   'magnetic_field_scale_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_temp_def =     'temperature_define'
      character(len=kchara), parameter, private                         &
     &      :: hd_comp_def =     'composition_define'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_model                                     &
     &         (id_control, hd_block, model_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_model_control), intent(inout) :: model_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(model_ctl%i_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_sph_mhd_model_items(id_control, model_ctl, c_buf)
      end do
      model_ctl%i_model = 1
!
      end subroutine read_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_mhd_model                                    &
     &         (id_control, hd_block, model_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_model_control), intent(in) :: model_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(model_ctl%i_model .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_sph_mhd_model_items(id_control, model_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_mhd_model
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_model_items(id_control, model_ctl, c_buf)
!
      use ctl_data_temp_model_IO
      use ctl_data_comp_model_IO
      use ctl_data_node_boundary_IO
      use ctl_data_surf_boundary_IO
!
      integer(kind = kint), intent(in) :: id_control
!
      type(mhd_model_control), intent(inout) :: model_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, model_ctl%fld_ctl, c_buf)
!
        call read_mhd_time_evo_ctl                                      &
     &     (id_control, hd_time_evo, model_ctl%evo_ctl, c_buf)
        call read_mhd_layer_ctl                                         &
     &     (id_control, hd_layers_ctl, model_ctl%earea_ctl, c_buf)
!
        call read_bc_4_node_ctl(id_control, hd_boundary_condition,      &
     &                          model_ctl%nbc_ctl, c_buf)
        call read_bc_4_node_ctl(id_control, hd_bc_4_node,               &
     &                          model_ctl%nbc_ctl, c_buf)
        call read_bc_4_surf_ctl(id_control, hd_bc_4_surf,               &
     &                          model_ctl%sbc_ctl, c_buf)
!
        call read_forces_ctl                                            &
     &     (id_control, hd_forces_ctl, model_ctl%frc_ctl, c_buf)
        call read_dimless_ctl                                           &
     &     (id_control, hd_dimless_ctl, model_ctl%dless_ctl, c_buf)
        call read_coef_term_ctl                                         &
     &     (id_control, hd_coef_term_ctl, model_ctl%eqs_ctl, c_buf)
!
        call read_gravity_ctl                                           &
     &     (id_control, hd_gravity_ctl, model_ctl%g_ctl, c_buf)
        call read_coriolis_ctl                                          &
     &     (id_control, hd_coriolis_ctl, model_ctl%cor_ctl, c_buf)
        call read_magneto_cv_ctl                                        &
     &     (id_control, hd_magneto_cv_ctl, model_ctl%mcv_ctl, c_buf)
        call read_magnetic_scale_ctl                                    &
     &     (id_control, hd_bscale_ctl, model_ctl%bscale_ctl, c_buf)
        call read_reftemp_ctl                                           &
     &     (id_control, hd_temp_def, model_ctl%reft_ctl, c_buf)
        call read_refcomp_ctl                                           &
     &     (id_control, hd_comp_def, model_ctl%refc_ctl, c_buf)
!
        call read_magneto_cv_ctl                                        &
     &     (id_control, hd_induction_ctl, model_ctl%mcv_ctl, c_buf)
!
      end subroutine read_sph_mhd_model_items
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_mhd_model_items                              &
     &         (id_control, model_ctl, level)
!
      use ctl_data_temp_model_IO
      use ctl_data_comp_model_IO
      use ctl_data_node_boundary_IO
      use ctl_data_surf_boundary_IO
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(mhd_model_control), intent(in) :: model_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      call write_phys_data_control                                      &
     &   (id_control, hd_phys_values, model_ctl%fld_ctl, level)
!
      call write_mhd_time_evo_ctl                                       &
     &   (id_control, hd_time_evo, model_ctl%evo_ctl, level)
      call write_mhd_layer_ctl                                          &
     &   (id_control, hd_layers_ctl, model_ctl%earea_ctl, level)
!
      call write_bc_4_node_ctl(id_control, hd_boundary_condition,       &
     &                         model_ctl%nbc_ctl, level)
      call write_bc_4_surf_ctl(id_control, hd_bc_4_surf,                &
     &                         model_ctl%sbc_ctl, level)
!
      call write_forces_ctl                                             &
     &   (id_control, hd_forces_ctl, model_ctl%frc_ctl, level)
      call write_dimless_ctl                                            &
     &   (id_control, hd_dimless_ctl, model_ctl%dless_ctl, level)
      call write_coef_term_ctl                                          &
     &   (id_control, hd_coef_term_ctl, model_ctl%eqs_ctl, level)
!
      call write_gravity_ctl                                            &
     &   (id_control, hd_gravity_ctl, model_ctl%g_ctl, level)
      call write_coriolis_ctl                                           &
     &   (id_control, hd_coriolis_ctl, model_ctl%cor_ctl, level)
      call write_magneto_cv_ctl                                         &
     &   (id_control, hd_magneto_cv_ctl, model_ctl%mcv_ctl, level)
      call write_magnetic_scale_ctl                                     &
     &   (id_control, hd_bscale_ctl, model_ctl%bscale_ctl, level)
      call write_reftemp_ctl                                            &
     &   (id_control, hd_temp_def, model_ctl%reft_ctl, level)
      call write_refcomp_ctl                                            &
     &   (id_control, hd_comp_def, model_ctl%refc_ctl, level)
!
      end subroutine write_sph_mhd_model_items
!
!   --------------------------------------------------------------------
!
      end module ctl_data_MHD_model_IO
