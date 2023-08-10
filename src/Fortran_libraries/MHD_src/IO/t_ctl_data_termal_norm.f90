!>@file   t_ctl_data_termal_norm.f90
!!@brief  module t_ctl_data_termal_norm
!!
!!@author H. Matsui
!!@date Programmed in March. 2006
!!@date Modified in July, 2013
!
!>@brief  Thermal equation parameters to read
!!
!!@verbatim
!!      subroutine init_thermal_ctl_label(hd_block, heat_ctl)
!!      subroutine init_composition_eq_ctl_label(hd_block, comp_ctl)
!!      subroutine read_thermal_ctl                                     &
!!     &         (id_control, hd_block, heat_ctl, c_buf)
!!      subroutine read_composition_eq_ctl                              &
!!     &         (id_control, hd_block, comp_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(heat_equation_control), intent(inout) :: heat_ctl
!!        type(heat_equation_control), intent(inout) :: comp_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_thermal_ctl                                    &
!!     &         (id_control, heat_ctl, level)
!!      subroutine write_composition_eq_ctl                             &
!!     &         (id_control, comp_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(heat_equation_control), intent(in) :: heat_ctl
!!        type(heat_equation_control), intent(in) :: comp_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_thermal_ctl(heat_ctl)
!!        type(heat_equation_control), intent(inout) :: heat_ctl
!!
!!   --------------------------------------------------------------------
!! example of control block
!!
!!      begin thermal
!!        array coef_4_termal_ctl     1
!!          coef_4_termal_ctl            One                   1.0
!!        end array coef_4_termal_ctl
!!
!!        array coef_4_t_diffuse_ctl  1
!!          coef_4_t_diffuse_ctl         Prandtl_number       -1.0
!!        end array coef_4_t_diffuse_ctl
!!
!!        array coef_4_heat_source_ctl  1
!!          coef_4_heat_source_ctl       One                   1.0
!!        end array coef_4_heat_source_ctl
!!      end  thermal
!!
!!   --------------------------------------------------------------------
!!
!!   --------------------------------------------------------------------
!! example of control block
!!
!!     begin composition
!!        array coef_4_composition_ctl     1
!!          coef_4_composition_ctl           One       1.0
!!        end array coef_4_composition_ctl
!!
!!        array coef_4_c_diffuse_ctl  1
!!          coef_4_c_diffuse_ctl    Composite_Prandtl_number  -1.0
!!        end array coef_4_c_diffuse_ctl
!!
!!        array coef_4_light_source_ctl  1
!!          coef_4_light_source_ctl     One                    1.0
!!        end array coef_4_light_source_ctl
!!      end  composition
!!
!!   --------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_termal_norm
!
      use m_precision
      use m_machine_parameter
      use t_control_array_charareal
!
      implicit  none
!
!
!>      Structure for coefficients of heat and composition equation
      type heat_equation_control
!>        Block name
        character(len=kchara) :: block_name = 'scalar'
!>        Structure for number and power to construct heat flux
!!@n        coef_4_adv_flux%c_tbl:  Name of number 
!!@n        coef_4_adv_flux%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_adv_flux
!
!>        Structure for number and power to construct thermal diffusion
!!@n        coef_4_diffuse%c_tbl:  Name of number 
!!@n        coef_4_diffuse%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_diffuse
!
!>        Structure for number and power to construct heat source
!!@n        coef_4_source%c_tbl:  Name of number 
!!@n        coef_4_source%vect:   Power of the number
        type(ctl_array_cr) :: coef_4_source
!
        integer (kind=kint) :: i_diff_adv = 0
      end type heat_equation_control
!
!   5th level for coefs for thermal
!
      character(len=kchara), parameter, private                         &
     &         :: hd_n_thermal = 'coef_4_termal_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_n_t_diff =  'coef_4_t_diffuse_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_n_h_src =  'coef_4_heat_source_ctl'
!
!   5th level for coefs for compositional scalar
!
      character(len=kchara), parameter, private                         &
     &         :: hd_n_dscalar =  'coef_4_composition_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_n_dsc_diff = 'coef_4_c_diffuse_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_n_dsc_src =    'coef_4_light_source_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_thermal_ctl                                       &
     &         (id_control, hd_block, heat_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(heat_equation_control), intent(inout) :: heat_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(heat_ctl%i_diff_adv .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_n_thermal, heat_ctl%coef_4_adv_flux, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_t_diff, heat_ctl%coef_4_diffuse, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_h_src, heat_ctl%coef_4_source, c_buf)
      end do
      heat_ctl%i_diff_adv = 1
!
      end subroutine read_thermal_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_composition_eq_ctl                                &
     &         (id_control, hd_block, comp_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(heat_equation_control), intent(inout) :: comp_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(comp_ctl%i_diff_adv .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_r(id_control,                         &
     &      hd_n_dscalar, comp_ctl%coef_4_adv_flux, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_dsc_diff, comp_ctl%coef_4_diffuse, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_n_dsc_src, comp_ctl%coef_4_source, c_buf)
      end do
      comp_ctl%i_diff_adv = 1
!
      end subroutine read_composition_eq_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_thermal_ctl(id_control, heat_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(heat_equation_control), intent(in) :: heat_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(heat_ctl%i_diff_adv .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 heat_ctl%block_name)
      call write_control_array_c_r(id_control, level,                   &
     &      heat_ctl%coef_4_adv_flux)
      call write_control_array_c_r(id_control, level,                   &
     &      heat_ctl%coef_4_diffuse)
      call write_control_array_c_r(id_control, level,                   &
     &      heat_ctl%coef_4_source)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                heat_ctl%block_name)
!
      end subroutine write_thermal_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_composition_eq_ctl(id_control, comp_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(heat_equation_control), intent(in) :: comp_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(comp_ctl%i_diff_adv .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 comp_ctl%block_name)
      call write_control_array_c_r(id_control, level,                   &
     &    comp_ctl%coef_4_adv_flux)
      call write_control_array_c_r(id_control, level,                   &
     &    comp_ctl%coef_4_diffuse)
      call write_control_array_c_r(id_control, level,                   &
     &    comp_ctl%coef_4_source)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                comp_ctl%block_name)
!
      end subroutine write_composition_eq_ctl
!
!   --------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_thermal_ctl_label(hd_block, heat_ctl)
      character(len=kchara), intent(in) :: hd_block
      type(heat_equation_control), intent(inout) :: heat_ctl
!
      heat_ctl%block_name = trim(hd_block)
        call init_c_r_ctl_array_label                                   &
     &     (hd_n_thermal, heat_ctl%coef_4_adv_flux)
        call init_c_r_ctl_array_label                                   &
     &     (hd_n_t_diff, heat_ctl%coef_4_diffuse)
        call init_c_r_ctl_array_label                                   &
     &     (hd_n_h_src, heat_ctl%coef_4_source)
!
      end subroutine init_thermal_ctl_label
!
! -----------------------------------------------------------------------
!
      subroutine init_composition_eq_ctl_label(hd_block, comp_ctl)
      character(len=kchara), intent(in) :: hd_block
      type(heat_equation_control), intent(inout) :: comp_ctl
!
      comp_ctl%block_name = trim(hd_block)
        call init_c_r_ctl_array_label                                   &
     &     (hd_n_dscalar, comp_ctl%coef_4_adv_flux)
        call init_c_r_ctl_array_label                                   &
     &     (hd_n_dsc_diff, comp_ctl%coef_4_diffuse)
        call init_c_r_ctl_array_label                                   &
     &     (hd_n_dsc_src, comp_ctl%coef_4_source)
!
      end subroutine init_composition_eq_ctl_label
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_thermal_ctl(heat_ctl)
!
      type(heat_equation_control), intent(inout) :: heat_ctl
!
!
      call dealloc_control_array_c_r(heat_ctl%coef_4_adv_flux)
      call dealloc_control_array_c_r(heat_ctl%coef_4_diffuse)
      call dealloc_control_array_c_r(heat_ctl%coef_4_source)
      heat_ctl%i_diff_adv = 0
!
      end subroutine dealloc_thermal_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_termal_norm
