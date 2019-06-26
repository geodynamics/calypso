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
!!      subroutine read_thermal_ctl                                     &
!!     &         (id_control, hd_block, heat_ctl, c_buf)
!!      subroutine read_composition_eq_ctl                              &
!!     &         (id_control, hd_block, comp_ctl, c_buf)
!!      subroutine bcast_thermal_ctl(heat_ctl)
!!      subroutine dealloc_thermal_ctl(heat_ctl)
!!        type(heat_equation_control), intent(inout) :: heat_ctl
!!        type(heat_equation_control), intent(inout) :: comp_ctl
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
      character(len=kchara) :: hd_n_thermal = 'coef_4_termal_ctl'
      character(len=kchara) :: hd_n_t_diff =  'coef_4_t_diffuse_ctl'
      character(len=kchara) :: hd_n_h_src =  'coef_4_heat_source_ctl'
!
!   5th level for coefs for compositional scalar
!
      character(len=kchara), parameter                                  &
     &         :: hd_n_dscalar =  'coef_4_composition_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_n_dsc_diff = 'coef_4_c_diffuse_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_n_dsc_src =    'coef_4_light_source_ctl'
!
      private :: hd_n_thermal, hd_n_t_diff, hd_n_h_src
      private :: hd_n_dscalar, hd_n_dsc_diff, hd_n_dsc_src
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
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(heat_ctl%i_diff_adv .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
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
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(comp_ctl%i_diff_adv .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
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
      subroutine bcast_thermal_ctl(heat_ctl)
!
      use bcast_control_arrays
!
      type(heat_equation_control), intent(inout) :: heat_ctl
!
!
      call bcast_ctl_array_cr(heat_ctl%coef_4_adv_flux)
      call bcast_ctl_array_cr(heat_ctl%coef_4_diffuse)
      call bcast_ctl_array_cr(heat_ctl%coef_4_source)
!
      call MPI_BCAST(heat_ctl%i_diff_adv, 1,                            &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_thermal_ctl
!
! -----------------------------------------------------------------------
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
