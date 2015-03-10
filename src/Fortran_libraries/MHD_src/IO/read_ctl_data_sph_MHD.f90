!>@file   read_ctl_data_sph_MHD.f90
!!@brief  module read_ctl_data_sph_MHD
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
!!      subroutine read_sph_mhd_model
!!      subroutine read_sph_mhd_control
!!@endverbatim
!
      module read_ctl_data_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit none
!
!   2nd level for MHD
!
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
!
      integer (kind=kint) :: i_model =        0
      integer (kind=kint) :: i_control =      0
!
      private :: hd_model, hd_control, i_model, i_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_model
!
      use m_ctl_data_4_fields
      use m_ctl_data_mhd_evolution
      use m_ctl_data_node_boundary
      use m_ctl_data_surf_boundary
      use m_ctl_data_mhd_forces
      use m_ctl_data_mhd_normalize
      use m_ctl_data_temp_model
!
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_values
!
        call read_mhd_time_evo_ctl
        call read_mhd_layer_ctl
!
        call read_bc_4_node
        call read_bc_4_surf
!
        call read_forces_ctl
        call read_dimless_ctl
        call read_coef_term_ctl
!
        call read_gravity_ctl
        call read_coriolis_ctl
        call read_magneto_ctl
        call read_temp_def
      end do
!
      end subroutine read_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_control
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_mhd_evo_scheme
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
!
        call read_time_step_ctl
        call read_restart_ctl
!
        call read_time_loop_ctl
      end do
!
      end subroutine read_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      end module read_ctl_data_sph_MHD
