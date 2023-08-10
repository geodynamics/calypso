!>@file   t_ctl_data_surf_boundary.f90
!!@brief  module t_ctl_data_surf_boundary
!!
!!@author H. Matsui
!>@brief   Control for surface boundary conditions for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine bcast_bc_4_surf_ctl(sbc_ctl)
!!      subroutine dealloc_bc_4_surf_ctl(sbc_ctl)
!!        type(surf_bc_control), intent(inout) :: sbc_ctl
!!
!! ------------------------------------------------------------------
!!   example
!!
!!    begin bc_4_surface
!!!!!  boundary condition for heat flux  !!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available type:  fixed, file, SGS_commute
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array heat_flux_surf
!!        heat_flux_surf  fixed       outer  0.000  end
!!        heat_flux_surf  SGS_commute inner  0.000  end
!!      end array heat_flux_surf
!!!!!  boundary condition for torque  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  available type
!!     fix_x,  fix_y,  fix_z
!!     file_x, file_y, file_z
!!     normal_velocity
!!     free_shell_in, free_shell_out
!!     free_4_plane
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array velocity_surf
!!        velocity_surf  free_shell_in inner_surf   0.000  end
!!        velocity_surf  free_shell_out  outer_surf   0.000  end
!!      end array velocity_surf
!!!!!  boundary condition for pressure gradiend !!!!!!!!!!!!!!!!!!!
!!  available type:  inner_shell, outer_shell
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!       array pressure_surf
!!         pressure_surf   inner_shell inner_surf 0.000  end
!!         pressure_surf   outer_shell outer_surf 0.000  end
!!      end array pressure_surf
!!!!!!  boundary condition for gradientof magnetic field  !!!!!!!!!!
!!     fix_x,  fix_y,  fix_z
!!     insulate_in, insulate_out (not recommended)
!!     far_away                  (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!       array magnetic_field_surf
!!          magnetic_field_surf  insulate_in  ICB_surf  0.000 end
!!          magnetic_field_surf  insulate_out CMB_surf  0.000 end
!!          magnetic_field_surf  far_away infinity_surf  0.000 end
!!      end array magnetic_field_surf
!!!!!  boundary condition for gradientof magnetic field  !!!!!!!!!!
!!     fix_x,  fix_y,  fix_z
!!     insulate_in, insulate_out (not recommended)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!       array vector_potential_surf
!!          vector_potential_surf  insulate_out CMB_surf  0.000 end
!!      end array vector_potential_surf
!!!!!  boundary condition for current density on surface  !!!!!!!!!!
!!     fix_x,  fix_y,  fix_z
!!     insulate_in,insulate_out (not recommended)
!!     far_away                  (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!       array current_surf
!!          current_surf  insulate_in  ICB_surf  0.000 end
!!          current_surf  insulate_out CMB_surf  0.000 end
!!          current_surf  far_away infinity_surf  0.000 end
!!      end array current_surf
!!!!!  boundary condition for magnetic potential !!!!!!!!!!!!!!!!!
!!  available type:  fixed (not used), file (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!       array electric_potential_surf
!!          electric_potential_surf  insulate_in  ICB_surf  0.000 end
!!          electric_potential_surf  insulate_out CMB_surf  0.000 end
!!          electric_potential_surf  far_away infinity_surf  0.000 end
!!      end array electric_potential_surf
!!!!!  boundary condition for dummy scalar !!!!!!!!!!!!!!!!!
!!  available type:  fixed_grad (not used), file_grad (not used)
!!                   fixed_field
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!       array composition_flux_surf
!!          composition_flux_surf  insulate_in  ICB_surf  0.000 end
!!          composition_flux_surf  insulate_out CMB_surf  0.000 end
!!          composition_flux_surf  far_away infinity_surf  0.000 end
!!      end array composition_flux_surf
!!!!!  boundary condition for infinity (obsolute) !!!!!!!!!!!!!!!!!
!!  available type:  fixed (not used), file (not used)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      array infinity_surf
!!        infinity_surf  fixed infinity_surf  0.000  end
!!      end array infinity_surf
!!    end  bc_4_surface
!!
!! ------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_surf_boundary
!
      use m_precision
!
      use m_machine_parameter
      use t_control_array_chara2real
!
      implicit  none
!
!
      type surf_bc_control
!>        Block name
        character(len=kchara) :: block_name = 'bc_4_surface'
!>        Structure for surface boundary conditions for heat flux
!!@n       surf_bc_HF_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_HF_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_HF_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_HF_ctl
!
!>        Structure for surface boundary conditions for stress
!!@n       surf_bc_ST_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_ST_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_ST_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_ST_ctl
!
!>        Structure for surface boundary conditions for pressure gradient
!!@n       surf_bc_PN_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_PN_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_PN_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_PN_ctl
!
!>        Structure for surface boundary conditions
!!           for grad of magnetic field
!!@n       surf_bc_BN_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_BN_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_BN_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_BN_ctl
!
!>        Structure for surface boundary conditions
!!           for grad of current density
!!@n       surf_bc_JN_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_JN_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_JN_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_JN_ctl
!
!>        Structure for surface boundary conditions
!!          for grad of magnetic vector potential
!!@n       surf_bc_AN_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_AN_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_AN_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_AN_ctl
!
!>        Structure for surface boundary conditions
!!          for grad of magnetic scalar potential
!!@n       surf_bc_MPN_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_MPN_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_MPN_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_MPN_ctl
!
!>        Structure for surface boundary conditions for composition
!!@n       surf_bc_CF_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_CF_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_CF_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_CF_ctl
!
!
!>        Structure for surface boundary conditions for infinity
!!@n       surf_bc_INF_ctl%c1_tbl:  Type of boundary conditions
!!@n       surf_bc_INF_ctl%c2_tbl:  Surface group name for boundary
!!@n       surf_bc_INF_ctl%vect:    boundary condition value
        type(ctl_array_c2r) :: surf_bc_INF_ctl
!
        integer (kind=kint) :: i_bc_4_surf =     0
      end type surf_bc_control
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_bc_4_surf_ctl(sbc_ctl)
!
      type(surf_bc_control), intent(inout) :: sbc_ctl
!
!
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_HF_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_ST_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_PN_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_BN_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_JN_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_AN_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_MPN_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_CF_ctl)
      call dealloc_control_array_c2_r(sbc_ctl%surf_bc_INF_ctl)
!
      sbc_ctl%i_bc_4_surf = 0
!
      end subroutine dealloc_bc_4_surf_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_surf_boundary
