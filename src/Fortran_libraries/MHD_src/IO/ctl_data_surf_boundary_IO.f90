!>@file   ctl_data_surf_boundary_IO.f90
!!@brief  module ctl_data_surf_boundary_IO
!!
!!@author H. Matsui
!>@brief   Control for surface boundary conditions for dynamo
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on Oct., 2007
!!
!!@verbatim
!!      subroutine read_bc_4_surf_ctl                                   &
!!     &         (id_control, hd_block, sbc_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(surf_bc_control), intent(inout) :: sbc_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_bc_4_surf_ctl                                  &
!!     &         (id_control, hd_block, sbc_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(surf_bc_control), intent(in) :: sbc_ctl
!!        integer(kind = kint), intent(inout) :: level
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
      module ctl_data_surf_boundary_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_control_array_chara2real
      use t_ctl_data_surf_boundary
!
      implicit  none
!
!   4th level for surface boundary
!
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_hf =     'heat_flux_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_mf =     'velocity_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_gradp =  'pressure_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_gradb =  'magnetic_field_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_grada =  'vector_potential_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_gradj =  'current_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_gradmp = 'electric_potential_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_gradc =  'composition_flux_surf'
      character(len=kchara), parameter, private                         &
     &       :: hd_n_bc_infty =  'infinity_surf'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_bc_4_surf_ctl                                     &
     &         (id_control, hd_block, sbc_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(surf_bc_control), intent(inout) :: sbc_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sbc_ctl%i_bc_4_surf .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_hf, sbc_ctl%surf_bc_HF_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_mf, sbc_ctl%surf_bc_ST_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_gradp, sbc_ctl%surf_bc_PN_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_gradb, sbc_ctl%surf_bc_BN_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_gradj, sbc_ctl%surf_bc_JN_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_grada, sbc_ctl%surf_bc_AN_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_gradmp, sbc_ctl%surf_bc_MPN_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_gradc, sbc_ctl%surf_bc_CF_ctl, c_buf)
        call read_control_array_c2_r(id_control,                        &
     &      hd_n_bc_infty, sbc_ctl%surf_bc_INF_ctl, c_buf)
      end do
      sbc_ctl%i_bc_4_surf = 1
!
      end subroutine read_bc_4_surf_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_bc_4_surf_ctl                                    &
     &         (id_control, hd_block, sbc_ctl, level)
!
      use t_read_control_elements
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(surf_bc_control), intent(in) :: sbc_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(sbc_ctl%i_bc_4_surf .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_hf, sbc_ctl%surf_bc_HF_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_mf, sbc_ctl%surf_bc_ST_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_gradp, sbc_ctl%surf_bc_PN_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_gradb, sbc_ctl%surf_bc_BN_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_grada, sbc_ctl%surf_bc_AN_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_gradj, sbc_ctl%surf_bc_JN_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_gradmp, sbc_ctl%surf_bc_MPN_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_gradc, sbc_ctl%surf_bc_CF_ctl)
      call write_control_array_c2_r(id_control, level,                  &
     &    hd_n_bc_infty, sbc_ctl%surf_bc_INF_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_bc_4_surf_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_data_surf_boundary_IO
