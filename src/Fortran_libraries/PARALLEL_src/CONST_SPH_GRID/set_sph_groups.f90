!>@file   set_sph_groups.f90
!!@brief  module set_sph_groups
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Set groups for spherical harmonics indexing
!!
!!@verbatim
!!      subroutine set_sph_rtp_groups(sph_param, sph_rtp,               &
!!     &          added_radial_grp, r_layer_grp, med_layer_grp,         &
!!     &          bc_rtp_grp, radial_rtp_grp, theta_rtp_grp,            &
!!     &          zonal_rtp_grp)
!!        type(sph_shell_parameters), intent(in) :: sph_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(group_data), intent(inout) :: bc_rtp_grp
!!        type(group_data), intent(inout) :: radial_rtp_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!        type(group_data), intent(inout) :: zonal_rtp_grp
!!      subroutine set_sph_rj_groups                                    &
!!     &         (sph_param, sph_rj, added_radial_grp,                  &
!!     &          radial_rj_grp, sphere_rj_grp)
!!        type(sph_shell_parameters), intent(in) :: sph_param
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!      subroutine set_rj_radial_grp                                    &
!!     &         (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!@endverbatim
!
      module set_sph_groups
!
      use m_precision
!
      use t_spheric_parameter
      use t_group_data
      use t_control_1D_layering
!
      implicit none
!
      private :: set_rtp_meridional_grp
      private :: set_rtp_radial_grp
      private :: set_rj_spectr_grp
      private :: set_no_rtp_node_grp
      private :: set_no_rtp_meridian_grp, set_no_rtp_zonal_grp
      private :: count_sph_radial_group, add_sph_SGS_radial_group
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_rtp_groups(sph_param, sph_rtp,                 &
     &          added_radial_grp, r_layer_grp, med_layer_grp,           &
     &          bc_rtp_grp, radial_rtp_grp, theta_rtp_grp,              &
     &          zonal_rtp_grp)
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(layering_group_list), intent(in) :: added_radial_grp
      type(layering_group_list), intent(in) :: r_layer_grp
      type(layering_group_list), intent(in) :: med_layer_grp
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
!
!      write(*,*) 'set_rtp_radial_grp'
      call set_rtp_radial_grp(sph_param, sph_rtp,                       &
     &   added_radial_grp, r_layer_grp, radial_rtp_grp)
!      write(*,*) 'set_rtp_meridional_grp'
      call set_rtp_meridional_grp                                       &
     &   (sph_rtp, med_layer_grp, theta_rtp_grp)
!      write(*,*) 'set_no_rtp_zonal_grp'
      call set_no_rtp_zonal_grp(zonal_rtp_grp)
!
!      write(*,*) 'set_no_rtp_node_grp'
      call set_no_rtp_node_grp(bc_rtp_grp)
!
      end subroutine set_sph_rtp_groups
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_rj_groups                                      &
     &         (sph_param, sph_rj, added_radial_grp,                    &
     &          radial_rj_grp, sphere_rj_grp)
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rj_grid), intent(in) :: sph_rj
      type(layering_group_list), intent(in) :: added_radial_grp
!
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
!
!      write(*,*) 'set_rj_radial_grp'
      call set_rj_radial_grp                                            &
     &   (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!      write(*,*) 'set_rj_spectr_grp'
      call set_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
      end subroutine set_sph_rj_groups
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_rtp_radial_grp(sph_param, sph_rtp,                 &
     &          added_radial_grp, r_layer_grp, radial_rtp_grp)
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(layering_group_list), intent(in) :: added_radial_grp
      type(layering_group_list), intent(in) :: r_layer_grp
!
      type(group_data), intent(inout) :: radial_rtp_grp
!
!
      call count_sph_radial_group(radial_rtp_grp%num_grp,               &
     &    sph_param, added_radial_grp, sph_rtp%nidx_global_rtp(1))
      call add_sph_SGS_radial_group                                     &
     &   (r_layer_grp, radial_rtp_grp%num_grp)
      call allocate_grp_type_num(radial_rtp_grp)
!
      call set_stack_rtp_radial_grp(sph_param, sph_rtp,                 &
     &    r_layer_grp, added_radial_grp, radial_rtp_grp)
!
      call allocate_grp_type_item(radial_rtp_grp)
      call set_item_rtp_radial_grp(sph_param, sph_rtp,                  &
     &   r_layer_grp, added_radial_grp, radial_rtp_grp)
!
      end subroutine set_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rtp_meridional_grp                                 &
     &         (sph_rtp, med_layer_grp, theta_rtp_grp)
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(layering_group_list), intent(in) :: med_layer_grp
!
      type(group_data), intent(inout) :: theta_rtp_grp
!
!
      theta_rtp_grp%num_grp =  med_layer_grp%nlayer
      call allocate_grp_type_num(theta_rtp_grp)
!
      call set_stack_rtp_meridional_grp                                 &
     &   (sph_rtp, med_layer_grp, theta_rtp_grp)
!
      call allocate_grp_type_item(theta_rtp_grp)
      call set_item_rtp_meridional_grp                                  &
     &   (sph_rtp, med_layer_grp, theta_rtp_grp)
!
      end subroutine set_rtp_meridional_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_radial_grp                                      &
     &         (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rj_grid), intent(in) :: sph_rj
      type(layering_group_list), intent(in) :: added_radial_grp
!
      type(group_data), intent(inout) :: radial_rj_grp
!
!
      call count_sph_radial_group(radial_rj_grp%num_grp,                &
     &    sph_param, added_radial_grp, sph_rj%nidx_global_rj(1))
      call allocate_grp_type_num(radial_rj_grp)
      call set_stack_rj_radial_grp                                      &
     &   (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!
      call allocate_grp_type_item(radial_rj_grp)
      call set_item_rj_radial_grp                                       &
     &   (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!
      end subroutine set_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
      use set_stack_4_sph_groups
      use set_item_4_sph_groups
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(inout) :: sphere_rj_grp
!
!
      sphere_rj_grp%num_grp =  4
!      write(*,*) 'allocate_rj_sphere_grp_stack'
      call allocate_grp_type_num(sphere_rj_grp)
!      write(*,*) 'set_stack_rj_spectr_grp'
      call set_stack_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
!      write(*,*) 'allocate_rj_sphere_grp_item'
      call allocate_grp_type_item(sphere_rj_grp)
!      write(*,*) 'set_item_rj_spectr_grp'
      call set_item_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
      end subroutine set_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_node_grp(bc_rtp_grp)
!
      type(group_data), intent(inout) :: bc_rtp_grp
!
      bc_rtp_grp%num_grp =  0
      bc_rtp_grp%num_item = 0
      call allocate_grp_type_num(bc_rtp_grp)
      call allocate_grp_type_item(bc_rtp_grp)
!
      end subroutine set_no_rtp_node_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_meridian_grp(theta_rtp_grp)
!
      type(group_data), intent(inout) :: theta_rtp_grp
!
      theta_rtp_grp%num_grp =  0
      theta_rtp_grp%num_item = 0
      call allocate_grp_type_num(theta_rtp_grp)
      call allocate_grp_type_item(theta_rtp_grp)
!
      end subroutine set_no_rtp_meridian_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_no_rtp_zonal_grp(zonal_rtp_grp)
!
      type(group_data), intent(inout) :: zonal_rtp_grp
!
!
      zonal_rtp_grp%num_grp =  0
      zonal_rtp_grp%num_item = 0
      call allocate_grp_type_num(zonal_rtp_grp)
      call allocate_grp_type_item(zonal_rtp_grp)
!
      end subroutine set_no_rtp_zonal_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_sph_radial_group                                 &
     &         (num_grp, sph_param, added_radial_grp, nidx_global_r)
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(layering_group_list), intent(in) :: added_radial_grp
      integer(kind = kint), intent(in) :: nidx_global_r
      integer(kind = kint), intent(inout) :: num_grp
!
!
      num_grp =  3 + added_radial_grp%nlayer
      if(sph_param%nlayer_2_center .gt. 0) num_grp =  num_grp + 2
      if(nidx_global_r .gt. sph_param%nlayer_CMB) num_grp =  num_grp+1
      if(sph_param%nlayer_mid_OC .gt. 0) num_grp =  num_grp + 1
!
      end subroutine count_sph_radial_group
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_SGS_radial_group(r_layer_grp, num_grp)
!
      type(layering_group_list), intent(in) :: r_layer_grp
      integer(kind = kint), intent(inout) :: num_grp
!
!
      num_grp = num_grp + r_layer_grp%nlayer
!
      end subroutine add_sph_SGS_radial_group
!
! -----------------------------------------------------------------------
!
      end module set_sph_groups
