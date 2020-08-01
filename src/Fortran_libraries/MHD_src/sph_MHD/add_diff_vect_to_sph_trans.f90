!>@file   add_diff_vect_to_sph_trans.f90
!!@brief  module add_diff_vect_to_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Add field addresses of difference of fields 
!!       for spherical harmonics transform
!!
!!@verbatim
!!      subroutine add_grad_4_sph_trns_by_pol                           &
!!     &         (ipol_grd, iphys_grd, b_trns_grd, trns)
!!      subroutine add_grad_4_sph_trns_snap                             &
!!     &         (ipol_grd, iphys_grd, b_trns_grd, trns)
!!        type(gradient_field_address), intent(in) :: ipol_grd
!!        type(gradient_field_address), intent(in) :: iphys_grd
!!        type(gradient_field_address), intent(inout) :: b_trns_grd
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_diff_vect_sph_trns_by_pol                        &
!!     &         (ipol_dv, iphys_dv, b_trns_dv, trns)
!!      subroutine add_diff_vect_scalar_trns_bpol                       &
!!     &         (ipol_dv, iphys_dv, f_trns_dv, trns)
!!        type(diff_vector_address), intent(in) :: ipol_dv, iphys_dv
!!        type(diff_vector_address), intent(inout) :: b_trns_dv
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_diff_vect_to_sph_trans
!
      use m_precision
!
      use t_diff_vector_labels
      use t_grad_field_labels
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_grad_4_sph_trns_by_pol                             &
     &         (ipol_grd, iphys_grd, b_trns_grd, trns)
!
      use add_field_to_sph_trans_list
!
      type(gradient_field_address), intent(in) :: ipol_grd
      type(gradient_field_address), intent(in) :: iphys_grd
      type(gradient_field_address), intent(inout) :: b_trns_grd
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   Gradient of temperature
      call add_field_4_sph_trns_by_pol(grad_temp,                       &
     &    ipol_grd%i_grad_temp, iphys_grd%i_grad_temp,                  &
     &    b_trns_grd%i_grad_temp, trns)
!
!   Gradient of composition
      call add_field_4_sph_trns_by_pol(grad_composition,                &
     &    ipol_grd%i_grad_composit, iphys_grd%i_grad_composit,          &
     &    b_trns_grd%i_grad_composit, trns)
!
      end subroutine add_grad_4_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_grad_4_sph_trns_snap                               &
     &         (ipol_grd, iphys_grd, b_trns_grd, trns)
!
      use add_field_to_sph_trans_list
!
      type(gradient_field_address), intent(in) :: ipol_grd
      type(gradient_field_address), intent(in) :: iphys_grd
      type(gradient_field_address), intent(inout) :: b_trns_grd
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   Gradient of temperature
      call add_field_name_4_sph_trns_snap(grad_temp,                    &
     &    ipol_grd%i_grad_temp, iphys_grd%i_grad_temp,                  &
     &    b_trns_grd%i_grad_temp, trns)
!
!   Gradient of composition
      call add_field_name_4_sph_trns_snap(grad_composition,             &
     &    ipol_grd%i_grad_composit, iphys_grd%i_grad_composit,          &
     &    b_trns_grd%i_grad_composit, trns)
!
      end subroutine add_grad_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_diff_vect_sph_trns_by_pol                          &
     &         (ipol_dv, iphys_dv, b_trns_dv, trns)
!
      use m_diff_vector_labels
      use add_field_to_sph_trans_list
!
      type(diff_vector_address), intent(in) :: ipol_dv, iphys_dv
      type(diff_vector_address), intent(inout) :: b_trns_dv
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   Gradient of Radial velocity
      call add_field_4_sph_trns_by_pol(grad_v_1,                        &
     &    ipol_dv%i_grad_vx, iphys_dv%i_grad_vx, b_trns_dv%i_grad_vx,   &
     &    trns)
!   Gradient of meridional velocity
      call add_field_4_sph_trns_by_pol(grad_v_2,                        &
     &    ipol_dv%i_grad_vy, iphys_dv%i_grad_vy, b_trns_dv%i_grad_vy,   &
     &    trns)
!   Gradient of zonal velocity
      call add_field_4_sph_trns_by_pol(grad_v_3,                        &
     &    ipol_dv%i_grad_vz, iphys_dv%i_grad_vz, b_trns_dv%i_grad_vz,   &
     &    trns)
!
!   Gradient of Radial vorticity
      call add_field_4_sph_trns_by_pol(grad_w_1,                        &
     &    ipol_dv%i_grad_wx, iphys_dv%i_grad_wx, b_trns_dv%i_grad_wx,   &
     &    trns)
!   Gradient of meridional vorticity
      call add_field_4_sph_trns_by_pol(grad_w_2,                        &
     &    ipol_dv%i_grad_wy, iphys_dv%i_grad_wy, b_trns_dv%i_grad_wy,   &
     &    trns)
!   Gradient of zonal vorticity
      call add_field_4_sph_trns_by_pol(grad_w_3,                        &
     &    ipol_dv%i_grad_wz, iphys_dv%i_grad_wz, b_trns_dv%i_grad_wz,   &
     &    trns)
!
!   Gradient of Radial magnetic vector potential
      call add_field_4_sph_trns_by_pol(grad_a_1,                        &
     &    ipol_dv%i_grad_ax, iphys_dv%i_grad_ax, b_trns_dv%i_grad_ax,   &
     &    trns)
!   Gradient of meridional vector potential
      call add_field_4_sph_trns_by_pol(grad_a_2,                        &
     &    ipol_dv%i_grad_ay, iphys_dv%i_grad_ay, b_trns_dv%i_grad_ay,   &
     &    trns)
!   Gradient of zonal vector potential
      call add_field_4_sph_trns_by_pol(grad_a_3,                        &
     &    ipol_dv%i_grad_az, iphys_dv%i_grad_az, b_trns_dv%i_grad_az,   &
     &    trns)
!
!   Gradient of Radial magnetic field
      call add_field_4_sph_trns_by_pol(grad_b_1,                        &
     &    ipol_dv%i_grad_bx, iphys_dv%i_grad_bx, b_trns_dv%i_grad_bx,   &
     &    trns)
!   Gradient of meridional magnetic field
      call add_field_4_sph_trns_by_pol(grad_b_2,                        &
     &    ipol_dv%i_grad_by, iphys_dv%i_grad_by, b_trns_dv%i_grad_by,   &
     &    trns)
!   Gradient of zonal magnetic field
      call add_field_4_sph_trns_by_pol(grad_b_3,                        &
     &    ipol_dv%i_grad_bz, iphys_dv%i_grad_bz, b_trns_dv%i_grad_bz,   &
     &    trns)
!
!   Gradient of Radial current density
      call add_field_4_sph_trns_by_pol(grad_j_1,                        &
     &    ipol_dv%i_grad_jx, iphys_dv%i_grad_jx, b_trns_dv%i_grad_jx,   &
     &    trns)
!   Gradient of meridional current density
      call add_field_4_sph_trns_by_pol(grad_j_2,                        &
     &    ipol_dv%i_grad_jy, iphys_dv%i_grad_jy, b_trns_dv%i_grad_jy,   &
     &    trns)
!   Gradient of zonal current density
      call add_field_4_sph_trns_by_pol(grad_j_3,                        &
     &    ipol_dv%i_grad_jz, iphys_dv%i_grad_jz, b_trns_dv%i_grad_jz,   &
     &    trns)
!
      end subroutine add_diff_vect_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_diff_vect_scalar_trns_bpol                         &
     &         (ipol_dv, iphys_dv, f_trns_dv, trns)
!
      use m_diff_vector_labels
      use add_field_to_sph_trans_list
!
      type(diff_vector_address), intent(in) :: ipol_dv, iphys_dv
      type(spherical_transform_data), intent(inout) :: trns
      type(diff_vector_address), intent(inout) :: f_trns_dv
!
!
!    velocity
      call add_scalar_4_sph_trns_by_pol(grad_v_1, ipol_dv%i_grad_vx,    &
     &    iphys_dv%i_grad_vx, f_trns_dv%i_grad_vx, trns)
      call add_scalar_4_sph_trns_by_pol(grad_v_2, ipol_dv%i_grad_vy,    &
     &    iphys_dv%i_grad_vy, f_trns_dv%i_grad_vy, trns)
      call add_scalar_4_sph_trns_by_pol(grad_v_3, ipol_dv%i_grad_vz,    &
     &    iphys_dv%i_grad_vz, f_trns_dv%i_grad_vz, trns)
!
!    vorticity
      call add_scalar_4_sph_trns_by_pol(grad_w_1, ipol_dv%i_grad_wx,    &
     &    iphys_dv%i_grad_wx, f_trns_dv%i_grad_wx, trns)
      call add_scalar_4_sph_trns_by_pol(grad_w_2, ipol_dv%i_grad_wy,    &
     &    iphys_dv%i_grad_wy, f_trns_dv%i_grad_wy, trns)
      call add_scalar_4_sph_trns_by_pol(grad_w_3, ipol_dv%i_grad_wz,    &
     &    iphys_dv%i_grad_wz, f_trns_dv%i_grad_wz, trns)
!
!    magnetic vector potential
      call add_scalar_4_sph_trns_by_pol(grad_a_1, ipol_dv%i_grad_ax,    &
     &    iphys_dv%i_grad_ax, f_trns_dv%i_grad_ax, trns)
      call add_scalar_4_sph_trns_by_pol(grad_a_2, ipol_dv%i_grad_ay,    &
     &    iphys_dv%i_grad_ay, f_trns_dv%i_grad_ay, trns)
      call add_scalar_4_sph_trns_by_pol(grad_a_3, ipol_dv%i_grad_az,    &
     &    iphys_dv%i_grad_az, f_trns_dv%i_grad_az, trns)
!
!    magnetic field
      call add_scalar_4_sph_trns_by_pol(grad_b_1, ipol_dv%i_grad_bx,    &
     &    iphys_dv%i_grad_bx, f_trns_dv%i_grad_bx, trns)
      call add_scalar_4_sph_trns_by_pol(grad_b_2, ipol_dv%i_grad_by,    &
     &    iphys_dv%i_grad_by, f_trns_dv%i_grad_by, trns)
      call add_scalar_4_sph_trns_by_pol(grad_b_3, ipol_dv%i_grad_bz,    &
     &    iphys_dv%i_grad_bz, f_trns_dv%i_grad_bz, trns)
!
!    current density
      call add_scalar_4_sph_trns_by_pol(grad_j_1, ipol_dv%i_grad_jx,    &
     &    iphys_dv%i_grad_jx, f_trns_dv%i_grad_jx, trns)
      call add_scalar_4_sph_trns_by_pol(grad_j_2, ipol_dv%i_grad_jy,    &
     &    iphys_dv%i_grad_jy, f_trns_dv%i_grad_jy, trns)
      call add_scalar_4_sph_trns_by_pol(grad_j_3, ipol_dv%i_grad_jz,    &
     &    iphys_dv%i_grad_jz, f_trns_dv%i_grad_jz, trns)
!
      end subroutine add_diff_vect_scalar_trns_bpol
!
!-----------------------------------------------------------------------
!
      end module add_diff_vect_to_sph_trans
