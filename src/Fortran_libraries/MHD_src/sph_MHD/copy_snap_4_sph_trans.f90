!>@file   copy_snap_4_sph_trans.f90
!!@brief  module copy_snap_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_snap_vec_fld_from_trans
!!      subroutine copy_snap_vec_fld_to_trans
!!@endverbatim
!
      module copy_snap_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_from_trans
!
      use m_node_phys_address
      use m_addresses_trans_sph_snap
!
!
!$omp parallel
!  Copy vectors
      call copy_vector_from_snap_trans(bs_trns%i_velo, iphys%i_velo)
      call copy_vector_from_snap_trans(bs_trns%i_vort, iphys%i_vort)
      call copy_vector_from_snap_trans(bs_trns%i_magne, iphys%i_magne)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_current, iphys%i_current)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_v_diffuse, iphys%i_v_diffuse)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_w_diffuse, iphys%i_w_diffuse)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_vp_diffuse, iphys%i_vp_diffuse)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_b_diffuse, iphys%i_b_diffuse)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_inertia, iphys%i_rot_inertia)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_Coriolis, iphys%i_rot_Coriolis)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_Lorentz, iphys%i_rot_Lorentz)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_buoyancy, iphys%i_rot_buoyancy)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_comp_buo, iphys%i_rot_comp_buo)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_press_grad, iphys%i_press_grad)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_induction, iphys%i_induction)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_grad_t, iphys%i_grad_t)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_grad_composit, iphys%i_grad_composit)
!
      call copy_vector_from_snap_trans                                  &
     &    (bs_trns%i_grad_vx, iphys%i_grad_vx)
      call copy_vector_from_snap_trans                                  &
     &    (bs_trns%i_grad_vy, iphys%i_grad_vy)
      call copy_vector_from_snap_trans                                  &
     &    (bs_trns%i_grad_vz, iphys%i_grad_vz)
!
!  Copy scalars
      call copy_scalar_from_snap_trans(bs_trns%i_temp, iphys%i_temp)
      call copy_scalar_from_snap_trans(bs_trns%i_light, iphys%i_light)
!
      call copy_scalar_from_snap_trans(bs_trns%i_press, iphys%i_press)
      call copy_scalar_from_snap_trans                                  &
     &      (bs_trns%i_par_temp, iphys%i_par_temp)
      call copy_scalar_from_snap_trans                                  &
     &      (bs_trns%i_filter_temp, iphys%i_filter_temp)
      call copy_scalar_from_snap_trans                                  &
     &      (bs_trns%i_t_diffuse, iphys%i_t_diffuse)
      call copy_scalar_from_snap_trans                                  &
     &      (bs_trns%i_c_diffuse, iphys%i_c_diffuse)
!
      call copy_scalar_from_snap_trans                                  &
     &      (bs_trns%i_div_Coriolis, iphys%i_div_Coriolis)
!$omp end parallel
!
      end subroutine copy_snap_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_to_trans
!
      use m_node_phys_address
      use m_addresses_trans_sph_snap
!
!
!$omp parallel
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_coriolis, iphys%i_Coriolis)
!
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_electric, iphys%i_electric)
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_poynting, iphys%i_poynting)
!
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_mag_stretch, iphys%i_mag_stretch)
!
!
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_me_gen, iphys%i_me_gen)
      call copy_scalar_from_snap_force(fs_trns%i_ujb, iphys%i_ujb)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_nega_ujb, iphys%i_nega_ujb)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_buo_gen, iphys%i_buo_gen)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_c_buo_gen, iphys%i_c_buo_gen)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_f_buo_gen, iphys%i_f_buo_gen)
!
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_velo_scale, iphys%i_velo_scale)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_magne_scale, iphys%i_magne_scale)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_temp_scale, iphys%i_temp_scale)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_comp_scale, iphys%i_comp_scale)
!$omp end parallel
!
      end  subroutine copy_snap_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_trans(i_trns, i_field)
!
      use m_addresses_trans_sph_snap
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_geometry_parameter
      use m_node_phys_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_scalar_from_trans                                       &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack,                      &
     &    numnod, fls_rtp(1,i_trns), d_nod(1,i_field) )
!
      end subroutine copy_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_trans(i_trns, i_field)
!
      use m_addresses_trans_sph_snap
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_geometry_parameter
      use m_node_phys_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_vector_from_trans                                       &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack,                      &
     &    numnod, fls_rtp(1,i_trns), d_nod(1,i_field) )
!
      end subroutine copy_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_force(i_trns, i_field)
!
      use m_addresses_trans_sph_snap
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_scalar_from_trans                                       &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack,                      &
     &    numnod, frs_rtp(1,i_trns), d_nod(1,i_field) )
!
      end subroutine copy_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_force(i_trns, i_field)
!
      use m_addresses_trans_sph_snap
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_geometry_parameter
      use m_node_phys_data
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_vector_from_trans                                       &
     &   (nnod_rtp, m_folding, inod_rtp_smp_stack,                      &
     &    numnod, frs_rtp(1,i_trns), d_nod(1,i_field) )
!
      end subroutine copy_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      end module copy_snap_4_sph_trans
