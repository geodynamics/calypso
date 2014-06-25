!> @file  copy_MHD_4_pole_trans.f90
!!      module copy_MHD_4_pole_trans
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief copy spectr data for spherical transform at poles
!!
!!@verbatim
!!      subroutine copy_mhd_vec_from_pole_trans
!!      subroutine copy_mhd_scl_from_pole_trans
!!
!!      subroutine copy_snap_vec_from_pole_trans
!!      subroutine copy_snap_scl_from_pole_trans
!!@endverbatim
!
      module copy_MHD_4_pole_trans
!
      use m_precision
      use m_constants
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_mhd_vec_from_pole_trans
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_addresses_trans_sph_MHD
!
      use copy_pole_field_sph_trans
!
!
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_rj_2_rtp, iphys%i_velo,               &
     &    b_trns%i_velo, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_rj_2_rtp, iphys%i_vort,               &
     &    b_trns%i_vort, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_rj_2_rtp, iphys%i_magne,              &
     &    b_trns%i_magne, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_rj_2_rtp, iphys%i_current,            &
     &    b_trns%i_current, d_nod)
!
      end subroutine copy_mhd_vec_from_pole_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_mhd_scl_from_pole_trans
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_addresses_trans_sph_MHD
!
      use copy_pole_field_sph_trans
!
!
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_rj_2_rtp, iphys%i_temp,               &
     &    b_trns%i_temp, d_nod)
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_rj_2_rtp, iphys%i_light,              &
     &    b_trns%i_light, d_nod)
!
      end subroutine copy_mhd_scl_from_pole_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_from_pole_trans
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_addresses_trans_sph_snap
!
      use copy_pole_field_sph_trans
!
!
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_velo,          &
     &    bsnap_trns%i_velo, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_vort,          &
     &    bsnap_trns%i_vort, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_magne,         &
     &    bsnap_trns%i_magne, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_current,       &
     &    bsnap_trns%i_current, d_nod)
!
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_v_diffuse,     &
     &    bsnap_trns%i_v_diffuse, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_w_diffuse,     &
     &    bsnap_trns%i_w_diffuse, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_vp_diffuse,    &
     &    bsnap_trns%i_vp_diffuse, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_b_diffuse,     &
     &    bsnap_trns%i_b_diffuse, d_nod)
!
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_induction,     &
     &    bsnap_trns%i_induction, d_nod)
!
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_grad_t,        &
     &    bsnap_trns%i_grad_t, d_nod)
      call copy_pole_vec_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp,                        &
     &    iphys%i_grad_composit, bsnap_trns%i_grad_composit, d_nod)
!
      end subroutine copy_snap_vec_from_pole_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_scl_from_pole_trans
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use m_addresses_trans_sph_snap
!
      use copy_pole_field_sph_trans
!
!
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_temp,          &
     &    bsnap_trns%i_temp, d_nod)
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_light,         &
     &    bsnap_trns%i_light, d_nod)
!
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_press,         &
     &    bsnap_trns%i_press, d_nod)
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_par_temp,      &
     &    bsnap_trns%i_par_temp, d_nod)
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_t_diffuse,     &
     &    bsnap_trns%i_t_diffuse, d_nod)
      call copy_pole_scl_fld_from_trans(numnod, internal_node, xx,      &
     &    num_tot_nod_phys, ncomp_snap_rj_2_rtp, iphys%i_c_diffuse,     &
     &    bsnap_trns%i_c_diffuse, d_nod)
!
      end subroutine copy_snap_scl_from_pole_trans
!
! -----------------------------------------------------------------------
!
      end module copy_MHD_4_pole_trans
