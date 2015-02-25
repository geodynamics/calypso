!>@file   copy_MHD_4_sph_trans.f90
!!@brief  module copy_MHD_4_sph_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine select_mhd_field_from_trans
!!@endverbatim
!
      module copy_MHD_4_sph_trans
!
      use m_precision
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
!
      implicit  none
!
      private :: sel_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine select_mhd_field_from_trans
!
      use m_node_phys_address
      use m_addresses_trans_sph_MHD
!
!
!$omp parallel
!   advection flag
      call sel_force_from_MHD_trans(f_trns%i_m_advect)
!   Coriolis flag
      call sel_force_from_MHD_trans(f_trns%i_coriolis)
!   Lorentz flag
      call sel_force_from_MHD_trans(f_trns%i_lorentz)
!
!   induction flag
      call sel_force_from_MHD_trans(f_trns%i_vp_induct)
!   divergence of heat flux flag
      call sel_force_from_MHD_trans(f_trns%i_h_flux)
!
!   divergence of composition flux flag
      call sel_force_from_MHD_trans(f_trns%i_c_flux)
!$omp end parallel
!
      end subroutine select_mhd_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_forces_to_snapshot_rtp
!
      use m_node_phys_address
      use m_addresses_trans_sph_MHD
!
!
!$omp parallel
!   advection flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_m_advect, iphys%i_m_advect)
!   Coriolis flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_coriolis, iphys%i_coriolis)
!   Lorentz flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_lorentz, iphys%i_lorentz)
!
!   induction flag
      call copy_force_from_MHD_trans                                    &
     &   (f_trns%i_vp_induct, iphys%i_vp_induct)
!   divergence of heat flux flag
      call copy_force_from_MHD_trans(f_trns%i_h_flux, iphys%i_h_flux)
!
!   divergence of composition flux flag
      call copy_force_from_MHD_trans(f_trns%i_c_flux, iphys%i_c_flux)
!$omp end parallel
!
      end subroutine copy_forces_to_snapshot_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_force_from_MHD_trans(i_trns)
!
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_geometry_parameter
      use sel_fld_copy_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_trns
!
!
      if(i_trns .le. 0) return
      call sel_vector_from_trans                                        &
     &   (nnod_rtp, frc_rtp(1,i_trns), frm_rtp(1,i_trns) )
!
      end subroutine sel_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_force_from_MHD_trans(i_trns, i_field)
!
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_geometry_parameter
      use m_node_phys_data
      use m_spheric_parameter
      use m_spheric_param_smp
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_vector_from_trans(nnod_rtp, inod_rtp_smp_stack,         &
     &   numnod, frm_rtp(1,i_trns), d_nod(1,i_field) )
!
      end subroutine copy_force_from_MHD_trans
!
!-----------------------------------------------------------------------
!
      end module copy_MHD_4_sph_trans
