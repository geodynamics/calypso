!> @file  pole_energy_flux_sph.f90
!!      module pole_energy_flux_sph
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Evaluate nonlinear terms at poles
!!
!!@verbatim
!!      subroutine pole_nonlinear_sph_MHD
!!      subroutine pole_energy_flux_rtp
!!@endverbatim
!
      module pole_energy_flux_sph
!
      use m_precision
      use m_constants
!
      implicit  none
!
      private :: pole_sph_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine pole_nonlinear_sph_MHD
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_physical_property
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use products_at_poles
!
!
      if( (iphys%i_m_advect*iflag_t_evo_4_velo) .gt. 0) then
        call pole_fld_cst_cross_prod                                    &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_velo, nod_fld1%ntot_phys,       &
     &      iphys%i_vort, iphys%i_velo, iphys%i_m_advect,               &
     &      nod_fld1%d_fld)
      end if
!
      if( (iphys%i_lorentz*iflag_4_lorentz) .gt. 0) then
        call pole_fld_cst_cross_prod                                    &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_lor, nod_fld1%ntot_phys,        &
     &      iphys%i_current, iphys%i_velo, iphys%i_lorentz,             &
     &      nod_fld1%d_fld)
      end if
!
!
      if( (iphys%i_vp_induct*iflag_t_evo_4_magne) .gt. 0) then
        call pole_fld_cst_cross_prod                                    &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_induct, nod_fld1%ntot_phys,     &
     &      iphys%i_velo, iphys%i_velo, iphys%i_vp_induct,              &
     &      nod_fld1%d_fld)
      end if
!
!
      if( (iphys%i_h_flux*iflag_t_evo_4_temp) .gt. 0) then
        call pole_fld_cst_vec_scalar_prod                               &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_induct, nod_fld1%ntot_phys,     &
     &      iphys%i_velo, iphys%i_temp, iphys%i_h_flux, nod_fld1%d_fld)
      end if
!
      if( (iphys%i_c_flux*iflag_t_evo_4_composit) .gt. 0) then
        call pole_fld_cst_vec_scalar_prod                               &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_induct, nod_fld1%ntot_phys,     &
     &      iphys%i_velo, iphys%i_light, iphys%i_c_flux,                &
     &      nod_fld1%d_fld)
      end if
!
      end subroutine pole_nonlinear_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine pole_energy_flux_rtp
!
      use m_control_parameter
      use m_machine_parameter
      use m_spheric_parameter
      use m_physical_property
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use products_at_poles
      use pole_poynting_flux_smp
!
!
!$omp parallel
      if( (iphys%i_lorentz*iphys%i_ujb) .gt. 0) then
        call pole_fld_cst_dot_prod                                      &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), one, nod_fld1%ntot_phys,             &
     &      iphys%i_lorentz, iphys%i_velo, iphys%i_ujb, nod_fld1%d_fld)
      end if
!
      if( (iphys%i_lorentz*iphys%i_nega_ujb) .gt. 0) then
        call pole_fld_cst_dot_prod                                      &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), dminus, nod_fld1%ntot_phys,          &
     &      iphys%i_lorentz, iphys%i_velo, iphys%i_nega_ujb,            &
     &      nod_fld1%d_fld)
      end if
!
      if( (iphys%i_induction*iphys%i_me_gen) .gt. 0) then
        call pole_fld_cst_dot_prod                                      &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), one, nod_fld1%ntot_phys,             &
     &      iphys%i_induction, iphys%i_magne, iphys%i_me_gen,           &
     &      nod_fld1%d_fld)
      end if
!
!
      if((iphys%i_current*iphys%i_vp_induct*iphys%i_electric) .gt. 0)   &
     &     then
        call cal_pole_electric_field_smp                                &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_d_magne, nod_fld1%ntot_phys,    &
     &      iphys%i_current, iphys%i_vp_induct, iphys%i_electric,       &
     &      nod_fld1%d_fld)
      end if
!
      if((iphys%i_current*iphys%i_vp_induct*iphys%i_poynting) .gt. 0)   &
     &     then
        call cal_pole_poynting_flux_smp                                 &
     &     (node1%numnod, node1%internal_node, node1%xx,                &
     &      nnod_rtp, nidx_rtp(1), coef_d_magne, nod_fld1%ntot_phys,    &
     &      iphys%i_current, iphys%i_vp_induct, iphys%i_magne,          &
     &      iphys%i_poynting, nod_fld1%d_fld)
      end if
!
!
      if(iphys%i_buo_gen .gt. 0) then
        if(iflag_4_ref_temp .eq. id_sphere_ref_temp) then
          call pole_sph_buoyancy_flux                                   &
     &       (node1%numnod, node1%internal_node, node1%xx,              &
     &        nnod_rtp, nidx_rtp(1), coef_buo, nod_fld1%ntot_phys,      &
     &        iphys%i_par_temp, iphys%i_velo, iphys%i_buo_gen,          &
     &        nod_fld1%d_fld)
        else
          call pole_sph_buoyancy_flux                                   &
     &       (node1%numnod, node1%internal_node, node1%xx,              &
     &        nnod_rtp, nidx_rtp(1), coef_buo, nod_fld1%ntot_phys,      &
     &        iphys%i_temp ,iphys%i_velo, iphys%i_buo_gen,              &
     &        nod_fld1%d_fld)
        end if
      end if
!
      if(iphys%i_c_buo_gen .gt. 0) then
        call pole_sph_buoyancy_flux                                     &
     &       (node1%numnod, node1%internal_node, node1%xx,              &
     &        nnod_rtp, nidx_rtp(1), coef_comp_buo, nod_fld1%ntot_phys, &
     &        iphys%i_light, iphys%i_velo, iphys%i_c_buo_gen,           &
     &        nod_fld1%d_fld)
      end if
!
      if(iphys%i_f_buo_gen .gt. 0) then
        call pole_sph_buoyancy_flux                                     &
     &       (node1%numnod, node1%internal_node, node1%xx,              &
     &        nnod_rtp, nidx_rtp(1), coef_buo, nod_fld1%ntot_phys,      &
     &        iphys%i_filter_temp, iphys%i_velo, iphys%i_f_buo_gen,     &
     &        nod_fld1%d_fld)
      end if
!$omp end parallel
!
      end subroutine pole_energy_flux_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pole_sph_buoyancy_flux                                 &
     &        (numnod, internal_node, xx, nnod_rtp, nidx_rtp_r, coef,   &
     &         ncomp_nod, i_temp, i_velo, i_buo_flux, d_nod)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      integer(kind = kint), intent(in) :: ncomp_nod, i_buo_flux
      integer(kind = kint), intent(in) :: i_temp, i_velo
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(inout) :: d_nod(numnod, ncomp_nod)
!
      integer(kind = kint) :: inod, kr
!
!
      inod = nnod_rtp
      if(inod .ge. internal_node) return
!
!  copy field for north pole
      if(xx(inod+1,3) .gt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_nod(inod,i_buo_flux)                                        &
     &       = coef*d_nod(inod,i_temp)*d_nod(inod,i_velo)*xx(inod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_nod(inod,i_buo_flux)                                        &
     &       = - coef*d_nod(inod,i_temp)*d_nod(inod,i_velo)*xx(inod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_nod(inod,i_buo_flux) = zero
!
      end subroutine pole_sph_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      end module pole_energy_flux_sph
