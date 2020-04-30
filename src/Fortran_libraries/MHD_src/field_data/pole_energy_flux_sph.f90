!> @file  pole_energy_flux_sph.f90
!!      module pole_energy_flux_sph
!!
!! @author  H. Matsui
!! @date Programmed in Oct., 2012
!
!> @brief Evaluate nonlinear terms at poles
!!
!!@verbatim
!!      subroutine pole_nonlinear_sph_MHD(sph_rtp, node,                &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop, iphys, nod_fld)
!!      subroutine pole_energy_flux_rtp(sph_rtp, node,                  &
!!     &          fl_prop, cd_prop, ref_param_T, ref_param_C,           &
!!     &         iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!
!!      subroutine pole_sph_buoyancy_flux                               &
!!     &        (numnod, internal_node, xx, nnod_rtp, nidx_rtp_r, coef, &
!!     &         ncomp_nod, i_temp, i_velo, i_buo_flux, d_nod)
!!@endverbatim
!
      module pole_energy_flux_sph
!
      use m_precision
      use m_constants
!
      use t_physical_property
      use t_spheric_rtp_data
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine pole_nonlinear_sph_MHD(sph_rtp, node,                  &
     &          fl_prop, cd_prop, ht_prop, cp_prop, iphys, nod_fld)
!
      use m_machine_parameter
!
      use products_at_poles
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (iphys%forces%i_m_advect * fl_prop%iflag_scheme) .gt. 0) then
        call pole_fld_cst_cross_prod                                    &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), fl_prop%coef_velo,   &
     &      nod_fld%ntot_phys, iphys%base%i_vort, iphys%base%i_velo,    &
     &      iphys%forces%i_m_advect, nod_fld%d_fld)
      end if
!
      if( (iphys%forces%i_lorentz * fl_prop%iflag_4_lorentz)            &
     &                                                .gt. 0) then
        call pole_fld_cst_cross_prod                                    &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), fl_prop%coef_lor,    &
     &      nod_fld%ntot_phys, iphys%base%i_current, iphys%base%i_velo, &
     &      iphys%forces%i_lorentz, nod_fld%d_fld)
      end if
!
!
      if( (iphys%forces%i_vp_induct * cd_prop%iflag_Bevo_scheme)        &
     &                                                    .gt. 0) then
        call pole_fld_cst_cross_prod                                    &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), cd_prop%coef_induct, &
     &      nod_fld%ntot_phys, iphys%base%i_velo, iphys%base%i_velo,    &
     &      iphys%forces%i_vp_induct, nod_fld%d_fld)
      end if
!
!
      if( (iphys%forces%i_h_flux * ht_prop%iflag_scheme) .gt. 0) then
        call pole_fld_cst_vec_scalar_prod                               &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), cd_prop%coef_induct, &
     &      nod_fld%ntot_phys, iphys%base%i_velo, iphys%base%i_temp,    &
     &      iphys%forces%i_h_flux, nod_fld%d_fld)
      end if
!
      if( (iphys%forces%i_c_flux * cp_prop%iflag_scheme) .gt. 0) then
        call pole_fld_cst_vec_scalar_prod                               &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), cd_prop%coef_induct, &
     &      nod_fld%ntot_phys, iphys%base%i_velo, iphys%base%i_light,   &
     &      iphys%forces%i_c_flux, nod_fld%d_fld)
      end if
!
      end subroutine pole_nonlinear_sph_MHD
!
!-----------------------------------------------------------------------
!
      subroutine pole_energy_flux_rtp(sph_rtp, node,                    &
     &          fl_prop, cd_prop, ref_param_T, ref_param_C,             &
     &         iphys, nod_fld)
!
      use m_machine_parameter
      use t_reference_scalar_param
!
      use products_at_poles
      use pole_poynting_flux_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!$omp parallel
      if(iphys%ene_flux%i_ujb .gt. 0) then
        call pole_fld_cst_dot_prod                                      &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), one,                 &
     &      nod_fld%ntot_phys, iphys%forces%i_lorentz,                  &
     &      iphys%base%i_velo, iphys%ene_flux%i_ujb, nod_fld%d_fld)
      end if
!
      if(iphys%ene_flux%i_nega_ujb .gt. 0) then
        call pole_fld_cst_dot_prod                                      &
     &    (node%numnod, node%internal_node, node%xx,                    &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), dminus,               &
     &     nod_fld%ntot_phys, iphys%forces%i_lorentz,                   &
     &     iphys%base%i_velo, iphys%ene_flux%i_nega_ujb, nod_fld%d_fld)
      end if
!
      if(iphys%ene_flux%i_me_gen .gt. 0) then
        call pole_fld_cst_dot_prod                                      &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), one,                 &
     &      nod_fld%ntot_phys, iphys%forces%i_induction,                &
     &      iphys%base%i_magne, iphys%ene_flux%i_me_gen, nod_fld%d_fld)
      end if
!
!
      if(iphys%prod_fld%i_electric .gt. 0) then
        call cal_pole_electric_field_smp                                &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                      &
     &      cd_prop%coef_diffuse, nod_fld%ntot_phys,                    &
     &      iphys%base%i_current, iphys%forces%i_vp_induct,             &
     &      iphys%prod_fld%i_electric, nod_fld%d_fld)
      end if
!
      if(iphys%prod_fld%i_poynting .gt. 0) then
        call cal_pole_poynting_flux_smp                                 &
     &     (node%numnod, node%internal_node, node%xx,                   &
     &      sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                      &
     &      cd_prop%coef_diffuse, nod_fld%ntot_phys,                    &
     &      iphys%base%i_current, iphys%forces%i_vp_induct,             &
     &      iphys%base%i_magne, iphys%prod_fld%i_poynting,              &
     &      nod_fld%d_fld)
      end if
!
!
      if(iphys%ene_flux%i_buo_gen .gt. 0) then
        if    (ref_param_T%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_T%iflag_reference .eq. id_takepiro_temp) then
          call pole_sph_buoyancy_flux                                   &
     &       (node%numnod, node%internal_node, node%xx,                 &
     &        sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), fl_prop%coef_buo,  &
     &        nod_fld%ntot_phys, iphys%base%i_per_temp,                 &
     &        iphys%base%i_velo, iphys%ene_flux%i_buo_gen,              &
     &        nod_fld%d_fld)
        else
          call pole_sph_buoyancy_flux                                   &
     &       (node%numnod, node%internal_node, node%xx,                 &
     &        sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), fl_prop%coef_buo,  &
     &        nod_fld%ntot_phys, iphys%base%i_temp ,iphys%base%i_velo,  &
     &        iphys%ene_flux%i_buo_gen, nod_fld%d_fld)
        end if
      end if
!
      if(iphys%ene_flux%i_c_buo_gen .gt. 0) then
        if    (ref_param_C%iflag_reference .eq. id_sphere_ref_temp      &
     &    .or. ref_param_C%iflag_reference .eq. id_takepiro_temp) then
          call pole_sph_buoyancy_flux                                   &
     &       (node%numnod, node%internal_node, node%xx,                 &
     &        sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                    &
     &        fl_prop%coef_comp_buo, nod_fld%ntot_phys,                 &
     &        iphys%base%i_per_light, iphys%base%i_velo,                &
     &        iphys%ene_flux%i_c_buo_gen, nod_fld%d_fld)
        else
          call pole_sph_buoyancy_flux                                   &
     &       (node%numnod, node%internal_node, node%xx,                 &
     &        sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1),                    &
     &        fl_prop%coef_comp_buo, nod_fld%ntot_phys,                 &
     &        iphys%base%i_light, iphys%base%i_velo,                    &
     &        iphys%ene_flux%i_c_buo_gen, nod_fld%d_fld)
        end if
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
