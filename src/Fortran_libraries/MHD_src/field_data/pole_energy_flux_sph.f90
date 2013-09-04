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
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use products_at_poles
!
!
      if( (iphys%i_m_advect*iflag_t_evo_4_velo) .gt. 0) then
        call pole_sph_cross_prod_w_const(numnod, internal_node, xx,     &
     &      nnod_rtp, nidx_rtp(1), coef_velo, d_nod(1,iphys%i_vort),    &
     &      d_nod(1,iphys%i_velo), d_nod(1,iphys%i_m_advect))
      end if
!
      if( (iphys%i_lorentz*iflag_4_lorentz) .gt. 0) then
        call pole_sph_cross_prod_w_const(numnod, internal_node, xx,     &
     &      nnod_rtp, nidx_rtp(1), coef_lor, d_nod(1,iphys%i_current),  &
     &      d_nod(1,iphys%i_velo), d_nod(1,iphys%i_lorentz))
      end if
!
!
      if( (iphys%i_vp_induct*iflag_t_evo_4_magne) .gt. 0) then
        call pole_sph_cross_prod_w_const(numnod, internal_node, xx,     &
     &      nnod_rtp, nidx_rtp(1), coef_induct, d_nod(1,iphys%i_velo),  &
     &      d_nod(1,iphys%i_velo), d_nod(1,iphys%i_vp_induct))
      end if
!
!
      if( (iphys%i_h_flux*iflag_t_evo_4_temp) .gt. 0) then
        call pole_vec_scalar_prod_w_const(numnod, internal_node, xx,    &
     &      nnod_rtp, nidx_rtp(1), coef_induct, d_nod(1,iphys%i_velo),  &
     &      d_nod(1,iphys%i_temp), d_nod(1,iphys%i_h_flux))
      end if
!
      if( (iphys%i_c_flux*iflag_t_evo_4_composit) .gt. 0) then
        call pole_vec_scalar_prod_w_const(numnod, internal_node, xx,    &
     &      nnod_rtp, nidx_rtp(1), coef_induct, d_nod(1,iphys%i_velo),  &
     &      d_nod(1,iphys%i_light), d_nod(1,iphys%i_c_flux))
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
      use m_geometry_parameter
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_address
      use products_at_poles
      use pole_poynting_flux_smp
!
!
!$omp parallel
      if( (iphys%i_lorentz*iphys%i_ujb) .gt. 0) then
        call pole_sph_dot_prod_w_const(numnod, internal_node, xx,       &
     &      nnod_rtp, nidx_rtp(1), one, d_nod(1,iphys%i_lorentz),       &
     &      d_nod(1,iphys%i_velo), d_nod(1,iphys%i_ujb) )
      end if
!
      if( (iphys%i_lorentz*iphys%i_nega_ujb) .gt. 0) then
        call pole_sph_dot_prod_w_const(numnod, internal_node, xx,       &
     &      nnod_rtp, nidx_rtp(1), dminus, d_nod(1,iphys%i_lorentz),    &
     &      d_nod(1,iphys%i_velo), d_nod(1,iphys%i_nega_ujb) )
      end if
!
      if( (iphys%i_induction*iphys%i_me_gen) .gt. 0) then
        call pole_sph_dot_prod_w_const(numnod, internal_node, xx,       &
     &      nnod_rtp, nidx_rtp(1), one, d_nod(1,iphys%i_induction),     &
     &      d_nod(1,iphys%i_magne), d_nod(1,iphys%i_me_gen) )
      end if
!
!
      if((iphys%i_current*iphys%i_vp_induct*iphys%i_electric) .gt. 0)   &
     &     then
        call cal_pole_electric_field_smp(numnod, internal_node, xx,     &
     &      nnod_rtp, nidx_rtp(1), coef_d_magne,                        &
     &      d_nod(1,iphys%i_current), d_nod(1,iphys%i_vp_induct),       &
     &      d_nod(1,iphys%i_electric))
      end if
!
      if((iphys%i_current*iphys%i_vp_induct*iphys%i_poynting) .gt. 0)   &
     &     then
        call cal_pole_poynting_flux_smp(numnod, internal_node, xx,      &
     &      nnod_rtp, nidx_rtp(1), coef_d_magne,                        &
     &      d_nod(1,iphys%i_current), d_nod(1,iphys%i_vp_induct),       &
     &      d_nod(1,iphys%i_magne), d_nod(1,iphys%i_poynting))
      end if
!
!
      if(iphys%i_buo_gen .gt. 0) then
        if(iflag_4_ref_temp .eq. id_sphere_ref_temp) then
          call pole_sph_buoyancy_flux                                   &
     &       (numnod, internal_node, xx, nnod_rtp, nidx_rtp(1),         &
     &        coef_buo, d_nod(1,iphys%i_par_temp),                      &
     &        d_nod(1,iphys%i_velo), d_nod(1,iphys%i_buo_gen) )
        else
          call pole_sph_buoyancy_flux                                   &
     &       (numnod, internal_node, xx, nnod_rtp, nidx_rtp(1),         &
     &        coef_buo, d_nod(1,iphys%i_temp), d_nod(1,iphys%i_velo),   &
     &        d_nod(1,iphys%i_buo_gen) )
        end if
      end if
!
      if(iphys%i_c_buo_gen .gt. 0) then
        call pole_sph_buoyancy_flux                                     &
     &       (numnod, internal_node, xx, nnod_rtp, nidx_rtp(1),         &
     &        coef_comp_buo,  d_nod(1,iphys%i_light),                   &
     &        d_nod(1,iphys%i_velo), d_nod(1,iphys%i_c_buo_gen) )
      end if
!
      if(iphys%i_f_buo_gen .gt. 0) then
        call pole_sph_buoyancy_flux                                     &
     &       (numnod, internal_node, xx, nnod_rtp, nidx_rtp(1),         &
     &        coef_buo, d_nod(1,iphys%i_filter_temp),                   &
     &        d_nod(1,iphys%i_velo), d_nod(1,iphys%i_f_buo_gen) )
      end if
!$omp end parallel
!
      end subroutine pole_energy_flux_rtp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pole_sph_buoyancy_flux(numnod, internal_node, xx,      &
     &         nnod_rtp, nidx_rtp_r, coef, temp_nod, vr_nod, d_flux)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nidx_rtp_r
      integer(kind = kint), intent(in) :: nnod_rtp
!
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(in) :: temp_nod(numnod)
      real(kind = kreal), intent(in) :: vr_nod(numnod)
      real(kind = kreal), intent(inout) :: d_flux(numnod)
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
          d_flux(inod) =   coef*temp_nod(inod)*vr_nod(inod)*xx(inod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for south pole
      if(xx(inod+1,3) .lt. zero) then
        do kr = 1, nidx_rtp_r
          inod = inod + 1
          d_flux(inod) = - coef*temp_nod(inod)*vr_nod(inod)*xx(inod,3)
        end do
      end if
!
      if(inod .ge. internal_node) return
!
!  copy field for center
      inod = inod + 1
      d_flux(inod) = zero
!
      end subroutine pole_sph_buoyancy_flux
!
! -----------------------------------------------------------------------
!
      end module pole_energy_flux_sph
