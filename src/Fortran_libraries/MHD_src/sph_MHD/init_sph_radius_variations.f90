!>@file   init_sph_radius_variations.f90
!!@brief  module init_sph_radius_variations
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Set radial variations for densityuy and diffusivities
!!
!!@verbatim
!!      subroutine init_radius_variations_sph_mhd(sph, r_2nd,           &
!!     &                                          MHD_prop, refs)
!!        type(sph_grids), intent(in) :: sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(radial_reference_field), intent(inout) :: refs
!!@endverbatim
!!
!
      module init_sph_radius_variations
!
      use m_precision
      use m_constants
      use t_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_sph_radial_interpolate
      use t_field_data_IO
      use t_fdm_coefs
      use t_radial_reference_field
!
      implicit none
!
      private :: check_r_variation_data_list
      private :: r_diffusivity_w_ICB_reduction
      private :: sel_radius_scalar_diffusivity
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine init_radius_variations_sph_mhd(sph, r_2nd,             &
     &                                          MHD_prop, refs)
!
      use m_base_field_labels
      use m_diffusion_term_labels
      use transfer_to_long_integers
      use set_sph_radial_variations
      use radial_reference_field_IO
      use calypso_mpi_real
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd

      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(radial_reference_field), intent(inout) :: refs
!
      character(len=kchara), parameter :: radius_name = 'radius'
      type(sph_radial_interpolate) :: r_itp
      type(field_IO) :: fld_IO
!
      integer(kind = kint) :: k
!
!
      if(iflag_debug .gt. 0) then
        call check_r_variation_data_list(MHD_prop, refs%ref_field)
      end if
!
      if(my_rank .eq. 0) then
        call set_sph_radial_density(my_rank, radius_name,               &
     &      density%name, MHD_prop%fl_prop%ir_rho,                      &
     &      sph%sph_rj, r_2nd, MHD_prop%polytrope_param,                &
     &      refs%ref_field, r_itp, fld_IO)
!
        if(MHD_prop%val_viscous_param%iflag_radial_diffusion            &
     &      .eq. iflag_constant) then
          call copy_const_diffusivity_to_ref                            &
     &       (MHD_prop%fl_prop%ir_nu, MHD_prop%fl_prop%ir_dnu_norm,     &
     &        refs%ref_field)
        else
          call set_sph_radial_diffusivity(my_rank, radius_name,         &
     &        kinetic_viscosity%name, MHD_prop%fl_prop%ir_nu,           &
     &        sph%sph_rj, r_2nd, MHD_prop%val_viscous_param,            &
     &        refs%ref_field, r_itp, fld_IO)
        end if
!
        if(MHD_prop%val_mag_diffuse_param%iflag_radial_diffusion        &
     &      .eq. iflag_constant) then
          call copy_const_diffusivity_to_ref                            &
     &       (MHD_prop%cd_prop%ir_eta, MHD_prop%cd_prop%ir_deta_norm,   &
     &        refs%ref_field)
        else
          call set_sph_radial_diffusivity(my_rank, radius_name,         &
     &        magnetic_diffusivity%name, MHD_prop%cd_prop%ir_eta,       &
     &        sph%sph_rj, r_2nd, MHD_prop%val_mag_diffuse_param,        &
     &        refs%ref_field, r_itp, fld_IO)
        end if
!
        call sel_radius_scalar_diffusivity                              &
     &     (sph, r_2nd, thermal_diffusivity%name,                       &
     &      MHD_prop%ht_prop, MHD_prop%val_thermal_diffuse_param, refs)
!
        call sel_radius_scalar_diffusivity                              &
     &     (sph, r_2nd, chemical_diffusivity%name,                      &
     &      MHD_prop%cp_prop, MHD_prop%val_comp_diffuse_param, refs)
      end if
!
      do k = 1, refs%ref_field%ntot_phys
        call calypso_mpi_bcast_real(refs%ref_field%d_fld(1,k),          &
     &      cast_long(refs%ref_field%n_point), 0)
      end do
!
      end subroutine init_radius_variations_sph_mhd
!
!  -------------------------------------------------------------------
!
      subroutine sel_radius_scalar_diffusivity(sph, r_2nd,             &
     &          diffusivity_name, sc_prop, v_diffuse_param, refs)
!
      use calypso_mpi
      use set_sph_radial_variations
      use radial_reference_field_IO
!
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      character(len=kchara), intent(in) :: diffusivity_name
!
      type(scalar_property), intent(inout) :: sc_prop
      type(val_diffuse_parameters), intent(inout) :: v_diffuse_param
      type(radial_reference_field), intent(inout) :: refs
!
      character(len=kchara), parameter :: radius_name = 'radius'
      type(sph_radial_interpolate) :: r_itp
      type(field_IO) :: fld_IO
!
!
      if(v_diffuse_param%iflag_radial_diffusion                         &
     &                          .eq. iflag_constant) then
        call copy_const_diffusivity_to_ref(sc_prop%ir_kappa,            &
     &      sc_prop%ir_dkappa_norm, refs%ref_field)
      else if(v_diffuse_param%iflag_radial_diffusion                    &
     &                          .eq. iflag_ICB_reduction) then
        call r_diffusivity_w_ICB_reduction                              &
     &     (sph%sph_params, refs%iref_radius, sc_prop, refs%ref_field)
      else
        call set_sph_radial_diffusivity(my_rank, radius_name,           &
     &      diffusivity_name, sc_prop%ir_kappa,                         &
     &      sph%sph_rj, r_2nd, v_diffuse_param,                         &
     &      refs%ref_field, r_itp, fld_IO)
      end if
!
      end subroutine sel_radius_scalar_diffusivity
!
!  -------------------------------------------------------------------
!
      subroutine r_diffusivity_w_ICB_reduction(sph_params, iref_radius, &
     &                                         scl_prop, ref_field)
!
      use radial_interpolation
!
      type(sph_shell_parameters), intent(in) :: sph_params
      integer(kind = kint), intent(in) :: iref_radius
!
      type(scalar_property), intent(inout) :: scl_prop
      type(phys_data), intent(inout) :: ref_field
!
      real(kind = kreal) :: range_ICB(3)
      integer(kind = kint) :: kr_reduce_inner
      integer(kind = kint) :: kr_reduce_outer
      integer(kind = kint) :: k_reduce_old2new_in(3)
      integer(kind = kint) :: k_reduce_old2new_out(3)
      real(kind = kreal) :: coef_reduce_old2new_in(3)
      real(kind = kreal) :: ratio
!
      integer(kind = kint) :: kr
!
!
      if(scl_prop%diffuse_reduction_radius_ICB .le. zero)               &
     &  scl_prop%diffuse_reduction_radius_ICB = sph_params%radius_ICB
!
        range_ICB(1) = scl_prop%diffuse_reduction_radius_ICB            &
     &                - scl_prop%diffuse_reduction_width_ICB
        range_ICB(2) = scl_prop%diffuse_reduction_radius_ICB
        range_ICB(3) = scl_prop%diffuse_reduction_radius_ICB            &
     &                + scl_prop%diffuse_reduction_width_ICB
!      write(*,*) 'ref_field%n_point', ref_field%n_point
!      write(*,*) 'ref_field%d_fld', size(ref_field%d_fld)
!      write(*,*) 'iref_radius', iref_radius
!      write(*,*) 'iref_diffusivity', scl_prop%ir_kappa,                &
!     &                               scl_prop%ir_dkappa_norm
!
      call cal_radial_interpolation_coef                                &
     &   (ref_field%n_point, ref_field%d_fld(1,iref_radius),            &
     &    ithree, range_ICB, kr_reduce_inner, kr_reduce_outer,          &
     &    k_reduce_old2new_in, k_reduce_old2new_out,                    &
     &    coef_reduce_old2new_in)
!
      if((scl_prop%ir_kappa * scl_prop%ir_dkappa_norm) .eq. 0) return
!
!        write(*,*) 'range_ICB',  range_ICB(1:3)
!        write(*,*) 'kr_reduce_inner',  kr_reduce_inner
!        write(*,*) 'kr_reduce_outer',  kr_reduce_outer
!        write(*,*) 'k_reduce_old2new_in',  k_reduce_old2new_in(1:3)
!        write(*,*) 'k_reduce_old2new_out',  k_reduce_old2new_out(1:3)
!        write(*,*) 'coef_reduce_old2new_in',coef_reduce_old2new_in(1:3)
!
!$omp parallel do
        do kr = 1, ref_field%n_point
          ref_field%d_fld(kr,scl_prop%ir_kappa) =       one
          ref_field%d_fld(kr,scl_prop%ir_dkappa_norm) = zero
        end do
!$omp end parallel do
!
!$omp parallel do private(ratio)
        do kr = k_reduce_old2new_in(1)+1, k_reduce_old2new_in(2)-1
          ratio = one - scl_prop%grad_diffusibity_ICB                   &
     &           * (ref_field%d_fld(kr,iref_radius) - range_ICB(1))
          ref_field%d_fld(kr,scl_prop%ir_kappa)                         &
     &           = ratio * ref_field%d_fld(kr,scl_prop%ir_kappa)
          ref_field%d_fld(kr,scl_prop%ir_dkappa_norm)                   &
     &           = - scl_prop%grad_diffusibity_ICB
        end do
!$omp end parallel do
!
        kr = k_reduce_old2new_in(2)
        ref_field%d_fld(kr,scl_prop%ir_kappa)                           &
     &     = scl_prop%diffuse_reduction_ratio_ICB                       &
     &      * ref_field%d_fld(kr,scl_prop%ir_kappa)
        ref_field%d_fld(kr,scl_prop%ir_dkappa_norm) =  zero
!
!$omp parallel do private(ratio)
        do kr = k_reduce_old2new_in(2)+1, k_reduce_old2new_in(3)
          ratio = one - scl_prop%grad_diffusibity_ICB                   &
     &           * (range_ICB(3) - ref_field%d_fld(kr,iref_radius))
          ref_field%d_fld(kr,scl_prop%ir_kappa)                         &
     &           = ratio * ref_field%d_fld(kr,scl_prop%ir_kappa)
          ref_field%d_fld(kr,scl_prop%ir_dkappa_norm)                   &
     &           =  scl_prop%grad_diffusibity_ICB
        end do
!$omp end parallel do
!
!      do kr = 1, ref_field%n_point
!        write(*,*) kr, ref_field%d_fld(kr,iref_radius),                &
!     &                 ref_field%d_fld(kr,scl_prop%ir_kappa),          &
!     &                 ref_field%d_fld(kr,scl_prop%ir_dkappa_norm)
!      end do
!
      end subroutine r_diffusivity_w_ICB_reduction
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine check_r_variation_data_list(MHD_prop, ref_field)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(phys_data), intent(in) :: ref_field
      integer(kind = kint) :: icou_ref
!
      write(*,*) 'ir_rho', MHD_prop%fl_prop%ir_rho
      write(*,*) 'ir_drho_norm',  MHD_prop%fl_prop%ir_drho_norm
      write(*,*) 'ir_d2rho_norm', MHD_prop%fl_prop%ir_d2rho_norm
!
      write(*,*) 'ir_nu', MHD_prop%fl_prop%ir_nu
      write(*,*) 'ir_dnu_norm', MHD_prop%fl_prop%ir_dnu_norm
      write(*,*) 'ir_eta', MHD_prop%cd_prop%ir_eta
      write(*,*) 'ir_deta_norm', MHD_prop%cd_prop%ir_deta_norm
      write(*,*) 'ir_kappa', MHD_prop%ht_prop%ir_kappa
      write(*,*) 'ir_dkappa_norm', MHD_prop%ht_prop%ir_dkappa_norm
      write(*,*) 'ir_kappa', MHD_prop%cp_prop%ir_kappa
      write(*,*) 'ir_dkappa_norm', MHD_prop%cp_prop%ir_dkappa_norm
      do icou_ref = 1, ref_field%num_phys
        write(*,*) icou_ref, trim(ref_field%phys_name(icou_ref))
      end do
!
      end subroutine check_r_variation_data_list
!
! -----------------------------------------------------------------------
!
      end module init_sph_radius_variations
