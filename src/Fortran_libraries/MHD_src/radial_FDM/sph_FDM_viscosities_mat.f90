!>@file   sph_FDM_viscosities_mat.f90
!!@brief  module sph_FDM_viscosities_mat
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine set_sph_FDM_viscosity_mat(n_in, n_out, kr,           &
!!     &          sph_rj, fl_prop, radial_variation, g_sph_rj,          &
!!     &          coef_d, nri_fdm, fdm_d1_mat, fdm_d2_mat, mat_viscous)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        integer(kind = kint), intent(in) :: n_in, n_out
!!        integer(kind = kint), intent(in) :: kr, nri_fdm
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: fdm_d1_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: fdm_d2_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_viscous(sph_rj%nidx_rj(2),n_in:n_out)
!!      subroutine set_sph_FDM_hdiv_viscosity_mat(kr, n_in, n_out,      &
!!     &          sph_rj, fl_prop, radial_variation, g_sph_rj,          &
!!     &          coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,          &
!!     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_data), intent(in) :: radial_variation
!!        integer(kind = kint), intent(in) :: n_in, n_out
!!        integer(kind = kint), intent(in) :: kr, nri_fdm
!!        real(kind = kreal), intent(in)                                &
!!     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm3e_d3_mat(nri_fdm,n_in:n_out)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!!@endverbatim
!!
      module sph_FDM_viscosities_mat
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
      use t_physical_property
      use t_fdm_coefs
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_FDM_viscosity_mat(n_in, n_out, kr,             &
     &          sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &          coef_d, nri_fdm, fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      use cal_sph_FDM_viscosity_mat
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_d2_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(sph_rj%nidx_rj(2),n_in:n_out)
!
!
      call set_sph_FDM_fix_viscous_mat(n_in, n_out, kr,                 &
     &    sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(kr,2), g_sph_rj,           &
     &    sph_rj%nidx_rj(1), fdm_d2_mat, mat_viscous)
!
      call add_sph_FDM_val_viscous_mat(n_in, n_out, kr,                 &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(kr,1),                     &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_nu),                   &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),             &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),            &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),           &
     &    sph_rj%nidx_rj(1), fdm_d1_mat, mat_viscous)
!
      mat_viscous(1:sph_rj%nidx_rj(2),n_in:n_out)                       &
     &       = coef_d * mat_viscous(1:sph_rj%nidx_rj(2),n_in:n_out)
!
      end subroutine set_sph_FDM_viscosity_mat
!
!  -------------------------------------------------------------------
!
      subroutine each_sph_FDM_viscosity_mat(n_in, n_out, kr,            &
     &          sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &          coef_d, fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      use cal_sph_FDM_viscosity_mat
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm_d1_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm_d2_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(sph_rj%nidx_rj(2),n_in:n_out)
!
!
      call each_sph_FDM_fix_viscous_mat(n_in, n_out,                    &
     &    sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(kr,2), g_sph_rj,           &
     &    fdm_d2_mat, mat_viscous)
!
      call each_sph_FDM_val_viscous_mat(n_in, n_out,                    &
     &    fl_prop%flag_viscous_variation,                               &
     &    fl_prop%flag_ref_density_valiation,                           &
     &    sph_rj%nidx_rj(2), sph_rj%ar_1d_rj(kr,1),                     &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_nu),                   &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_dnu_norm),             &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_drho_norm),            &
     &    radial_variation%d_fld(kr+1,fl_prop%ir_d2rho_norm),           &
     &    fdm_d1_mat, mat_viscous)
!
      mat_viscous(1:sph_rj%nidx_rj(2),n_in:n_out)                       &
     &       = coef_d * mat_viscous(1:sph_rj%nidx_rj(2),n_in:n_out)
!
      end subroutine each_sph_FDM_viscosity_mat
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_FDM_hdiv_viscosity_mat(kr, n_in, n_out,        &
     &          sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &          coef_d, nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat,            &
     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d3_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: hdiv_visous_mat(sph_rj%nidx_rj(2),n_in:n_out)
!
!
      call set_sph_FDM_fix_hdiv_vscs_mat                                &
     &   (kr, n_in, n_out, sph_rj%nidx_rj(2),                           &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat,            &
     &    hdiv_visous_mat)
!
      if(fl_prop%flag_viscous_variation                                 &
     &    .or. fl_prop%flag_ref_density_valiation) then
        call add_sph_FDM_val_hdiv_vscs_mat                              &
     &     (kr, n_in, n_out, fl_prop%flag_viscous_variation,            &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), g_sph_rj,                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,          &
     &      hdiv_visous_mat)
      end if
!
      hdiv_visous_mat(1:sph_rj%nidx_rj(2),n_in:n_out)                   &
     &       = coef_d * hdiv_visous_mat(1:sph_rj%nidx_rj(2),n_in:n_out)
!
      end subroutine set_sph_FDM_hdiv_viscosity_mat
!
! -----------------------------------------------------------------------
!
      subroutine each_sph_FDM_hdiv_viscosity_mat(kr, n_in, n_out,       &
     &          sph_rj, fl_prop, radial_variation, g_sph_rj,            &
     &          coef_d, fdm3e_d0_mat, fdm3e_d1_mat,                     &
     &          fdm3e_d2_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fluid_property), intent(in) :: fl_prop
      type(phys_data), intent(in) :: radial_variation
!
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr
      real(kind = kreal), intent(in)                                    &
     &             :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: hdiv_visous_mat(sph_rj%nidx_rj(2),n_in:n_out)
!
!
      call each_sph_FDM_fix_hdiv_vscs_mat                               &
     &   (n_in, n_out, sph_rj%nidx_rj(2),                               &
     &    sph_rj%ar_ele_rj(kr,2), sph_rj%ar_ele_rj(kr,3), g_sph_rj,     &
     &    fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      if(fl_prop%flag_viscous_variation                                 &
     &    .or. fl_prop%flag_ref_density_valiation) then
        call each_sph_FDM_val_hdiv_vscs_mat                             &
     &     (n_in, n_out, fl_prop%flag_viscous_variation,                &
     &      fl_prop%flag_ref_density_valiation,                         &
     &      sph_rj%nidx_rj(2), sph_rj%ar_ele_rj(kr,1),                  &
     &      sph_rj%ar_ele_rj(kr,2), g_sph_rj,                           &
     &      radial_variation%d_fld(kr,fl_prop%ir_nu),                   &
     &      radial_variation%d_fld(kr,fl_prop%ir_dnu_norm),             &
     &      radial_variation%d_fld(kr,fl_prop%ir_drho_norm),            &
     &      radial_variation%d_fld(kr,fl_prop%ir_d2rho_norm),           &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
      end if
      write(*,*) 'hdiv_visous_mat', hdiv_visous_mat(1,:)
!
      hdiv_visous_mat(1:sph_rj%nidx_rj(2),n_in:n_out)                   &
     &       = coef_d * hdiv_visous_mat(1:sph_rj%nidx_rj(2),n_in:n_out)
!
      end subroutine each_sph_FDM_hdiv_viscosity_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_sph_FDM_val_hdiv_vscs_mat(kr, n_in, n_out,         &
     &         flag_viscous_variation, flag_ref_density_valiation,      &
     &         jmax, a1r_ele_rj, a2r_ele_rj, g_sph_rj,                  &
     &         relative_d, h_nu, h_rho, h_drhodr,                       &
     &         nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,       &
     &         hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: kr, nri_fdm
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
      real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d0_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d1_mat(nri_fdm,n_in:n_out)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm3e_d2_mat(nri_fdm,n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      real(kind = kreal) :: mat_tmp(n_in:n_out)
      real(kind = kreal) :: relative_de, h_nu_e
      real(kind = kreal) :: h_rho_e, h_drhodr_e
      integer(kind = kint) :: i_next
!
!
      relative_de = half * (relative_d(1) + relative_d(2))
      h_nu_e =      half * (h_nu(1) +     h_nu(2))
      h_rho_e =     half * (h_rho(1) +    h_rho(2))
      h_drhodr_e =  half * (h_drhodr(1) + h_drhodr(2))
!
      if(flag_ref_density_valiation) then
        call add_sph_hdiv_viscous_rho_depend(kr, n_in, n_out, jmax,     &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho_e, h_drhodr_e,      &
     &      nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,          &
     &      hdiv_visous_mat)
!
        if(flag_viscous_variation) then
          mat_tmp(n_in:n_out)                                           &
     &       = h_nu_e * h_rho_e * fdm3e_d1_mat(kr,n_in:n_out)
          do i_next = n_in, n_out
            hdiv_visous_mat(1:jmax,i_next)                              &
     &        = hdiv_visous_mat(1:jmax,i_next) + mat_tmp(i_next)
          end do
        end if
      end if
      if(flag_viscous_variation) then
        call add_sph_hdiv_viscous_nu_depend(kr, n_in, n_out, jmax,      &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_de, h_nu_e,      &
     &      nri_fdm, fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,          &
     &      hdiv_visous_mat)
      end if
!
      end subroutine add_sph_FDM_val_hdiv_vscs_mat
!
! -----------------------------------------------------------------------
!
      subroutine each_sph_FDM_val_hdiv_vscs_mat(n_in, n_out,            &
     &         flag_viscous_variation, flag_ref_density_valiation,      &
     &         jmax, a1r_ele_rj, a2r_ele_rj, g_sph_rj,                  &
     &         relative_d, h_nu, h_rho, h_drhodr,                       &
     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,                &
     &         hdiv_visous_mat)
!
      use cal_sph_FDM3e_hdiv_viscous
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_in, n_out
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj
      real(kind = kreal), intent(in) :: a2r_ele_rj
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: relative_d(2), h_nu(2)
      real(kind = kreal), intent(in) :: h_rho(2), h_drhodr(2)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(n_in:n_out)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(n_in:n_out)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(jmax,n_in:n_out)
!
      real(kind = kreal) :: mat_tmp(n_in:n_out)
      real(kind = kreal) :: relative_de, h_nu_e
      real(kind = kreal) :: h_rho_e, h_drhodr_e
      integer(kind = kint) :: i_next
!
!
      relative_de = half * (relative_d(1) + relative_d(2))
      h_nu_e =      half * (h_nu(1) +     h_nu(2))
      h_rho_e =     half * (h_rho(1) +    h_rho(2))
      h_drhodr_e =  half * (h_drhodr(1) + h_drhodr(2))
!
      if(flag_ref_density_valiation) then
        call each_sph_hdiv_vscs_rho_depend(n_in, n_out, jmax,           &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho_e, h_drhodr_e,      &
     &       fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,                  &
     &      hdiv_visous_mat)
!
        if(flag_viscous_variation) then
          mat_tmp(n_in:n_out)                                           &
     &       = h_nu_e * h_rho_e * fdm3e_d1_mat(n_in:n_out)
          do i_next = n_in, n_out
            hdiv_visous_mat(1:jmax,i_next)                              &
     &        = hdiv_visous_mat(1:jmax,i_next) + mat_tmp(i_next)
          end do
        end if
      end if
      if(flag_viscous_variation) then
        call each_sph_hdiv_vscs_nu_depend(n_in, n_out, jmax,            &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_de, h_nu_e,      &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
      end if
!
      end subroutine each_sph_FDM_val_hdiv_vscs_mat
!
! -----------------------------------------------------------------------
!
      end module sph_FDM_viscosities_mat
