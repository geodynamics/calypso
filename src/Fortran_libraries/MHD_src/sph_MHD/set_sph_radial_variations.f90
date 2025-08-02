!>@file   set_sph_radial_variations.f90
!!@brief  module set_sph_radial_variations
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Set radial variations for densityuy and diffusivities
!!
!!@verbatim
!!      subroutine set_sph_radial_density                               &
!!     &         (my_rank, radius_name, density_name, ir_density,       &
!!     &          sph_rj, r_2nd, polytrope_param, radial_variation,     &
!!     &          r_itp, fld_IO)
!!        character(len=kchara), intent(in) :: density_name
!!        integer(kind = kint), intent(in) :: ir_density
!!        type(polytrope_parameters), intent(inout) :: polytrope_param
!!      subroutine set_sph_radial_diffusivity                           &
!!     &         (my_rank, radius_name, diffusivity_name, ir_diffusion, &
!!     &          sph_rj, r_2nd, diffuse_param, radial_variation,       &
!!     &          r_itp, fld_IO)
!!        character(len=kchara), intent(in) :: diffusivity_name
!!        integer(kind = kint), intent(in) :: ir_diffusion
!!        integer, intent(in) :: my_rank
!!        character(len=kchara), intent(in) :: radius_name
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(val_diffuse_parameters), intent(inout) :: diffuse_param
!!        type(phys_data), intent(inout) :: radial_variation
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!!
!
      module set_sph_radial_variations
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
      use t_phys_data
      use t_sph_radial_interpolate
      use t_field_data_IO
      use t_fdm_coefs
!
      implicit none
!
      private :: find_address_from_field_IO
      private :: set_sph_polytrope_density
      private :: sph_radial_variation_from_list
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_radial_density                                 &
     &         (my_rank, radius_name, density_name, ir_density,         &
     &          sph_rj, r_2nd, polytrope_param, radial_variation,       &
     &          r_itp, fld_IO)
!
      use t_ctl_param_val_density
      use radial_interpolation
      use cal_sph_exp_1st_diff
      use field_file_IO
!
      integer, intent(in) :: my_rank
      character(len=kchara), intent(in) :: radius_name
      character(len=kchara), intent(in) :: density_name
      integer(kind = kint), intent(in) :: ir_density
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(polytrope_parameters), intent(inout) :: polytrope_param
!
      type(phys_data), intent(inout) :: radial_variation
      type(sph_radial_interpolate), intent(inout) :: r_itp
      type(field_IO), intent(inout) :: fld_IO
!
      type(time_data) :: t_IO
      integer(kind = kint) :: iend
      integer(kind = kint) :: i_r, i_den, k
!
!
      if(ir_density .le. 0) return
      if(polytrope_param%polytrope_idx .le. 0.0d0) then
        if(polytrope_param%num_density_list .le. 0) then
          call read_and_alloc_step_field                                &
     &       (polytrope_param%density_file_name,                        &
     &        my_rank, t_IO, fld_IO, iend)
!
          i_r =   find_address_from_field_IO(radius_name, fld_IO)
          i_den = find_address_from_field_IO(density_name, fld_IO)
          write(*,*) 'i_den', i_den, i_r
          call alloc_density_variation_list(fld_IO%nnod_IO,             &
     &                                      polytrope_param)
          polytrope_param%density_radius(1:fld_IO%nnod_IO)              &
     &                            = fld_IO%d_IO(1:fld_IO%nnod_IO,i_r)
          polytrope_param%density_list(1:fld_IO%nnod_IO)                &
     &                            = fld_IO%d_IO(1:fld_IO%nnod_IO,i_den)
          call dealloc_phys_data_IO(fld_IO)
          call dealloc_phys_name_IO(fld_IO)
        end if
!
        call sph_radial_variation_from_list(sph_rj, r_2nd,              &
     &      polytrope_param%num_density_list,                           &
     &      polytrope_param%density_radius,                             &
     &      polytrope_param%density_list,                               &
     &      radial_variation%n_point, radial_variation%d_fld(1,1),      &
     &      radial_variation%d_fld(1,ir_density  ),                     &
     &      radial_variation%d_fld(1,ir_density+1), r_itp)
!
        call cal_sph_nod_gradient_1d(ione, sph_rj%nidx_rj(1),           &
     &      sph_rj%nidx_rj(1), r_2nd%dmat(-1,1,1),                      &
     &      radial_variation%d_fld(2,ir_density+1),                     &
     &      radial_variation%d_fld(2,ir_density+2))
        radial_variation%d_fld(2,ir_density+1) = zero
        k = sph_rj%nidx_rj(1) + 1
        radial_variation%d_fld(k,ir_density+2) = zero
      else
        call set_sph_polytrope_density                                  &
     &     (sph_rj, polytrope_param, radial_variation%n_point,          &
     &      radial_variation%d_fld(1,ir_density  ),                     &
     &      radial_variation%d_fld(1,ir_density+1),                     &
     &      radial_variation%d_fld(1,ir_density+2))
      end if
!
      end subroutine set_sph_radial_density
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_radial_diffusivity                             &
     &         (my_rank, radius_name, diffusivity_name, ir_diffusion,   &
     &          sph_rj, r_2nd, diffuse_param, radial_variation,         &
     &          r_itp, fld_IO)
!
      use t_ctl_param_val_diffusion
      use radial_interpolation
      use const_diffusive_profile
      use field_file_IO
!
      integer, intent(in) :: my_rank
      character(len=kchara), intent(in) :: radius_name
      character(len=kchara), intent(in) :: diffusivity_name
      integer(kind = kint), intent(in) :: ir_diffusion
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(val_diffuse_parameters), intent(inout) :: diffuse_param
!
      type(phys_data), intent(inout) :: radial_variation
      type(sph_radial_interpolate), intent(inout) :: r_itp
      type(field_IO), intent(inout) :: fld_IO
!
      type(time_data) :: t_IO
      integer(kind = kint) :: iend
      integer(kind = kint) :: i_r, i_dif
!
!
      if(ir_diffusion .le. 0) return
      if(diffuse_param%num_diffusion_list .le. 0) then
        call read_and_alloc_step_field                                  &
     &     (diffuse_param%diffuse_file_name,                            &
     &      my_rank, t_IO, fld_IO, iend)
!
        i_r =   find_address_from_field_IO(radius_name, fld_IO)
        i_dif = find_address_from_field_IO(diffusivity_name, fld_IO)
        call alloc_val_diffuse_parameters(fld_IO%nnod_IO,               &
     &                                    diffuse_param)
        diffuse_param%diffusion_radius(1:fld_IO%nnod_IO)                &
     &                          = fld_IO%d_IO(1:fld_IO%nnod_IO,i_r)
        diffuse_param%diffusion_list(1:fld_IO%nnod_IO)                  &
     &                          = fld_IO%d_IO(1:fld_IO%nnod_IO,i_dif)
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      call sph_radial_variation_from_list(sph_rj, r_2nd,                &
     &    diffuse_param%num_diffusion_list,                             &
     &    diffuse_param%diffusion_radius,                               &
     &    diffuse_param%diffusion_list,                                 &
     &    radial_variation%n_point, radial_variation%d_fld(1,1),        &
     &    radial_variation%d_fld(1,ir_diffusion  ),                     &
     &    radial_variation%d_fld(1,ir_diffusion+1), r_itp)
!
      end subroutine set_sph_radial_diffusivity
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      integer(kind = kint) function find_address_from_field_IO          &
     &                                          (target_name, fld_IO)
!
      character(len = kchara), intent(in) :: target_name
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: i_field, icou, i
!
      i_field = 0
      icou = 0
      do i = 1, fld_IO%num_field_IO
        if(fld_IO%fld_name(i) .eq. target_name) then
          i_field = icou + 1
          exit
        end if
        icou = icou + fld_IO%num_comp_IO(i)
      end do
      if(i_field .le. 0) write(*,*) trim(target_name),                  &
     &                           ' cannot be found...'
      find_address_from_field_IO = i_field
!
      end function find_address_from_field_IO
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_polytrope_density(sph_rj, polytrope_param,     &
     &          n_point, rho_r, drho_norm, d2rho_norm)
!
      use t_ctl_param_val_density
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(polytrope_parameters), intent(in) :: polytrope_param
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: rho_r(n_point)
      real(kind = kreal), intent(inout) :: drho_norm(n_point)
      real(kind = kreal), intent(inout) :: d2rho_norm(n_point)
!
      integer(kind = kint) :: k
      real(kind = kreal) :: r_in, r_out, rho_in, rho_out
      real(kind = kreal) :: beta, N_p, xi_0, p_idx
      real(kind = kreal) :: c_0, c_1, xi_r, dxi_dr, d2xi_dr
!
!
        p_idx =   polytrope_param%polytrope_idx
        r_in =    polytrope_param%rho_bottom(1)
        r_out =   polytrope_param%rho_top(1)
        rho_in =  polytrope_param%rho_bottom(2)
        rho_out = polytrope_param%rho_top(2)
        beta = r_in / r_out
        N_p = log(rho_in / rho_out)
        xi_0 = (one + beta) / (one + beta * exp(N_p / p_idx))
        c_0 = (two * xi_0 - beta - one) / (one - beta)
        c_1 = (one + beta) * (one - xi_0) / ((one - beta)**2)
!
        do k = 1, sph_rj%nidx_rj(1)
          xi_r =    c_0 + c_1 * sph_rj%ar_1d_rj(k,1)
          dxi_dr =      - c_1 * sph_rj%ar_1d_rj(k,2)
          d2xi_dr = two * c_1 * sph_rj%ar_1d_rj(k,2)                    &
     &             * sph_rj%ar_1d_rj(k,1)
!
          rho_r(k+1) =     xi_r**p_idx
          drho_norm(k+1) = p_idx * dxi_dr / xi_r
!     &        = p_idx * xi_r**(p_idx-1.0d0) * dxi_dr / xi_r**p_idx
!
          drho_norm(k+1) = p_idx * (d2xi_dr / xi_r                      &
     &                    + (dxi_dr / xi_r)**2)
        end do
!
      end subroutine set_sph_polytrope_density
!
!  -------------------------------------------------------------------
!
      subroutine sph_radial_variation_from_list                         &
     &         (sph_rj, r_2nd, n_list, radius_list, value_list,         &
     &          n_point, radius, val_r, dval_norm, r_itp)
!
      use radial_interpolation
      use const_diffusive_profile
      use cal_sph_exp_1st_diff
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
!
      integer(kind = kint), intent(in) :: n_list
      real(kind = kreal), intent(in) :: radius_list(n_list)
      real(kind = kreal), intent(in) :: value_list(n_list)
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: radius(n_point)
!
      real(kind = kreal), intent(inout) :: val_r(n_point)
      real(kind = kreal), intent(inout) :: dval_norm(n_point)
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      integer(kind = kint) :: k
!
!
      call alloc_org_radius_interpolate(n_list, r_itp)
      call alloc_radial_interpolate(n_point, r_itp)
      call alloc_original_sph_data(n_list, r_itp)

      r_itp%source_radius(1:r_itp%nri_source)                           &
     &               = radius_list(1:r_itp%nri_source)
      call cal_radial_interpolation_coef                                &
     &   (r_itp%nri_source, r_itp%source_radius, n_point, radius,       &
     &    r_itp%kr_inner_source, r_itp%kr_outer_source,                 &
     &    r_itp%k_old2new_in, r_itp%k_old2new_out,                      &
     &    r_itp%coef_old2new_in)
!      call check_sph_radial_interpolate                                &
!     &   (r_itp%nri_source, r_itp%source_radius,                       &
!     &    n_point, radius, r_itp)
      call interpolate_radial_field(n_point,                            &
     &    r_itp%k_old2new_in, r_itp%k_old2new_out,                      &
     &    r_itp%coef_old2new_in, ione,                                  &
     &    r_itp%nri_source, value_list(1), val_r(1))
      call dealloc_original_sph_data(r_itp)
      call dealloc_radial_interpolate(r_itp)
      call dealloc_org_radius_interpolate(r_itp)
!
      call cal_sph_nod_gradient_1d                                      &
     &   (ione, sph_rj%nidx_rj(1), sph_rj%nidx_rj(1),                   &
     &    r_2nd%dmat(-1,1,1), val_r(2), dval_norm(2))
!
!$omp parallel workshare
      dval_norm(2:n_point-1) = dval_norm(2:n_point-1)                   &
     &                     / val_r(2:n_point-1)
!$omp end parallel workshare
!
      dval_norm(1) = zero
      k = sph_rj%nidx_rj(1) + 1
      dval_norm(k) = zero
!
      end subroutine sph_radial_variation_from_list
!
!  -------------------------------------------------------------------
!
      end module set_sph_radial_variations
