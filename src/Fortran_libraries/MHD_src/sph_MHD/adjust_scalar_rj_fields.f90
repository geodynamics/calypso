!>@file   adjust_scalar_rj_fields.f90
!!@brief      module adjust_scalar_rj_fields
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2024
!
!> @brief  Evaluate shifted scalar by CMB value
!!
!!@verbatim
!!      subroutine s_adjust_scalar_rj_fields                            &
!!     &         (sph, ipol_base, ipol_cmp, ipol_prod, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(base_field_address), intent(in) :: ipol_base
!!        type(field_component_address), intent(in) :: ipol_cmp
!!        type(phys_products_address), intent(in) :: ipol_prod
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module adjust_scalar_rj_fields
!
      use t_spheric_parameter
!
      implicit none
!
      private :: shift_by_CMB_average, remove_sphere_average
      private :: copy_magnetic_dipole, copy_toroidal_dipole
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_adjust_scalar_rj_fields                              &
     &         (sph, ipol_base, ipol_cmp, ipol_prod, rj_fld)
!
      use t_base_field_labels
      use t_field_component_labels
      use t_field_product_labels
      use t_phys_data
!
      type(sph_grids), intent(in) :: sph
      type(base_field_address), intent(in) :: ipol_base
      type(field_component_address), intent(in) :: ipol_cmp
      type(phys_products_address), intent(in) :: ipol_prod
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ipol_cmp%i_temp_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_temp),                           &
     &      rj_fld%d_fld(1,ipol_cmp%i_temp_from_CMB))
      end if
!
      if(ipol_cmp%i_light_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_light),                          &
     &      rj_fld%d_fld(1,ipol_cmp%i_light_from_CMB))
      end if
!
      if(ipol_cmp%i_entropy_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_entropy),                        &
     &      rj_fld%d_fld(1,ipol_cmp%i_entropy_from_CMB))
      end if
!
      if(ipol_cmp%i_density_from_CMB .gt. 0) then
        call shift_by_CMB_average(sph%sph_params, sph%sph_rj,           &
     &      rj_fld%d_fld(1,ipol_base%i_density),                        &
     &      rj_fld%d_fld(1,ipol_cmp%i_density_from_CMB))
      end if
!
!
      if(ipol_cmp%i_asph_pressure .gt. 0) then
        call remove_sphere_average(sph%sph_rj,                          &
     &      rj_fld%d_fld(1,ipol_base%i_press),                          &
     &      rj_fld%d_fld(1,ipol_cmp%i_asph_pressure))
      end if
!
!
      if(ipol_prod%i_dipole_B .gt. 0) then
        call copy_magnetic_dipole(sph%sph_rj,                           &
     &      rj_fld%d_fld(1,ipol_base%i_magne),                          &
     &      rj_fld%d_fld(1,ipol_prod%i_dipole_B))
      end if
!
      if(ipol_prod%i_dipole_J .gt. 0) then
        call copy_toroidal_dipole(sph%sph_rj,                           &
     &      rj_fld%d_fld(1,ipol_base%i_current),                        &
     &      rj_fld%d_fld(1,ipol_prod%i_dipole_J))
      end if
!
      end subroutine s_adjust_scalar_rj_fields
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine shift_by_CMB_average(sph_params, sph_rj,               &
     &                                d_scalar, d_rj_part)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: d_scalar(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: d_rj_part(sph_rj%nnod_rj)
!
      integer(kind = kint) :: kr, inod, iCMB
!
!
!$omp parallel workshare
      d_rj_part(1:sph_rj%nnod_rj) = d_scalar(1:sph_rj%nnod_rj)
!$omp end parallel workshare
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
      iCMB = 1 + (sph_params%nlayer_CMB-1) * sph_rj%istep_rj(1)         &
     &         + (sph_rj%idx_rj_degree_zero-1) * sph_rj%istep_rj(2)
!$omp parallel do private(kr,inod)
      do kr = 1, sph_rj%nidx_rj(1)
        inod = 1 + (kr-1) * sph_rj%istep_rj(1)                          &
     &           + (sph_rj%idx_rj_degree_zero-1) * sph_rj%istep_rj(2)
        d_rj_part(inod) = d_rj_part(inod) - d_scalar(iCMB)
      end do
!$omp end parallel do
!
      end subroutine shift_by_CMB_average
!
!-----------------------------------------------------------------------
!
      subroutine remove_sphere_average(sph_rj, d_scalar, d_rj_part)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: d_scalar(sph_rj%nnod_rj)
      real(kind = kreal), intent(inout) :: d_rj_part(sph_rj%nnod_rj)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel workshare
      d_rj_part(1:sph_rj%nnod_rj) = d_scalar(1:sph_rj%nnod_rj)
!$omp end parallel workshare
!
      if(sph_rj%idx_rj_degree_zero .eq. 0) return
!
!$omp parallel do private(kr,inod)
      do kr = 1, sph_rj%nidx_rj(1)
        inod = 1 + (kr-1) * sph_rj%istep_rj(1)                          &
     &           + (sph_rj%idx_rj_degree_zero-1) * sph_rj%istep_rj(2)
        d_rj_part(inod) = 0.0d0
      end do
!$omp end parallel do
!
      end subroutine remove_sphere_average
!
!-----------------------------------------------------------------------
!
      subroutine copy_magnetic_dipole(sph_rj, d_vect, d_rj_part)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: d_vect(sph_rj%nnod_rj,3)
      real(kind = kreal), intent(inout) :: d_rj_part(sph_rj%nnod_rj,3)
!
      integer(kind = kint) :: kr, inod
!
!
!$omp parallel workshare
      d_rj_part(1:sph_rj%nnod_rj,1) = 0.0d0
      d_rj_part(1:sph_rj%nnod_rj,2) = 0.0d0
      d_rj_part(1:sph_rj%nnod_rj,3) = 0.0d0
!$omp end parallel workshare

      if(sph_rj%idx_rj_degree_one(0) .eq. 0) return
!
!$omp parallel do private(kr,inod)
      do kr = 1, sph_rj%nidx_rj(1)
        inod = 1 + (kr-1) * sph_rj%istep_rj(1)                          &
     &           + (sph_rj%idx_rj_degree_one(0)-1) * sph_rj%istep_rj(2)
        d_rj_part(inod,2) = d_vect(inod,1)
        d_rj_part(inod,3) = d_vect(inod,2)
      end do
!$omp end parallel do
!
      end subroutine copy_magnetic_dipole
!
!-----------------------------------------------------------------------
!
      subroutine copy_toroidal_dipole(sph_rj, d_vect, d_rj_part)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      real(kind = kreal), intent(in) :: d_vect(sph_rj%nnod_rj,3)
      real(kind = kreal), intent(inout) :: d_rj_part(sph_rj%nnod_rj,3)
!
      integer(kind = kint) :: kr, inod, iCMB
!
!
!$omp parallel workshare
      d_rj_part(1:sph_rj%nnod_rj,1) = 0.0d0
      d_rj_part(1:sph_rj%nnod_rj,2) = 0.0d0
      d_rj_part(1:sph_rj%nnod_rj,3) = 0.0d0
!$omp end parallel workshare

      if(sph_rj%idx_rj_degree_one(0) .eq. 0) return
!
!$omp parallel do private(kr,inod)
      do kr = 1, sph_rj%nidx_rj(1)
        inod = 1 + (kr-1) * sph_rj%istep_rj(1)                          &
     &           + (sph_rj%idx_rj_degree_one(0)-1) * sph_rj%istep_rj(2)
        d_rj_part(inod,3) = d_vect(inod,3)
      end do
!$omp end parallel do
!
      end subroutine copy_toroidal_dipole
!
!-----------------------------------------------------------------------
!
      end module adjust_scalar_rj_fields
