!>@file   fill_scalar_field.f90
!!@brief  module fill_scalar_field
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!!
!!@verbatim
!!      subroutine copy_degree0_comps_to_sol(nri, jmax,                 &
!!     &          inod_rj_center, idx_rj_degree_zero, is_field,         &
!!     &          n_point, ntot_phys_rj, d_rj, sol_00)
!!      subroutine copy_degree0_comps_from_sol(nri, jmax,               &
!!     &          inod_rj_center, idx_rj_degree_zero, sol_00, is_field, &
!!     &          n_point, ntot_phys_rj, d_rj)
!!      subroutine fill_scalar_at_external(sph_bc,                      &
!!     &          inod_rj_center, idx_rj_degree_zero, nri, jmax,        &
!!     &          ipol_scalar, n_point, ntot_phys_rj, d_rj)
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!
!!      subroutine fill_scalar_1d_external                              &
!!     &         (sph_bc, inod_rj_center, nri, d_r)
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!@endverbatim
!
      module fill_scalar_field
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_to_sol(nri, jmax,                   &
     &          inod_rj_center, idx_rj_degree_zero, is_field,           &
     &          n_point, ntot_phys_rj, d_rj, sol_00)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      integer(kind = kint), intent(in) :: n_point,  ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: sol_00(0:nri)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel do private(inod)
      do kr = 1, nri
        inod = idx_rj_degree_zero + (kr-1) * jmax
        sol_00(kr) = d_rj(inod,is_field)
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) then
        sol_00(0) = d_rj(inod_rj_center,is_field)
      end if
!
!       write(*,*) 'kr, Average RHS'
!       do kr = 0, nri
!         write(*,*) kr, sol_00(kr)
!       end do
!
      end subroutine copy_degree0_comps_to_sol
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_degree0_comps_from_sol(nri, jmax,                 &
     &          inod_rj_center, idx_rj_degree_zero, sol_00, is_field,   &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: is_field
      real(kind = kreal), intent(in) :: sol_00(0:nri)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: kr, inod
!
!$omp parallel do private(inod)
      do kr = 1, nri
        inod = idx_rj_degree_zero + (kr-1) * jmax
        d_rj(inod,is_field) = sol_00(kr)
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) then
        d_rj(inod_rj_center,is_field) = sol_00(0)
      end if
!
!       write(*,*) 'kr, average Solution'
!       do kr = 0, nri
!         write(*,*) kr, sol_00(kr)
!      end do
!
      end subroutine copy_degree0_comps_from_sol
!
! -----------------------------------------------------------------------
!
      subroutine fill_scalar_at_external(sph_bc,                        &
     &          inod_rj_center, idx_rj_degree_zero, nri, jmax,          &
     &          ipol_scalar, n_point, ntot_phys_rj, d_rj)
!
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: ipol_scalar
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, i_bc, k, j
!
!
!$omp parallel do private (k,j,inod,i_bc)
      do k = 1, sph_bc%kr_in - 1
        do j = 1, jmax
          i_bc = j + (sph_bc%kr_in-1) * jmax
          inod = j + (k-1) * jmax
          d_rj(inod,ipol_scalar) = d_rj(i_bc,ipol_scalar)
        end do
      end do
!$omp end parallel do
!
!
!$omp parallel do private (k,j,inod,i_bc)
      do k = sph_bc%kr_out + 1, nri
        do j = 1, jmax
          i_bc = j + (sph_bc%kr_out-1) * jmax
          inod = j + (k-1) * jmax
          d_rj(inod,ipol_scalar) = d_rj(i_bc,ipol_scalar)
        end do
      end do
!$omp end parallel do
!
      if(sph_bc%iflag_icb .eq. iflag_fixed_field                        &
     &  .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
        if(inod_rj_center .gt. 0) then
          i_bc = idx_rj_degree_zero + (sph_bc%kr_in-1) * jmax
          d_rj(inod_rj_center,ipol_scalar) = d_rj(i_bc,ipol_scalar)
        end if
      end if
!
      end subroutine fill_scalar_at_external
!
! -----------------------------------------------------------------------
!
      subroutine fill_scalar_1d_external                                &
     &         (sph_bc, inod_rj_center, nri, d_r)
!
      use t_boundary_params_sph_MHD
!
      type(sph_boundary_type), intent(in) :: sph_bc
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: nri
!
      real (kind=kreal), intent(inout) :: d_r(0:nri)
!
      integer(kind = kint) :: k
!
!
      do k = 1, sph_bc%kr_in - 1
        d_r(k) = d_r(sph_bc%kr_in)
      end do
      do k = sph_bc%kr_out + 1, nri
        d_r(k) = d_r(sph_bc%kr_out)
      end do
!
      if(sph_bc%iflag_icb .eq. iflag_fixed_field                        &
     &  .or. sph_bc%iflag_icb .eq. iflag_evolve_field) then
        if(inod_rj_center .gt. 0) d_r(0) = d_r(sph_bc%kr_in)
      end if
!
      end subroutine fill_scalar_1d_external
!
! -----------------------------------------------------------------------
!
      end module fill_scalar_field
