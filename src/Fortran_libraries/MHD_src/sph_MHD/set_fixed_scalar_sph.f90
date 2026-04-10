!>@file   set_fixed_scalar_sph.f90
!!@brief  module set_fixed_scalar_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Evaluate scalar fields at boundaries
!!@n     Adjust temperature and composition boundary conditions
!!       if perturbation is solved
!!
!!
!!@verbatim
!!      subroutine s_set_fixed_scalar_sph                               &
!!     &         (jmax, inod_rj_center, idx_rj_degree_zero,             &
!!     &          kr_bc_st, kr_bc_ed, fixed_bc, S_CTR,                  &
!!     &          n_point, d_rj_fld)
!!        integer(kind = kint), intent(in) :: inod_rj_center
!!        integer(kind = kint), intent(in) :: idx_rj_degree_zero
!!        integer(kind = kint), intent(in) :: jmax, kr_bc_st, kr_bc_ed
!!        real(kind = kreal), intent(in) :: fixed_bc(jmax)
!!        real(kind = kreal), intent(in) :: S_CTR
!!        integer(kind = kint), intent(in) :: n_point
!!        real (kind=kreal), intent(inout) :: d_rj_fld(n_point)
!!@endverbatim
!!
!!@param  n_point  Number of points for spectrum data
!!@param  jmax        Number of modes for local spectrum
!!@param  kr_bc_st    Start radial address to set fixed field
!!@param  kr_bc_ed    End radial address to set fixed field
!!@param  fixed_bc(jmax)   Boundary condition spectrum
!!
!!@param d_rj_fld           Spectrum data
!!
      module set_fixed_scalar_sph
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
      subroutine s_set_fixed_scalar_sph                                 &
     &         (jmax, inod_rj_center, idx_rj_degree_zero,               &
     &          kr_bc_st, kr_bc_ed, fixed_bc, S_CTR,                    &
     &          n_point, d_rj_fld)
!
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_bc_st, kr_bc_ed
      real(kind = kreal), intent(in) :: fixed_bc(jmax)
      real(kind = kreal), intent(in) :: S_CTR
!
      integer(kind = kint), intent(in) :: n_point
      real (kind=kreal), intent(inout) :: d_rj_fld(n_point)
!
      integer(kind = kint) :: j, inod, k
!
!
!$omp parallel do private (k,j,inod)
      do k = kr_bc_st, kr_bc_ed
        do j = 1, jmax
          inod = j + (k-1) * jmax
          d_rj_fld(inod) = fixed_bc(j)
        end do
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) return
      if(idx_rj_degree_zero .eq. 0) return
      if(kr_bc_st .ne. ione) return
!
      d_rj_fld(inod_rj_center) = S_CTR
!
      end subroutine s_set_fixed_scalar_sph
!
! -----------------------------------------------------------------------
!
      end module set_fixed_scalar_sph
