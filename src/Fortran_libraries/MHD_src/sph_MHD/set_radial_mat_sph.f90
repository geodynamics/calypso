!>@file   set_radial_mat_sph.f90
!!@brief  module set_radial_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for spherical shell dynamo model
!!
!!@verbatim
!!      subroutine set_unit_mat5_4_time_evo(nri, jmax, mat5)
!!      subroutine set_unit_mat_4_time_evo(nri, jmax, mat3)
!!      subroutine set_unit_mat_4_poisson(nri, jmax, kr_in, kr_out,     &
!!     &          mat3)
!!
!!      subroutine add_scalar_poisson_mat_sph(nri, jmax, kr_in, kr_out, &
!!     &          coef_p, mat3)
!!         
!!      subroutine add_vector_poisson_mat_sph(nri, jmax, kr_in, kr_out, &
!!     &          kr_in, kr_out, coef_p, mat3)
!!
!!      subroutine add_scalar_r_diffuse_mat_sph(nri, jmax,              &
!!     &          kr_in, kr_out, coef_p, val_r, dval_r, mat3)
!!      subroutine add_vector_r_diffuse_mat_sph(nri, jmax,              &
!!     &          kr_in, kr_out, coef_p, val_r, dval_r, mat3)
!!
!!    Format of band matrix
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Original band matrix
!!      band_a(i-j+iband+1,j) = a(i,j)
!!      band_a(k,j) = a(k+j-iband-1,j)
!!   3-band matrix
!!      band_a(i-j+2,j) = a(i,j)
!!      band_a(k,j) = a(k+j-2,j)
!!   5-band matrix
!!      band_lu(i-j+3,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-3,j)
!!   7-band matrix
!!      band_lu(i-j+4,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-4,j)
!!@endverbatim
!!
!!@n @param nri     Number of radial points
!!@n @param jmax    Number of spherical harmonics modes
!!@n @param kr_st   Start radial address to construct matrix
!!@n @param kr_ed   End radial address to construct matrix
!!@n @param kr_in    Radial address for inner boundary
!!@n @param kr_out   Radial address for outer boundary
!!@n @param coef_imp   Coefficient for contribution of implicit term
!!@n @param coef_d     Coefficient of diffusiotn term
!!@n @param coef_p     Coefficient of pressure gradient
!!
!!@n @param mat5(5,nri,jmax)  Band matrix
!!@n @param mat3(3,nri,jmax)  Band matrix
!
      module set_radial_mat_sph
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_fdm_coefs
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat5_4_time_evo(nri, jmax, mat5)
!
      integer(kind = kint), intent(in) :: jmax, nri
!
      real(kind = kreal), intent(inout) :: mat5(5,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do j = 1, jmax
        do k = 3, nri
          mat5(5,k-2,j) = zero
        end do
        do k = 2, nri
          mat5(4,k-1,j) = zero
        end do
        do k = 1, nri
          mat5(3,k,  j) = one
        end do
        do k = 1, nri-1
          mat5(2,k+1,j) = zero
        end do
        do k = 1, nri-2
          mat5(1,k+2,j) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat5_4_time_evo
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat_4_time_evo(nri, jmax, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do j = 1, jmax
        do k = 2, nri
          mat3(3,k-1,j) = zero
        end do
        do k = 1, nri
          mat3(2,k,  j) = one
        end do
        do k = 1, nri-1
          mat3(1,k+1,j) = zero
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat_4_time_evo
!
! -----------------------------------------------------------------------
!
      subroutine set_unit_mat_4_poisson(nri, jmax, kr_in, kr_out,       &
     &          mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do j = 1, jmax
        do k = 1, nri
          mat3(3,k,j) = zero
          mat3(1,k,j) = zero
        end do
        do k = 1, kr_in-1
          mat3(2,k,j) = one
        end do
        do k = kr_in, kr_out
          mat3(2,k,j) = zero
        end do
        do k = kr_out+1, nri
          mat3(2,k,j) = one
        end do
      end do
!$omp end parallel do
!
      end subroutine set_unit_mat_4_poisson
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_scalar_poisson_mat_sph(nri, jmax, kr_in, kr_out,   &
     &          coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          mat3(3,k-1,j) = mat3(3,k-1,j)                                 &
     &                   - coef_p * (d2nod_mat_fdm_2(k,-1)              &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k,-1))
          mat3(2,k,  j) = mat3(2,k,  j)                                 &
     &                   - coef_p * (d2nod_mat_fdm_2(k, 0)              &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 0)    &
     &                 - g_sph_rj(j,3)*ar_1d_rj(k,2) )
          mat3(1,k+1,j) = mat3(1,k+1,j)                                 &
     &                   - coef_p * (d2nod_mat_fdm_2(k, 1)              &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 1))
        end do
      end do
!$omp end parallel do
!
      end subroutine add_scalar_poisson_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine add_vector_poisson_mat_sph(nri, jmax, kr_in, kr_out,   &
     &          coef_p, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          mat3(3,k-1,j) = mat3(3,k-1,j)                                 &
     &                   - coef_p *  d2nod_mat_fdm_2(k,-1)
          mat3(2,k,  j) = mat3(2,k,  j)                                 &
     &                   - coef_p * (d2nod_mat_fdm_2(k, 0)              &
     &                    - g_sph_rj(j,3)*ar_1d_rj(k,2) )
          mat3(1,k+1,j) = mat3(1,k+1,j)                                 &
     &                   - coef_p *  d2nod_mat_fdm_2(k, 1)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_vector_poisson_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_scalar_r_diffuse_mat_sph(nri, jmax,                &
     &          kr_in, kr_out, coef_p, val_r, dval_r, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: val_r(nri), dval_r(nri)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          mat3(3,k-1,j) = mat3(3,k-1,j)                                 &
     &                   - coef_p * val_r(k) * (d2nod_mat_fdm_2(k,-1)   &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k,-1))   &
     &                   - coef_p * dval_r(k) * d1nod_mat_fdm_2(k,-1)
          mat3(2,k,  j) = mat3(2,k,  j)                                 &
     &                   - coef_p * val_r(k) * (d2nod_mat_fdm_2(k, 0)   &
     &                  + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 0)   &
     &                 - g_sph_rj(j,3)*ar_1d_rj(k,2) )                  &
     &                   - coef_p * dval_r(k) * d1nod_mat_fdm_2(k,-1)
          mat3(1,k+1,j) = mat3(1,k+1,j)                                 &
     &                   - coef_p  * val_r(k) * (d2nod_mat_fdm_2(k, 1)  &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 1))   &
     &                   - coef_p * dval_r(k) * d1nod_mat_fdm_2(k,-1)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_scalar_r_diffuse_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine add_vector_r_diffuse_mat_sph(nri, jmax,                &
     &          kr_in, kr_out, coef_p, val_r, dval_r, mat3)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: val_r(nri), dval_r(nri)
!
      real(kind = kreal), intent(inout) :: mat3(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp parallel do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          mat3(3,k-1,j) = mat3(3,k-1,j)                                 &
     &                   - coef_p *  d2nod_mat_fdm_2(k,-1)
          mat3(2,k,  j) = mat3(2,k,  j)                                 &
     &                   - coef_p * (d2nod_mat_fdm_2(k, 0)              &
     &                    - g_sph_rj(j,3)*ar_1d_rj(k,2) )
          mat3(1,k+1,j) = mat3(1,k+1,j)                                 &
     &                   - coef_p *  d2nod_mat_fdm_2(k, 1)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_vector_r_diffuse_mat_sph
!
! -----------------------------------------------------------------------
!
      end module set_radial_mat_sph
