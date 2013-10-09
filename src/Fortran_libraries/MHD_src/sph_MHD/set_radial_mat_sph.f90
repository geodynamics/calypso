!>@file   set_radial_mat_sph.f90
!!@brief  module set_radial_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr, 2009
!
!>@brief  Construct matrix for spherical shell dynamo model
!!
!!@verbatim
!!      subroutine set_radial_scalar_evo_mat_sph(nri, jmax,             &
!!     &           kr_in, kr_out, coef_imp, coef_d, evo_mat)
!!      subroutine set_radial_vect_evo_mat_sph(nri, jmax,               &
!!     &          kr_in, kr_out, coef_imp, coef_d, evo_mat)
!!
!!      subroutine set_radial_vp3_mat_sph(nri, jmax, kr_in, kr_out,     &
!!     &          poisson_mat)
!!      subroutine set_radial_press_mat_sph(nri, jmax, kr_in, kr_out,   &
!!     &          coef_p, poisson_mat)
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
!!@n @param evo_mat(3,nri,jmax)  Band matrix for time evolution
!!@n @param poisson_mat(3,nri,jmax)  Band matrix for Poisson equation
!
      module set_radial_mat_sph
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_t_int_parameter
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
      subroutine set_radial_scalar_evo_mat_sph(nri, jmax,               &
     &          kr_in, kr_out, coef_imp, coef_d, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          evo_mat(3,k-1,j)                                              &
     &          =     - coef_imp*dt*coef_d * (  d2nod_mat_fdm_2(k,-1)   &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k,-1) )
          evo_mat(2,k,  j)                                              &
     &          = one + coef_imp*dt*coef_d * ( -d2nod_mat_fdm_2(k, 0)   &
     &                 - two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 0)    &
     &                 + g_sph_rj(j,3)*ar_1d_rj(k,2) )
          evo_mat(1,k+1,j)                                              &
     &          =     - coef_imp*dt*coef_d * (  d2nod_mat_fdm_2(k, 1)   &
     &                 + two * ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 1) )
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_scalar_evo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_vect_evo_mat_sph(nri, jmax,                 &
     &          kr_in, kr_out, coef_imp, coef_d, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          evo_mat(3,k-1,j)                                              &
     &          =     - coef_imp*dt*coef_d *    d2nod_mat_fdm_2(k,-1)
          evo_mat(2,k,  j)                                              &
     &          = one + coef_imp*dt*coef_d * ( -d2nod_mat_fdm_2(k, 0)   &
     &                 + g_sph_rj(j,3)*ar_1d_rj(k,2) )
          evo_mat(1,k+1,j)                                              &
     &          =     - coef_imp*dt*coef_d *    d2nod_mat_fdm_2(k, 1)
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_vect_evo_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_vp3_mat_sph(nri, jmax, kr_in, kr_out,       &
     &          poisson_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
!
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          poisson_mat(3,k-1,j) = - d2nod_mat_fdm_2(k,-1)
          poisson_mat(2,k,  j) = - d2nod_mat_fdm_2(k, 0)             &
     &                             + g_sph_rj(j,3)*ar_1d_rj(k,2)
          poisson_mat(1,k+1,j) = - d2nod_mat_fdm_2(k, 1)
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_vp3_mat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_radial_press_mat_sph(nri, jmax, kr_in, kr_out,     &
     &          coef_p, poisson_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: coef_p
!
      real(kind = kreal), intent(inout) :: poisson_mat(3,nri,jmax)
!
      integer(kind = kint) :: k, j
!
!
!$omp do private (k,j)
      do k = kr_in+1, kr_out-1
        do j = 1, jmax
          poisson_mat(3,k-1,j) = coef_p * (d2nod_mat_fdm_2(k,-1)        &
     &                    + two*ar_1d_rj(k,1) * d1nod_mat_fdm_2(k,-1))
          poisson_mat(2,k,  j) = coef_p * (d2nod_mat_fdm_2(k, 0)        &
     &                    + two*ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 0)   &
     &                    - g_sph_rj(j,3)*ar_1d_rj(k,2) )
          poisson_mat(1,k+1,j) = coef_p * (d2nod_mat_fdm_2(k, 1)        &
     &                    + two*ar_1d_rj(k,1) * d1nod_mat_fdm_2(k, 1) )
        end do
      end do
!$omp end do nowait
!
      end subroutine set_radial_press_mat_sph
!
! -----------------------------------------------------------------------
!
      end module set_radial_mat_sph
