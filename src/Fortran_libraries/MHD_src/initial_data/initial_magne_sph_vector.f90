!>@file   initial_magne_sph_vector.f90
!!@brief  module initial_magne_sph_vector
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial vecotr field
!!
!!@verbatim
!!      subroutine reset_initial_sph_vector(n_point, d_rj_vect)
!!      subroutine reduce_initial_sph_vector(ratio, n_point, d_rj_vect)
!!        real(kind = kreal), intent(in) :: ratio
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_vect(n_point,3)
!!@endverbatim
!
      module initial_magne_sph_vector
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine reset_initial_sph_vector(n_point, d_rj_vect)
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: d_rj_vect(n_point,3)
!
      call reduce_initial_sph_vector(zero, n_point, d_rj_vect)
!
      end subroutine reset_initial_sph_vector
!
!-----------------------------------------------------------------------
!
      subroutine reduce_initial_sph_vector(ratio, n_point, d_rj_vect)
!
      real(kind = kreal), intent(in) :: ratio
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_vect(n_point,3)
!
!$omp parallel workshare
        d_rj_vect(1:n_point,1) =   ratio * d_rj_vect(1:n_point,1)
        d_rj_vect(1:n_point,2) =   ratio * d_rj_vect(1:n_point,2)
        d_rj_vect(1:n_point,3) =   ratio * d_rj_vect(1:n_point,3)
!$omp end parallel workshare
!
      end subroutine reduce_initial_sph_vector
!
!-----------------------------------------------------------------------
!
      end module initial_magne_sph_vector
