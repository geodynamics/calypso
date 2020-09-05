!>@file   t_spherical_harmonics.f90
!!@brief  module t_spherical_harmonics
!!
!!@author H. Matsui
!!@date Programmed in 1993
!!@n    Modified in 2009
!!@n    Modified in 2016
!
!> @brief Data array for spherical harmonics
!!
!!@verbatim
!!       subroutine alloc_index_4_sph(nth, sph)
!!       subroutine alloc_spherical_harmonics(sph)
!!
!!       subroutine dealloc_index_4_sph(sph)
!!       subroutine dealloc_spherical_harmonics(sph)
!!
!!      subroutine init_sph_indices(nth, leg, sph)
!!      subroutine finalize_sph_indices(leg, sph)
!!      subroutine spheric_1point(theta, phi, leg, sph)
!!@endverbatim
!
      module t_spherical_harmonics
!
      use m_precision
!
      implicit  none
!
!
!>      structure of spherical harmonics at single point
      type sph_1point_type
!>        truncation degree
        integer(kind = kint) :: ltr_tri
!>        number of modes
        integer(kind = kint) :: jmax_tri
!
!>        integer list for spherical harmonics
        integer(kind = kint), allocatable:: idx(:,:)
!>        coefficients list for spherical harmonics
        real(kind = kreal), allocatable:: g(:,:)
!>        spherical harmonics
        real(kind = kreal), allocatable:: y_lm(:,:)
      end type sph_1point_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_index_4_sph(nth, sph)
!
      integer(kind = kint), intent(in) :: nth
      type(sph_1point_type), intent(inout) :: sph
!
      sph%ltr_tri =  nth
      sph%jmax_tri = nth*(nth+2)
!
      allocate ( sph%idx(0:sph%jmax_tri,2) )
      allocate ( sph%g(0:sph%jmax_tri,17) )
!
      sph%idx = 0
      sph%g = 0.0d0
!
      end subroutine alloc_index_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine alloc_spherical_harmonics(sph)
!
      type(sph_1point_type), intent(inout) :: sph
!
!
      allocate ( sph%y_lm(0:sph%jmax_tri,0:3) )
!
      end subroutine alloc_spherical_harmonics
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_index_4_sph(sph)
!
      type(sph_1point_type), intent(inout) :: sph
!
      deallocate ( sph%idx, sph%g )
!
      end subroutine dealloc_index_4_sph
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_spherical_harmonics(sph)
!
      type(sph_1point_type), intent(inout) :: sph
!
      deallocate(sph%y_lm)
!
      end subroutine dealloc_spherical_harmonics
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_sph_indices(nth, leg, sph)
!
      use t_schmidt_polynomial
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: nth
      type(legendre_polynomials), intent(inout) :: leg
      type(sph_1point_type), intent(inout) :: sph
!
!
      call alloc_schmidt_polynomial(nth, leg)
      call alloc_index_4_sph(nth, sph)
      call idx28(sph%ltr_tri, sph%jmax_tri, sph%idx, sph%g)
!
      end subroutine init_sph_indices
!
! -----------------------------------------------------------------------
!
      subroutine finalize_sph_indices(leg, sph)
!
      use t_schmidt_polynomial
!
      type(legendre_polynomials), intent(inout) :: leg
      type(sph_1point_type), intent(inout) :: sph
!
!
      call dealloc_index_4_sph(sph)
      call dealloc_schmidt_polynomial(leg)
!
      end subroutine finalize_sph_indices
!
! -----------------------------------------------------------------------
!
      subroutine spheric_1point(theta, phi, leg, sph)
!
      use t_schmidt_polynomial
      use spherical_harmonics
!
      real(kind = kreal), intent(in) :: theta, phi
      type(legendre_polynomials), intent(inout) :: leg
      type(sph_1point_type), intent(inout) :: sph
!
!
      call dschmidt(theta, leg)
      call spheric(leg%nth, sph%jmax_tri, sph%idx,                      &
     &    phi, leg%p, leg%dp, sph%y_lm)
!
      end subroutine spheric_1point
!
! -----------------------------------------------------------------------
!
      end module t_spherical_harmonics
