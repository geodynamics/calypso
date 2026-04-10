!>@file   check_sph_radial_mat.f90
!!@brief  module check_sph_radial_mat
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Output band matrix data to check
!!
!!@verbatim
!!      subroutine check_radial_3band_mat(id_file, nri, jmax,           &
!!     &                                  j_sph, rr, mat)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        integer(kind = kint), intent(in) :: j_sph(jmax,3)
!!        real(kind = kreal), intent(in) :: rr(nri)
!!        real(kind = kreal), intent(in) :: mat(3,nri,jmax)
!!      subroutine check_radial_5band_mat(id_file, nri, jmax,           &
!!     &                                  j_sph, rr, mat)
!!        real(kind = kreal), intent(in) :: mat(5,nri,jmax)
!!      subroutine check_radial_7band_mat(id_file, nri, jmax,           &
!!     &                                  j_sph, rr, mat)
!!        real(kind = kreal), intent(in) :: mat(7,nri,jmax)
!!      subroutine check_radial_9band_mat(id_file, nri, jmax,           &
!!     &                                  j_sph, rr, mat)
!!        real(kind = kreal), intent(in) :: mat(9,nri,jmax)
!!@endverbatim
!!
!!@n @param id_file   File ID
!!@n @param nri       Number of radial points
!!@n @param jmax      Number of modes
!!@n @param j_sph     Modes
!!@n @param rr        radius
!!@n @param mat       Band matrix
!
      module check_sph_radial_mat
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
      subroutine check_radial_3band_mat(id_file, nri, jmax,             &
     &                                  j_sph, rr, mat)
!
      use check_single_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(id_file,'(a,4i6)') '(j, global_j, l, m): ',               &
     &                            j, j_sph(j,1:3)
        call check_single_radial_3band_mat                              &
     &     (id_file, nri, rr, mat(1,1,j))
      end do
!
      end subroutine check_radial_3band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_radial_5band_mat(id_file, nri, jmax,             &
     &                                  j_sph, rr, mat)
!
      use check_single_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(5,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(id_file,'(a,4i6)') '(j, global_j, l, m): ',               &
     &                            j, j_sph(j,1:3)
        call check_single_radial_5band_mat(id_file, nri, rr,            &
     &                                     mat(1,1,j))
      end do
!
      end subroutine check_radial_5band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_radial_7band_mat(id_file, nri, jmax,             &
     &                                  j_sph, rr, mat)
!
      use check_single_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(7,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(id_file,'(a,4i6)') '(j, global_j, l, m): ',               &
     &                            j, j_sph(j,1:3)
        call check_single_radial_7band_mat(id_file, nri, rr,            &
     &                                     mat(1,1,j))
      end do
!
      end subroutine check_radial_7band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_radial_9band_mat(id_file, nri, jmax,             &
     &                                  j_sph, rr, mat)
!
      use check_single_radial_mat
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(9,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(id_file,'(a,4i6)') '(j, global_j, l, m): ',               &
     &                            j, j_sph(j,1:3)
        call check_single_radial_9band_mat(id_file, nri, rr,            &
     &                                     mat(1,1,j))
      end do
!
      end subroutine check_radial_9band_mat
!
! -----------------------------------------------------------------------
!
      end module check_sph_radial_mat
