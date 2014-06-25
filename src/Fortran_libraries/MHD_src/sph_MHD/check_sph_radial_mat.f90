!>@file   check_sph_radial_mat.f90
!!@brief  module check_sph_radial_mat
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Output band matrix data to check
!!
!!@verbatim
!!      subroutine check_radial_3band_mat_w_ctr(my_rank, nri, rr, mat00)
!!
!!      subroutine check_radial_3band_mat(my_rank, nri, jmax, j_sph,    &
!!    &           rr, mat)
!!      subroutine check_radial_5band_mat(my_rank, nri, jmax, j_sph,    &
!!    &           rr, mat)
!!      subroutine check_radial_7band_mat(my_rank, nri, jmax, j_sph,    &
!!    &           rr, mat)
!!
!!      subroutine check_single_radial_3band_mat(my_rank, nri,  rr, mat)
!!      subroutine check_single_radial_5band_mat(my_rank, nri,  rr, mat)
!!      subroutine check_single_radial_7band_mat(my_rank, nri,  rr, mat)
!!@endverbatim
!!
!!@n @param my_rank   radius to three next points of ICB
!!@n @param nri       radius to three next points of ICB
!!@n @param jmax      radius to three next points of ICB
!!@n @param j_sph     radius to three next points of ICB
!!@n @param rr        radius to three next points of ICB
!!@n @param mat       radius to three next points of ICB
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
      subroutine check_radial_3band_mat_w_ctr(my_rank, nri, rr, mat00)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat00(3,0:nri)
!
      integer(kind = kint) :: k
!
!
      write(50+my_rank,'(a)') 'k, r, a(k,k-1), a(k,k), a(k,k+1)'
      write(50+my_rank,'(i6,1p4E25.15e3)')  izero, zero,                &
     &              -1.0d30, mat00(2,0), mat00(1,1)
      do k = 1, nri-1
        write(50+my_rank,'(i6,1p4E25.15e3)') k, rr(k),                  &
     &              mat00(3,k-1), mat00(2,k), mat00(1,k+1)
      end do
      write(50+my_rank,'(i6,1p4E25.15e3)') nri, rr(nri),                &
     &             mat00(3,nri-1), mat00(2,nri), 1.0d30
!
      end subroutine check_radial_3band_mat_w_ctr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_radial_3band_mat(my_rank, nri, jmax, j_sph,      &
     &           rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(50+my_rank,'(a,4i6)') '(j, global_j, l, m): ',            &
     &                            j, j_sph(jmax,1:3)
        call check_single_radial_3band_mat                              &
     &     (my_rank, nri, rr, mat(1,1,j))
      end do
!
      end subroutine check_radial_3band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_radial_5band_mat(my_rank, nri, jmax, j_sph,      &
     &           rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(5,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(50+my_rank,'(a,4i6)') '(j, global_j, l, m): ',            &
     &                            j, j_sph(jmax,1:3)
        call check_single_radial_5band_mat                              &
     &     (my_rank, nri, rr, mat(1,1,j))
      end do
!
      end subroutine check_radial_5band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_radial_7band_mat(my_rank, nri, jmax, j_sph,      &
     &           rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri, jmax
      integer(kind = kint), intent(in) :: j_sph(jmax,3)
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(7,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        write(50+my_rank,'(a,4i6)') '(j, global_j, l, m): ',            &
     &                            j, j_sph(jmax,1:3)
        call check_single_radial_7band_mat                              &
     &     (my_rank, nri, rr, mat(1,1,j))
      end do
!
      end subroutine check_radial_7band_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_single_radial_3band_mat(my_rank, nri, rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(3,nri)
!
      integer(kind = kint) :: k
!
!
      write(50+my_rank,'(a)')                                           &
     &          'k, r, a(k,k-1), a(k,k), a(k,k+1)'
      write(50+my_rank,'(i6,1p4E25.15e3)') ione, rr(1),                 &
     &              -1.0d30, mat(2,1), mat(1,2)
      do k = 2, nri-1
        write(50+my_rank,'(i6,1p4E25.15e3)') k, rr(k),                  &
     &              mat(3,k-1), mat(2,k), mat(1,k+1)
      end do
      write(50+my_rank,'(i6,1p4E25.15e3)') nri, rr(nri),                &
     &              mat(3,nri-1), mat(2,nri), 1.0d30
!
      end subroutine check_single_radial_3band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_single_radial_5band_mat(my_rank, nri, rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(5,nri)
!
      integer(kind = kint) :: k, l
!
!
      write(50+my_rank,'(a)')                                           &
     &          'k, r, a(k,k-2), a(k,k-1), a(k,k), a(k,k+1), a(k,k+2)'
      write(50+my_rank,'(i6,1p6E25.15e3)') ione, rr(1),                 &
     &              -1.0d30, -1.0d30, (mat(3-l,1+l),l=0,2)
      write(50+my_rank,'(i6,1p6E25.15e3)') itwo, rr(2),                 &
     &              -1.0d30, (mat(3-l,2+l),l=-1,2)
      do k = 3, nri-2
        write(50+my_rank,'(i6,1p6E25.15e3)') k, rr(k),                  &
     &             (mat(3-l,k+l),l=-2,2)
      end do
      write(50+my_rank,'(i6,1p6E25.15e3)') (nri-1), rr(nri-1),          &
     &              (mat(3-l,nri-1+l),l=-2,1), 1.0d30
      write(50+my_rank,'(i6,1p6E25.15e3)') nri, rr(nri),                &
     &              (mat(3-l,nri+l),l=-2,0), 1.0d30, 1.0d30
!
      end subroutine check_single_radial_5band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_single_radial_7band_mat(my_rank, nri,  rr, mat)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(7,nri)
!
      integer(kind = kint) :: k, l
!
!
      write(50+my_rank,'(a,a)')                                         &
     &       'k, r, a(k,k-3), a(k,k-2), a(k,k-1), a(k,k), ',            &
     &       'a(k,k+1), a(k,k+2), a(k,k+3)'
!
      write(50+my_rank,'(i6,1p8E25.15e3)') ione,   rr(1),               &
     &              -1.0d30, -1.0d30, -1.0d30, (mat(4-l,1+l),l=0,3)
      write(50+my_rank,'(i6,1p8E25.15e3)') itwo,   rr(2),               &
     &              -1.0d30, -1.0d30, (mat(4-l,1+l),l=-1,3)
      write(50+my_rank,'(i6,1p8E25.15e3)') ithree, rr(3),               &
     &              -1.0d30, (mat(4-l,2+l),l=-2,3)
      do k = 4, nri-3
        write(50+my_rank,'(i6,1p8E25.15e3)') k, rr(k),                  &
     &              (mat(4-l,k+l),l=-3,3)
      end do
      write(50+my_rank,'(i6,1p8E25.15e3)') (nri-2), rr(nri-2),          &
     &              (mat(4-l,nri-1+l),l=-3,2), 1.0d30
      write(50+my_rank,'(i6,1p8E25.15e3)') (nri-1), rr(nri-1),          &
     &              (mat(4-l,nri+l),l=-3,1), 1.0d30, 1.0d30
      write(50+my_rank,'(i6,1p8E25.15e3)') nri,     rr(nri  ),          &
     &              (mat(4-l,nri+l),l=-3,0), 1.0d30, 1.0d30, 1.0d30
!
      end subroutine check_single_radial_7band_mat
!
! -----------------------------------------------------------------------
!
      end module check_sph_radial_mat
