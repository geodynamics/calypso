!>@file   check_sph_radial_mat.f90
!!@brief  module check_sph_radial_mat
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Output band matrix data to check
!!
!!@verbatim
!!      subroutine check_radial_3band_mat(my_rank, nri, jmax, j_sph,    &
!!    &           rr, mat)
!!      subroutine check_radial_5band_mat(my_rank, nri, jmax, j_sph,    &
!!    &           rr, mat)
!!      subroutine check_radial_7band_mat(my_rank, nri, jmax, j_sph,    &
!!    &           rr, mat)
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
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
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
      integer(kind = kint) :: i, j, k
!
!
      write(50+my_rank,'(a)') 'j, l, m, k, r, a(k,k-1), a(k,k), a(k,k+1)'
      do j = 1, jmax
!
        i = 1 + (j-1)*nri
        write(50+my_rank,'(5i6,1p4E25.15e3)') j, j_sph(j,1:3), 1,       &
     &              rr(1), -1.0d30, mat(2,1,j), mat(1,2,j)
        do k = 2, nri-1
          i = k + (j-1)*nri
          write(50+my_rank,'(5i6,1p4E25.15e3)') j, j_sph(j,1:3), k,     &
     &              rr(k), mat(3,k-1,j), mat(2,k,j), mat(1,k+1,j)
        end do
        i = nri + (j-1)*nri
        write(50+my_rank,'(5i6,1p4E25.15e3)') j, j_sph(j,1:3), nri,     &
     &              rr(nri), mat(3,nri-1,j), mat(2,nri,j), 1.0d30
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
      integer(kind = kint) :: i, j, k, l
!
!
      write(50+my_rank,'(a)') 'j, l, m, k, r, a(k,k-1), a(k,k), a(k,k+1)'
      do j = 1, jmax
!
        i = 1 + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), 1,       &
     &              rr(1), -1.0d30, -1.0d30, (mat(3-l,1+l,j),l=0,2)
        i = 2 + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), 2,       &
     &              rr(2), -1.0d30, (mat(3-l,2+l,j),l=-1,2)
        do k = 3, nri-2
          i = k + (j-1)*nri
          write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), k,     &
     &              rr(k), (mat(3-l,k+l,j),l=-2,2)
        end do
        i = (nri-1) + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), (nri-1), &
     &              rr(nri-1), (mat(3-l,nri-1+l,j),l=-2,1), 1.0d30
        i = nri + (j-1)*nri
        write(50+my_rank,'(5i6,1p6E25.15e3)') j, j_sph(j,1:3), nri,     &
     &              rr(nri), (mat(3-l,nri+l,j),l=-2,0), 1.0d30, 1.0d30
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
      integer(kind = kint) :: i, j, k, l
!
!
      write(50+my_rank,'(a)')                                           &
     &  'j, l, m, k, r, a(k,k-2), a(k,k-1), a(k,k), a(k,k+1), a(k,k+2)'
      do j = 1, jmax
!
        i = 1 + (j-1)*nri
        write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), 1,       &
     &              rr(1), -1.0d30, -1.0d30, (mat(4-l,1+l,j),l=0,3)
        i = 2 + (j-1)*nri
        write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), 1,       &
     &              rr(1), -1.0d30, -1.0d30, (mat(4-l,1+l,j),l=-1,3)
        i = 3 + (j-1)*nri
        write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), 2,       &
     &              rr(2), -1.0d30, (mat(4-l,2+l,j),l=-2,3)
        do k = 4, nri-3
          i = k + (j-1)*nri
          write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), k,     &
     &              rr(k), (mat(4-l,k+l,j),l=-3,3)
        end do
        i = (nri-2) + (j-1)*nri
        write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), (nri-1), &
     &              rr(nri-1), (mat(4-l,nri-1+l,j),l=-3,2), 1.0d30
        i = (nri-1) + (j-1)*nri
        write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), nri,     &
     &              rr(nri), (mat(4-l,nri+l,j),l=-3,1), 1.0d30, 1.0d30
        i = nri + (j-1)*nri
        write(50+my_rank,'(5i6,1p8E25.15e3)') j, j_sph(j,1:3), nri,     &
     &              rr(nri), (mat(4-l,nri+l,j),l=-3,0), 1.0d30, 1.0d30
      end do
!
      end subroutine check_radial_7band_mat
!
! -----------------------------------------------------------------------
!
      end module check_sph_radial_mat
