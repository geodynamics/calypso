!>@file   check_single_radial_mat.f90
!!@brief  module check_single_radial_mat
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Output single band matrix data to check
!!
!!@verbatim
!!      subroutine check_single_radial_3band_mat(id_file, nri,  rr, mat)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: rr(nri)
!!        real(kind = kreal), intent(in) :: mat(3,nri)
!!      subroutine check_single_radial_5band_mat(id_file, nri,  rr, mat)
!!        real(kind = kreal), intent(in) :: mat(5,nri)
!!      subroutine check_single_radial_7band_mat(id_file, nri,  rr, mat)
!!        real(kind = kreal), intent(in) :: mat(7,nri)
!!      subroutine check_single_radial_9band_mat(id_file, nri,  rr, mat)
!!        real(kind = kreal), intent(in) :: mat(9,nri)
!!
!!      subroutine check_radial_3band_mat_w_ctr(id_file, nri, rr, mat00)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: rr(nri)
!!        real(kind = kreal), intent(in) :: mat00(3,0:nri)
!!@endverbatim
!!
!!@n @param id_file   File ID
!!@n @param nri       Number of radial points
!!@n @param jmax      Number of modes
!!@n @param j_sph     Modes
!!@n @param rr        radius
!!@n @param mat       Band matrix
!
      module check_single_radial_mat
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
      subroutine check_single_radial_3band_mat(id_file, nri, rr, mat)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(3,nri)
!
      integer(kind = kint) :: k
!
!
      write(id_file,'(a)')                                              &
     &          'k, r, a(k,k-1), a(k,k), a(k,k+1)'
      write(id_file,'(i6,1p4E25.15e3)') ione, rr(1),                    &
     &              -1.0d30, mat(2,1), mat(1,2)
      do k = 2, nri-1
        write(id_file,'(i6,1p4E25.15e3)') k, rr(k),                     &
     &              mat(3,k-1), mat(2,k), mat(1,k+1)
      end do
      write(id_file,'(i6,1p4E25.15e3)') nri, rr(nri),                   &
     &              mat(3,nri-1), mat(2,nri), 1.0d30
!
      end subroutine check_single_radial_3band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_single_radial_5band_mat(id_file, nri, rr, mat)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(5,nri)
!
      integer(kind = kint) :: k, l
!
!
      write(id_file,'(a)')                                              &
     &          'k, r, a(k,k-2), a(k,k-1), a(k,k), a(k,k+1), a(k,k+2)'
      write(id_file,'(i6,1p6E25.15e3)') ione, rr(1),                    &
     &                           -1.0d30, -1.0d30, (mat(3-l,1+l),l=0,2)
      write(id_file,'(i6,1p6E25.15e3)') itwo, rr(2),                    &
     &                                   -1.0d30, (mat(3-l,2+l),l=-1,2)
      do k = 3, nri-2
        write(id_file,'(i6,1p6E25.15e3)') k, rr(k),                     &
     &                                            (mat(3-l,k+l),l=-2,2)
      end do
      write(id_file,'(i6,1p6E25.15e3)') (nri-1), rr(nri-1),             &
     &                               (mat(3-l,nri-1+l),l=-2,1), 1.0d30
      write(id_file,'(i6,1p6E25.15e3)') nri, rr(nri),                   &
     &                         (mat(3-l,nri+l),l=-2,0), 1.0d30, 1.0d30
!
      end subroutine check_single_radial_5band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_single_radial_7band_mat(id_file, nri, rr, mat)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(7,nri)
!
      integer(kind = kint) :: k, l
!
!
      write(id_file,'(a,a)')                                            &
     &       'k, r, a(k,k-3), a(k,k-2), a(k,k-1), a(k,k), ',            &
     &       'a(k,k+1), a(k,k+2), a(k,k+3)'
!
      write(id_file,'(i6,1p8E25.15e3)') ione,   rr(1),                  &
     &                  -1.0d30, -1.0d30, -1.0d30, (mat(4-l,1+l),l=0,3)
      write(id_file,'(i6,1p8E25.15e3)') itwo,   rr(2),                  &
     &                          -1.0d30, -1.0d30, (mat(4-l,2+l),l=-1,3)
      write(id_file,'(i6,1p8E25.15e3)') ithree, rr(3),                  &
     &                                   -1.0d30, (mat(4-l,3+l),l=-2,3)
      do k = 4, nri-3
        write(id_file,'(i6,1p8E25.15e3)') k, rr(k),                     &
     &                                            (mat(4-l,k+l),l=-3,3)
      end do
      write(id_file,'(i6,1p8E25.15e3)') (nri-2), rr(nri-2),             &
     &                               (mat(4-l,nri-2+l),l=-3,2), 1.0d30
      write(id_file,'(i6,1p8E25.15e3)') (nri-1), rr(nri-1),             &
     &                       (mat(4-l,nri-1+l),l=-3,1), 1.0d30, 1.0d30
      write(id_file,'(i6,1p8E25.15e3)') nri,     rr(nri  ),             &
     &               (mat(4-l,nri  +l),l=-3,0), 1.0d30, 1.0d30, 1.0d30
!
      end subroutine check_single_radial_7band_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_single_radial_9band_mat(id_file, nri, rr, mat)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat(9,nri)
!
      integer(kind = kint) :: k, l
!
!
      write(id_file,'(a,a)')                                            &
     &       'k, r, a(k,k-4), a(k,k-3), a(k,k-2), a(k,k-1), a(k,k), ',  &
     &       'a(k,k+1), a(k,k+2), a(k,k+3), a(k,k+4)'
!
      write(id_file,'(i6,1p10E25.15e3)') ione,   rr(1),                 &
     &        -1.0d30, -1.0d30, -1.0d30, -1.0d30, (mat(5-l,1+l),l= 0,4)
      write(id_file,'(i6,1p10E25.15e3)') itwo,   rr(2),                 &
     &                 -1.0d30, -1.0d30, -1.0d30, (mat(5-l,2+l),l=-1,4)
      write(id_file,'(i6,1p10E25.15e3)') ithree, rr(3),                 &
     &                          -1.0d30, -1.0d30, (mat(5-l,3+l),l=-2,4)
      write(id_file,'(i6,1p10E25.15e3)') ifour,  rr(4),                 &
     &                                   -1.0d30, (mat(5-l,4+l),l=-3,4)
      do k = 5, nri-4
        write(id_file,'(i6,1p10E25.15e3)') k, rr(k),                    &
     &                                            (mat(5-l,k+l),l=-4,4)
      end do
      write(id_file,'(i6,1p10E25.15e3)') (nri-3), rr(nri-3),            &
     &        (mat(5-l,nri-3+l),l=-4,3), 1.0d30
      write(id_file,'(i6,1p10E25.15e3)') (nri-2), rr(nri-2),            &
     &        (mat(5-l,nri-2+l),l=-4,2), 1.0d30, 1.0d30
      write(id_file,'(i6,1p10E25.15e3)') (nri-1), rr(nri-1),            &
     &        (mat(5-l,nri-1+l),l=-4,1), 1.0d30, 1.0d30, 1.0d30
      write(id_file,'(i6,1p10E25.15e3)') nri,     rr(nri  ),            &
     &        (mat(5-l,nri  +l),l=-4,0), 1.0d30, 1.0d30, 1.0d30, 1.0d30
!
      end subroutine check_single_radial_9band_mat
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_radial_3band_mat_w_ctr(id_file, nri, rr, mat00)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: rr(nri)
      real(kind = kreal), intent(in) :: mat00(3,0:nri)
!
      integer(kind = kint) :: k
!
!
      write(id_file,'(a)') 'k, r, a(k,k-1), a(k,k), a(k,k+1)'
      write(id_file,'(i6,1p4E25.15e3)')  izero, zero,                   &
     &              -1.0d30, mat00(2,0), mat00(1,1)
      do k = 1, nri-1
        write(id_file,'(i6,1p4E25.15e3)') k, rr(k),                     &
     &              mat00(3,k-1), mat00(2,k), mat00(1,k+1)
      end do
      write(id_file,'(i6,1p4E25.15e3)') nri, rr(nri),                   &
     &             mat00(3,nri-1), mat00(2,nri), 1.0d30
!
      end subroutine check_radial_3band_mat_w_ctr
!
! -----------------------------------------------------------------------
!
      end module check_single_radial_mat
