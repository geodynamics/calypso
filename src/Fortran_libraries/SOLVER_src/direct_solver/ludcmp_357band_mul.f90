!>@file   ludcmp_357band_mul.f90
!!@brief  module ludcmp_357band_mul
!!
!!@author H. Matsui
!!@date Programmed on 2007
!
!>@brief  LU decomposition of 3, 5, or 7 band matrices at once
!!
!!@verbatim
!!---------------------------------------------------------------------
!!      subroutine ludcmp_3band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_a, band_lu, i_pivot, d)
!!      subroutine ludcmp_5band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_a, band_lu, i_pivot, d)
!!      subroutine ludcmp_7band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_a, band_lu, i_pivot, d)
!!
!! Given a matrix a(1:n,1:n), with physical dimension np by np,
!! this routine replaces it by the LU decomposition of a rowwise
!! permutation of itself. a and n are input.
!! a is output, arranged as in equation (2.3.14) above; index(1:n)
!! is an output vector that records the row permutation effected by
!! partial povoting; dis output as +/- 1 depending on whether
!! the number of row interchanges was even or odd, respectively.
!! This routine is used in combination with lubksb to solve
!! linear equations or ivert a matrix.
!
!!
!! band_lu, n, np and i_pivot are not modified by this routine
!! and be left in place for successive calls with different
!! right hand sides b. This routine takes into account
!! the possibility that b will begin with many zero elements,
!! so it is efficient for use in matrix inversion.
!!
!!
!!   Format of band matrix
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Arbitorary band matrix
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
!!   9-band matrix
!!      band_lu(i-j+5,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-5,j)
!!---------------------------------------------------------------------
!!@endverbatim
!
      module ludcmp_357band_mul
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_3band_mul(Msmp, Msmp_stack, mcomp, n,           &
     &          band_a, band_lu, i_pivot, d)
!
      use m_ludcmp_3band
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      real(kind = kreal), intent(in) :: band_a(3,n,mcomp)
!
      integer(kind = kint), intent(inout) :: i_pivot(n,mcomp)
      real(kind = kreal), intent(inout) :: band_lu(5,n,mcomp)
      real(kind = kreal), intent(inout) :: d(mcomp)
!
      integer(kind = kint) :: ip, mst, med, i, ierr_MP
!
!
      ierr_MP = 0
!$omp parallel do private(ip,mst,med,i) reduction(max:ierr_MP)
      do ip = 1, Msmp
        mst = Msmp_stack(ip-1) + 1
        med = Msmp_stack(ip)
        do i = mst, med
          call ludcmp_3band(n, band_a(1,1,i), i_pivot(1,i), ierr_MP,    &
     &      band_lu(1,1,i), d(i))
        end do
      end do
!$omp end parallel do
!
      if(ierr_MP .eq. 100) write(*,*) 'Singler matrices are there'
!
      end subroutine ludcmp_3band_mul
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_5band_mul(Msmp, Msmp_stack, mcomp, n,           &
     &          band_a, band_lu, i_pivot, d)
!
      use m_ludcmp_band
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      real(kind = kreal), intent(in) :: band_a(5,n,mcomp)
!
      integer (kind = kint), intent(inout) :: i_pivot(n,mcomp)
      real(kind = kreal), intent(inout) :: band_lu(9,n,mcomp)
      real(kind = kreal), intent(inout) :: d(mcomp)
!
      integer(kind = kint) :: ip, mst, med, i
!
!
!$omp parallel do private(mst,med,i)
      do ip = 1, Msmp
        mst = Msmp_stack(ip-1) + 1
        med = Msmp_stack(ip)
        do i = mst, med
          call ludcmp_band(n, ifive, band_a(1,1,i), band_lu(1,1,i),     &
     &        i_pivot(1,i), d(i))
        end do
      end do
!$omp end parallel do
!
      end subroutine ludcmp_5band_mul
!
! ----------------------------------------------------------------------
!
      subroutine ludcmp_7band_mul(Msmp, Msmp_stack, mcomp, n,           &
     &          band_a, band_lu, i_pivot, d)
!
      use m_ludcmp_band
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      real(kind = kreal), intent(in) :: band_a(7,n,mcomp)
!
      integer (kind = kint), intent(inout) :: i_pivot(n,mcomp)
      real(kind = kreal), intent(inout) :: band_lu(13,n,mcomp)
      real(kind = kreal), intent(inout) :: d(mcomp)
!
      integer(kind = kint) :: ip, mst, med, i
!
!
!$omp parallel do private(ip,mst,med,i)
      do ip = 1, Msmp
        mst = Msmp_stack(ip-1) + 1
        med = Msmp_stack(ip)
        do i = mst, med
          call ludcmp_band(n, iseven, band_a(1,1,i), band_lu(1,1,i),    &
     &        i_pivot(1,i), d(i))
        end do
      end do
!$omp end parallel do
!
      end subroutine ludcmp_7band_mul
!
! ----------------------------------------------------------------------
!
      end module ludcmp_357band_mul
