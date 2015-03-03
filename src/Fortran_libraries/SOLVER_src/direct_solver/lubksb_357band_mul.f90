!>@file   lubksb_357band_mul.f90
!!@brief  module lubksb_357band_mul
!!
!!@author H. Matsui
!!@date Programmed on 2007
!
!>@brief  Solve more than one 3, 5, or 7 band matriox at once
!!         after LU decomposition
!!
!!@verbatim
!!---------------------------------------------------------------------
!!      subroutine lubksb_3band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_lu, i_pivot, x)
!!      subroutine lubksb_5band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_lu, i_pivot, x)
!!      subroutine lubksb_7band_mul(Msmp, Msmp_stack, mcomp, n,         &
!!     &          band_lu, i_pivot, x)
!!
!! solve the set of n linear equations Ax=b. Here is band_lu input 
!! matrix after LU decompsition, determined by the routine ludcmp_band.
!! i_pivot is input as the
!! permutation vector returned by ludcmp. b(1:n) is input
!! as the right-hand side vectror b, and returns with the
!! solution vector x.
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
      module lubksb_357band_mul
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_3band_mul(Msmp, Msmp_stack, mcomp, n,           &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(n,mcomp)
      real(kind = kreal), intent(in) :: band_lu(5,n,mcomp)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(i,m)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
        x(mst:med,2) = x(mst:med,2)                                     &
     &                  - band_lu(4,1,mst:med) * x(mst:med,1)
        do i = 3, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(5,i-2,mst:med)*x(mst:med,i-2)         &
     &                  - band_lu(4,i-1,mst:med)*x(mst:med,i-1)
        end do
!
        x(mst:med,n)   =  x(mst:med,n) / band_lu(3,n,mst:med)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                  - band_lu(2,n,mst:med) * x(mst:med,n))          &
     &                  / band_lu(3,n-1,mst:med)
!
        do i = n-2,1,-1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                  - band_lu(2,i+1,mst:med) * x(mst:med,i+1)       &
     &                  - band_lu(1,i+2,mst:med) * x(mst:med,i+2) )     &
     &                  / band_lu(3,i,mst:med)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine lubksb_3band_mul
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_5band_mul(Msmp, Msmp_stack, mcomp, n,           &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(n,mcomp)
      real(kind = kreal), intent(in) :: band_lu(9,n,mcomp)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(i,m)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
        x(mst:med,2) = x(mst:med,2) - band_lu(6,1,mst:med)*x(mst:med,1)
        x(mst:med,3) = x(mst:med,3) - band_lu(7,1,mst:med)*x(mst:med,1) &
     &                              - band_lu(6,2,mst:med)*x(mst:med,2)
        x(mst:med,4) = x(mst:med,4) - band_lu(8,1,mst:med)*x(mst:med,1) &
     &                              - band_lu(7,2,mst:med)*x(mst:med,2) &
     &                              - band_lu(6,3,mst:med)*x(mst:med,3)
        do i = 5, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(9,i-4,mst:med)*x(mst:med,i-4)         &
     &                  - band_lu(8,i-3,mst:med)*x(mst:med,i-3)         &
     &                  - band_lu(7,i-2,mst:med)*x(mst:med,i-2)         &
     &                  - band_lu(6,i-1,mst:med)*x(mst:med,i-1)
        end do
!
        x(mst:med,n) =    x(mst:med,n) / band_lu(5,n,mst:med)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                   - band_lu(4,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(5,n-1,mst:med)
        x(mst:med,n-2) = (x(mst:med,n-2)                                &
     &                   - band_lu(4,n-1,mst:med) * x(mst:med,n-1)      &
     &                   - band_lu(3,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(5,n-2,mst:med)
        x(mst:med,n-3) = (x(mst:med,n-3)                                &
     &                   - band_lu(4,n-2,mst:med) * x(mst:med,n-2)      &
     &                   - band_lu(3,n-1,mst:med) * x(mst:med,n-1)      &
     &                   - band_lu(2,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(5,n-3,mst:med)
        do i = n-4, 1, -1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                   - band_lu(4,i+1,mst:med) * x(mst:med,i+1)      &
     &                   - band_lu(3,i+2,mst:med) * x(mst:med,i+2)      &
     &                   - band_lu(2,i+3,mst:med) * x(mst:med,i+3)      &
     &                   - band_lu(1,i+4,mst:med) * x(mst:med,i+4))     &
     &                  / band_lu(5,i,mst:med)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine lubksb_5band_mul
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_7band_mul(Msmp, Msmp_stack, mcomp, n,           &
     &          band_lu, i_pivot, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: i_pivot(n,mcomp)
      real(kind = kreal), intent(in) :: band_lu(13,n,mcomp)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
      real(kind = kreal) :: sum
      integer(kind = kint) :: i, ii, ll, ist
      integer(kind = kint) :: mp, mst, med, m
!
!
!$omp parallel do private (ist,mst,med,i,ii,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      i_pivot(i,m)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
!
        x(mst:med,2) = x(mst:med,2)                                     &
     &                - band_lu(8, 1,mst:med)*x(mst:med,1)
        x(mst:med,3) = x(mst:med,3)                                     &
     &                - band_lu(9, 1,mst:med)*x(mst:med,1)              &
     &                - band_lu(8, 2,mst:med)*x(mst:med,2)
        x(mst:med,4) = x(mst:med,4)                                     &
     &                - band_lu(10,1,mst:med)*x(mst:med,1)              &
     &                - band_lu(9, 2,mst:med)*x(mst:med,2)              &
     &                - band_lu(8, 3,mst:med)*x(mst:med,3)
        x(mst:med,5) = x(mst:med,5)                                     &
     &                - band_lu(11,1,mst:med)*x(mst:med,1)              &
     &                - band_lu(10,2,mst:med)*x(mst:med,2)              &
     &                - band_lu(9, 3,mst:med)*x(mst:med,3)              &
     &                - band_lu(8, 4,mst:med)*x(mst:med,4)
        x(mst:med,6) = x(mst:med,6)                                     &
     &                - band_lu(12,1,mst:med)*x(mst:med,1)              &
     &                - band_lu(11,2,mst:med)*x(mst:med,2)              &
     &                - band_lu(10,3,mst:med)*x(mst:med,3)              &
     &                - band_lu(9, 4,mst:med)*x(mst:med,4)              &
     &                - band_lu(8, 5,mst:med)*x(mst:med,5)
        do i = 7, n
          x(mst:med,i) = x(mst:med,i)                                   &
     &                  - band_lu(13,i-6,mst:med)*x(mst:med,i-6)        &
     &                  - band_lu(12,i-5,mst:med)*x(mst:med,i-5)        &
     &                  - band_lu(11,i-4,mst:med)*x(mst:med,i-4)        &
     &                  - band_lu(10,i-3,mst:med)*x(mst:med,i-3)        &
     &                  - band_lu(9, i-2,mst:med)*x(mst:med,i-2)        &
     &                  - band_lu(8, i-1,mst:med)*x(mst:med,i-1)
        end do
!
        x(mst:med,n) =   x(mst:med,n) / band_lu(7,n,  mst:med)
        x(mst:med,n-1) = (x(mst:med,n-1)                                &
     &                   - band_lu(6,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(7,n-1,mst:med)
        x(mst:med,n-2) = (x(mst:med,n-2)                                &
     &                   - band_lu(6,n-1,mst:med) * x(mst:med,n-1)      &
     &                   - band_lu(5,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(7,n-2,mst:med)
        x(mst:med,n-3) = (x(mst:med,n-3)                                &
     &                   - band_lu(6,n-2,mst:med) * x(mst:med,n-2)      &
     &                   - band_lu(5,n-1,mst:med) * x(mst:med,n-1)      &
     &                   - band_lu(4,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(7,n-3,mst:med)
        x(mst:med,n-4) = (x(mst:med,n-4)                                &
     &                   - band_lu(6,n-3,mst:med) * x(mst:med,n-3)      &
     &                   - band_lu(5,n-2,mst:med) * x(mst:med,n-2)      &
     &                   - band_lu(4,n-1,mst:med) * x(mst:med,n-1)      &
     &                   - band_lu(3,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(7,n-4,mst:med)
        x(mst:med,n-5) = (x(mst:med,n-5)                                &
     &                   - band_lu(6,n-4,mst:med) * x(mst:med,n-4)      &
     &                   - band_lu(5,n-3,mst:med) * x(mst:med,n-3)      &
     &                   - band_lu(4,n-2,mst:med) * x(mst:med,n-2)      &
     &                   - band_lu(3,n-1,mst:med) * x(mst:med,n-1)      &
     &                   - band_lu(2,n,  mst:med) * x(mst:med,n  ))     &
     &                  / band_lu(7,n-5,mst:med)
        do i = n-6, 1, -1
          x(mst:med,i) = (x(mst:med,i)                                  &
     &                   - band_lu(6,i+1,mst:med) * x(mst:med,i+1)      &
     &                   - band_lu(5,i+2,mst:med) * x(mst:med,i+2)      &
     &                   - band_lu(4,i+3,mst:med) * x(mst:med,i+3)      &
     &                   - band_lu(3,i+4,mst:med) * x(mst:med,i+4)      &
     &                   - band_lu(2,i+5,mst:med) * x(mst:med,i+5)      &
     &                   - band_lu(1,i+6,mst:med) * x(mst:med,i+6))     &
     &                  / band_lu(7,i,mst:med)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine lubksb_7band_mul
!
! ----------------------------------------------------------------------
!
      end module lubksb_357band_mul
