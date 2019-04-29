!
!      module m_ludcmp_3band
!
!      modified by H. Matsui
!
!  ---------------------------------------------------------------------
!
!      SUBROUTINE ludcmp_3band(n, band_a, i_pivot, ierr, band_lu, d)
!
!c Given a matrix a(1:n,1:n), with physical dimension np by np,
!c this routine replaces it by the LU decomposition of a rowwise
!c permutation of itself. a and n are input.
!c a is output, arranged as in equation (2.3.14) above; index(1:n)
!c is an output vector that records the row permutation effected by
!c partial povoting; dis output as +/- 1 depending on whether
!c the number of row interchanges was even or odd, respectively.
!c This routine is used in combination with lubksb to solve
!c linear equations or ivert a matrix.
!
!
!      SUBROUTINE lubksb_3band(n, band_lu, i_pivot, x)
!
!c solve the set of n linear equations Ax=b. Here is a input,
!c not at the matrix A but rather as its LU decompsition,
!c determined by the routine ludcmp. i_pivot is input as the
!c permutation vector returned by ludcmp. b(1:n) is input
!c as the right-hand side vectror b, and returns with the
!c solution vector x.
!c a, n, np and i_pivot are not modified by this routine
!c and be left in place for successive calls with different
!c right hand sides b. This routine takes into account
!c the possibility that b will begin with many zero elements,
!c so it is efficient for use in matrix inversion.
! 
!*
!*               | a(2,1)  a(1,2)  ........     0         0     |
!*               | a(3,1)  a(2,2)  ........     .         .     |
!*               |   0     a(3,2)  ........     .         .     |
!*    a(i,j)  =  |   .       0     ........     0         .     |
!*               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!*               |   .       .     ........  a(1,N-2)     0     |
!*               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!*               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!
!   Original band matrix
!      band_a(i-j+iband+1,j) = a(i,j)
!      band_a(k,j) = a(k+j-iband-1,j)
!   3-band matrix
!      band_a(i-j+2,j) = a(i,j)
!      band_a(k,j) = a(k+j-2,j)
!   5-band matrix
!      band_lu(i-j+3,j) = a(i,j)
!      band_lu(k,j) = a(k+j-3,j)
!   7-band matrix
!      band_lu(i-j+4,j) = a(i,j)
!      band_lu(k,j) = a(k+j-4,j)
!   9-band matrix
!      band_lu(i-j+5,j) = a(i,j)
!      band_lu(k,j) = a(k+j-5,j)
!
      module m_ludcmp_3band
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
      SUBROUTINE ludcmp_3band(n, band_a, i_pivot, ierr, band_lu, d)
!
      INTEGER (kind = kint), intent(in) :: n
      REAL(kind = kreal), intent(in) :: band_a(3,n)
!
      INTEGER(kind = kint), intent(inout) :: ierr
      INTEGER(kind = kint), intent(inout) :: i_pivot(n)
      REAL(kind = kreal), intent(inout) :: band_lu(5,n)
!
      REAL(kind = kreal), intent(inout) :: d
!
!
      INTEGER  (kind = kint) :: i, j, k
      INTEGER  (kind = kint) :: kst, ked
      REAL(kind = kreal) :: aamax,dum
      integer(kind = kint), allocatable :: idx_org(:)
      REAL(kind = kreal), allocatable :: vwk(:)
 
!
      allocate(vwk(n), idx_org(n))
      vwk = 0.0d0
!
      d = 1.0d0
!
      band_lu = 0.0d0
      band_lu(3,1) = band_a(2,1)
      band_lu(4,1) = band_a(3,1)
      do j = 2, n-1
        band_lu(2,j) = band_a(1,j)
        band_lu(3,j) = band_a(2,j)
        band_lu(4,j) = band_a(3,j)
      end do
      band_lu(2,n) = band_a(1,n)
      band_lu(3,n) = band_a(2,n)
!
!
      do i = 1, n
        aamax = abs(band_lu(3,i))
        if(i .lt. n) aamax = max(aamax, abs(band_lu(itwo, i+ione)) )
        if(i .gt. 1) aamax = max(aamax, abs(band_lu(ifour,i-ione)) )
!
        if (aamax.eq.0.0d0) then
           write(*,*) 'singular matrix in ludcmp'
           ierr = 100
           return
        else
          vwk(i) = 1./aamax
        end if
        idx_org(i) = i
      end do
!
      do j = 1, n
!
        if(j .eq. 2) then
          band_lu(3,j) = band_lu(3,j) - band_lu(4,j-1)*band_lu(2,j)
        else if(j .gt. 2) then
          band_lu(2,j) = band_lu(2,j) - band_lu(4,j-2)*band_lu(1,j)
          band_lu(3,j) = band_lu(3,j) - band_lu(5,j-2)*band_lu(1,j)     &
     &                                - band_lu(4,j-1)*band_lu(2,j)
        end if
        i_pivot(j) = j
!
        if (j .lt. n) then
          aamax = vwk(j)*abs( band_lu(3,j) )
!
          if(j.gt.1)  band_lu(4,j) = band_lu(4,j)                       &
     &                             - band_lu(5,j-1)*band_lu(2,j)
          dum = vwk(j+1)*abs( band_lu(4,j) )
!
          if (dum .ge. aamax .and. j.le.idx_org(j) ) then
            k =             idx_org(j+1)
            idx_org(j+1) =  idx_org(j)
            idx_org(j) =    k
!
            i_pivot(j) = j+1
            kst = max(ione,  j-ione)
            ked = min(j+itwo,n)
            do k = kst, ked
              dum =              band_lu(j+4-k,k)
              band_lu(j+4-k,k) = band_lu(j+3-k,k)
              band_lu(j+3-k,k) = dum
            end do
            d = -d
            vwk(j+1) = vwk(j)
          end if
        end if
!
        if(band_lu(3,j).eq.0.0d0) band_lu(3,j) = TINY*TINY
        dum = 1.0d0/band_lu(3,j)
        if (j .lt. n) band_lu(4,j) = band_lu(4,j)*dum
!
      end do
!
      deallocate(vwk, idx_org)
      ierr = 0
!
      END subroutine ludcmp_3band
!
! ----------------------------------------------------------------------
!
      end module m_ludcmp_3band
