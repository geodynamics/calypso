!>@file   m_ludcmp_band.f90
!!@brief  module m_ludcmp_band
!!
!!@author H. Matsui
!!@date Programmed on 2007
!
!
!>@brief  linear solver using LU decompotion for band matrix
!!
!!@verbatim
!!---------------------------------------------------------------------
!!
!!
!!      SUBROUTINE ludcmp_band(n, nband, band_a, band_lu ,indx, d)
!!
!! Given a matrix a(1:n,1:n), with physical dimension n by n,
!! this routine replaces it by the LU decomposition of a rowwise
!! permutation of itself. a and n are input.
!! a is output, arranged as in equation (2.3.14) above; index(1:n)
!! is an output vector that records the row permutation effected by
!! partial povoting; dis output as +/- 1 depending on whether
!! the number of row interchanges was even or odd, respectively.
!! This routine is used in combination with lubksb to solve
!! linear equations or ivert a matrix.
!!
!!
!!      SUBROUTINE lubksb_band(n, nband, band_lu, indx, x)
!!      SUBROUTINE lubksb_band_mul(Msmp, Msmp_stack, mcomp, n, nband, &
!!     &          band_lu, indx, x)
!!
!! solve the set of n linear equations Ax=b. Here is a input,
!! not at the matrix A but rather as its LU decompsition,
!! determined by the routine ludcmp. i_pivot is input as the
!! permutation vector returned by ludcmp. x(1:n) is input
!! as the right-hand side vectror b, and returns with the
!! solution vector x.
!! a, n,  and i_pivot are not modified by this routine
!! and be left in place for successive calls with different
!! right hand sides b. This routine takes into account
!! the possibility that b will begin with many zero elements,
!! so it is efficient for use in matrix inversion.
!!
!!
!!   data converter between general matrix and band matrix
!!      subroutine set_band_mt_2_mat(n, np, nband, band_a, a)
!!      subroutine set_mt_2_band_mat(n, np, nband, a, band_a)
!!
!!      Data formt of band matrix
!!
!!               | a(2,1)  a(1,2)  ........     0         0     |
!!               | a(3,1)  a(2,2)  ........     .         .     |
!!               |   0     a(3,2)  ........     .         .     |
!!    a(i,j)  =  |   .       0     ........     0         .     |
!!               | ...... a(3,k-1)  a(2,k)  a(1,k+1) .......... |
!!               |   .       .     ........  a(1,N-2)     0     |
!!               |   .       .     ........  a(2,N-2)  a(1,N-1) |
!!               |   0       0     ........  a(3,N-2)  a(2,N-1) |
!!
!!   Original band matrix (band width: nb1)
!!      nb1 = = (nband-1) / 2
!!      band_a(i-j+nb1+1,j) = a(i,j)
!!      band_a(k,j) = a(k+j-nb1-1,j)
!!
!!      nb2 = nband-1
!!      band_lu(i-j+nb2+1,j) = a(i,j)
!!      band_lu(k,j) = a(k+j-nb2-1,j)
!!---------------------------------------------------------------------
!!@endverbatim
!
      module m_ludcmp_band
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
      subroutine ludcmp_band(n, nband, band_a, band_lu ,indx, d)
!
      integer (kind = kint), intent(in) :: n, nband
      real(kind = kreal), intent(in) :: band_a(nband,n)
!
      integer (kind = kint), intent(inout) :: indx(n)
      real(kind = kreal), intent(inout) :: band_lu(2*nband-1,n)
      real(kind = kreal), intent(inout) :: d
!
!      real(kind = kreal) :: a(n,n)
      real(kind = kreal), parameter :: TINY = 1.d-30
      real(kind = kreal) :: aamax,dum,sum
      integer(kind = kint), allocatable :: idx_org(:)
      real(kind = kreal), allocatable :: vv(:)
      integer(kind = kint) :: nb1, nb2
      integer(kind = kint) :: i, imax, j, k, l, m1, m2
      integer(kind = kint) :: ist, ied, jst, jed, kst, ked
!
!
      nb1 = (nband-1) / 2
      nb2 = nband-1
      allocate( vv(n), idx_org(n) )
      vv = 0.0d0
!
      d = 1.0d0
      do i = 1,n
        jst = max(i-nb1,1)
        jed = min(i+nb1,n)
        k = i - jst + nb1+1
        aamax = abs(band_a(k,jst))
!        aamax = abs(a(i,jst))
        do j = jst+1, jed
          k = i - j + nb1+1
          aamax = max(aamax, abs(band_a(k,j)))
!          aamax = max(aamax, abs(a(i,j)))
        end do
        if (aamax.eq.0.0d0) pause 'singular matrix in ludcmp_band'
        vv(i)=1.0d0 / aamax
        idx_org(i) = i
      end do
!
      band_lu = 0.0d0
      do i = 1, n
        jst = max(i-nb1,1)
        jed = min(i+nb1,n)
        do j = jst, jed
          k = i - j + nb1+1
          l = i - j + nb2+1
          band_lu(l,j) = band_a(k,j)
        end do
      end do
!
      do j = 1,n
        ist = max( (j-nb2),1 )
        ied = min( (j+nb1),n )
!
        do i = ist, j-1
            l = i - j + nb2+1
          do k = ist, i-1
            m1 = i - k + nb2+1
            m2 = k - j + nb2+1
            band_lu(l,j) = band_lu(l,j) - band_lu(m1,k) * band_lu(m2,j)
!            a(i,j) = a(i,j) - a(i,k) * a(k,j)
          end do
        end do
!
        aamax = 0.0d0
        do i = j, ied
          l = i - j + nb2+1
          kst = max( (i-nb2),1 )
          do k = kst, j-1
            m1 = i - k + nb2+1
            m2 = k - j + nb2+1
            band_lu(l,j) = band_lu(l,j) - band_lu(m1,k)*band_lu(m2,j)
!            a(i,j) = a(i,j) - a(i,k)*a(k,j)
          end do
          dum = vv(i)*abs( band_lu(l,j) )
!          dum = vv(i)*abs( a(i,j) )
          if (dum.ge.aamax .and. i.le.(idx_org(j)+nb1)) then
            imax = i
            aamax = dum
          end if
        end do
!
        if (j.ne.imax) then
          k =             idx_org(imax)
          idx_org(imax) = idx_org(j)
          idx_org(j) = k
!
          kst = max( (j-nb2),1 )
          ked = min( (imax+nb2),n ) 
          do k = kst, ked
            m1 = j - k + nb2+1
            m2 = imax - k + nb2+1
            dum =           band_lu(m2,k)
            band_lu(m2,k) = band_lu(m1,k)
            band_lu(m1,k) = dum
!            dum = a(imax,k)
!            a(imax,k) = a(j,k)
!            a(j,k) = dum
          end do
          d = -d
          vv(imax) = vv(j)
        end if
        indx(j) = imax
!
        k = i - j + nb2+1
        if(band_lu(nb2+1,j).eq.0.0d0) band_lu(nb2+1,j) = TINY
!        if(a(j,j).eq.0.0d0) a(j,j) = TINY
        if(j .ne. n) then
!          dum = 1.0d0 / a(j,j)
          dum = 1.0d0 / band_lu(nb2+1,j)
          ied = min( (j+nb2),n )
          do i = j+1, ied
            k = i - j + nb2+1
            band_lu(k,j) = band_lu(k,j) * dum
!            a(i,j) = a(i,j) * dum
          end do
        end if
      end do
!
!      write(*,*) 'idx_org',  idx_org
      deallocate( vv, idx_org )
!
      end subroutine ludcmp_band
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_band(n, nband, band_lu, indx, x)
!
      integer (kind = kint), intent(in) :: n, nband
      integer (kind = kint), intent(in) :: indx(n)
      real(kind = kreal), intent(inout) :: band_lu(2*nband-1,n)
      real(kind = kreal), intent(inout) :: x(n)
!
!      real(kind = kreal), intent(in) :: a(n,n)
      integer(kind = kint) :: ist, i, j, ll, jst, jed, nb2, k
      real(kind = kreal) :: sum
!
!
      nb2 = nband-1
!
      do i = 1, n
        ll =    indx(i)
        sum =   x(ll)
        x(ll) = x(i)
        x(i) =  sum
      end do
!
      do i = 1, n
        if (x(i) .ne. 0.0d0) then
          ist = i
          exit
        end if
      end do
!
!     x(ist) = x(ist)
      do i = ist+1, n
        jst = max( (i-nb2),1 )
        do j = jst,i-1
          k = i - j + nb2+1
          x(i) = x(i) - band_lu(k,j) * x(j)
!          x(i) = x(i) - a(i,j)*x(j)
        end do
      end do
!
      do i = n, 1, -1
        jed = min( (i+nb2),n )
        do j = i+1, jed
          k = i - j + nb2+1
          x(i) = x(i) - band_lu(k,j) * x(j)
!          x(i) = x(i) - a(i,j)*x(j)
        end do
        x(i) = x(i) / band_lu(nb2+1,i)
!        x(i) = x(i) / a(i,i)
      end do
!
      END subroutine lubksb_band
!
! ----------------------------------------------------------------------
!
      subroutine lubksb_band_mul(Msmp, Msmp_stack, mcomp, n, nband,     &
     &          band_lu, indx, x)
!
      integer (kind = kint), intent(in) :: Msmp, mcomp, n, nband
      integer (kind = kint), intent(in) :: Msmp_stack(0:Msmp)
      integer (kind = kint), intent(in) :: indx(n,mcomp)
      real(kind = kreal), intent(inout) :: band_lu(2*nband-1,n,mcomp)
      real(kind = kreal), intent(inout) :: x(mcomp,n)
!
!      real(kind = kreal), intent(in) :: a(n,n)
      integer(kind = kint) :: i, j, ll, nb2, k, m, mp
      integer(kind = kint) :: ii, ist, mst, med, jst,jed
      real(kind = kreal) :: sum
!
!
      nb2 = nband-1
!
!$omp parallel do private (ist,jst,jed,mst,med,i,ii,j,k,m,ll,sum)
      do mp = 1, Msmp
        mst = Msmp_stack(mp-1) + 1
        med = Msmp_stack(mp)
!
        do i = 1, n
          do m = mst, med
            ll =      indx(i,m)
            sum =     x(m,ll)
            x(m,ll) = x(m,i)
            x(m,i) =  sum
          end do
        end do
!
        ist = n
        do m = mst, med
          do i = 1, n
            if (x(m,i) .ne. 0.0d0) then
              ii = i
              exit
            end if
          end do
          ist = min(ii,ist)
        end do
!
        do i = ist+1, n
          jst = max( (i-nb2),1 )
          do j = jst,i-1
            k = i - j + nb2+1
            x(mst:med,i) = x(mst:med,i)                                 &
     &                    - band_lu(k,j,mst:med) * x(mst:med,j)
          end do
        end do
!
        do i = n, 1, -1
          jed = min( (i+nb2),n )
          do j = i+1, jed
            k = i - j + nb2+1
            x(mst:med,i) = x(mst:med,i)                                 &
     &                    - band_lu(k,j,mst:med) * x(mst:med,j)
          end do
          x(mst:med,i) = x(mst:med,i) / band_lu(nb2+1,i,mst:med)
        end do
!
      end do
!$omp end parallel do
!
      end subroutine lubksb_band_mul
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_band_mt_2_mat(n, np, nband, band_a, a)
!
      integer (kind = kint), intent(in) :: n, np, nband
      real(kind = kreal), intent(in) :: band_a(nband,n)
      real(kind = kreal), intent(inout) :: a(np,np)
      integer(kind = kint) :: i, j, k, nb1, jst, jed
!
!
      nb1 = (nband-1) / 2
      do i = 1, n
        jst = max( (i-nb1),1 )
        jed = min( (i+nb1),n )
        do j = jst, jed
          k = i - j + nb1+1
          a(i,j) = band_a(k,j)
        end do
      end do
!
      end subroutine set_band_mt_2_mat
!
! ----------------------------------------------------------------------
!
      subroutine set_mt_2_band_mat(n, np, nband, a, band_a)
!
      integer (kind = kint), intent(in) :: n, np, nband
      real(kind = kreal), intent(in) :: a(np,np)
      real(kind = kreal), intent(inout) :: band_a(nband,n)
      integer(kind = kint) :: i, j, k, nb1, jst, jed
!
!
      nb1 = (nband-1) / 2
      do i = 1, n
        jst = max( (i-nb1),1 )
        jed = min( (i+nb1),n )
        do j = jst, jed
          k = i - j + nb1+1
           band_a(k,j) = a(i,j)
        end do
      end do
!
      end subroutine set_mt_2_band_mat
!
! ----------------------------------------------------------------------
!
      end module m_ludcmp_band
