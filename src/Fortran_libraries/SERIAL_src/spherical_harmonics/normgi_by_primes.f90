!normgi_by_primes.f90
      module normgi_by_primes
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint) :: nmax_prms, num_fact
      integer(kind = kint), allocatable :: int_prime_ks(:)
      integer(kind = kint), allocatable :: num_prm_ks(:)
      integer(kind = kint), allocatable :: itbl_prime_ks(:)
      integer(kind = kint), allocatable :: iprm_ks(:,:)
      integer(kind = kint), allocatable :: iexp_ks(:,:)
      integer(kind = kint), allocatable :: iexp_gi(:)
      integer(kind = kint), allocatable :: iexp_gsi(:)
      integer(kind = kint), allocatable :: iexp_ti(:)
!
      private :: int_prime_ks, num_prm_ks, nmax_prms, num_fact
      private :: iprm_ks, iexp_ks, iexp_gi, iexp_gsi, iexp_ti
!
!      double precision function gs_prime(m1,l1,m2,l2,m3,l3)
!
!  ---------------------------------------------------------------------
!*
      contains
!
!  ---------------------------------------------------------------------
!*
      subroutine alloc_int_prime_ks
!
!
      allocate( int_prime_ks(nmax_prms) )
      allocate( num_prm_ks(num_fact) )
      allocate( itbl_prime_ks(num_fact) )
      allocate( iprm_ks(nmax_prms,num_fact) )
      allocate( iexp_ks(nmax_prms,num_fact) )
      int_prime_ks =  0
      itbl_prime_ks = 0
      num_prm_ks =    0
      itbl_prime_ks = 0
      iprm_ks = 0
      iexp_ks = 0
!
!
      allocate( iexp_gi(nmax_prms) )
      allocate( iexp_gsi(nmax_prms) )
      allocate( iexp_ti(nmax_prms) )
!
      iexp_gi =  0
      iexp_gsi = 0
      iexp_ti =  0
!
      end subroutine alloc_int_prime_ks
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!*
      subroutine dealloc_int_prime_ks
!
!
      deallocate(iexp_gi, iexp_gsi, iexp_ti)
      deallocate(int_prime_ks, num_prm_ks, itbl_prime_ks)
      deallocate(iprm_ks, iexp_ks)
!
      end subroutine dealloc_int_prime_ks
!
!  ---------------------------------------------------------------------
!*
      subroutine resize_int_prime_ks(imax_fact, nmax_prm_ks)
!
      integer(kind = kint), intent(in) :: imax_fact, nmax_prm_ks
!
      nmax_prms = nmax_prm_ks
      num_fact =  imax_fact
      if(allocated(int_prime_ks)) call dealloc_int_prime_ks
      call alloc_int_prime_ks
!
      end subroutine resize_int_prime_ks
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!*
      double precision function gs_prime(m1,l1,m2,l2,m3,l3)
!*
      use quicksort
      use primefac
!
!      real(kind = kreal) ::  gs_prime
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      integer(kind = kint) kiu(0:2), kil(0:4), ktu(0:2), ktl(0:4)
      integer(kind = kint) ksu(0:4), ksl(0:2)
      integer(kind = kint) :: itmin, itmax, is, it, i2exp
      integer(kind = kint) :: i, m, nmax_prm_ks
      real(kind = kreal) ::  tsum, tfac
!
!
      call conut_prims_normgi(m1,l1,m2,l2,m3,l3)
!
      is = int((l1+l2+l3)/2)
!*
      itmax = min ( (l1-m1) ,(l3-m3) ,(l2+l3-m1))
      itmin = -( min ( (l2-l3+m1) ,(l1+m1) , 0 ) )
!*
!*
      kil(0) = 0
      kil(1) = is-l1
      kil(2) = is-l2
      kil(3) = is-l3
      kil(4) = 2*is+1
      kiu(0) = 0
      kiu(1) = 2*is-2*l3
      kiu(2) = is
      ksu(0) = 0
      ksu(1) = l1-m1
      ksu(2) = l2+m2
      ksu(3) = l3+m3
      ksu(4) = l3-m3
      ksl(0) = 0
      ksl(1) = l1+m1
      ksl(2) = l2-m2
!*
      call quicksort_int(5,kil(0),1,5)
      call quicksort_int(3,kiu(0),1,3)
      call quicksort_int(3,ksl(0),1,3)
      call quicksort_int(5,ksu(0),1,5)
!
!
      iexp_gi = 0
      do m = 1, 2
        call const_prime_table_4_factor(num_fact, nmax_prms,            &
     &      itbl_prime_ks(1), num_prm_ks, iprm_ks(1,1), iexp_ks(1,1),   &
     &      (kiu(m-1)+1) ,kiu(m), (3-m), iexp_gi(1) )
      end do
!
      do m = 1, 4
        call const_prime_table_4_factor(num_fact, nmax_prms,            &
     &      itbl_prime_ks(1), num_prm_ks, iprm_ks(1,1), iexp_ks(1,1),   &
     &      (kil(m-1)+1) ,kil(m), (m-5), iexp_gi(1) )
      end do
!
!
      iexp_gsi = 0
      do m = 1, 4
        call const_prime_table_4_factor(num_fact, nmax_prms,            &
     &      itbl_prime_ks(1), num_prm_ks, iprm_ks(1,1), iexp_ks(1,1),   &
     &      (ksu(m-1)+1) ,ksu(m), (5-m), iexp_gsi(1) )
      end do
!
      do m = 1, 2
        call const_prime_table_4_factor(num_fact, nmax_prms,            &
     &      itbl_prime_ks(1), num_prm_ks, iprm_ks(1,1), iexp_ks(1,1),   &
     &      (ksl(m-1)+1) ,ksl(m), (m-3), iexp_gsi(1) )
      end do
!
      tsum = 0
      do it = itmin ,itmax
        ktu(0) = 0
        ktu(1) = l1+m1+it
        ktu(2) = l2+l3-m1-it
        ktl(0) = 0
        ktl(1) = l1-m1-it
        ktl(2) = l2-l3+m1+it
        ktl(3) = l3-m3-it
        ktl(4) = it
!*
!*
        call quicksort_int(3,ktu(0),1,3)
        call quicksort_int(5,ktl(0),1,5)
!*
        iexp_ti = 0
        do m = 1, 2
          call const_prime_table_4_factor(num_fact, nmax_prms,          &
     &        itbl_prime_ks(1), num_prm_ks(1), iprm_ks(1,1),            &
     &        iexp_ks(1,1), (ktu(m-1)+1) ,ktu(m), (3-m), iexp_ti(1) )
        end do
!
        do m = 1, 4
          call const_prime_table_4_factor(num_fact, nmax_prms,          &
     &        itbl_prime_ks(1), num_prm_ks(1), iprm_ks(1,1),            &
     &        iexp_ks(1,1), (ktl(m-1)+1) ,ktl(m), (m-5), iexp_ti(1) )
        end do
!
        tfac = (-1)**it
        do i = 1, nmax_prms
          i2exp = 2*iexp_gi(i) + 2*iexp_ti(i) + iexp_gsi(i)
          tfac = tfac * sqrt(dble(int_prime_ks(i))**i2exp)
        end do
        tsum = tsum + tfac
      end do
!
      gs_prime = (-one)**(is-l2-m3) * two
      gs_prime = gs_prime*tsum
!*
      if (m1 .ne. 0)  then
        gs_prime = gs_prime * sqrt(two)
      end if
      if (m2 .ne. 0)  then
        gs_prime = gs_prime * sqrt(two)
      end if
      if (m3 .ne. 0)  then
        gs_prime = gs_prime * sqrt(two)
      end if
!*
      call dealloc_int_prime_ks
!
      end function gs_prime
!
!-----------------------------------------------------------------------
!*
      subroutine conut_prims_normgi(m1,l1,m2,l2,m3,l3)
!*
      use quicksort
      use primefac
!
      integer(kind = kint), intent(in) :: l1, l2, l3
      integer(kind = kint), intent(in) :: m1, m2, m3
!
      integer(kind = kint) kiu(0:2), kil(0:4), ktu(0:2), ktl(0:4)
      integer(kind = kint) ksu(0:4), ksl(0:2)
      integer(kind = kint) :: itmin, itmax, is, it
      integer(kind = kint) :: i, j, NUMBER
!
!
      is = int((l1+l2+l3)/2)
!
      itmax = min ( (l1-m1) ,(l3-m3) ,(l2+l3-m1))
      itmin = -( min ( (l2-l3+m1) ,(l1+m1) , 0 ) )
!
!
      kil(0) = 0
      kil(1) = is-l1
      kil(2) = is-l2
      kil(3) = is-l3
      kil(4) = 2*is+1
      kiu(0) = 0
      kiu(1) = 2*is-2*l3
      kiu(2) = is
      ksu(0) = 0
      ksu(1) = l1-m1
      ksu(2) = l2+m2
      ksu(3) = l3+m3
      ksu(4) = l3-m3
      ksl(0) = 0
      ksl(1) = l1+m1
      ksl(2) = l2-m2
!*
      call quicksort_int(5,kil(0),1,5)
      call quicksort_int(3,kiu(0),1,3)
      call quicksort_int(3,ksl(0),1,3)
      call quicksort_int(5,ksu(0),1,5)
!
      num_fact = max(kil(4),kiu(2),ksu(4),ksl(2))
!
      do it = itmin ,itmax
        ktu(0) = 0
        ktu(1) = l1+m1+it
        ktu(2) = l2+l3-m1-it
        ktl(0) = 0
        ktl(1) = l1-m1-it
        ktl(2) = l2-l3+m1+it
        ktl(3) = l3-m3-it
        ktl(4) = it
!*
        call quicksort_int(3,ktu(0),1,3)
        call quicksort_int(5,ktl(0),1,5)
!*
        num_fact = max(num_fact,ktu(2),ktl(4))
      end do
!
      call countprimes(num_fact, nmax_prms)
!
      call alloc_int_prime_ks
!
      call setprimes(num_fact, nmax_prms, int_prime_ks(1))
!
      itbl_prime_ks = 0
      do i = 1, nmax_prms
        j = int_prime_ks(i)
        itbl_prime_ks(j) = i
      end do
!
      do i = 2, num_fact
        NUMBER = i
        call prmfac(NUMBER, nmax_prms, num_prm_ks(i),                   &
     &      iprm_ks(1,i), iexp_ks(1,i) )
      end do
      num_prm_ks(1) = 0
!
      end subroutine conut_prims_normgi
!
!-----------------------------------------------------------------------
!
      subroutine const_prime_table_4_factor(num_fact, nmax_prms,        &
     &          itbl_prime, num_prime, iprime, iexp, kst, ked,          &
     &          order, iexp_fact)
!
      integer(kind = kint), intent(in) :: order
      integer(kind = kint), intent(in) :: kst, ked
      integer(kind = kint), intent(in) :: num_fact, nmax_prms
      integer(kind = kint), intent(in) :: num_prime(nmax_prms)
      integer(kind = kint), intent(in) :: iprime(nmax_prms,num_fact)
      integer(kind = kint), intent(in) :: iexp(nmax_prms,num_fact)
      integer(kind = kint), intent(in) :: itbl_prime(num_fact)
      integer(kind = kint), intent(inout) :: iexp_fact(nmax_prms)
      integer(kind = kint) :: i, j, k, l
!
!
      do i = kst, ked
        do j = 1, num_prime(i)
          l = iprime(j,i)
          k = itbl_prime(l)
          iexp_fact(k) = iexp_fact(k) + order * iexp(j,i)
        end do
      end do
!
      end subroutine const_prime_table_4_factor
!
!  ---------------------------------------------------------------------
!
      end module normgi_by_primes
