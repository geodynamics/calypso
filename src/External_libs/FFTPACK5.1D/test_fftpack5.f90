!
!
      program test_ispack_fft
!
!
      use FFTPACK5_wrapper
!
      implicit none
!
      integer(kind = kint), parameter :: Nfft = 120
      real(kind = kreal) :: xsm(4,Nfft), xcm(4,Nfft)
      real(kind = kreal) :: xst(4,Nfft), xct(4,0:Nfft)
      real(kind = kreal) :: x(8,Nfft)
      real(kind = kreal) :: pi
!
      real(kind = kreal), allocatable :: t(:)
!
      real(kind = kreal), allocatable :: wsave(:)
      real(kind = kreal), allocatable :: work(:)
      integer(kind = kint) :: it(5*Nfft)
!
      integer(kind = kint), parameter :: ncomp_s = 4, ncomp_c = 4
      integer(kind = kint), parameter :: ncomp = 8
      integer(kind = kint), parameter :: Nsmp = 2
      integer(kind = kint), parameter :: Nstack_c(0:2) = (/0,2,4/)
      integer(kind = kint), parameter :: Nstacksmp(0:2) = (/0,4,8/)
      integer(kind = kint) :: lenwrk, lensav
!
      integer(kind = kint) :: i, j, ierr, half_o, half_e
!
      half_o = (Nfft)/2-1
      half_e = (Nfft+1)/2-1
      write(*,*) 'half address for Nfft', Nfft, half_e
      write(*,*) 'half address for (Nfft-1)', Nfft-1, half_e
!
      pi = 4.0d0*atan(1.0d0)
!
      do i = 1, Nfft
        do j = 1, ncomp_c
          xsm(j,i) = sin( dble(j)*pi*dble(2*i-1) / dble(2*Nfft) )
          xcm(j,i) = cos( dble(j)*pi*dble(2*i-1) / dble(2*Nfft) )
          x(2*j-1,i) = sin( dble(2*j)*pi*dble(i-1) / dble(Nfft) )
          x(2*j,  i) = cos( dble(2*j)*pi*dble(i-1) / dble(Nfft) )
        end do
      end do
      do i = 1, Nfft-1
        do j = 1, ncomp_c
          xst(j,i) = sin( dble(j)*pi*dble(i) / dble(Nfft) )
        end do
      end do
      do i = 0, Nfft
        do j = 1, ncomp_c
          xct(j,i) = cos( dble(j)*pi*dble(i) / dble(Nfft) )
        end do
      end do
!
        write(25,*) 'original'
      do i = 1, Nfft
        write(25,'(1p8E25.15e3)') x(1:8,i)
      end do
!
      call verify_work_4_FFTPACK(Nsmp, Nstacksmp, Nfft)
      call RFFTMF_kemo(Nsmp, Nstacksmp, ncomp, Nfft, x)
!
!
        write(25,*) 'spectr'
      do i = 1, Nfft
        write(25,'(1p8E25.15e3)') x(1:8,i)
      end do
!
      call RFFTMB_kemo(Nsmp, Nstacksmp, ncomp, Nfft, x)
!
        write(25,*) 'reversed'
      do i = 1, Nfft
        write(25,'(1p8E25.15e3)') x(1:8,i)
      end do
!
      close(25)
!
      end program test_ispack_fft
