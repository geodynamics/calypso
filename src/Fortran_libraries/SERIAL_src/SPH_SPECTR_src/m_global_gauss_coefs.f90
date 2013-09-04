!m_global_gauss_coefs.f90
!
!      module m_global_gauss_coefs
!
!     Written by H. Matsui on March, 2012
!
!      subroutine allocate_gauss_coef_monitor
!      subroutine allocate_gauss_global_coefs
!      subroutine deallocate_gauss_coef_monitor
!      subroutine deallocate_gauss_global_coefs
!
!      subroutine read_gauss_global_coefs
!      subroutine write_gauss_global_coefs
!
!
      module m_global_gauss_coefs
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ltr_w, jmax_w
      integer(kind = kint), allocatable :: index_w(:,:)
      real(kind = kreal), allocatable :: w_gauss(:)
      real(kind = kreal) :: r_gauss
!
      integer(kind = kint), parameter :: id_gauss = 12
      character(len = kchara) :: fhead_gauss
      character(len = kchara) :: fname_gauss
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine allocate_gauss_global_coefs
!
      integer(kind = kint) :: j, l, m
!
!
      jmax_w = ltr_w*(ltr_w+1)
!
      allocate(index_w(jmax_w,2))
      allocate(w_gauss(jmax_w))
!
      w_gauss(1:jmax_w) = 0.0d0
      do j = 1, jmax_w
        l = int(aint( sqrt(real(j)) ))
        m = j - l*(l + 1)
        index_w(j,1) = l
        index_w(j,2) = m
      end do
!
      end subroutine allocate_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine deallocate_gauss_global_coefs
!
!
      deallocate(index_w, w_gauss)
!
      end subroutine deallocate_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine read_gauss_global_coefs
!
      use skip_comment_f
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: l, m, j
      real(kind = kreal) :: rtmp
!
!
      open(id_gauss,file = fname_gauss)
!
      call skip_comment(character_4_read,id_gauss)
      read(character_4_read,*) ltr_w, r_gauss
!
      call allocate_gauss_global_coefs
!
      call skip_comment(character_4_read, id_gauss)
      read(character_4_read,*) l, m, rtmp
      j = l*(l+1) + m
      if(j .le. jmax_w) w_gauss(j) = rtmp
!
      do
        read(id_gauss,*,err=99,end=99) l, m, rtmp
        j = l*(l+1) + m
        if(j .le. jmax_w) w_gauss(j) = rtmp
      end do
!
  99  continue
      close(id_gauss)
!
!      write(*,*) 'j, index_w(j,1:2), w_gauss(j)'
!      do j = 1, jmax_w
!        write(*,*) j, index_w(j,1:2), w_gauss(j)
!      end do
!
      end subroutine read_gauss_global_coefs
!
!  -------------------------------------------------------------------
!
      subroutine write_gauss_global_coefs
!
!
      open(id_gauss,file = fname_gauss)
!
      write(id_gauss,'(a)') '#'
      write(id_gauss,'(a)') '# truncation, radius for potential'
      write(id_gauss,'(a)') '#'
!
      write(id_gauss,'(i10,1pe25.15e3)') ltr_w, r_gauss
!
!
      write(id_gauss,'(a)') '#'
      write(id_gauss,'(a)') '# degree, order, potential'
      write(id_gauss,'(a)') '#'
!
      close(id_gauss)
!
      end subroutine write_gauss_global_coefs
!
!  -------------------------------------------------------------------
!
      end module m_global_gauss_coefs
