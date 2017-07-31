!>@file   t_global_gauss_coefs.f90
!!@brief  module t_global_gauss_coefs
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Gauss coefficients data for output
!!
!!@verbatim
!!      subroutine alloc_gauss_coef_monitor(d_gauss)
!!      subroutine alloc_gauss_global_coefs(d_gauss)
!!      subroutine dealloc_gauss_coef_monitor(d_gauss)
!!      subroutine dealloc_gauss_global_coefs(d_gauss)
!!
!!      subroutine read_gauss_global_coefs(d_gauss)
!!      subroutine write_gauss_global_coefs(d_gauss)
!!@endverbatim
!
!
      module t_global_gauss_coefs
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter :: id_gauss = 12
!
      type global_gauss_points
        integer(kind = kint) :: ltr_w
        integer(kind = kint) :: jmax_w
        integer(kind = kint), allocatable :: index_w(:,:)
        real(kind = kreal), allocatable :: w_gauss(:)
        real(kind = kreal) :: r_gauss
!
        character(len = kchara) :: fhead_gauss
        character(len = kchara) :: fname_gauss
      end type global_gauss_points
!
      private :: id_gauss
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine alloc_gauss_global_coefs(d_gauss)
!
      type(global_gauss_points), intent(inout) :: d_gauss
!
      integer(kind = kint) :: j, l, m
!
!
      d_gauss%jmax_w = d_gauss%ltr_w*(d_gauss%ltr_w+2)
!
      allocate(d_gauss%index_w(d_gauss%jmax_w,2))
      allocate(d_gauss%w_gauss(d_gauss%jmax_w))
!
      d_gauss%w_gauss(1:d_gauss%jmax_w) = 0.0d0
      do l = 1, d_gauss%ltr_w
        do m = -l, l
          j = l*(l + 1) + m
          d_gauss%index_w(j,1) = l
          d_gauss%index_w(j,2) = m
        end do
      end do
!
      end subroutine alloc_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine dealloc_gauss_global_coefs(d_gauss)
!
      type(global_gauss_points), intent(inout) :: d_gauss
!
!
      deallocate(d_gauss%index_w, d_gauss%w_gauss)
!
      end subroutine dealloc_gauss_global_coefs
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine read_gauss_global_coefs(d_gauss)
!
      use skip_comment_f
!
      type(global_gauss_points), intent(inout) :: d_gauss
!
      character(len=255) :: character_4_read
      integer(kind = kint) :: l, m, j
      real(kind = kreal) :: rtmp
!
!
      open(id_gauss,file = d_gauss%fname_gauss)
!
      call skip_comment(character_4_read,id_gauss)
      read(character_4_read,*) d_gauss%ltr_w, d_gauss%r_gauss
!
      call alloc_gauss_global_coefs(d_gauss)
!
      call skip_comment(character_4_read, id_gauss)
      read(character_4_read,*) l, m, rtmp
      j = l*(l+1) + m
      if(j .le. d_gauss%jmax_w) d_gauss%w_gauss(j) = rtmp
!
      do
        read(id_gauss,*,err=99,end=99) l, m, rtmp
        j = l*(l+1) + m
        if(j .le. d_gauss%jmax_w) d_gauss%w_gauss(j) = rtmp
      end do
!
  99  continue
      close(id_gauss)
!
      write(*,*) 'j, index_w(j,1:2), w_gauss(j)'
      do j = 1, d_gauss%jmax_w
        write(*,*) j, d_gauss%index_w(j,1:2), d_gauss%w_gauss(j)
      end do
!
      end subroutine read_gauss_global_coefs
!
!  -------------------------------------------------------------------
!
      subroutine write_gauss_global_coefs(d_gauss)
!
      type(global_gauss_points), intent(in) :: d_gauss
!
!
      open(id_gauss,file = d_gauss%fname_gauss)
!
      write(id_gauss,'(a)') '#'
      write(id_gauss,'(a)') '# truncation, radius for potential'
      write(id_gauss,'(a)') '#'
!
      write(id_gauss,'(i16,1pe25.15e3)') d_gauss%ltr_w, d_gauss%r_gauss
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
      end module t_global_gauss_coefs
