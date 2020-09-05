!>@file   gauss_points_IO.f90
!!        module gauss_points_IO
!!
!!@author H. Matsui
!!@date   Programmed in 2003
!!
!!
!> @brief file IO for Gauss-Legendre points
!!
!!@verbatim
!!      subroutine write_gauss_points(file_name, gauss)
!!        character(len=kchara), intent(in) :: file_name
!!        type(gauss_points), intent(in) :: gauss
!!      subroutine read_alloc_gauss_points(file_name, gauss)
!!        character(len=kchara), intent(in) :: file_name
!!        type(gauss_points), intent(in) :: gauss
!!      integer(kind = kint) function compare_gauss_point_data          &
!!     &                            (gauss_1, gauss_2)
!!        type(gauss_points), intent(in) :: gauss_2, gauss_1
!!@endverbatim
!
      module gauss_points_IO
!
      use m_precision
      use m_constants
      use t_gauss_points
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!>       Check Gauss points
      subroutine write_gauss_points(file_name, gauss)
!
      character(len=kchara), intent(in) :: file_name
      type(gauss_points), intent(in) :: gauss
!
      integer(kind = kint), parameter :: id_file = 11
      integer (kind = kint) :: i
!
!
      open(id_file, file=file_name)
!
      write(id_file,'(a)') 'Number of points'
      write(id_file,'(i5)')  gauss%n_point
      write(id_file,'(a)') 'gauss points and coefficients'
      do i = 1, gauss%n_point
        write(id_file,'(i5,1p2E25.15e3)')                               &
     &         i, gauss%point(i), gauss%weight(i)
      end do
!
      write(id_file,'(a)') 'Gauss-Legendre colatitude'
      do i = 1, gauss%n_point
        write(id_file,'(i5,1p3E25.15e3)') i, gauss%point(i),            &
     &         gauss%colat(i), gauss%colat_deg(i)
      end do
!
      write(id_file,'(a)') 'Azimuth'
      do i = 1, 2 * gauss%n_point
        write(id_file,'(i5,1p2E25.15e3)')                               &
     &         i, gauss%azimuth(i), gauss%azim_deg(i)
      end do
      close(id_file)
!
      end subroutine write_gauss_points
!
! -----------------------------------------------------------------------
!>       Check Gauss points
      subroutine read_alloc_gauss_points(file_name, gauss)
!
      character(len=kchara), intent(in) :: file_name
      type(gauss_points), intent(inout) :: gauss
!
      integer(kind = kint), parameter :: id_file = 12
      integer (kind = kint) :: i, num, itmp
      real(kind = kreal) :: rtmp
      character(len=kchara) :: tmpchara
!
!
      open(id_file, file=file_name, status='old')
!
      read(id_file,'(a)') tmpchara
      read(id_file,*)  num
      call alloc_gauss_points(num, gauss)
      call alloc_gauss_colatitude(gauss)
!
      read(id_file,'(a)') tmpchara
      do i = 1, gauss%n_point
        read(id_file,*) itmp, gauss%point(i), gauss%weight(i)
      end do
!
      read(id_file,'(a)') tmpchara
      do i = 1, gauss%n_point
        read(id_file,'(i5,1p3E25.15e3)') itmp, rtmp,                    &
     &         gauss%colat(i), gauss%colat_deg(i)
      end do
!
      read(id_file,'(a)') tmpchara
      do i = 1, 2 * gauss%n_point
        read(id_file,'(i5,1p2E25.15e3)')                                &
     &         itmp, gauss%azimuth(i), gauss%azim_deg(i)
      end do
      close(id_file)
!
      end subroutine read_alloc_gauss_points
!
! -----------------------------------------------------------------------
!>       Check Gauss points
      integer(kind = kint) function compare_gauss_point_data            &
     &                            (gauss_1, gauss_2)
!
      type(gauss_points), intent(in) :: gauss_2, gauss_1
!
      integer (kind = kint) :: i
      real(kind = kreal) :: diff
!
!
      compare_gauss_point_data = 0
      if(gauss_2%n_point .ne. gauss_1%n_point) then
        write(*,*) 'Error in number of gauss points'
        compare_gauss_point_data = 1
        return
      end if
!
      do i = 1, gauss_1%n_point
        diff = gauss_2%point(i) -  gauss_1%point(i)
        if(abs(diff) .gt. TINY) compare_gauss_point_data = 1
        diff = gauss_2%weight(i) -  gauss_1%weight(i)
        if(abs(diff) .gt. TINY) compare_gauss_point_data = 2
      end do
!
      do i = 1, gauss_1%n_point
        diff = gauss_2%colat(i) -  gauss_1%colat(i)
        if(abs(diff) .gt. TINY) compare_gauss_point_data = 11
      end do
!
      do i = 1, 2 * gauss_1%n_point
        diff = gauss_2%azimuth(i) -  gauss_1%azimuth(i)
        if(abs(diff) .gt. TINY) compare_gauss_point_data = 21
      end do
!
      end function compare_gauss_point_data
!
! -----------------------------------------------------------------------
!
      end module gauss_points_IO
