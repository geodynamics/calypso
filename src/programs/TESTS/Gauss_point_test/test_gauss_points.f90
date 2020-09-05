!
!      program test_gauss_points
!
      program test_gauss_points
!
      use m_precision
      use t_gauss_points
      use gauss_points_IO
!
      implicit none
!
      character(len=kchara), parameter :: file_prefix = 'gauss_point'
!
      integer (kind = kint) :: i, nth_g
      type(gauss_points) :: gauss_pt
      character(len=kchara) :: charaint, file_name
!
   10 continue
!
      write(*,*) 'imput number of points (end: negative values)'
      read(*,*) nth_g
!
      write(charaint,*) nth_g
      write(file_name,'(a,a,a4)')                                       &
     &       trim(file_prefix), trim(ADJUSTL(charaint)), '.dat'
!
      if (nth_g.le.0) go to 999
!
      call const_gauss_colatitude(nth_g, gauss_pt)
!
      call write_gauss_points(file_name, gauss_pt)
      write(*,*) 'Gauss points are written in ', trim(file_name)
!
      call dealloc_gauss_colatitude(gauss_pt)
!
      go to 10
!
!
 999  continue
      stop
      end program test_gauss_points
