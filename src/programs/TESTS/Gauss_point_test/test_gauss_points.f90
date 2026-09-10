!>@file   test_gauss_points.f90
!!@brief  program test_gauss_points
!!
!!@author H. Matsui
!!@date Programmed in 2012
!!
!!
!>@brief Test program for gauss point position and coefficients
!!
!!@verbatim
!!    Command with option
!!       % gauss_points [# of points]
!!    Command without option  (Follow messages in the program)
!!       % gauss_points
!!
!!    Data file: gauss_point[# of points].dat
!!@endverbatim
!!
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
      logical :: flag_command_parameter = .TRUE.
      integer (kind = kint) :: nth_g
      type(gauss_points) :: gauss_pt
      character(len=kchara) :: charaint, file_name
!
!
      if(command_argument_count() .lt. 1) then
        flag_command_parameter = .FALSE.
      end if
!
   10 continue
!
      if(flag_command_parameter) then
        call get_command_argument(1, charaint)
        read(charaint,*) nth_g
      else
        write(*,*) 'Input number of points (end: negative values)'
        read(*,*) nth_g
        write(charaint,*) nth_g
      end if
!
      write(file_name,'(a,a,a4)')                                       &
     &       trim(file_prefix), trim(ADJUSTL(charaint)), '.dat'
!
      if(nth_g .le. 0) go to 999
!
      call const_gauss_colatitude(nth_g, gauss_pt)
!
      call write_gauss_points(file_name, gauss_pt)
      write(*,*) 'Gauss points are written in ', trim(file_name)
!
      call dealloc_gauss_colatitude(gauss_pt)
!
      if(flag_command_parameter) go to 999
      go to 10
!
!
 999  continue
      stop
      end program test_gauss_points
