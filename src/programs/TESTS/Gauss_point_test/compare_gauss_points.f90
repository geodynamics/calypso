!>@file   compare_gauss_points.f90
!!@brief  program compare_gauss_points
!!
!!@author H. Matsui
!!@date Programmed in 2012
!!
!!
!>@brief Compare gauss point position and coefficients from reference
!
      program compare_gauss_points
!
      use m_precision
      use t_gauss_points
      use gauss_points_IO
!
      implicit none
!
      type(gauss_points) :: gauss_pt_1, gauss_pt_2
      character(len=kchara) :: file_name1, file_name2
      integer(kind = kint) :: iflag
!
      if(command_argument_count() .lt. 2) then
        write(*,*) 'compare_gauss_points FILE1 FILE2'
        write(*,'(i1)') 1
        stop
      end if
!
      call get_command_argument(1, file_name1)
      call get_command_argument(2, file_name2)
!
      call read_alloc_gauss_points(file_name1, gauss_pt_1)
      call read_alloc_gauss_points(file_name2, gauss_pt_2)
      iflag = compare_gauss_point_data(gauss_pt_1, gauss_pt_2)
!
      call dealloc_gauss_colatitude(gauss_pt_1)
      call dealloc_gauss_colatitude(gauss_pt_2)
!
      if(iflag .eq. 0) then
        write(*,*) 'Data are cosistent'
      else if(iflag .eq. 1) then
        write(*,*) 'Gauss points are incosistent'
      else if(iflag .eq. 2) then
        write(*,*) 'Weights are incosistent'
      else if(iflag .eq. 3) then
        write(*,*) 'Gauss-Legendre latitude are incosistent'
      else if(iflag .eq. 4) then
        write(*,*) 'Longitudes are incosistent'
      else
        write(*,*) 'Something is wrong'
      end if
      write(*,'(i1)') iflag
!
      stop
      end program compare_gauss_points
