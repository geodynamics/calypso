!
!      program compare_gauss_points
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
      write(*,*) 'input reference data file name'
      read(*,*) file_name1
      write(*,*) 'input second data file name'
      read(*,*) file_name2
!
      call read_alloc_gauss_points(file_name1, gauss_pt_1)
      call read_alloc_gauss_points(file_name2, gauss_pt_2)
      iflag = compare_gauss_point_data(gauss_pt_1, gauss_pt_2)
!
      if(iflag .eq. 0) then
        write(*,*) 'Data are cosistent'
      else if(iflag .eq. 1) then
        write(*,*) 'Gauss points are incosistent'
      else if(iflag .eq. 2) then
        write(*,*) 'Weights are incosistent'
      else if(iflag .eq. 11) then
        write(*,*) 'Gauss-Legendre latitude are incosistent'
      else if(iflag .eq. 21) then
        write(*,*) 'Longitudes are incosistent'
      else
        write(*,*) 'Something is wrong'
      end if
!
      call dealloc_gauss_colatitude(gauss_pt_1)
      call dealloc_gauss_colatitude(gauss_pt_2)
!
      stop
      end program compare_gauss_points
