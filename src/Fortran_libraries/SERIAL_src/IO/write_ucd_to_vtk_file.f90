!write_ucd_to_vtk_file.f90
!      module write_ucd_to_vtk_file
!
!        programmed by H.Matsui on July, 2006
!        Modified by H.Matsui on March, 2013
!
!      subroutine write_parallel_vtk_file(my_rank, istep)
!
!      subroutine write_udt_data_2_vtk_file(my_rank, istep)
!      subroutine write_udt_data_2_vtk_phys(my_rank, istep)
!      subroutine write_udt_data_2_vtk_grid(my_rank)
!
      module write_ucd_to_vtk_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_field_file_format
      use m_ucd_data
      use set_ucd_file_names
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_parallel_vtk_file(my_rank, nprocs, istep)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: my_rank, nprocs, istep
!
      character(len=kchara)  :: file_name, fname_tmp
      character(len=kchara) :: fname_nodir
      integer(kind = kint) :: ip
!
!
      if(my_rank .gt. 0) return
!
      call delete_directory_name(ucd_header_name, fname_nodir)
      call add_int_suffix(istep, ucd_header_name, fname_tmp)
      call add_pvtk_extension(fname_tmp, file_name)
!
      write(*,*) 'Write parallel VTK file: ', trim(file_name)
      open(ucd_file_code, file=file_name)
!
      write(ucd_file_code,'(a)') '<File version="pvtk-1.0"'
      write(ucd_file_code,'(a)')                                        &
     &     '       dataType="vtkUnstructuredGrid"'
      write(ucd_file_code,'(a,i6,a)')                                   &
     &     '       numberOfPieces="', nprocs, '" >'
      do ip = 0, nprocs-1
        call set_parallel_ucd_file_name(fname_nodir, iflag_vtk,         &
     &      ip, istep, file_name)
        write(ucd_file_code,'(3a)') '   <Piece fileName="',             &
     &                       trim(file_name), '" />'
      end do
      write(ucd_file_code,'(a)') '</File>'
!
      close(ucd_file_code)
!
      end subroutine write_parallel_vtk_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_file(my_rank, istep)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_vtk,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK file: ', trim(file_name)
!
      call write_vtk_file(file_name, ucd_file_code,                     &
     &    nnod_ucd, nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd,           &
     &    num_field_ucd, ntot_comp_ucd, num_comp_ucd, phys_name_ucd,    &
     &    d_nod_ucd)
!
      end subroutine write_udt_data_2_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_phys(my_rank, istep)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank, istep
      character(len=kchara) :: file_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_vtd,       &
     &    my_rank, istep, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK fields: ', trim(file_name)
!
      call write_vtk_phys(file_name, ucd_file_code,                     &
     &    nnod_ucd, num_field_ucd, ntot_comp_ucd,                       &
     &    num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      end subroutine write_udt_data_2_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_udt_data_2_vtk_grid(my_rank)
!
      use vtk_file_IO
!
      integer(kind = kint), intent(in) ::  my_rank
      character(len=kchara) :: file_name
!
!
      call set_parallel_grd_file_name(ucd_header_name, iflag_vtd,       &
     &    my_rank, file_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Write ascii VTK mesh: ', trim(file_name)
!
      call write_vtk_grid(file_name, ucd_file_code,                     &
     &    nnod_ucd, nele_ucd, nnod_4_ele_ucd, xx_ucd, ie_ucd)
!
      end subroutine write_udt_data_2_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module write_ucd_to_vtk_file
