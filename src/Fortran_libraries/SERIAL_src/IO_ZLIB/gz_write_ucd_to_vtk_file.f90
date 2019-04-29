!>@file   gz_write_ucd_to_vtk_file.f90
!!@brief  module gz_write_ucd_to_vtk_file
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui on July, 2006
!!@n       Modified by H.Matsui on March, 2013
!
!>@brief Output FEM field data to distributed VTK file (gzipped)
!!
!!@verbatim
!!      subroutine write_gz_parallel_vtk_file(id_rank, nprocs,          &
!!     &          istep, file_prefix)
!!
!!      subroutine write_ucd_data_2_gz_vtk(id_rank, gzip_name, ucd)
!!      subroutine write_ucd_data_2_gz_vtk_phys(id_rank, gzip_name, ucd)
!!      subroutine write_ucd_data_2_gz_vtk_grid(id_rank, gzip_name, ucd)
!!@endverbatim
!!
!!@param id_rank    subdomain ID
!!@param gzip_name  file name
!
      module gz_write_ucd_to_vtk_file
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_field_file_format
!
      use t_ucd_data
      use set_ucd_file_names
      use skip_gz_comment
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_parallel_vtk_file(id_rank, nprocs,            &
     &          istep, file_prefix)
!
      use set_parallel_file_name
      use set_ucd_extensions
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_prefix
      integer, intent(in) :: id_rank, nprocs
      integer(kind = kint), intent(in) :: istep
!
      character(len=kchara) :: gzip_name, file_name, fname_tmp
      character(len=kchara) :: fname_nodir
      integer :: ip
!
!
      if(id_rank .gt. 0) return
!
      fname_nodir = delete_directory_name(file_prefix)
      fname_tmp = add_int_suffix(istep, file_prefix)
      file_name = add_pvtk_extension(fname_tmp)
      gzip_name = add_gzip_extension(file_name)
!
      write(*,*) 'Write gzipped parallel VTK file: ', trim(gzip_name)
      call open_wt_gzfile_f(gzip_name)
!
      write(textbuf,'(a,a1)') '<File version="pvtk-1.0"', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)')                                           &
     &     '       dataType="vtkUnstructuredGrid"', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,i6,a,a1)')                                      &
     &     '       numberOfPieces="', nprocs, '" >', char(0)
      call gz_write_textbuf_w_lf
      do ip = 0, nprocs-1
        file_name = set_parallel_ucd_file_name(fname_nodir, iflag_vtk,  &
     &                                         ip, istep)
        write(textbuf,'(3a,a1)') '   <Piece fileName="',                &
     &                       trim(file_name), '" />', char(0)
        call gz_write_textbuf_w_lf
      end do
      write(textbuf,'(a,a1)') '</File>', char(0)
      call gz_write_textbuf_w_lf
!
      call close_gzfile_f
!
      end subroutine write_gz_parallel_vtk_file
!
!  ---------------------------------------------------------------------
!
      subroutine write_ucd_data_2_gz_vtk(id_rank, gzip_name, ucd)
!
      use gz_vtk_file_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0) write(*,*)                                       &
     &    'Write gzipped VTK data: ', trim(gzip_name)
!
      call write_gz_vtk_file(gzip_name,                                 &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    ucd%num_field, ucd%ntot_comp, ucd%num_comp, ucd%phys_name,    &
     &    ucd%d_ucd)
!
      end subroutine write_ucd_data_2_gz_vtk
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_data_2_gz_vtk_phys(id_rank, gzip_name, ucd)
!
      use gz_vtk_file_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0) write(*,*)                                       &
     &    'Write gzipped VTK field: ', trim(gzip_name)
!
      call write_gz_vtk_phys(gzip_name, ucd%nnod, ucd%num_field,        &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
!
      end subroutine write_ucd_data_2_gz_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_ucd_data_2_gz_vtk_grid(id_rank, gzip_name, ucd)
!
      use gz_vtk_file_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0) write(*,*)                                       &
     &    'Write gzipped VTK grid: ', trim(gzip_name)
!
      call write_gz_vtk_grid(gzip_name,                                 &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie)
!
      end subroutine write_ucd_data_2_gz_vtk_grid
!
! -----------------------------------------------------------------------
!
      end module gz_write_ucd_to_vtk_file
