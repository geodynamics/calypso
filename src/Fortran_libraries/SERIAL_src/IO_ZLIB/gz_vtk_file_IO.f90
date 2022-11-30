!>@file  gz_vtk_file_IO.f90
!!       module gz_vtk_file_IO
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
!!      subroutine write_gz_vtk_file(gzip_name,                         &
!!     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,  &
!!     &          ncomp_field, field_name, d_nod)
!!      subroutine write_gz_vtk_phys(id_rank, gzip_name, ucd)
!!      subroutine write_gz_vtk_grid(id_rank, gzip_name, ucd)
!!        type(ucd_data), intent(in) :: ucd
!!
!!      subroutine read_gz_vtk_file(id_rank, gzip_name, ucd)
!!      subroutine read_gz_vtk_phys(id_rank, gzip_name, ucd)
!!      subroutine read_gz_vtk_grid(id_rank, gzip_name, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine read_alloc_gz_vtk_file(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_vtk_phys(id_rank, gzip_name, ucd)
!!      subroutine read_alloc_gz_vtk_grid(id_rank, gzip_name, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module gz_vtk_file_IO
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_field_file_format
!
      use t_ucd_data
      use t_buffer_4_gzip
      use gz_udt_to_VTK_data_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_vtk
      character, pointer, private, save :: FPz_vtk
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_parallel_vtk_file(id_rank, nprocs,            &
     &          istep, file_prefix)
!
      use set_parallel_file_name
      use set_ucd_file_names
      use set_ucd_extensions
      use skip_gz_comment
      use gzip_file_access
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
      call open_wt_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
!
      write(zbuf_vtk%fixbuf(1),'(a,2a1)') '<File version="pvtk-1.0"',   &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_vtk, zbuf_vtk)
      write(zbuf_vtk%fixbuf(1),'(a,2a1)')                               &
     &     '       dataType="vtkUnstructuredGrid"', char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_vtk, zbuf_vtk)
      write(zbuf_vtk%fixbuf(1),'(a,i6,a,2a1)')                          &
     &     '       numberOfPieces="', nprocs, '" >', char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_vtk, zbuf_vtk)
      do ip = 0, nprocs-1
        file_name = set_parallel_ucd_file_name(fname_nodir, iflag_vtk,  &
     &                                         ip, istep)
        write(zbuf_vtk%fixbuf(1),'(3a,2a1)') '   <Piece fileName="',    &
     &                       trim(file_name), '" />', char(10), char(0)
        call gz_write_textbuf_no_lf(FPz_vtk, zbuf_vtk)
      end do
      write(zbuf_vtk%fixbuf(1),'(a,2a1)') '</File>', char(10), char(0)
      call gz_write_textbuf_no_lf(FPz_vtk, zbuf_vtk)
!
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine write_gz_parallel_vtk_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_gz_vtk_file(id_rank, gzip_name, ucd)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0) write(*,*)                                       &
     &    'Write gzipped VTK data: ', trim(gzip_name)
!
      call open_wt_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call write_gz_ucd_mesh_to_VTK(FPz_vtk, ucd, zbuf_vtk)
      call write_gz_ucd_field_to_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine write_gz_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_phys(id_rank, gzip_name, ucd)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0) write(*,*)                                       &
     &    'Write gzipped VTK field: ', trim(gzip_name)
!
      call open_wt_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call write_gz_ucd_field_to_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine write_gz_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_grid(id_rank, gzip_name, ucd)
!
      use set_parallel_file_name
      use skip_gz_comment
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: gzip_name
      type(ucd_data), intent(in) :: ucd
!
!
      if(id_rank.le.0) write(*,*)                                       &
     &    'Write gzipped VTK grid: ', trim(gzip_name)
!
      call open_wt_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call write_gz_ucd_mesh_to_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine write_gz_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped ascii VTK file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call read_gz_ucd_grd_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call read_gz_udt_field_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine read_gz_vtk_file
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_vtk_phys(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped ascii VTK fields: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call read_gz_udt_field_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine read_gz_vtk_phys
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_vtk_grid(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped ascii VTK mesh: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call read_gz_ucd_grd_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine read_gz_vtk_grid
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_alloc_gz_vtk_file(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped ascii VTK file: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call read_alloc_gz_ucd_grd_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call read_alloc_gz_udt_fld_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine read_alloc_gz_vtk_file
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_vtk_phys(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped ascii VTK fields: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call read_alloc_gz_udt_fld_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine read_alloc_gz_vtk_phys
!
!-----------------------------------------------------------------------
!
      subroutine read_alloc_gz_vtk_grid(id_rank, gzip_name, ucd)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(ucd_data), intent(inout) :: ucd
!
!
      if(id_rank.le.0 .or. i_debug .gt. 0) write(*,*)                   &
     &     'Read gzipped ascii VTK mesh: ', trim(gzip_name)
!
      call open_rd_gzfile_a(FPz_vtk, gzip_name, zbuf_vtk)
      call read_alloc_gz_ucd_grd_from_VTK(FPz_vtk, ucd, zbuf_vtk)
      call close_gzfile_a(FPz_vtk, zbuf_vtk)
!
      end subroutine read_alloc_gz_vtk_grid
!
!-----------------------------------------------------------------------
!
      end module gz_vtk_file_IO
