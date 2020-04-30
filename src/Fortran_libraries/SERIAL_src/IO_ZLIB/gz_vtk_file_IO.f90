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
      use gz_vtk_data_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_vtk
!
      private :: write_gz_vtk_data, write_gz_vtk_mesh
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
      call open_wt_gzfile_a(gzip_name, zbuf_vtk)
!
      write(zbuf_vtk%fixbuf(1),'(a,2a1)') '<File version="pvtk-1.0"',   &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_vtk)
      write(zbuf_vtk%fixbuf(1),'(a,2a1)')                               &
     &     '       dataType="vtkUnstructuredGrid"', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_vtk)
      write(zbuf_vtk%fixbuf(1),'(a,i6,a,2a1)')                          &
     &     '       numberOfPieces="', nprocs, '" >', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_vtk)
      do ip = 0, nprocs-1
        file_name = set_parallel_ucd_file_name(fname_nodir, iflag_vtk,  &
     &                                         ip, istep)
        write(zbuf_vtk%fixbuf(1),'(3a,2a1)') '   <Piece fileName="',    &
     &                       trim(file_name), '" />', char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf_vtk)
      end do
      write(zbuf_vtk%fixbuf(1),'(a,2a1)') '</File>', char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_vtk)
!
      call close_gzfile_a(zbuf_vtk)
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
      call open_wt_gzfile_a(gzip_name, zbuf_vtk)
      call write_gz_vtk_mesh                                            &
     &   (ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie, zbuf_vtk)
      call write_gz_vtk_data(ucd%nnod, ucd%num_field, ucd%ntot_comp,    &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd, zbuf_vtk)
!
      call close_gzfile_a(zbuf_vtk)
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
      call open_wt_gzfile_a(gzip_name, zbuf_vtk)
      call write_gz_vtk_data(ucd%nnod, ucd%num_field, ucd%ntot_comp,    &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd, zbuf_vtk)
      call close_gzfile_a(zbuf_vtk)
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
      call open_wt_gzfile_a(gzip_name, zbuf_vtk)
      call write_gz_vtk_mesh                                            &
     &   (ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie, zbuf_vtk)
      call close_gzfile_a(zbuf_vtk)
!
      end subroutine write_gz_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_data(nnod, num_field, ntot_comp,          &
     &          ncomp_field, field_name, d_nod, zbuf)
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: icou, j
!
!
      call write_gz_vtk_fields_head(nnod, zbuf)
!
      IF(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          call write_gz_vtk_each_field_head                             &
     &       (ncomp_field(j), field_name(j), zbuf)
          call write_gz_vtk_each_field                                  &
     &       (nnod, ncomp_field(j), nnod, d_nod(1,icou), zbuf)
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_gz_vtk_data
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie, zbuf)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call write_gz_vtk_node_head(nnod, zbuf)
      call write_gz_vtk_each_field(nnod, n_vector, nnod, xx, zbuf)
!
      call write_gz_vtk_connect_head(nele, nnod_ele, zbuf)
      call write_gz_vtk_connect_data(nele, nnod_ele, nele, ie, zbuf)
!
      call write_gz_vtk_cell_type(nele, nnod_ele, zbuf)
!
      end subroutine write_gz_vtk_mesh
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_file_IO
