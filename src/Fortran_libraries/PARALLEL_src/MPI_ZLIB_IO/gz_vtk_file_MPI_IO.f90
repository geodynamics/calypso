!>@file  gz_vtk_file_MPI_IO.f90
!!       module gz_vtk_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usging MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_vtk_data_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged_intnod)
!!      subroutine gz_write_vtk_mesh_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, nele, nnod_ele, xx, ie,                         &
!!     &          istack_merged_intnod, istack_merged_ele)
!!@endverbatim
!
      module gz_vtk_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_buffer_4_gzip
      use m_calypso_mpi_IO
      use vtk_data_to_buffer
!
      implicit none
!
      private :: gz_write_vtk_scalar_mpi
      private :: gz_write_vtk_tensor_mpi, gz_write_vtk_vecotr_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_vtk_data_mpi(id_vtk, ioff_gl,                 &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint) :: icou, j
      integer(kind = kint_gl) :: nt_nod
!
!
      nt_nod = istack_merged_intnod(nprocs)
!
!     Field header
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_fields_head(nt_nod))
!
!      field loop
!
      if(ntot_comp .le. 0) return
      icou = 1
      do j = 1, num_field
!
!         Scalar
        if ( ncomp_field(j) .eq. n_scalar) then
          call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                 &
     &        vtk_scalar_head(field_name(j)))
!
          call gz_write_vtk_scalar_mpi(id_vtk, ioff_gl,                 &
     &        nnod, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_vector) then
          call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                 &
     &        vtk_vector_head(field_name(j)))
!
          call gz_write_vtk_vecotr_mpi(id_vtk, ioff_gl,                 &
     &       nnod, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_sym_tensor) then
          call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                 &
     &        vtk_tensor_head(field_name(j)))
!
          call gz_write_vtk_tensor_mpi(id_vtk, ioff_gl,                 &
     &          nnod, d_nod(1,icou), istack_merged_intnod)
        end if
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine gz_write_vtk_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_mesh_mpi(id_vtk, ioff_gl,                 &
     &          nnod, nele, nnod_ele, xx, ie,                           &
     &          istack_merged_intnod, istack_merged_ele)
!
      use m_phys_constants
      use zlib_cvt_vtk_data
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer, intent(in) ::  id_vtk
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = kint_gl) :: nt_nod, nt_ele
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
!
!       Node header
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_node_head(nt_nod))
!
!      Node position
!
      call gz_write_vtk_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, xx, istack_merged_intnod)
!
!        Element connectivity
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_connect_head(nt_ele, nnod_ele))
!
      call defleate_vtk_connect(nele, ie, nnod_ele, zbuf)
      call calypso_gz_mpi_seek_write(id_vtk, ioff_gl, zbuf)
      call dealloc_zip_buffer(zbuf)
!
!       Element type
!
      call gz_write_vtk_header_mpi                                      &
     &   (id_vtk, ioff_gl, vtk_cell_type_head(nt_ele))
!
      call defleate_vtk_celltype(nele, nnod_ele, zbuf)
      call calypso_gz_mpi_seek_write(id_vtk, ioff_gl, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_vtk_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_header_mpi(id_vtk, ioff_gl, header_txt)
!
      use zlib_convert_text
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_vtk
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        call defleate_characters(len(header_txt), header_txt, zbuf)
!
        ioffset = int(ioff_gl)
        call calypso_mpi_seek_write_gz(id_vtk, ioffset, zbuf)
        call dealloc_zip_buffer(zbuf)
      end if
      call MPI_BCAST(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,          &
     &    0, CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + zbuf%ilen_gzipped
!
      end subroutine gz_write_vtk_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_scalar_mpi(id_vtk, ioff_gl,               &
     &          nnod, vect, istack_merged_intnod)
!
      use zlib_cvt_vtk_data
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod)
!
      integer, intent(in) ::  id_vtk
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = kint_gl) :: num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      call defleate_vtk_scalar(nnod, num, vect, zbuf)
!
      call calypso_gz_mpi_seek_write(id_vtk, ioff_gl, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_vtk_scalar_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_vecotr_mpi(id_vtk, ioff_gl,               &
     &          nnod, vect, istack_merged_intnod)
!
      use t_buffer_4_gzip
      use zlib_cvt_vtk_data
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod,3)
!
      integer, intent(in) ::  id_vtk
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = kint_gl) :: num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      call defleate_vtk_vector(nnod, num, vect, zbuf)
!
      call calypso_gz_mpi_seek_write(id_vtk, ioff_gl, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_vtk_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_vtk_tensor_mpi(id_vtk, ioff_gl,               &
     &          nnod, vect, istack_merged_intnod)
!
      use zlib_cvt_vtk_data
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod,6)
!
      integer, intent(in) ::  id_vtk
!
      type(buffer_4_gzip) :: zbuf
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) ::  num
      integer(kind = kint) :: ip
      integer(kind = kint_gl) :: ilen_gzipped_list(nprocs)
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      call defleate_vtk_tensor(nnod, num, vect, zbuf)
!
      call MPI_Allgather(zbuf%ilen_gzipped, 1, CALYPSO_GLOBAL_INT,      &
     &    ilen_gzipped_list(1), 1, CALYPSO_GLOBAL_INT,                  &
     &    CALYPSO_COMM, ierr_MPI)
      ioffset = int(ioff_gl)
      do ip = 1, my_rank
        ioffset = ioffset + ilen_gzipped_list(ip)
      end do
!
      if(zbuf%ilen_gzipped .gt. 0) then
        call calypso_mpi_seek_write_gz(id_vtk, ioffset, zbuf)
      end if
      do ip = 1, nprocs
        ioff_gl = ioff_gl + ilen_gzipped_list(ip)
      end do
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_vtk_tensor_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_file_MPI_IO
