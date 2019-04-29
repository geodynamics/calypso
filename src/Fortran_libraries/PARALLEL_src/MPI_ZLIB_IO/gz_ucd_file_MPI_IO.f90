!>@file  gz_ucd_file_MPI_IO.f90
!!       module gz_ucd_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_ucd_data_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod, istack_merged_intnod)
!!      subroutine gz_write_ucd_mesh_mpi(id_vtk, ioff_gl,               &
!!     &          nnod, nele, nnod_ele, ntot_comp, xx, ie,              &
!!     &          istack_merged_intnod, istack_merged_ele)
!!@endverbatim
!
      module gz_ucd_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_buffer_4_gzip
      use vtk_data_to_buffer
      use ucd_data_to_buffer
!
      implicit none
!
      character(len=1), allocatable :: gzip_buf(:)
!
      private :: gz_write_ucd_vecotr_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_data_mpi(id_vtk, ioff_gl,                 &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod, istack_merged_intnod)
!
      use gz_vtk_file_MPI_IO
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
      integer(kind = kint) :: j
!
!
      call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                     &
     &    ucd_num_comps(num_field, ncomp_field))
!
      do j = 1, num_field
        call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                   &
     &      ucd_field_name(field_name(j)))
      end do
!
      call gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, ntot_comp, d_nod, istack_merged_intnod)
!
      end subroutine gz_write_ucd_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_mesh_mpi(id_vtk, ioff_gl,                 &
     &          nnod, nele, nnod_ele, ntot_comp, xx, ie,                &
     &          istack_merged_intnod, istack_merged_ele)
!
      use m_phys_constants
      use zlib_cvt_ucd_data
      use gz_vtk_file_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
      integer(kind = kint), intent(in) :: nnod_ele, ntot_comp
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
      call gz_write_vtk_header_mpi(id_vtk, ioff_gl,                     &
     &    ucd_connect_head(nt_nod, nt_ele, ntot_comp))
!
      call gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, n_vector, xx, istack_merged_intnod)
!
      call defleate_ucd_connect                                         &
     &   (nele, ie, nnod_ele, istack_merged_ele(my_rank), zbuf)
      call calypso_gz_mpi_seek_write(id_vtk, ioff_gl, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_ucd_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,               &
     &          nnod, ntot_comp, vect, istack_merged_intnod)
!
      use zlib_cvt_ucd_data
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: ntot_comp
      real(kind = kreal), intent(in) :: vect(nnod,ntot_comp)
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
      call defleate_ucd_vector(nnod, num, ntot_comp, vect,              &
     &    istack_merged_intnod(my_rank), zbuf)
!
      call calypso_gz_mpi_seek_write(id_vtk, ioff_gl, zbuf)
      call dealloc_zip_buffer(zbuf)
!
      end subroutine gz_write_ucd_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_ucd_file_MPI_IO
