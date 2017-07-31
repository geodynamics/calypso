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
      use vtk_data_to_buffer
      use ucd_data_to_buffer
!
      implicit none
!
      character(len=1), allocatable :: gzip_buf(:)
!
      private :: gz_write_ucd_header_mpi
      private :: gz_write_ucd_vecotr_mpi, gz_write_ucd_connect_mpi
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
      call gz_write_ucd_header_mpi(id_vtk, ioff_gl,                     &
     &    ucd_num_comps(num_field, ncomp_field))
!
      do j = 1, num_field
        call gz_write_ucd_header_mpi(id_vtk, ioff_gl,                   &
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
      integer(kind = kint_gl) :: nt_nod, nt_ele
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
!
      call gz_write_ucd_header_mpi(id_vtk, ioff_gl,                     &
     &    ucd_connect_head(nt_nod, nt_ele, ntot_comp))
!
      call gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,                     &
     &    nnod, n_vector, xx, istack_merged_intnod)
!
      call gz_write_ucd_connect_mpi(id_vtk, ioff_gl,                    &
     &    nele, nnod_ele, ie, istack_merged_ele)
!
      end subroutine gz_write_ucd_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_header_mpi(id_vtk, ioff_gl, header_txt)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      character(len=*), intent(in) :: header_txt
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) then
        ilength = len(header_txt)
        ilen_gz = int(real(ilength) *1.01) + 24
        allocate(gzip_buf(ilen_gz))
        call gzip_defleat_once                                          &
     &     (ilength, header_txt, ilen_gz, ilen_gzipped, gzip_buf(1))
        ilength = ilen_gzipped
!
        ioffset = ioff_gl
        call calypso_mpi_seek_write_chara                               &
     &     (id_vtk, ioffset, ilen_gzipped, gzip_buf(1))
        deallocate(gzip_buf)
      end if
      call MPI_BCAST(ilen_gzipped, ione, CALYPSO_INTEGER, izero,        &
     &    CALYPSO_COMM, ierr_MPI)
      ioff_gl = ioff_gl + ilen_gzipped
!
      end subroutine gz_write_ucd_header_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_vecotr_mpi(id_vtk, ioff_gl,               &
     &          nnod, ntot_comp, vect, istack_merged_intnod)
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
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
      integer(kind = kint_gl) :: inod, num, inod_gl
      real(kind = kreal)  :: dat_1(ntot_comp)
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
!
      inod_gl = 1
      dat_1(1:ntot_comp) = zero
      ilength = len(ucd_each_field(inod_gl, ntot_comp, dat_1))
      ilen_gz = int(real(num*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
      if(num .eq. 1) then
        inod_gl = 1 + istack_merged_intnod(my_rank)
        dat_1(1:ntot_comp) = vect(1,1:ntot_comp)
        call gzip_defleat_once(ilength,                                 &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1),                  &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
!
      else if(num .gt. 1) then
        inod_gl = 1 + istack_merged_intnod(my_rank)
        dat_1(1:ntot_comp) = vect(1,1:ntot_comp)
        call gzip_defleat_begin(ilength,                                &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1),                  &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do inod = 2, num-1
          inod_gl =    inod + istack_merged_intnod(my_rank)
          dat_1(1:ntot_comp) = vect(inod,1:ntot_comp)
          call gzip_defleat_cont(ilength,                               &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1),                  &
     &      ilen_gz, ilen_gzipped)
        end do
        inod_gl =    num + istack_merged_intnod(my_rank)
        dat_1(1:ntot_comp) = vect(num,1:ntot_comp)
        call gzip_defleat_last(ilength,                                 &
     &      ucd_each_field(inod_gl, ntot_comp, dat_1),                  &
     &    ilen_gz, ilen_gzipped)
      else
        ilen_gzipped = 0
      end if
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
!
      deallocate(gzip_buf)
!
      end subroutine gz_write_ucd_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine gz_write_ucd_connect_mpi(id_vtk, ioff_gl,              &
     &          nele, nnod_ele, ie, istack_merged_ele)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_ele(0:nprocs)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: iele, iele_gl
!
      integer(kind = kint) :: ilen_gz, ilen_gzipped, ilength
!
!
      iele_gl = 1
      ie0(1:nnod_ele) = 0
      ilength = len(ucd_each_connect(iele_gl, nnod_ele, ie0))
      ilen_gz = int(real(nele*ilength) * 1.01) + 24
      allocate(gzip_buf(ilen_gz))
      if(nele .eq. 1) then
        ie0(1:nnod_ele) = ie(1,1:nnod_ele)
        call gzip_defleat_once(ilength,                                 &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
!
      else if(nele .gt. 1) then
        iele_gl = 1 + istack_merged_ele(my_rank)
        ie0(1:nnod_ele) = ie(1,1:nnod_ele)
        call gzip_defleat_begin(ilength,                                &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &      ilen_gz, ilen_gzipped, gzip_buf(1))
        do iele = 2, nele-1
          iele_gl = iele + istack_merged_ele(my_rank)
          ie0(1:nnod_ele) = ie(iele,1:nnod_ele)
          call gzip_defleat_cont(ilength,                               &
     &        ucd_each_connect(iele_gl, nnod_ele, ie0),                 &
     &        ilen_gz, ilen_gzipped)
        end do
        iele_gl = nele + istack_merged_ele(my_rank)
        ie0(1:nnod_ele) = ie(nele,1:nnod_ele)
        call gzip_defleat_last(ilength,                                 &
     &      ucd_each_connect(iele_gl, nnod_ele, ie0),                   &
     &      ilen_gz, ilen_gzipped)
      else
        ilen_gzipped = 0
      end if
!
      call calypso_gz_mpi_seek_write                                    &
     &   (id_vtk, ioff_gl, ilen_gzipped, gzip_buf(1))
      deallocate(gzip_buf)
!
      end subroutine gz_write_ucd_connect_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_ucd_file_MPI_IO
