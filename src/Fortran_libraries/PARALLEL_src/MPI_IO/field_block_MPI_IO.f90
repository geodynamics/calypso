!>@file  field_block_MPI_IO.f90
!!       module field_block_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged field file using MPI-IO
!!
!!@verbatim
!!      subroutine write_field_data_mpi                                 &
!!     &         (id_fld, num_pe, id_rank, ioff_gl,                     &
!!     &          t_IO, nnod, num_field, ntot_comp, ncomp_field,        &
!!     &          field_name, d_nod, istack_merged)
!!
!!      subroutine read_field_data_mpi(id_fld, num_pe, id_rank,         &
!!     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,     &
!!     &          field_name, d_nod, istack_merged)
!!      subroutine read_field_names_mpi(id_fld, num_pe, id_rank,        &
!!     &          ioff_gl, num_field, ncomp_field, field_name,          &
!!     &          istack_merged)
!!@endverbatim
!
      module field_block_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
!
      implicit none
!
      private :: write_fld_vecotr_mpi, read_fld_vecotr_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_field_data_mpi                                   &
     &         (id_fld, num_pe, id_rank, ioff_gl,                       &
     &          t_IO, nnod, num_field, ntot_comp, ncomp_field,          &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use time_data_IO
      use field_data_IO
      use field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      type(time_data), intent(in) :: t_IO
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j, icou
!
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_fld, ioff_gl, step_data_buffer(num_pe, t_IO))
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    field_istack_nod_buffer(num_pe, istack_merged))
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    field_num_buffer(num_field))
      call calypso_mpi_seek_write_head_c                                &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      icou = 1
      do j = 1, num_field
        call write_field_name_mpi(id_fld, ioff_gl, field_name(j))
        call write_fld_vecotr_mpi(id_fld, num_pe, id_rank, ioff_gl,     &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_data_mpi(id_fld, num_pe, id_rank,           &
     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,       &
     &          field_name, d_nod, istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: num_field, ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len = kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j, icou
!
!
      icou = 1
      do j = 1, num_field
        call read_field_name_mpi(id_fld, ioff_gl, field_name(j))
        call read_fld_vecotr_mpi(id_fld, num_pe, id_rank, ioff_gl,      &
     &      nnod, ncomp_field(j), d_nod(1,icou), istack_merged)
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_names_mpi(id_fld, num_pe, id_rank,          &
     &          ioff_gl, num_field, ncomp_field, field_name,            &
     &          istack_merged)
!
      use m_phys_constants
      use field_data_IO
      use field_data_MPI_IO
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      character(len=kchara), intent(inout) :: field_name(num_field)
!
      integer, intent(in) ::  id_fld
!
      integer(kind = kint) :: j
!
!
      do j = 1, num_field
        call read_field_name_mpi(id_fld, ioff_gl, field_name(j))
        call skip_fld_vecotr_mpi(num_pe, id_rank, ioff_gl,              &
     &      ncomp_field(j), istack_merged)
      end do
!
      end subroutine read_field_names_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_fld_vecotr_mpi(id_fld, num_pe, id_rank,          &
     &          ioff_gl, nnod, ncomp, vector, istack_merged)
!
      use field_data_IO
      use data_IO_to_textline
!
      integer, intent(in) :: id_rank, num_pe
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      real(kind = kreal), intent(in) :: vector(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      real(kind = kreal) :: v1(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
      integer(kind = kint_gl) :: istack_buffer(0:num_pe)
      integer(kind = kint_gl) :: inod
!
!
      v1(1:ncomp) = 0.0d0
      ilength = len_vector_textline(ncomp)
      istack_buffer(0:num_pe) = ilength * istack_merged(0:num_pe)
!
      call calypso_mpi_seek_write_head_c(id_fld, ioff_gl,               &
     &    buffer_istack_nod_buffer(num_pe, istack_buffer))
!
      ioffset = ioff_gl + ilength * istack_merged(id_rank)
      do inod = 1, nnod
        v1(1:ncomp) = vector(inod,1:ncomp)
        call mpi_write_one_chara_b(id_fld, ioffset, ilength,            &
     &      vector_textline(ncomp, v1))
      end do
      ioff_gl = ioff_gl + ilength * istack_merged(num_pe)
!
      end subroutine write_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_fld_vecotr_mpi(id_fld, num_pe, id_rank,           &
     &          ioff_gl, nnod, ncomp, vector, istack_merged)
!
      use field_data_IO
      use data_IO_to_textline
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe, id_rank
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint_gl), intent(in) :: istack_merged(0:num_pe)
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vector(nnod,ncomp)
!
      integer, intent(in) ::  id_fld
!
      character(len=ncomp*25+1) :: textbuf_d
      real(kind = kreal) :: v1(ncomp)
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
      integer(kind = kint_gl) :: inod
!
!
      if(id_rank .ge. num_pe) return
!   Skip buffer size
      ilength = len(buffer_istack_nod_buffer(num_pe, istack_merged))
      ioff_gl = ioff_gl + ilength
!
      v1(1:ncomp) = 0.0d0
      ilength = len(textbuf_d)
      ioffset = ioff_gl + ilength * istack_merged(id_rank)
!
      do inod = 1, nnod
        call mpi_read_one_chara_b                                       &
     &    (id_fld, ioffset, ilength, textbuf_d)
        call read_vector_textline(textbuf_d, ncomp, v1)
        vector(inod,1:ncomp) = v1(1:ncomp)
      end do
      ioff_gl = ioff_gl + ilength * istack_merged(num_pe)
!
      end subroutine read_fld_vecotr_mpi
!
! -----------------------------------------------------------------------
!
      end module field_block_MPI_IO
