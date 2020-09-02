!>@file  gz_field_block_MPI_IO.f90
!!       module gz_field_block_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_field_head_gz_mpi(id_fld, ioff_gl,             &
!!     &          t_IO, num_field, ncomp_field, istack_merged)
!!      subroutine write_field_data_gz_mpi(id_fld, ioff_gl,             &
!!     &           nnod, num_field, ntot_comp, ncomp_field,             &
!!     &           field_name, d_nod)
!!
!!      subroutine read_field_step_gz_mpi                               &
!!     &         (id_fld, num_pe, ioff_gl, t_IO)
!!      subroutine read_field_header_gz_mpi(id_fld, num_pe, id_rank,    &
!!     &          ioff_gl, nnod, num_field, istack_merged)
!!      subroutine read_field_num_gz_mpi                                &
!!     &         (id_fld, ioff_gl, num_field, ncomp_field)
!!      subroutine read_field_data_gz_mpi(id_fld, num_pe, id_rank,      &
!!     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,     &
!!     &          field_name, d_nod)
!!@endverbatim
!
      module gz_field_block_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_time_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_field_head_gz_mpi(id_fld, ioff_gl,               &
     &          t_IO, num_field, ncomp_field, istack_merged)
!
      use m_phys_constants
      use time_data_IO
      use field_data_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: istack_merged(0:nprocs)
!
      type(time_data), intent(in) :: t_IO
!
      integer(kind=kint), intent(in) :: num_field
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
!
      integer, intent(in) ::  id_fld
!
!
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, step_data_buffer(nprocs, t_IO))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_istack_nod_buffer(nprocs, istack_merged))
      call gz_write_fld_header_mpi(id_fld, ioff_gl,                     &
     &    field_num_buffer(num_field))
      call gz_write_fld_header_mpi                                      &
     &   (id_fld, ioff_gl, field_comp_buffer(num_field, ncomp_field))
!
      end subroutine write_field_head_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_field_data_gz_mpi(id_fld, ioff_gl,               &
     &           nnod, num_field, ntot_comp, ncomp_field,               &
     &           field_name, d_nod)
!
      use m_phys_constants
      use field_data_IO
      use gz_field_data_MPI_IO
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: nnod
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
      icou = 1
      do j = 1, num_field
        call gz_write_fld_header_mpi                                    &
     &     (id_fld, ioff_gl, each_field_name_buffer(field_name(j)))
        call gz_write_fld_vecotr_mpi(id_fld, ioff_gl,                   &
     &      nnod, ncomp_field(j), d_nod(1,icou))
!        write(*,*) 'gz_write_fld_vecotr_mpi end', j, my_rank, ioff_gl
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_field_data_gz_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_field_step_gz_mpi                                 &
     &         (id_fld, num_pe, ioff_gl, t_IO)
!
      use m_error_IDs
      use time_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) :: num_pe
      type(time_data), intent(inout) :: t_IO
!
      integer(kind=kint) :: iread
      character(len=len_step_data_buf) :: textbuf_c
!
!
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, len_step_data_buf, textbuf_c)
!
      if(my_rank .eq. 0) then
        call read_step_data_buffer(textbuf_c, iread, t_IO)
      end if
!
      if(my_rank.eq.0 .and. num_pe .ne. iread) then
        call calypso_mpi_abort(ierr_fld, 'Set correct field data file')
      end if
!
      call sync_field_time_mpi(t_IO)
!
      end subroutine read_field_step_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_header_gz_mpi(id_fld, num_pe, id_rank,      &
     &          ioff_gl, nnod, num_field, istack_merged)
!
      use calypso_mpi_int
      use calypso_mpi_int8
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
      use transfer_to_long_integers
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer, intent(in) :: num_pe, id_rank
      integer(kind = kint), intent(inout) :: num_field
      integer(kind = kint_gl), intent(inout) :: nnod
      integer(kind = kint_gl), intent(inout) :: istack_merged(0:num_pe)
!
      integer :: ilength
      character(len=31+1+16+1) ::           textbuf_c
      character(len=25+1+num_pe*16+1) :: textbuf_d
!
!
      ilength = len(textbuf_d)
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_d)
      if(my_rank .eq. 0) call read_field_istack_nod_buffer              &
     &                      (textbuf_d, num_pe, istack_merged)
!
      ilength = len(field_num_buffer(izero))
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf_c)
      if(my_rank .eq. 0) call read_field_num_buffer                     &
     &                      (textbuf_c, num_field)
!
      call calypso_mpi_bcast_int8                                       &
     &   (istack_merged, cast_long(num_pe+1), 0)
      call calypso_mpi_bcast_one_int(num_field, 0)
!
      call sync_field_header_mpi                                        &
     &   (num_pe, id_rank, istack_merged, nnod)
!
      end subroutine read_field_header_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_num_gz_mpi                                  &
     &         (id_fld, ioff_gl, num_field, ncomp_field)
!
      use calypso_mpi_int
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
      use transfer_to_long_integers
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
!
      integer(kind=kint), intent(inout) :: num_field
      integer(kind=kint), intent(inout) :: ncomp_field(num_field)
!
      integer :: ilength
      character(len=num_field*5+1) :: textbuf
!
!
      ilength = len(textbuf)
      call gz_read_fld_charhead_mpi                                     &
     &   (id_fld, ioff_gl, ilength, textbuf)
      if(my_rank .eq. 0) call read_field_comp_buffer                    &
     &                      (textbuf, num_field, ncomp_field)
!
      call calypso_mpi_bcast_int(ncomp_field, cast_long(num_field), 0)
!
      end subroutine read_field_num_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_data_gz_mpi(id_fld, num_pe, id_rank,        &
     &          ioff_gl, nnod, num_field, ntot_comp, ncomp_field,       &
     &          field_name, d_nod)
!
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe, id_rank
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(inout) :: field_name(num_field)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: j, icou
!
!
      icou = 1
      do j = 1, num_field
        call gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name(j))
!         write(*,*) 'gz_read_each_field_mpi start', j, my_rank
        call gz_read_each_field_mpi(id_fld, num_pe, id_rank,            &
     &      ioff_gl, nnod, ncomp_field(j), d_nod(1,icou))
!         write(*,*) 'gz_read_each_field_mpi end', j, my_rank
         if(my_rank .eq. 0) write(*,*) 'Read ', j, trim(field_name(j))
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine read_field_data_gz_mpi
!
! -----------------------------------------------------------------------
!
      subroutine read_field_names_gz_mpi(id_fld, num_pe, id_rank,       &
     &          ioff_gl, num_field, field_name)
!
      use field_data_IO
      use field_data_MPI_IO
      use gz_field_data_MPI_IO
!
      integer, intent(in) ::  id_fld
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer, intent(in) :: num_pe, id_rank
!
      integer(kind=kint), intent(in) :: num_field
      character(len=kchara), intent(inout) :: field_name(num_field)
!
      integer(kind = kint) :: j
!
!
      do j = 1, num_field
        call gz_read_fld_1word_mpi(id_fld, ioff_gl, field_name(j))
        call gz_skip_each_field_mpi(id_fld, num_pe, ioff_gl)
      end do
!
      end subroutine read_field_names_gz_mpi
!
! -----------------------------------------------------------------------
!
      end module gz_field_block_MPI_IO
