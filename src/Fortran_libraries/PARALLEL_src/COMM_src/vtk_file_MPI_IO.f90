!>@file  vtk_file_MPI_IO.f90
!!       module vtk_file_MPI_IO
!!
!!@author H. Matsui
!!@date   Programmed in Ma, 2015
!
!> @brief Output merged VTK file usgin MPI-IO
!!
!!@verbatim
!!      subroutine write_vtk_file_mpi(file_name, ucd, m_ucd)
!!      subroutine write_vtk_phys_mpi(file_name, ucd, m_ucd)
!!      subroutine write_vtk_grid_mpi(file_name, ucd, m_ucd)
!!@endverbatim
!
      module vtk_file_MPI_IO
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use vtk_data_to_buffer
      use t_ucd_data
!
      implicit none
!
      private :: write_vtk_data_mpi, write_vtk_mesh_mpi
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vtk_file_mpi(file_name, ucd, m_ucd)
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_file_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys_mpi(file_name, ucd, m_ucd)
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_data_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%num_field, ucd%ntot_comp, ucd%num_comp,         &
     &    ucd%phys_name, ucd%d_ucd, m_ucd%istack_merged_intnod)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_phys_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid_mpi(file_name, ucd, m_ucd)
!
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
      type(merged_ucd_data), intent(in) :: m_ucd
!
      integer :: id_vtk
      integer(kind = kint_gl) :: ioff_gl
!
!
      call calypso_mpi_write_file_open(file_name, nprocs, id_vtk)
!
      ioff_gl = 0
      call write_vtk_mesh_mpi(id_vtk, ioff_gl,                          &
     &    ucd%nnod, ucd%nele, ucd%nnod_4_ele, ucd%xx, ucd%ie,           &
     &    m_ucd%istack_merged_intnod, m_ucd%istack_merged_ele)
!
      call calypso_close_mpi_file(id_vtk)
!
      end subroutine write_vtk_grid_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_data_mpi(id_vtk, ioff_gl,                    &
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
      call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,               &
     &   vtk_fields_head(nt_nod))
!
      if(ntot_comp .le. 0) return
      icou = 1
      do j = 1, num_field
        if ( ncomp_field(j) .eq. n_scalar) then
          call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,           &
     &        vtk_scalar_head(field_name(j)))
          call write_vtk_field_mpi(id_vtk, ioff_gl,                     &
     &          nnod, n_scalar, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_vector) then
          call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,           &
     &        vtk_vector_head(field_name(j)))
          call write_vtk_field_mpi(id_vtk, ioff_gl,                     &
     &          nnod, n_vector, d_nod(1,icou), istack_merged_intnod)
!
        else if ( ncomp_field(j) .eq. n_sym_tensor) then
          call calypso_mpi_seek_write_head_c(id_vtk, ioff_gl,           &
     &        vtk_tensor_head(field_name(j)))
!
          call write_vtk_tensor_mpi(id_vtk, ioff_gl,                    &
     &          nnod, d_nod(1,icou), istack_merged_intnod)
        end if
        icou = icou + ncomp_field(j)
      end do
!
      end subroutine write_vtk_data_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_mesh_mpi(id_vtk, ioff_gl,                    &
     &          nnod, nele, nnod_ele, xx, ie,                           &
     &          istack_merged_intnod, istack_merged_ele)
!
      use m_phys_constants
      use field_data_MPI_IO
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
      integer(kind = kint_gl) :: nt_nod, nt_ele
!
!
      nt_nod = istack_merged_intnod(nprocs)
      nt_ele = istack_merged_ele(nprocs)
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_vtk, ioff_gl, vtk_node_head(nt_nod))
      call write_vtk_field_mpi(id_vtk, ioff_gl,                         &
     &    nnod, n_vector, xx(1,1), istack_merged_intnod)
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_vtk, ioff_gl, vtk_connect_head(nt_ele, nnod_ele))
!
      call write_vtk_connect_mpi(id_vtk, ioff_gl, nt_ele,               &
     &   nele, nnod_ele, ie, istack_merged_ele(my_rank))
!
!
      call calypso_mpi_seek_write_head_c                                &
     &   (id_vtk, ioff_gl, vtk_cell_type_head(nt_ele))
      call write_vtk_celltype_mpi(id_vtk, ioff_gl, nt_ele,              &
     &   nele,  nnod_ele, istack_merged_ele(my_rank))
!
      end subroutine write_vtk_mesh_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_field_mpi(id_vtk, ioff_gl,                   &
     &          nnod, ncomp, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in) :: vect(nnod,ncomp)
!
      integer, intent(in) ::  id_vtk
!
      character(len=ncomp*25+1), allocatable, target :: textbuf_n(:)
      character(len=ncomp*25+1), pointer :: charatmp
      character(len=kchara) :: fmt_txt
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
      integer(kind = kint_gl) :: i, num
!
!
      ilength = ncomp*25 + 1
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
      ioffset = int(ioff_gl + ilength * istack_merged_intnod(my_rank))
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      if(num .le. 0) return
      write(fmt_txt,'(a1,i5,a16)') '(', ncomp, '(1pE25.15e3),a1)'
!
      allocate(textbuf_n(nnod))
!
      do i = 1, num
        charatmp => textbuf_n(i)
        write(charatmp,fmt_txt) vect(i,1:ncomp), char(10)
      end do
      call calypso_mpi_seek_write_ext(id_vtk, ioffset, (num*ilength),   &
     &    textbuf_n(1))
!
      deallocate(textbuf_n)
!
      end subroutine write_vtk_field_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_tensor_mpi(id_vtk, ioff_gl,                  &
     &          nnod, vect, istack_merged_intnod)
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in)                               &
     &         :: istack_merged_intnod(0:nprocs)
      integer(kind = kint_gl), intent(in) :: nnod
      real(kind = kreal), intent(in) :: vect(nnod,6)
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint), parameter :: ilength = 3*25 + 1
      character(len=ilength), allocatable, target :: textbuf_n(:)
      character(len=ilength), pointer :: charatmp
      character(len=kchara), parameter :: fmt_txt = '(6(1pE25.15e3),a1)'
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint_gl) :: i, num
!
!
      num = istack_merged_intnod(my_rank+1)                             &
     &     - istack_merged_intnod(my_rank)
      ioffset = int(ioff_gl + ilength * istack_merged_intnod(my_rank))
      ioff_gl = ioff_gl + ilength * istack_merged_intnod(nprocs)
!
      if(num .le. 0) return
      allocate(textbuf_n(3*nnod))
!
      do i = 1, num
        charatmp => textbuf_n(3*i-2)
        write(charatmp,fmt_txt)                                         &
     &                    vect(i,1), vect(i,2), vect(i,3), char(10)
        charatmp => textbuf_n(3*i-1)
        write(charatmp,fmt_txt)                                         &
     &                    vect(i,2), vect(i,4), vect(i,5), char(10)
        charatmp => textbuf_n(3*i  )
        write(charatmp,fmt_txt)                                         &
     &                    vect(i,3), vect(i,5), vect(i,6), char(10)
      end do
      call calypso_mpi_seek_write_ext(id_vtk, ioffset, (3*num*ilength), &
     &    textbuf_n(1))
!
      deallocate(textbuf_n)
!
      end subroutine write_vtk_tensor_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_connect_mpi(id_vtk, ioff_gl, nt_ele,         &
     &          nele, nnod_ele, ie, istack)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele, nt_ele, istack
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
!
      integer, intent(in) ::  id_vtk
!
      character(len=16+16*nnod_ele+1), allocatable, target              &
     &                                :: textbuf_n(:)
!
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint_gl) :: iele
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer(kind = kint) :: ilength
!
!
      ie0(1:nnod_ele) = 0
      ilength = len(vtk_each_connect(nnod_ele, ie0))
      ioffset = int(ioff_gl + ilength * istack)
      ioff_gl = ioff_gl + ilength * nt_ele
!
      if(nele .le. 0) return
      allocate(textbuf_n(nele))
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        textbuf_n(iele) = vtk_each_connect(nnod_ele,ie0)
      end do
      call calypso_mpi_seek_write_ext(id_vtk, ioffset, (nele*ilength),  &
     &    textbuf_n(1))
      deallocate(textbuf_n)
!
      end subroutine write_vtk_connect_mpi
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_celltype_mpi(id_vtk, ioff_gl, nt_ele,        &
     &          nele, nnod_ele, istack)
!
      use m_phys_constants
!
      integer(kind = kint_gl), intent(inout) :: ioff_gl
      integer(kind = kint_gl), intent(in) :: nele, nt_ele, istack
      integer(kind = kint), intent(in) :: nnod_ele
!
      integer, intent(in) ::  id_vtk
!
      integer(kind = kint), parameter :: ilength = 5 + 1
      character(len=ilength), allocatable, target :: textbuf_n(:)
!
      integer(kind = kint) :: icellid
      integer(kind = kint_gl) :: iele
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      icellid = vtk_cell_type(nnod_ele)
      ioffset = int(ioff_gl + ilength * istack)
      ioff_gl = ioff_gl + ilength * nt_ele
!
      if(nele .le. 0) return
      allocate(textbuf_n(nele))
!
      do iele = 1, nele
        textbuf_n(iele) = vtk_each_cell_type(icellid)
      end do
      call calypso_mpi_seek_write_ext(id_vtk, ioffset, (nele*ilength),  &
     &    textbuf_n(1))
      deallocate(textbuf_n)
!
      end subroutine write_vtk_celltype_mpi
!
! -----------------------------------------------------------------------
!
      end module vtk_file_MPI_IO
