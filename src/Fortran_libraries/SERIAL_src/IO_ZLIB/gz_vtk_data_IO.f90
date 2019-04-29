!>@file  gz_vtk_data_IO.f90
!!       module gz_vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for gzipped VTK data segments
!!
!!@verbatim
!!      subroutine write_gz_vtk_fields_head(nnod)
!!      subroutine write_gz_vtk_each_field_head(ncomp_field, field_name)
!!
!!      subroutine write_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,&
!!     &          d_nod)
!!
!!      subroutine write_gz_vtk_node_head(nnod)
!!
!!      subroutine write_gz_vtk_connect_head(nele, nnod_ele)
!!      subroutine write_gz_vtk_cell_type(nele, nnod_ele)
!!
!!      subroutine write_gz_vtk_connect_data(ntot_ele, nnod_ele,        &
!!     &          nele, ie)
!!
!!
!!      subroutine read_gz_vtk_fields_head(nnod)
!!      subroutine read_gz_vtk_each_field_head(iflag_end,               &
!!     &          ncomp_field, field_name)
!!
!!      subroutine read_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,&
!!     &          d_nod)
!!
!!      subroutine read_gz_vtk_node_head(nnod)
!!
!!      subroutine read_gz_vtk_connect_head(nele, nnod_ele)
!!      subroutine read_gz_vtk_cell_type(nele)
!!
!!      subroutine read_gz_vtk_connect_data(ntot_ele, nnod_ele,         &
!!     &          nele, ie)
!!@endverbatim
!!
!!@n @param iflag_end              Integer flag for the end of file
!!@n @param nnod                   Number of nodes
!!@n @param nele                   Number of elements
!!@n @param nnod_ele               Number of nodes for each element
!!@n @param xx(nnod,3)             position of nodes
!!@n @param nnod_ele               number of nodes for each element
!!@n @param ie(nele,nnod_ele)      element connectivity
!!@n @param ntot_comp              total number of components
!!@n @param ncomp_field(num_field) number of components
!!@n @param field_name(num_field)  list of field names
!!@n @param d_nod(nnod,ntot_comp)  field data
!
      module gz_vtk_data_IO
!
      use m_precision
      use m_constants
!
      use skip_gz_comment
      use vtk_data_to_buffer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_vtk_fields_head(nnod)
!
      use calypso_mpi
!
      integer(kind=kint_gl), intent(in) :: nnod
!
!
      textbuf =  vtk_fields_head(nnod) // char(0)
      call gz_write_textbuf_no_lf
!
      end subroutine write_gz_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_each_field_head(ncomp_field, field_name)
!
      use m_phys_constants
!
      integer(kind=kint ), intent(in) :: ncomp_field
      character(len=kchara), intent(in) :: field_name
!
!
      if (ncomp_field .eq. n_scalar) then
        textbuf = vtk_scalar_head(field_name) // char(0)
      else if (ncomp_field .eq. n_vector) then
        textbuf =  vtk_vector_head(field_name) // char(0)
      else if (ncomp_field .eq. n_sym_tensor) then
        textbuf =  vtk_tensor_head(field_name) // char(0)
      end if
      call gz_write_textbuf_no_lf
!
      end subroutine write_gz_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,   &
     &          d_nod)
!
      use m_phys_constants
!
      integer(kind=kint), intent(in) :: ncomp_field
      integer(kind=kint_gl), intent(in) :: ntot_nod, nnod
      real(kind = kreal), intent(in) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint_gl) :: inod
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          write(textbuf,'(3a,a1)')                                      &
     &    vtk_each_vector(d_nod(inod,1), d_nod(inod,2), d_nod(inod,3)), &
     &    vtk_each_vector(d_nod(inod,2), d_nod(inod,4), d_nod(inod,5)), &
     &    vtk_each_vector(d_nod(inod,3), d_nod(inod,5), d_nod(inod,6)), &
     &    char(0)
          call gz_write_textbuf_no_lf
        end do
      else if(ncomp_field .eq. n_vector) then
        do inod = 1, nnod
          textbuf = vtk_each_vector(d_nod(inod,1), d_nod(inod,2),       &
     &                              d_nod(inod,3)) // char(0)
          call gz_write_textbuf_no_lf
        end do
      else
        do inod = 1, nnod
          textbuf = vtk_each_scalar(d_nod(inod,1)) // char(0)
          call gz_write_textbuf_no_lf
        end do
      end if
!
      end subroutine write_gz_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_node_head(nnod)
!
      integer(kind = kint_gl), intent(in) :: nnod
!
!
      textbuf =  vtk_node_head(nnod) // char(0)
      call gz_write_textbuf_no_lf
!
      end subroutine write_gz_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_connect_head(nele, nnod_ele)
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
!
      textbuf =  vtk_connect_head(nele, nnod_ele) // char(0)
      call gz_write_textbuf_no_lf
!
      end subroutine write_gz_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_cell_type(nele, nnod_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint) :: icellid
      integer(kind = kint_gl) :: iele
!
!
      icellid = vtk_cell_type(nnod_ele)
!
      textbuf = vtk_cell_type_head(nele) // char(0)
      call gz_write_textbuf_no_lf
!
      do iele = 1, nele
        textbuf =  vtk_each_cell_type(icellid) // char(0)
        call gz_write_textbuf_no_lf
      end do
!
      end subroutine write_gz_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_connect_data(ntot_ele, nnod_ele,          &
     &          nele, ie)
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ntot_ele, nele
      integer(kind = kint_gl), intent(in) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint_gl), dimension(nnod_ele) :: ie0
!
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        textbuf =  vtk_each_connect(nnod_ele,ie0) // char(0)
        call gz_write_textbuf_no_lf
      end do
!
      end subroutine write_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_fields_head(nnod)
!
      integer(kind=kint_gl), intent(inout) :: nnod
      character(len=kchara)  :: label
!
!
      call skip_gz_comment_chara_lint(label, nnod)
!
      end subroutine read_gz_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_each_field_head(iflag_end,                 &
     &          ncomp_field, field_name)
!
      use m_phys_constants
!
      integer(kind=kint ), intent(inout) :: iflag_end, ncomp_field
      character(len=kchara), intent(inout) :: field_name
!
      integer(kind = kint) :: nchara
      character(len=kchara)  :: vtk_fld_type
!
!
      call get_one_line_from_gz_f
      if(nchara .eq. izero) go to 99
!
      read(textbuf,*) vtk_fld_type, field_name
      if(vtk_fld_type .eq. 'TENSORS') then
        ncomp_field = n_sym_tensor
      else if(vtk_fld_type .eq. 'VECTORS') then
        ncomp_field = n_vector
      else
        call get_one_line_from_gz_f
        ncomp_field = n_scalar
      end if
      iflag_end = izero
      return
!
  99  continue
      iflag_end = ione
      return
!
      end subroutine read_gz_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_each_field(ntot_nod, ncomp_field, nnod,   &
     &          d_nod)
!
      use m_phys_constants
!
      integer(kind=kint), intent(in) :: ncomp_field
      integer(kind=kint_gl), intent(in) :: ntot_nod, nnod
      real(kind = kreal), intent(inout) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint_gl) :: inod
      real(kind = kreal) :: rtmp
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          call get_one_line_from_gz_f
          read(textbuf,*) d_nod(inod,1:3)
          call get_one_line_from_gz_f
          read(textbuf,*) rtmp, d_nod(inod,4:5)
          call get_one_line_from_gz_f
          read(textbuf,*) rtmp, rtmp, d_nod(inod,6)
        end do
      else
        do inod = 1, nnod
          call get_one_line_from_gz_f
          read(textbuf,*) d_nod(inod,1:ncomp_field)
        end do
      end if
!
      end subroutine read_gz_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_node_head(nnod)
!
      integer(kind = kint), intent(inout) :: nnod
!
      character(len=kchara) :: tmpchara
!
!
      call skip_gz_comment_chara(tmpchara)
      call get_one_line_from_gz_f
      call get_one_line_from_gz_f
!
      call get_one_line_from_gz_f
      read(textbuf,'(a,i16,a)')  tmpchara, nnod
!
      end subroutine read_gz_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_connect_head(nele, nnod_ele)
!
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
!
      integer(kind = kint_gl) :: nums
      character(len=kchara) :: tmpchara
!
!
      call get_one_line_from_gz_f
      read(textbuf,*) tmpchara, nele, nums
      nnod_ele = int(nums/nele, KIND(nnod_ele)) - 1
!
      end subroutine read_gz_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_cell_type(nele)
!
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint_gl) :: iele
!
!
      call get_one_line_from_gz_f
!
      do iele = 1, nele
        call get_one_line_from_gz_f
      end do
!
      end subroutine read_gz_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_connect_data(ntot_ele, nnod_ele,           &
     &          nele, ie)
!
      integer(kind = kint_gl), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: nele
      integer(kind = kint), intent(inout) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: itmp
!
!
      do iele = 1, nele
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, ie(iele,1:nnod_ele)
        ie(iele,1:nnod_ele) = ie(iele,1:nnod_ele) + 1
      end do
!
      end subroutine read_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_data_IO
