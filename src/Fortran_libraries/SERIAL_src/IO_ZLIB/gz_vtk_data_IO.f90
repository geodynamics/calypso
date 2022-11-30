!>@file  gz_vtk_data_IO.f90
!!       module gz_vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for gzipped VTK data segments
!!
!!@verbatim
!!      subroutine write_gz_vtk_fields_head(FPz_f, nnod, zbuf)
!!      subroutine write_gz_vtk_each_field                              &
!!     &         (FPz_f, ntot_nod, ncomp_field, nnod, d_nod, zbuf)
!!      subroutine write_gz_vtk_data(FPz_f, nnod, num_field, ntot_comp, &
!!     &          ncomp_field, field_name, d_nod, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!       type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine write_gz_vtk_node_head(FPz_f, nnod, zbuf)
!!      subroutine write_gz_vtk_connect_head                            &
!!     &         (FPz_f, nele, nnod_ele, zbuf)
!!      subroutine write_gz_vtk_cell_type(FPz_f, nele, nnod_ele, zbuf)
!!      subroutine write_gz_vtk_connect_data                            &
!!     &         (FPz_f, ntot_ele, nnod_ele, nele, ie, zbuf)
!!
!!      subroutine read_gz_vtk_fields_head(FPz_f, nnod, zbuf)
!!      subroutine read_gz_vtk_each_field_head                          &
!!     &         (FPz_f, iflag_end, ncomp_field, field_name, zbuf)
!!      subroutine read_gz_vtk_each_field                               &
!!     &         (FPz_f, ntot_nod, ncomp_field, nnod, d_nod, zbuf)
!!      subroutine read_gz_vtk_node_head(FPz_f, nnod, zbuf)
!!      subroutine read_gz_vtk_connect_head(FPz_f, nele, nnod_ele, zbuf)
!!      subroutine read_gz_vtk_cell_type(FPz_f, nele, zbuf)
!!      subroutine read_gz_vtk_connect_data                             &
!!     &         (ntot_ele, nnod_ele, nele, ie, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        type(buffer_4_gzip), intent(inout) :: zbuf
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
      use m_phys_constants
!
      use t_buffer_4_gzip
      use vtk_data_to_buffer
!
      implicit none
!
      private :: write_gz_vtk_each_field_head
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_vtk_fields_head(FPz_f, nnod, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint_gl), intent(in) :: nnod
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) =  vtk_fields_head(nnod) // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      end subroutine write_gz_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_each_field_head                           &
     &         (FPz_f, ncomp_field, field_name, zbuf)
!
      use m_phys_constants
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint ), intent(in) :: ncomp_field
      character(len=kchara), intent(in) :: field_name
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      if (ncomp_field .eq. n_scalar) then
        zbuf%fixbuf(1) = vtk_scalar_head(field_name) // char(0)
      else if (ncomp_field .eq. n_vector) then
        zbuf%fixbuf(1) =  vtk_vector_head(field_name) // char(0)
      else if (ncomp_field .eq. n_sym_tensor) then
        zbuf%fixbuf(1) =  vtk_tensor_head(field_name) // char(0)
      end if
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      end subroutine write_gz_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_each_field                                &
     &         (FPz_f, ntot_nod, ncomp_field, nnod, d_nod, zbuf)
!
      use m_phys_constants
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint), intent(in) :: ncomp_field
      integer(kind=kint_gl), intent(in) :: ntot_nod, nnod
      real(kind = kreal), intent(in) :: d_nod(ntot_nod,ncomp_field)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: inod
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          write(zbuf%fixbuf(1),'(3a,a1)')                               &
     &    vtk_each_vector(d_nod(inod,1), d_nod(inod,2), d_nod(inod,3)), &
     &    vtk_each_vector(d_nod(inod,2), d_nod(inod,4), d_nod(inod,5)), &
     &    vtk_each_vector(d_nod(inod,3), d_nod(inod,5), d_nod(inod,6)), &
     &    char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
        end do
      else if(ncomp_field .eq. n_vector) then
        do inod = 1, nnod
          zbuf%fixbuf(1)                                                &
     &            = vtk_each_vector(d_nod(inod,1), d_nod(inod,2),       &
     &                              d_nod(inod,3)) // char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
        end do
      else
        do inod = 1, nnod
          zbuf%fixbuf(1) = vtk_each_scalar(d_nod(inod,1)) // char(0)
          call gz_write_textbuf_no_lf(FPz_f, zbuf)
        end do
      end if
!
      end subroutine write_gz_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_node_head(FPz_f, nnod, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint_gl), intent(in) :: nnod
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) =  vtk_node_head(nnod) // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      end subroutine write_gz_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_connect_head                              &
     &         (FPz_f, nele, nnod_ele, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      zbuf%fixbuf(1) =  vtk_connect_head(nele, nnod_ele) // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      end subroutine write_gz_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_cell_type(FPz_f, nele, nnod_ele, zbuf)
!
      use m_geometry_constants
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nele
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: icellid
      integer(kind = kint_gl) :: iele
!
!
      icellid = vtk_cell_type(nnod_ele)
!
      zbuf%fixbuf(1) = vtk_cell_type_head(nele) // char(0)
      call gz_write_textbuf_no_lf(FPz_f, zbuf)
!
      do iele = 1, nele
        zbuf%fixbuf(1) =  vtk_each_cell_type(icellid) // char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine write_gz_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_connect_data                              &
     &         (FPz_f, ntot_ele, nnod_ele, nele, ie, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ntot_ele, nele
      integer(kind = kint_gl), intent(in) :: ie(ntot_ele,nnod_ele)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint_gl), dimension(nnod_ele) :: ie0
!
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        zbuf%fixbuf(1) =  vtk_each_connect(nnod_ele,ie0) // char(0)
        call gz_write_textbuf_no_lf(FPz_f, zbuf)
      end do
!
      end subroutine write_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_data(FPz_f, nnod, num_field, ntot_comp,   &
     &          ncomp_field, field_name, d_nod, zbuf)
!
      character, pointer, intent(in) :: FPz_f
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
      IF(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          call write_gz_vtk_each_field_head                             &
     &       (FPz_f, ncomp_field(j), field_name(j), zbuf)
          call write_gz_vtk_each_field                                  &
     &       (FPz_f, nnod, ncomp_field(j), nnod, d_nod(1,icou), zbuf)
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_gz_vtk_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_fields_head(FPz_f, nnod, zbuf)
!
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint_gl), intent(inout) :: nnod
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara)  :: label
!
!
      call skip_gz_comment_chara_lint(FPz_f, label, nnod, zbuf)
!
      end subroutine read_gz_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_each_field_head                            &
     &         (FPz_f, iflag_end, ncomp_field, field_name, zbuf)
!
      use m_phys_constants
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint ), intent(inout) :: iflag_end, ncomp_field
      character(len=kchara), intent(inout) :: field_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara)  :: vtk_fld_type
!
!
      call get_one_line_text_from_gz(FPz_f, zbuf)
      if(len_trim(zbuf%fixbuf(1)) .eq. izero) go to 99
!
      read(zbuf%fixbuf(1),*) vtk_fld_type, field_name
      if(vtk_fld_type .eq. 'TENSORS') then
        ncomp_field = n_sym_tensor
      else if(vtk_fld_type .eq. 'VECTORS') then
        ncomp_field = n_vector
      else
        call get_one_line_text_from_gz(FPz_f, zbuf)
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
      subroutine read_gz_vtk_each_field                                 &
     &         (FPz_f, ntot_nod, ncomp_field, nnod, d_nod, zbuf)
!
      use m_phys_constants
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind=kint), intent(in) :: ncomp_field
      integer(kind=kint_gl), intent(in) :: ntot_nod, nnod
!
      real(kind = kreal), intent(inout) :: d_nod(ntot_nod,ncomp_field)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: inod
      real(kind = kreal) :: rtmp
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          call get_one_line_text_from_gz(FPz_f, zbuf)
          read(zbuf%fixbuf(1),*) d_nod(inod,1:3)
          call get_one_line_text_from_gz(FPz_f, zbuf)
          read(zbuf%fixbuf(1),*) rtmp, d_nod(inod,4:5)
          call get_one_line_text_from_gz(FPz_f, zbuf)
          read(zbuf%fixbuf(1),*) rtmp, rtmp, d_nod(inod,6)
        end do
      else
        do inod = 1, nnod
          call get_one_line_text_from_gz(FPz_f, zbuf)
          read(zbuf%fixbuf(1),*) d_nod(inod,1:ncomp_field)
        end do
      end if
!
      end subroutine read_gz_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_node_head(FPz_f, nnod, zbuf)
!
      use gzip_file_access
      use skip_gz_comment
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint_gl), intent(inout) :: nnod
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      character(len=kchara) :: tmpchara
!
!
      call skip_gz_comment_chara(FPz_f, tmpchara, zbuf)
      call get_one_line_text_from_gz(FPz_f, zbuf)
      call get_one_line_text_from_gz(FPz_f, zbuf)
!
      call get_one_line_text_from_gz(FPz_f, zbuf)
      read(zbuf%fixbuf(1),'(a,i16)')  tmpchara, nnod
!
      end subroutine read_gz_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_connect_head(FPz_f, nele, nnod_ele, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: nums
      character(len=kchara) :: tmpchara
!
!
      call get_one_line_text_from_gz(FPz_f, zbuf)
      read(zbuf%fixbuf(1),*) tmpchara, nele, nums
      nnod_ele = int(nums/nele, KIND(nnod_ele)) - 1
!
      end subroutine read_gz_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_cell_type(FPz_f, nele, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint_gl), intent(in) :: nele
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: iele
!
!
      call get_one_line_text_from_gz(FPz_f, zbuf)
!
      do iele = 1, nele
        call get_one_line_text_from_gz(FPz_f, zbuf)
      end do
!
      end subroutine read_gz_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine read_gz_vtk_connect_data                               &
     &         (FPz_f, ntot_ele, nnod_ele, nele, ie, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint_gl), intent(in) :: ntot_ele, nele
      integer(kind = kint), intent(in) :: nnod_ele
!
      integer(kind = kint_gl), intent(inout) :: ie(ntot_ele,nnod_ele)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: itmp
!
!
      do iele = 1, nele
        call get_one_line_text_from_gz(FPz_f, zbuf)
        read(zbuf%fixbuf(1),*) itmp, ie(iele,1:nnod_ele)
        ie(iele,1:nnod_ele) = ie(iele,1:nnod_ele) + 1
      end do
!
      end subroutine read_gz_vtk_connect_data
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_data_IO
