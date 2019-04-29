!>@file  vtk_data_IO.f90
!!       module vtk_data_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for VTK data segments
!!
!!@verbatim
!!      subroutine write_vtk_fields_head(id_vtk, nnod)
!!      subroutine write_vtk_each_field_head(id_vtk, ncomp_field,       &
!!     &          field_name)
!!
!!      subroutine write_vtk_each_field(id_vtk, ntot_nod, ncomp_field,  &
!!     &          nnod, d_nod)
!!
!!      subroutine write_vtk_node_head(id_vtk, nnod)
!!
!!      subroutine write_vtk_connect_data                               &
!!     &         (id_vtk, ntot_ele, nnod_ele, nele, ie)
!!
!!      subroutine read_vtk_fields_head(id_vtk, nnod)
!!      subroutine read_vtk_each_field_head(id_vtk, iflag_end,          &
!!     &          ncomp_field, field_name)
!!      subroutine read_vtk_each_field(id_vtk, ntot_nod, ncomp_field,   &
!!     &          nnod, d_nod)
!!
!!      subroutine read_vtk_node_head(id_vtk, nnod)
!!      subroutine read_vtk_connect_head(id_vtk, nele, nnod_ele)
!!      subroutine read_vtk_cell_type(id_vtk, nele)
!!      subroutine read_vtk_connect_data(id_vtk, ntot_ele, nnod_ele,    &
!!     &          nele, ie)
!!@endverbatim
!!
!!@n @param id_vtk                 file ID for VTK data file
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
      module vtk_data_IO
!
      use m_precision
      use m_constants
      use vtk_data_to_buffer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_fields_head(id_vtk, nnod)
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) :: id_vtk
!
!
      write(id_vtk,'(a)',advance='NO') vtk_fields_head(nnod)
! 
      end subroutine write_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_each_field_head(id_vtk, ncomp_field,         &
     &          field_name)
!
      use m_phys_constants
!
      integer(kind=kint ), intent(in) :: ncomp_field
      character(len=kchara), intent(in) :: field_name
!
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      if (ncomp_field .eq. n_scalar) then
        write(id_vtk,'(a)',advance='NO') vtk_scalar_head(field_name)
      else if (ncomp_field .eq. n_vector) then
        write(id_vtk,'(a)',advance='NO') vtk_vector_head(field_name)
      else if (ncomp_field .eq. n_sym_tensor) then
        write(id_vtk,'(a)',advance='NO') vtk_tensor_head(field_name)
      end if
!
      end subroutine write_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_each_field(id_vtk, ntot_nod, ncomp_field,    &
     &          nnod, d_nod)
!
      use m_phys_constants
!
      integer(kind=kint_gl), intent(in) :: ntot_nod, nnod
      integer(kind=kint), intent(in) :: ncomp_field
      real(kind = kreal), intent(in) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint_gl) :: inod
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          write(id_vtk,'(1p3E23.12e3,a1)')                              &
     &          d_nod(inod,1), d_nod(inod,2), d_nod(inod,3)
          write(id_vtk,'(1p3E23.12e3,a1)')                              &
     &          d_nod(inod,2), d_nod(inod,4), d_nod(inod,5)
          write(id_vtk,'(1p3E23.12e3,a1)')                              &
     &          d_nod(inod,3), d_nod(inod,5), d_nod(inod,6)
        end do
      else if(ncomp_field .eq. n_vector) then
        do inod = 1, nnod
          write(id_vtk,'(1p3E23.12e3,a1)')                              &
     &          d_nod(inod,1), d_nod(inod,2), d_nod(inod,3)
        end do
      else
        do inod = 1, nnod
          write(id_vtk,'(1pE23.12e3,a1)') d_nod(inod,1)
        end do
      end if
!
      end subroutine write_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_node_head(id_vtk, nnod)
!
      integer(kind = kint_gl), intent(in) :: nnod
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      write(id_vtk,'(a)',advance='NO') vtk_node_head(nnod)
!
      end subroutine write_vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_connect_data                                 &
     &         (id_vtk, ntot_ele, nnod_ele, nele, ie)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: id_vtk
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ntot_ele, nele
      integer(kind = kint_gl), intent(in) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint_gl) :: ie0(nnod_ele)
      integer(kind = kint) :: icellid
!
!
      icellid = vtk_cell_type(nnod_ele)
      write(id_vtk,'(a)',advance='NO') vtk_connect_head(nele, nnod_ele)
!
      do iele = 1, nele
        ie0(1:nnod_ele) = ie(iele,1:nnod_ele) - 1
        write(id_vtk,'(a)',advance='NO')                                &
     &         vtk_each_connect(nnod_ele, ie0)
      end do
!
      write(id_vtk,'(a)',advance='NO') vtk_cell_type_head(nele)
      do iele = 1, nele
        write(id_vtk,'(a)',advance='NO') vtk_each_cell_type(icellid)
      end do
!
      end subroutine  write_vtk_connect_data
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_vtk_fields_head(id_vtk, nnod)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_vtk
      integer(kind=kint_gl), intent(inout) :: nnod
!
      character(len=kchara)  :: tmpchara, label
!
!
      call skip_comment(tmpchara, id_vtk)
      read(tmpchara,*) label, nnod
! 
      end subroutine read_vtk_fields_head
!
! -----------------------------------------------------------------------
!
      subroutine read_vtk_each_field_head(id_vtk, iflag_end,            &
     &          ncomp_field, field_name)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) ::  id_vtk
      integer(kind=kint ), intent(inout) :: ncomp_field, iflag_end
      character(len=kchara), intent(inout) :: field_name
!
      character(len=kchara)  :: vtk_fld_type, tmpchara
!
!
      read(id_vtk,*,err=99) vtk_fld_type, field_name
      if(vtk_fld_type .eq. 'TENSORS') then
        ncomp_field = n_sym_tensor
      else if(vtk_fld_type .eq. 'VECTORS') then
        ncomp_field = n_vector
      else
        read(id_vtk,*) tmpchara
        ncomp_field = n_scalar
      end if
      iflag_end = 0
      return
!
  99  continue
      iflag_end = 1
      return
!
      end subroutine read_vtk_each_field_head
!
! -----------------------------------------------------------------------
!
      subroutine read_vtk_each_field(id_vtk, ntot_nod, ncomp_field,     &
     &          nnod, d_nod)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) ::  id_vtk, ncomp_field
      integer(kind=kint_gl), intent(in) :: ntot_nod, nnod
!
      real(kind = kreal), intent(inout) :: d_nod(ntot_nod,ncomp_field)
!
      integer(kind = kint_gl) :: inod
      real(kind = kreal) :: rtmp
!
!
      if (ncomp_field .eq. n_sym_tensor) then
        do inod = 1, nnod
          read(id_vtk,*) d_nod(inod,1:3)
          read(id_vtk,*) rtmp, d_nod(inod,4:5)
          read(id_vtk,*) rtmp, rtmp, d_nod(inod,6)
        end do
      else
        do inod = 1, nnod
          read(id_vtk,*) d_nod(inod,1:ncomp_field)
        end do
      end if
!
      end subroutine read_vtk_each_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_vtk_node_head(id_vtk, nnod)
!
      integer(kind = kint), intent(in) :: id_vtk
      integer(kind = kint_gl), intent(inout) :: nnod
!
      character(len=kchara) :: tmpchara
!
!
      read(id_vtk,*) tmpchara
      read(id_vtk,*) tmpchara
      read(id_vtk,*) tmpchara
      read(id_vtk,*) tmpchara
!
      read(id_vtk,*)  tmpchara, nnod
!
      end subroutine read_vtk_node_head
!
! -----------------------------------------------------------------------
!
      subroutine read_vtk_connect_head(id_vtk, nele, nnod_ele)
!
      integer(kind = kint), intent(in) ::  id_vtk
      integer(kind = kint), intent(inout) :: nnod_ele
      integer(kind = kint_gl), intent(inout) :: nele
!
      integer(kind = kint_gl) :: nums
      character(len=kchara) :: tmpchara
!
!
      read(id_vtk,*) tmpchara, nele, nums
      nnod_ele = int(nums/nele,KIND(nnod_ele)) - 1
!
      end subroutine read_vtk_connect_head
!
! -----------------------------------------------------------------------
!
      subroutine read_vtk_cell_type(id_vtk, nele)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: id_vtk
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: icellid
      character(len=kchara) :: tmpchara
!
!
      read(id_vtk,*) tmpchara
      do iele = 1, nele
        read(id_vtk,*) icellid
      end do
!
      end subroutine read_vtk_cell_type
!
! -----------------------------------------------------------------------
!
      subroutine read_vtk_connect_data(id_vtk, ntot_ele, nnod_ele,      &
     &          nele, ie)
!
      integer(kind = kint), intent(in) :: id_vtk, nnod_ele
      integer(kind = kint_gl), intent(in) :: ntot_ele
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint_gl), intent(inout) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint_gl) :: iele
      integer(kind = kint) :: itmp
!
!
      do iele = 1, nele
        read(id_vtk,*) itmp, ie(iele,1:nnod_ele)
        ie(iele,1:nnod_ele) = ie(iele,1:nnod_ele) + 1
      end do
!
      end subroutine  read_vtk_connect_data
!
! ----------------------------------------------------------------------
!
      end module vtk_data_IO
