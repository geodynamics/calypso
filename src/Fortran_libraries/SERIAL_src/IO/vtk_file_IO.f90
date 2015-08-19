!>@file  vtk_file_IO.f90
!!       module vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in March, 2007
!!@n      Modified by H. Matsui in July, 2013
!
!> @brief Output VTK file
!!
!!@verbatim
!!      subroutine write_vtk_file(file_name, id_vtk,                    &
!!     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,  &
!!     &          ncomp_field, field_name, d_nod)
!!      subroutine write_vtk_phys(file_name, id_vtk,                    &
!!     &          nnod, num_field, ntot_comp, ncomp_field,              &
!!     &          field_name, d_nod)
!!      subroutine write_vtk_grid(file_name, id_vtk,                    &
!!     &          nnod, nele, nnod_ele, xx, ie)
!!@endverbatim
!
      module vtk_file_IO
!
      use m_precision
      use m_constants
!
      use vtk_data_IO
      use t_ucd_data
!
      implicit none
!
      private :: write_vtk_data, write_vtk_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vtk_file(file_name, id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
!
      call write_vtk_mesh(id_vtk, ucd%nnod, ucd%nele, ucd%nnod_4_ele,   &
     &    ucd%xx, ucd%ie)
!
      call write_vtk_data(id_vtk, ucd%nnod, ucd%num_field,              &
     &    ucd%ntot_comp, ucd%num_comp, ucd%phys_name, ucd%d_ucd)
      endfile(id_vtk)
!
      close(id_vtk)
!
      end subroutine write_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys(file_name, id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
!
      call write_vtk_data                                               &
     &   (id_vtk, ucd%nnod, ucd%num_field, ucd%ntot_comp,               &
     &    ucd%num_comp, ucd%phys_name, ucd%d_ucd)
      endfile(id_vtk)
!
      close(id_vtk)
!
      end subroutine write_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid(file_name, id_vtk, ucd)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      type(ucd_data), intent(in) :: ucd
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
      call write_vtk_mesh(id_vtk, ucd%nnod, ucd%nele, ucd%nnod_4_ele,   &
     &    ucd%xx, ucd%ie)
      close(id_vtk)
!
      end subroutine write_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_vtk_data(id_vtk, nnod, num_field, ntot_comp,     &
     &          ncomp_field, field_name, d_nod)
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint) :: icou, j
!
!
      call write_vtk_fields_head(id_vtk, nnod)
!
      IF(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          call write_vtk_each_field_head(id_vtk, ncomp_field(j),        &
     &        field_name(j) )
          call write_vtk_each_field(id_vtk, nnod, ncomp_field(j),       &
     &        nnod, d_nod(1,icou) )
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_vtk_data
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_mesh(id_vtk, nnod, nele, nnod_ele,  xx, ie)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) ::  id_vtk
!
!
      call write_vtk_node_head(id_vtk, nnod)
      call write_vtk_each_field(id_vtk, nnod, ithree, nnod, xx)
!
      call write_vtk_connect_data(id_vtk, nele, nnod_ele, nele, ie)
!
      end subroutine write_vtk_mesh
!
! -----------------------------------------------------------------------
!
      end module vtk_file_IO
