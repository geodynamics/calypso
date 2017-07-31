!>@file  gz_vtk_file_IO.f90
!!       module gz_vtk_file_IO
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for gzipped VTK data segments
!!
!!@verbatim
!!      subroutine write_gz_vtk_file(gzip_name,                         &
!!     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,  &
!!     &          ncomp_field, field_name, d_nod)
!!      subroutine write_gz_vtk_phys(gzip_name, nnod, num_field,        &
!!     &          ntot_comp, ncomp_field, field_name, d_nod)
!!      subroutine write_gz_vtk_grid(gzip_name, nnod, nele, nnod_ele,   &
!!     &          xx, ie)
!!@endverbatim
!
      module gz_vtk_file_IO
!
      use m_precision
      use m_constants
!
      use gz_vtk_data_IO
      use skip_gz_comment
!
      implicit none
!
      private :: write_gz_vtk_data, write_gz_vtk_mesh
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_gz_vtk_file(gzip_name,                           &
     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,    &
     &          ncomp_field, field_name, d_nod)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: gzip_name
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      call open_wt_gzfile_f(gzip_name)
      call write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
      call write_gz_vtk_data(nnod, num_field, ntot_comp, ncomp_field,   &
     &    field_name, d_nod)
!
      call close_gzfile_f
!
      end subroutine write_gz_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_phys(gzip_name, nnod, num_field,          &
     &          ntot_comp, ncomp_field, field_name, d_nod)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: gzip_name
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      call open_wt_gzfile_f(gzip_name)
      call write_gz_vtk_data(nnod, num_field, ntot_comp, ncomp_field,   &
     &    field_name, d_nod)
      call close_gzfile_f
!
      end subroutine write_gz_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_grid(gzip_name, nnod, nele, nnod_ele,     &
     &          xx, ie)
!
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: gzip_name
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
!
      call open_wt_gzfile_f(gzip_name)
      call write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
      call close_gzfile_f
!
      end subroutine write_gz_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_data(nnod, num_field, ntot_comp,          &
     &          ncomp_field, field_name, d_nod)
!
      integer(kind=kint_gl), intent(in) :: nnod
      integer(kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint) :: icou, j
!
!
      call write_gz_vtk_fields_head(nnod)
!
      IF(ntot_comp .ge. 1) then
        icou = 1
        do j = 1, num_field
          call write_gz_vtk_each_field_head(ncomp_field(j),             &
     &        field_name(j) )
          call write_gz_vtk_each_field(nnod, ncomp_field(j), nnod,      &
     &        d_nod(1,icou)  )
          icou = icou + ncomp_field(j)
        end do
      end if
!
      end subroutine write_gz_vtk_data
!
! -----------------------------------------------------------------------
!
      subroutine write_gz_vtk_mesh(nnod, nele, nnod_ele, xx, ie)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: nnod, nele
      integer(kind = kint_gl), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
!
      call write_gz_vtk_node_head(nnod)
      call write_gz_vtk_each_field(nnod, n_vector, nnod, xx)
!
      call write_gz_vtk_connect_head(nele, nnod_ele)
      call write_gz_vtk_connect_data(nele, nnod_ele, nele, ie)
!
      call write_gz_vtk_cell_type(nele, nnod_ele)
!
      end subroutine write_gz_vtk_mesh
!
! -----------------------------------------------------------------------
!
      end module gz_vtk_file_IO
