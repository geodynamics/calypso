!
!      module vtk_file_IO
!
!      Written by H. Matsui on Feb., 2007
!
!      subroutine write_vtk_file(file_name, id_vtk,                     &
!     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,   &
!     &          ncomp_field, field_name, d_nod)
!      subroutine write_vtk_phys(file_name, id_vtk,                     &
!     &          nnod, num_field, ntot_comp, ncomp_field,               &
!     &          field_name, d_nod)
!      subroutine write_vtk_grid(file_name, id_vtk,                     &
!     &          nnod, nele, nnod_ele, xx, ie)
!
!      subroutine output_multi_vtk_file(file_name, id_vtk,              &
!     &         ntot_nod, ist_nod, ied_nod, xx,                         &
!     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie,               &
!     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine output_multi_vtk_phys(file_name, id_vtk,              &
!     &         ntot_nod, ist_nod, ied_nod,                             &
!     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!      subroutine output_multi_vtk_grid(file_name, id_vtk,              &
!     &         ntot_nod, ist_nod, ied_nod, xx,                         &
!     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie)
!
      module vtk_file_IO
!
      use m_precision
      use m_constants
!
      use vtk_data_IO
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_vtk_file(file_name, id_vtk,                      &
     &          nnod, nele, nnod_ele, xx, ie, num_field,  ntot_comp,    &
     &          ncomp_field, field_name, d_nod)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
!
      call write_vtk_mesh(id_vtk, nnod, nele, nnod_ele, xx, ie)
!
      call write_vtk_data(id_vtk, nnod, num_field, ntot_comp,           &
     &    ncomp_field, field_name, d_nod)
!
      close(id_vtk)
!
      end subroutine write_vtk_file
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_phys(file_name, id_vtk,                      &
     &          nnod, num_field, ntot_comp, ncomp_field,                &
     &          field_name, d_nod)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      integer (kind=kint), intent(in) :: nnod
      integer (kind=kint), intent(in) :: num_field, ntot_comp
      integer(kind=kint ), intent(in) :: ncomp_field(num_field)
      character(len=kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
!
      call write_vtk_data(id_vtk, nnod, num_field, ntot_comp,           &
     &    ncomp_field, field_name, d_nod)
!
      close(id_vtk)
!
      end subroutine write_vtk_phys
!
! -----------------------------------------------------------------------
!
      subroutine write_vtk_grid(file_name, id_vtk,                      &
     &          nnod, nele, nnod_ele, xx, ie)
!
      integer(kind = kint), intent(in) ::  id_vtk
      character(len=kchara), intent(in) :: file_name
!
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint), intent(in) :: ie(nele,nnod_ele)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
!
      open(id_vtk, file=file_name, form='formatted', status ='unknown')
      call write_vtk_mesh(id_vtk, nnod, nele, nnod_ele, xx, ie)
      close(id_vtk)
!
      end subroutine write_vtk_grid
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_multi_vtk_file(file_name, id_vtk,               &
     &         ntot_nod, ist_nod, ied_nod, xx,                          &
     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie,                &
     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint), intent(in) :: ntot_nod
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      real(kind = kreal), intent(in) :: xx(ntot_nod, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: ie(ntot_ele,nnod_ele)
!
      integer(kind = kint), intent(in) :: num_field, ntot_comp
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
      real(kind = kreal), intent(in) :: d_nod(ntot_nod, ntot_comp)
!
!
      write(*,*) 'write ascii VTK data: ', trim(file_name)
      open (id_vtk, file=file_name, form='formatted')
!
      call write_multi_vtk_mesh(id_vtk, ntot_nod, ist_nod, ied_nod,     &
     &    ntot_ele, nnod_ele, ist_ele, ied_ele, xx, ie)
      call write_multi_vtk_data(id_vtk, ntot_nod, ist_nod, ied_nod,     &
     &    num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      close(id_vtk)
!
      end subroutine output_multi_vtk_file
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_vtk_phys(file_name, id_vtk,               &
     &         ntot_nod, ist_nod, ied_nod,                              &
     &         num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: ncomp_field(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
!
      integer(kind = kint), intent(in) :: ntot_nod, ntot_comp
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      real(kind = kreal), intent(in) :: d_nod(ntot_nod, ntot_comp)
!
!
      write(*,*) 'write ascii VTK field data: ', trim(file_name)
      open (id_vtk, file=file_name, form='formatted')
!
      call write_multi_vtk_data(id_vtk, ntot_nod, ist_nod, ied_nod,     &
     &    num_field, ntot_comp, ncomp_field, field_name, d_nod)
!
      close(id_vtk)
!
      end subroutine output_multi_vtk_phys
!
! ----------------------------------------------------------------------
!
      subroutine output_multi_vtk_grid(file_name, id_vtk,               &
     &         ntot_nod, ist_nod, ied_nod, xx,                          &
     &         ntot_ele, nnod_ele, ist_ele, ied_ele, ie)
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) ::  id_vtk
!
      integer(kind = kint), intent(in) :: ntot_nod
      integer(kind = kint), intent(in) :: ist_nod, ied_nod
      real(kind = kreal), intent(in) :: xx(ntot_nod, 3)
!
      integer(kind = kint), intent(in) :: ntot_ele, nnod_ele
      integer(kind = kint), intent(in) :: ist_ele, ied_ele
      integer(kind = kint), intent(in) :: ie(ntot_ele,nnod_ele)
!
!
      write(*,*) 'write ascii VTK grid data: ', trim(file_name)
      open (id_vtk, file=file_name, form='formatted')
!
      call write_multi_vtk_mesh(id_vtk, ntot_nod, ist_nod, ied_nod,     &
     &    ntot_ele, nnod_ele, ist_ele, ied_ele, xx, ie)
!
      close(id_vtk)
!
      end subroutine output_multi_vtk_grid
!
! ----------------------------------------------------------------------
!
      end module vtk_file_IO
