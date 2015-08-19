!>@file  vtk_data_to_buffer.f90
!!       module vtk_data_to_buffer
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2007
!
!> @brief Output routine for VTK data segments
!!
!!@verbatim
!!      function vtk_fields_head(nnod)
!!
!!      function vtk_scalar_head(field_name)
!!      function vtk_vector_head(field_name)
!!      function vtk_tensor_head(field_name)
!!      function vtk_each_scalar(d_nod)
!!      function vtk_each_vector(d1, d2, d3)
!!
!!      function vtk_node_head(nnod)
!!
!!      function vtk_connect_head(nele, nnod_ele)
!!      function vtk_cell_type_head(nele)
!!      integer(kind = kint) function vtk_cell_type(nnod_ele)
!!      function vtk_each_cell_type(icellid)
!!
!!      function vtk_each_connect(nnod_ele, ie0)
!!@endverbatim
!!
!!@n @param nnod                   Number of nodes
!!@n @param nele                   Number of elements
!!@n @param nnod_ele               number of nodes for each element
!!@n @param ie0(nnod_ele)          element connectivity
!!@n @param ntot_comp              total number of components
!!@n @param field_name             field name
!!@n @param d_nod                  scalar data
!!@n @param d_nod                  x-component
!!@n @param d_nod                  y-component
!!@n @param d_nod                  z-component
!
      module vtk_data_to_buffer
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      character(len=26), parameter                                      &
     &   :: VTK_HD1 = '# vtk DataFile Version 2.0'
      character(len=47), parameter                                      &
     &   :: VTK_HD2 = 'converted data of tri-linear hexahedral element'
      character(len=5), parameter :: VTK_HD3 = 'ASCII'
      character(len=25), parameter                                      &
     &   :: VTK_HD4 = 'DATASET UNSTRUCTURED_GRID'
      character(len=8), parameter :: VTK_HD5 = ' POINTS '
      character(len=8), parameter :: VTK_HD6 = ' double '
!
      integer(kind = kint) :: len_VTK_HD1 = 26 + 1
      integer(kind = kint) :: len_VTK_HD2 = 47 + 1
      integer(kind = kint) :: len_VTK_HD3 =  5 + 1
      integer(kind = kint) :: len_VTK_HD4 = 25 + 1
      integer(kind = kint) :: len_VTK_HD5 =  8+ 16 + 8 + 1
!
      character(len=*), parameter :: VTK_PT1 = 'POINT_DATA '
!
      character(len=*), parameter :: VTK_CL1 = ' CELLS '
      character(len=*), parameter :: VTK_CL2 = ' CELL_TYPES '
!
      character(len=*), parameter :: VTK_SC1 = ' SCALARS '
      character(len=*), parameter :: VTK_VC1 = ' VECTORS '
      character(len=*), parameter :: VTK_TS1 = ' TENSORS '
      character(len=*), parameter :: VTK_LK1 = 'LOOKUP_TABLE default'
!
      private :: VTK_HD1, VTK_HD2, VTK_HD3, VTK_HD4, VTK_HD5, VTK_HD6
      private :: VTK_PT1, VTK_CL1, VTK_CL2
      private :: VTK_SC1, VTK_VC1, VTK_TS1, VTK_LK1
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      function make_string( n )
      integer,intent(in) :: n
      character(n) make_string
!
      write(make_string,'(a)') 'a'
!
      end function make_string
!
! -----------------------------------------------------------------------
!
      function vtk_fields_head(nnod)
!
      integer(kind=kint_gl), intent(in) :: nnod
!
      integer(kind = kint), parameter :: l1 = 1
      integer(kind = kint), parameter :: l2 = 11 + 16 + 1
      integer(kind = kint), parameter :: nchara = l1 + l2
      character(len=16) :: tmpchara
      character(len=nchara) :: vtk_fields_head
!
      write(tmpchara,'(i16)') nnod
!
      vtk_fields_head = char(10)                                        &
     &          // VTK_PT1 // tmpchara // char(10)
! 
      end function vtk_fields_head
!
! -----------------------------------------------------------------------
!
      function vtk_scalar_head(field_name)
!
      use m_phys_constants
!
      character(len=kchara), intent(in) :: field_name
!
      character(len=16) :: tmpchara
      character(len=9+len_trim(field_name) +8+16+1+20+1)                &
     &                                :: vtk_scalar_head
!
      write(tmpchara,'(i16)') ione
        vtk_scalar_head =  VTK_SC1 // trim(field_name) // VTK_HD6       &
     &                     // tmpchara  // char(10)                     &
     &                   // VTK_LK1 // char(10)
!
      end function vtk_scalar_head
!!
!! ----------------------------------------------------------------------!
      function vtk_vector_head(field_name)
!
      use m_phys_constants
!
      character(len=kchara), intent(in) :: field_name
!
      character(len=9+len_trim(field_name) +8+1) :: vtk_vector_head
!
      vtk_vector_head =  VTK_VC1 // trim(field_name) // VTK_HD6         &
     &                 // char(10)
!
      end function vtk_vector_head
!
! ----------------------------------------------------------------------
!
      function vtk_tensor_head(field_name)
!
      use m_phys_constants
!
      character(len=kchara), intent(in) :: field_name
!
      character(len=9+len_trim(field_name) +8+1) :: vtk_tensor_head
!
      vtk_tensor_head =  VTK_TS1 // trim(field_name) // VTK_HD6         &
     &                 // char(10)
!
      end function vtk_tensor_head
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      function vtk_each_scalar(d_nod)
!
      real(kind = kreal), intent(in) :: d_nod
      character(len=23+1) :: vtk_each_scalar
!
      write(vtk_each_scalar,'(1pE23.12e3,a1)') d_nod, char(10)
!
      end function vtk_each_scalar
!
! -----------------------------------------------------------------------
!
      function vtk_each_vector(d1, d2, d3)
!
      real(kind = kreal), intent(in) :: d1, d2, d3
      character(len=3*23+1) :: vtk_each_vector
!
      write(vtk_each_vector,'(1p3E23.12e3,a1)') d1, d2, d3, char(10)
!
      end function vtk_each_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function vtk_node_head(nnod)
!
      integer(kind = kint_gl), intent(in) :: nnod
!
      integer(kind = kint), parameter :: l1 = 26 + 1
      integer(kind = kint), parameter :: l2 = 47 + 1
      integer(kind = kint), parameter :: l3 =  5 + 1
      integer(kind = kint), parameter :: l4 = 25 + 1
      integer(kind = kint), parameter :: l5 = 8 + 16 + 8 + 1
      integer(kind = kint), parameter :: nchara = l1+l2+l3+l4+l5
      character(len=16) :: tmpchara
      character(len=nchara) :: vtk_node_head
!
!
      write(tmpchara,'(i16)') nnod
      vtk_node_head =  VTK_HD1 // char(10)                              &
     &          // VTK_HD2 // char(10)                                  &
     &          // VTK_HD3 // char(10)                                  &
     &          // VTK_HD4 // char(10)                                  &
     &          // VTK_HD5 // tmpchara // VTK_HD6 // char(10)
!
      end function vtk_node_head
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function vtk_connect_head(nele, nnod_ele)
!
      integer(kind = kint_gl), intent(in) :: nele
      integer(kind = kint), intent(in) :: nnod_ele
!
      integer(kind = kint), parameter :: nchara = 7 + 2*16 + 1
!
      character(len=32) :: tmpchara
      character(len=nchara) :: vtk_connect_head
!
      write(tmpchara,'(2i16)')  nele, (nele * (nnod_ele+1))
      vtk_connect_head = VTK_CL1 // tmpchara // char(10)
!
      end function vtk_connect_head
!
! -----------------------------------------------------------------------
!
      function vtk_cell_type_head(nele)
!
      integer(kind = kint_gl), intent(in) :: nele
!
      integer(kind = kint), parameter :: nchara = 12 + 16 + 1
      character(len=nchara) :: vtk_cell_type_head
!
!
      write(vtk_cell_type_head,'(a12,i16,a1)')  VTK_CL2, nele, char(10)
!
      end function vtk_cell_type_head
!
! ----------------------------------------------------------------------!
!
      integer(kind = kint) function vtk_cell_type(nnod_ele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
!
!
      if (nnod_ele .eq. num_t_linear) then
        vtk_cell_type = 12
      else if (nnod_ele .eq. num_t_quad) then
        vtk_cell_type = 25
      else if (nnod_ele .eq. num_triangle) then
        vtk_cell_type = 5
      else if (nnod_ele .eq. num_linear_edge) then
        vtk_cell_type = 3
      else
        vtk_cell_type = 12
      end if
!
      end function vtk_cell_type
!
! ----------------------------------------------------------------------
!
      function vtk_each_cell_type(icellid)
!
      integer(kind = kint), intent(in) :: icellid
      character(len=5+1) :: vtk_each_cell_type
!
      write(vtk_each_cell_type,'(i5,a1)')  icellid, char(10)
!
      end function vtk_each_cell_type
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function vtk_each_connect(nnod_ele, ie0)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_ele
      integer(kind = kint_gl), intent(in) :: ie0(nnod_ele)
!
      character(len=16+16*nnod_ele+1) :: vtk_each_connect
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a1,i3,a9)')  '(', (nnod_ele+1), '(i16),a1)'
!
      write(vtk_each_connect,fmt_txt)                                   &
     &      nnod_ele, ie0(1:nnod_ele), char(10)
!
      end function  vtk_each_connect
!
! ----------------------------------------------------------------------
!
      end module vtk_data_to_buffer
