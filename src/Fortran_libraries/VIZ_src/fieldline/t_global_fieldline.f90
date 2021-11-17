!>@file   t_global_fieldline.f90
!!@brief  module t_global_fieldline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!>@brief  Structure fof global fieldline data
!!
!!@verbatim
!!      subroutine alloc_global_fline_num(fline_gl)
!!      subroutine dealloc_global_fline_num(fline_gl)
!!      subroutine dealloc_global_fline(fline_gl)
!!      subroutine raise_global_fline_connect(fline_gl)
!!      subroutine raise_global_fline_connect(fline_gl)
!!
!!      subroutine write_global_fline_ucd(id_file, fline_gl)
!!      subroutine write_global_fline_vtk(id_vtk, fline_gl)
!!      subroutine write_global_fline_dx(id_file, fline_gl)
!
      module t_global_fieldline
!
      use m_precision
      use m_constants
!
      implicit  none
!
      type global_fieldline_data
        integer(kind = kint) :: ntot_nod_line_gl_buf
        integer(kind = kint) :: ntot_ele_line_gl_buf
        integer(kind = kint) :: ntot_nod_line_gl
        integer(kind = kint) :: ntot_ele_line_gl
        integer(kind = kint), allocatable :: nnod_line_gl(:)
        integer(kind = kint), allocatable :: nele_line_gl(:)
        integer(kind = kint), allocatable :: istack_nod_line_gl(:)
        integer(kind = kint), allocatable :: istack_ele_line_gl(:)
!
        integer(kind = kint), allocatable :: iedge_line_gl(:,:)
        real(kind = kreal), allocatable ::   xx_line_gl(:,:)
        real(kind = kreal), allocatable ::   col_line_gl(:)
        character(len = kchara) :: color_name_gl
!
!
!>                 work array for communication (wait)
        integer, allocatable :: sta1_fline(:,:)
!>                 work array for communication (wait)
        integer, allocatable :: sta2_fline(:,:)
!>                 work array for communication (wait)
        integer, allocatable :: req1_fline(:  )
!>                 work array for communication (wait)
        integer, allocatable :: req2_fline(:  )
      end type global_fieldline_data
!
      private :: alloc_global_fline
      private :: alloc_global_fline_conn, alloc_global_fline_data
      private :: dealloc_global_fline_conn
      private :: dealloc_global_fline_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_global_fline_num(fline_gl)
!
      use calypso_mpi
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      allocate(fline_gl%istack_nod_line_gl(0:nprocs))
      allocate(fline_gl%istack_ele_line_gl(0:nprocs))
      allocate(fline_gl%nnod_line_gl(nprocs))
      allocate(fline_gl%nele_line_gl(nprocs))
!
      fline_gl%istack_nod_line_gl = 0
      fline_gl%istack_ele_line_gl = 0
      fline_gl%nnod_line_gl = 0
      fline_gl%nele_line_gl = 0
!
      allocate(fline_gl%sta1_fline(MPI_STATUS_SIZE,nprocs))
      allocate(fline_gl%sta2_fline(MPI_STATUS_SIZE,nprocs))
      allocate(fline_gl%req1_fline(nprocs))
      allocate(fline_gl%req2_fline(nprocs))
!
      call alloc_global_fline(fline_gl)
!
      end subroutine alloc_global_fline_num
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_global_fline(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      fline_gl%ntot_nod_line_gl_buf = 2
      fline_gl%ntot_ele_line_gl_buf = 1
      fline_gl%ntot_nod_line_gl = 0
      fline_gl%ntot_ele_line_gl = 0
!
      call alloc_global_fline_conn(fline_gl)
      call alloc_global_fline_data(fline_gl)
!
      end subroutine alloc_global_fline
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_global_fline_conn(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      allocate(fline_gl%iedge_line_gl(2,fline_gl%ntot_ele_line_gl_buf))
      if(fline_gl%ntot_ele_line_gl_buf .gt. 0) then
        fline_gl%iedge_line_gl = 0
      end if
!
      end subroutine alloc_global_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_global_fline_data(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      allocate(fline_gl%xx_line_gl(3,fline_gl%ntot_nod_line_gl_buf))
      allocate(fline_gl%col_line_gl(fline_gl%ntot_nod_line_gl_buf))
      if(fline_gl%ntot_nod_line_gl_buf .gt. 0) then
        fline_gl%xx_line_gl = 0.0d0
      end if
      if(fline_gl%ntot_nod_line_gl_buf .gt. 0) then
        fline_gl%col_line_gl = 0.0d0
      end if
!
      end subroutine alloc_global_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_global_fline_num(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      deallocate(fline_gl%nnod_line_gl, fline_gl%istack_nod_line_gl)
      deallocate(fline_gl%nele_line_gl, fline_gl%istack_ele_line_gl)
      deallocate(fline_gl%sta1_fline, fline_gl%sta2_fline)
      deallocate(fline_gl%req1_fline, fline_gl%req2_fline)
!
      end subroutine dealloc_global_fline_num
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_global_fline(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      call dealloc_global_fline_conn(fline_gl)
      call dealloc_global_fline_data(fline_gl)
!
      end subroutine dealloc_global_fline
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_global_fline_conn(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      deallocate(fline_gl%iedge_line_gl)
!
      end subroutine dealloc_global_fline_conn
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_global_fline_data(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      deallocate(fline_gl%xx_line_gl, fline_gl%col_line_gl)
!
      end subroutine dealloc_global_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine raise_global_fline_connect(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      call dealloc_global_fline_conn(fline_gl)
!
      fline_gl%ntot_ele_line_gl_buf = fline_gl%ntot_ele_line_gl
      call alloc_global_fline_conn(fline_gl)
!
      end subroutine raise_global_fline_connect
!
!  ---------------------------------------------------------------------
!
      subroutine raise_global_fline_data(fline_gl)
!
      type(global_fieldline_data), intent(inout) :: fline_gl
!
!
      call dealloc_global_fline_data(fline_gl)
!
      fline_gl%ntot_nod_line_gl_buf = fline_gl%ntot_nod_line_gl
      call alloc_global_fline_data(fline_gl)
!
      end subroutine raise_global_fline_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_global_fline_ucd(id_file, fline_gl)
!
      integer(kind = kint), intent(in) :: id_file
      type(global_fieldline_data), intent(in) :: fline_gl
!
      integer(kind = kint) :: i
!
!
      write(id_file,*) fline_gl%ntot_nod_line_gl,                       &
    &                  fline_gl%ntot_ele_line_gl
      do i = 1, fline_gl%ntot_nod_line_gl
        write(id_file,'(i16,1p3e16.7)') i, fline_gl%xx_line_gl(1:3,i)
      end do
!
      do i = 1, fline_gl%ntot_ele_line_gl
        write(id_file,'(2i16,a7,2i16)') i, ione,                        &
     &               '  line ', fline_gl%iedge_line_gl(1:2,i)
      end do
!
      write(id_file,'(2i4)') ione, ione
      write(id_file,'(a,a1)') trim(fline_gl%color_name_gl), ','
      do i = 1, fline_gl%ntot_nod_line_gl
        write(id_file,'(i16,1pe16.7)') i, fline_gl%col_line_gl(i)
      end do
!
      end subroutine write_global_fline_ucd
!
!  ---------------------------------------------------------------------
!
      subroutine write_global_fline_vtk(id_vtk, fline_gl)
!
      use m_geometry_constants
      use m_phys_constants
      use vtk_data_to_buffer
      use vtk_data_IO
!
      integer(kind = kint), intent(in) :: id_vtk
      type(global_fieldline_data), intent(in) :: fline_gl
!
      integer(kind = kint_gl) :: n8
      integer(kind = kint) :: i, icellid
      integer(kind = kint_gl) :: ie0(num_linear_edge)
!
!
      n8 = fline_gl%ntot_nod_line_gl
      write(id_vtk,'(a)',advance='NO') vtk_node_head(n8)

      do i = 1, fline_gl%ntot_nod_line_gl
        write(id_vtk,'(1p3e16.7)') fline_gl%xx_line_gl(1:3,i)
      end do
!
      n8 = fline_gl%ntot_ele_line_gl
      write(id_vtk,'(a)',advance='NO')                                  &
     &               vtk_connect_head(n8, num_linear_edge)
!
      do i = 1, fline_gl%ntot_ele_line_gl
        ie0(1:num_linear_edge)                                          &
     &            = fline_gl%iedge_line_gl(1:num_linear_edge,i) - 1
        write(id_vtk,'(a)',advance='NO')                                &
     &              vtk_each_connect(num_linear_edge,ie0)
      end do
!
      n8 = fline_gl%ntot_ele_line_gl
      write(id_vtk,'(a)',advance='NO') vtk_cell_type_head(n8)
!
      icellid = vtk_cell_type(num_linear_edge)
      do i = 1, fline_gl%ntot_ele_line_gl
        write(id_vtk,'(a)',advance='NO') vtk_each_cell_type(icellid)
      end do
!
      n8 = fline_gl%ntot_nod_line_gl
      write(id_vtk,'(a)',advance='NO') vtk_fields_head(n8)
      write(id_vtk,'(a)',advance='NO')                                  &
     &    vtk_scalar_head(fline_gl%color_name_gl)
!
      n8 = fline_gl%ntot_nod_line_gl
      call write_vtk_each_field                                         &
     &   (id_vtk, n8, n_scalar, n8, fline_gl%col_line_gl)
!
      end subroutine write_global_fline_vtk
!
!  ---------------------------------------------------------------------
!
      subroutine write_global_fline_dx(id_file, fline_gl)
!
      integer(kind = kint), intent(in) :: id_file
      type(global_fieldline_data), intent(in) :: fline_gl
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '#  node information'
      write(id_file,'(a,i16,a)')                                        &
     &   'object 1 class array type float rank 1 shape 3 items  ',      &
     &   fline_gl%ntot_nod_line_gl, ' data follows'
!
      do i = 1, fline_gl%ntot_nod_line_gl
        write(id_file,'(1p3e16.7)') fline_gl%xx_line_gl(1:3,i)
      end do
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# element connectivity'
      write(id_file,'(a,i16,a)')                                        &
     &   'object 2 class array type int rank 1 shape 2 items    ',      &
     &   fline_gl%ntot_ele_line_gl, ' data follows'
      do i = 1, fline_gl%ntot_ele_line_gl
        write(id_file,'(2i16)') (fline_gl%iedge_line_gl(1:2,i)-1)
      end do
      write(id_file,'(a)') 'attribute "element type" string "lines"'
      write(id_file,'(a)') 'attribute "ref" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') '# scalar'
      write(id_file,'(a,i16,a)')                                        &
     &   'object 3 class array type float rank 1 shape 1 items  ',      &
     &   fline_gl%ntot_nod_line_gl, ' data follows'
      do i = 1, fline_gl%ntot_nod_line_gl
        write(id_file,'(1pe16.7)') fline_gl%col_line_gl(i)
      end do
      write(id_file,'(a)') 'attribute "dep" string "positions"'
!
      write(id_file,'(a)') '#'
      write(id_file,'(a)') 'object "irregular positions irregular connections ascii file" class field'
      write(id_file,'(a)') 'component "positions" value 1'
      write(id_file,'(a)') 'component "connections" value 2'
      write(id_file,'(a)') 'component "data" value    3'
      write(id_file,'(a)') 'end'
!
      end subroutine write_global_fline_dx
!
!  ---------------------------------------------------------------------
!
      end module t_global_fieldline
