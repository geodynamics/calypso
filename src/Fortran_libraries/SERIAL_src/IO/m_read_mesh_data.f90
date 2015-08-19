!>@file   m_read_mesh_data.f90
!!@brief  module m_read_mesh_data
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!      subroutine allocate_node_data_dummy
!!      subroutine allocate_ele_info_dummy
!!      subroutine allocate_connect_dummy
!!
!!      subroutine deallocate_mesh_arrays
!!
!!      subroutine deallocate_node_data_dummy
!!      subroutine deallocate_ele_info_dummy
!!
!!      subroutine allocate_ele_scalar_IO
!!      subroutine deallocate_ele_scalar_IO
!!@endverbatim
!
      module m_read_mesh_data
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint) :: numnod_dummy
      integer(kind=kint) :: internal_node_dummy
      integer(kind=kint_gl), allocatable :: globalnodid_dummy(:)
      real(kind=kreal),   allocatable :: xx_dummy(:,:)
! 
      real(kind=kreal),   allocatable :: ele_vector_IO(:,:)
      real(kind=kreal),   allocatable :: ele_scalar_IO(:)
!
      integer(kind=kint) :: numele_dummy
      integer (kind = kint) :: nnod_4_ele_dummy = 8
      integer(kind=kint_gl), allocatable:: globalelmid_dummy(:)
      integer(kind=kint), allocatable   :: i_ele_dummy(:)
      integer(kind=kint), allocatable   :: nodelm_dummy(:)
      integer(kind=kint), allocatable :: ie_dummy(:,:)
! 
      integer(kind = kint) :: nsf_4_ele_IO
      integer(kind = kint) :: nsurf_in_ele_IO = 6
      integer(kind = kint), allocatable  :: isf_4_ele_IO(:,:)
!
      integer(kind = kint) :: ned_4_ele_IO
      integer(kind = kint) :: nedge_in_ele_IO = 12
      integer(kind = kint), allocatable  :: iedge_4_ele_IO(:,:)
!
!
      integer(kind = kint)  :: iflag_mesh_file_fmt = 0
      integer(kind = kint)  :: iflag_mesh_file_ext = 1
!
      character(len=kchara), parameter:: def_mesh_file_head = 'mesh/in'
      character(len=kchara), parameter                                  &
                  :: mesh_ele_def_head = 'mesh/element'
      character(len=kchara), parameter                                  &
     &            :: mesh_def_surf_head = 'mesh/surface'
      character(len=kchara), parameter                                  &
     &            :: mesh_def_edge_head = 'mesh/edge'
!
      character(len=kchara) :: mesh_file_head =      def_mesh_file_head
      character(len=kchara) :: mesh_ele_file_head =  mesh_ele_def_head
      character(len=kchara) :: mesh_surf_file_head = mesh_def_surf_head
      character(len=kchara) :: mesh_edge_file_head = mesh_def_edge_head
!
!
      character(len=kchara) :: mesh_file_name
!
!
!   mesh file name
      integer(kind=kint ) ::  input_file_code = 14
!   i/o code for ucd data output file
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine allocate_node_data_dummy
!
       allocate(xx_dummy(numnod_dummy,3))
       allocate(globalnodid_dummy(numnod_dummy))
       if ( numnod_dummy .gt. 0) then
        xx_dummy=0.0d00
        globalnodid_dummy=0
       end if
!
      end subroutine allocate_node_data_dummy
!
!------------------------------------------------------------------
!
      subroutine allocate_ele_info_dummy
!
       allocate(i_ele_dummy(numele_dummy))
       allocate(nodelm_dummy(numele_dummy))
       allocate(globalelmid_dummy(numele_dummy))
       if ( numele_dummy .gt. 0) then
        i_ele_dummy=0
        nodelm_dummy = 0
        globalelmid_dummy=0
       end if
!
      end subroutine allocate_ele_info_dummy
!
!------------------------------------------------------------------
!
      subroutine allocate_connect_dummy
!
       allocate(ie_dummy(numele_dummy,nnod_4_ele_dummy) )
       if ( numele_dummy .gt. 0) then
        ie_dummy = 0
       end if
!
      end subroutine allocate_connect_dummy
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_node_data_dummy
!
       deallocate(xx_dummy)
       deallocate(globalnodid_dummy)
!
      end subroutine deallocate_node_data_dummy
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_info_dummy
!
      deallocate(i_ele_dummy)
      deallocate(nodelm_dummy)
      deallocate(globalelmid_dummy)
      deallocate(ie_dummy)
!
      end subroutine deallocate_ele_info_dummy
!
!------------------------------------------------------------------
!
      subroutine deallocate_mesh_arrays
!
       call deallocate_ele_info_dummy
       call deallocate_node_data_dummy
!
      end subroutine deallocate_mesh_arrays
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_surface_connect_IO
!
      allocate( isf_4_ele_IO(nsf_4_ele_IO,nsurf_in_ele_IO) )
      isf_4_ele_IO = 0
!
      end subroutine allocate_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_edge_connect_IO
!
      allocate( iedge_4_ele_IO(ned_4_ele_IO,nedge_in_ele_IO) )
      iedge_4_ele_IO = 0
!
      end subroutine allocate_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_surface_connect_IO
!
      deallocate( isf_4_ele_IO )
!
      end subroutine deallocate_surface_connect_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_edge_connect_IO
!
      deallocate( iedge_4_ele_IO )
!
      end subroutine deallocate_edge_connect_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_vector_IO
!
      allocate( ele_vector_IO(numnod_dummy,3) )
      ele_vector_IO = 0.0d0
!
      end subroutine allocate_ele_vector_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_vector_IO
!
      deallocate( ele_vector_IO )
!
      end subroutine deallocate_ele_vector_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_ele_scalar_IO
!
      allocate( ele_scalar_IO(numnod_dummy) )
      ele_scalar_IO = 0.0d0
!
      end subroutine allocate_ele_scalar_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_ele_scalar_IO
!
      deallocate( ele_scalar_IO )
!
      end subroutine deallocate_ele_scalar_IO
!
!------------------------------------------------------------------
!
      end module m_read_mesh_data
