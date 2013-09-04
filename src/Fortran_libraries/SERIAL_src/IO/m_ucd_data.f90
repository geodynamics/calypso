!
!      module m_ucd_data
!
!        Written by H.Matsui on June, 2006
!
!>@file   m_grid_fdm_coefs.f90
!!@brief  module m_grid_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Arrays for Field data IO
!!
!!@verbatim
!!      subroutine allocate_ucd_node
!!      subroutine allocate_ucd_ele
!!      subroutine allocate_ucd_phys_name
!!      subroutine allocate_ucd_phys_data
!!
!!      subroutine deallocate_ucd_node
!!      subroutine deallocate_ucd_ele
!!      subroutine deallocate_ucd_phys_data
!!      subroutine deallocate_ucd_phys_name
!!      subroutine deallocate_ucd_data
!!
!!      subroutine disconnect_ucd_node
!!      subroutine disconnect_ucd_data
!!
!!      subroutine cal_istack_ucd_component
!!@endverbatim
!
      module m_ucd_data
!
      use m_precision
      use m_field_file_format
      use m_file_format_switch
!
      implicit none
!
!>      file ID for field data IO
      integer (kind = kint), parameter :: ucd_file_code = 16
!>      file name for field data
      character(len=kchara) :: ucd_file_name
!
!
!>      file header for field data
      character(len=kchara) :: ucd_header_name = "field/out"
!>      file header for original field data
      character(len=kchara) :: org_ucd_header =  "field_org/out"
!
!>      file type for field data IO
      integer (kind = kint) :: itype_ucd_data_file = iflag_fld
!
!>      number of nodes for field data
      integer(kind = kint) :: nnod_ucd
!>      number of elements for field data
      integer(kind = kint) :: nele_ucd
!>      number of nodes for each element for field data
      integer(kind = kint) :: nnod_4_ele_ucd
!
!>      position of nodes
      real (kind=kreal), pointer :: xx_ucd(:,:)
!>      global node ID
      integer(kind = kint), pointer :: inod_gl_ucd(:)
!>      global element ID
      integer(kind = kint), pointer :: iele_gl_ucd(:)
!>      element connectivity
      integer(kind = kint), pointer :: ie_ucd(:,:)
!>      element flag for hexahedral element
      character (len=5) :: hexmark = ' hex '
!>      element flag for triangle element
      character (len=5) :: trimark = ' tri '
!
!
!>      number of field for IO
      integer(kind=kint) :: num_field_ucd
!>      total number of component for IO
      integer(kind=kint) :: ntot_comp_ucd
!>      number of component for each field
      integer(kind=kint), pointer :: num_comp_ucd(:)
!>      end address of component for each field
      integer(kind=kint), pointer :: istack_comp_ucd(:)
!>      field name
      character (len=kchara), pointer :: phys_name_ucd(:)
!
!>      field data for IO
      real (kind=kreal), pointer :: d_nod_ucd(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_node
!
      allocate(xx_ucd(nnod_ucd,3))
      allocate(inod_gl_ucd(nnod_ucd))
!
      if(nnod_ucd .gt. 0) then
        xx_ucd = 0.0d0
        inod_gl_ucd = 0
      end if
!
      end subroutine allocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_ele
!
      allocate(ie_ucd(nele_ucd,nnod_4_ele_ucd))
      allocate(iele_gl_ucd(nele_ucd))
!
      if(nele_ucd .gt. 0) then
        ie_ucd = 0
        iele_gl_ucd =   0
      end if
!
      end subroutine allocate_ucd_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_name
!
      allocate( num_comp_ucd(num_field_ucd) )
      allocate( istack_comp_ucd(0:num_field_ucd) )
      allocate( phys_name_ucd(num_field_ucd) )
!
      istack_comp_ucd = 0
      if(num_field_ucd .gt. 0) num_comp_ucd = 0
!
      end subroutine allocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_data
!
      allocate(d_nod_ucd(nnod_ucd,ntot_comp_ucd) )
      if( (nnod_ucd*ntot_comp_ucd) .gt. 0) d_nod_ucd = 0.0d0
!
      end subroutine allocate_ucd_phys_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_node
!
      deallocate(xx_ucd)
      deallocate(inod_gl_ucd)
!
      end subroutine deallocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_ele
!
      deallocate(ie_ucd)
      deallocate(iele_gl_ucd)
!
      end subroutine deallocate_ucd_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_data
!
      deallocate(d_nod_ucd)
!
      end subroutine deallocate_ucd_phys_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_name
!
      deallocate(num_comp_ucd, istack_comp_ucd)
      deallocate(phys_name_ucd)
!
      end subroutine deallocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_data
!
      call deallocate_ucd_phys_name
      call deallocate_ucd_phys_data
!
      end subroutine deallocate_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_node
!
      nullify(xx_ucd)
      nullify(inod_gl_ucd)
!
      end subroutine disconnect_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_data
!
      nullify(num_comp_ucd, istack_comp_ucd)
      nullify(phys_name_ucd)
      nullify(d_nod_ucd)
!
      end subroutine disconnect_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_istack_ucd_component
!
      use m_constants
      use cal_minmax_and_stacks
!
!
      call s_cal_total_and_stacks(num_field_ucd, num_comp_ucd,          &
     &    izero, istack_comp_ucd, ntot_comp_ucd)
!
      end subroutine cal_istack_ucd_component
!
! -------------------------------------------------------------------
!
      end module m_ucd_data
