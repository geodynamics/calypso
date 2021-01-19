!>@file   t_ucd_data.f90
!!@brief  module t_ucd_data
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Structure for Field data IO
!!
!!@verbatim
!!      subroutine allocate_ucd_nodal_data(ucd)
!!      subroutine allocate_ucd_node(ucd)
!!      subroutine allocate_ucd_ele(ucd)
!!      subroutine allocate_ucd_phys_name(ucd)
!!      subroutine allocate_ucd_phys_data(ucd)
!!
!!      subroutine alloc_merged_ucd_nod_stack(num_pe, ucd)
!!      subroutine alloc_merged_ucd_ele_stack(num_pe, ucd)
!!
!!      subroutine deallocate_ucd_node(ucd)
!!      subroutine deallocate_ucd_ele(ucd)
!!      subroutine deallocate_ucd_phys_data(ucd)
!!      subroutine deallocate_ucd_phys_name(ucd)
!!      subroutine deallocate_ucd_data(ucd)
!!      subroutine deallocate_ucd_mesh(ucd)
!!      subroutine deallocate_parallel_ucd_mesh(ucd)
!!
!!      subroutine disconnect_ucd_node(ucd)
!!      subroutine disconnect_ucd_phys_data(ucd)
!!      subroutine disconnect_ucd_phys_name(ucd)
!!      subroutine disconnect_ucd_data(ucd)
!!      subroutine disconnect_ucd_mesh(ucd)
!!      subroutine disconnect_merged_ucd_mesh(ucd)
!!
!!      subroutine unlink_merged_ucd_nod_stack(ucd)
!!      subroutine unlink_merged_ucd_ele_stack(ucd)
!!
!!      subroutine dealloc_merged_ucd_nod_stack(ucd)
!!      subroutine dealloc_merged_ucd_ele_stack(ucd)
!!
!!      subroutine cal_istack_ucd_component(ucd)
!!
!!      subroutine append_new_ucd_field_name(new_field_name,            &
!!     &          ncomp_new_field, tmp, ucd)
!!      subroutine append_new_ucd_field_data(ncomp_new_field, d_tmp,    &
!!     &          tmp, ucd)
!!        type(ucd_data), intent(inout) :: ucd
!!
!!      subroutine check_read_ucd_data(ucd_b)
!!      subroutine compare_read_ucd_data(ucd_b, ucd_z)
!!        type(ucd_data), intent(in) :: ucd_b, ucd_z
!!@endverbatim
!
      module t_ucd_data
!
      use m_precision
      use m_constants
      use m_field_file_format
      use m_file_format_switch
!
      implicit none
!
!>        Structure for FEM field data IO
      type ucd_data
!>        end point for number of node for each subdomain
        integer(kind = kint_gl), pointer :: istack_merged_nod(:)
!>        end point for number of element for each subdomain
        integer(kind = kint_gl), pointer :: istack_merged_ele(:)
!>        end point for number of internal node for each subdomain
        integer(kind = kint_gl), pointer :: istack_merged_intnod(:)
!
!>        number of nodes for field data
        integer(kind = kint_gl) :: nnod
!>        number of elements for field data
        integer(kind = kint_gl) :: nele
!>        number of nodes for each element for field data
        integer(kind = kint) :: nnod_4_ele
!
!>        position of nodes
        real (kind=kreal), pointer :: xx(:,:)
!>        global node ID
        integer(kind = kint_gl), pointer :: inod_global(:)
!>        global element ID
        integer(kind = kint_gl), pointer :: iele_global(:)
!>        element connectivity
        integer(kind = kint_gl), pointer :: ie(:,:)
!
!>        number of field for IO
        integer(kind=kint) :: num_field
!>        total number of component for IO
        integer(kind=kint) :: ntot_comp
!>        number of component for each field
        integer(kind=kint), pointer :: num_comp(:)
!>        field name
        character (len=kchara), pointer :: phys_name(:)
!
!>        field data for IO
        real (kind=kreal), pointer :: d_ucd(:,:)
      end type ucd_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_nodal_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call allocate_ucd_node(ucd)
      call allocate_ucd_phys_name(ucd)
      call allocate_ucd_phys_data(ucd)
!
      end subroutine allocate_ucd_nodal_data
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_node(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%xx(ucd%nnod,3))
      allocate(ucd%inod_global(ucd%nnod))
!
      if(ucd%nnod .gt. 0) then
        ucd%xx = 0.0d0
        ucd%inod_global = 0
      end if
!
      end subroutine allocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_ele(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%ie(ucd%nele,ucd%nnod_4_ele))
      allocate(ucd%iele_global(ucd%nele))
!
      if(ucd%nele .gt. 0) then
        ucd%ie = 0
        ucd%iele_global =   0
      end if
!
      end subroutine allocate_ucd_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_name(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate( ucd%num_comp(ucd%num_field) )
      allocate( ucd%phys_name(ucd%num_field) )
!
      if(ucd%num_field .gt. 0) ucd%num_comp = 0
!
      end subroutine allocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine allocate_ucd_phys_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%d_ucd(ucd%nnod,ucd%ntot_comp) )
      if( (ucd%nnod*ucd%ntot_comp) .gt. 0) ucd%d_ucd = 0.0d0
!
      end subroutine allocate_ucd_phys_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_merged_ucd_nod_stack(num_pe, ucd)
!
      integer, intent(in) :: num_pe
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%istack_merged_nod(0:num_pe))
      allocate(ucd%istack_merged_intnod(0:num_pe))
      ucd%istack_merged_nod =    0
      ucd%istack_merged_intnod = 0
!
      end subroutine alloc_merged_ucd_nod_stack
!
! -----------------------------------------------------------------------
!
      subroutine alloc_merged_ucd_ele_stack(num_pe, ucd)
!
      integer, intent(in) :: num_pe
      type(ucd_data), intent(inout) :: ucd
!
!
      allocate(ucd%istack_merged_ele(0:num_pe))
      ucd%istack_merged_ele =    0
!
      end subroutine alloc_merged_ucd_ele_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_node(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%xx, ucd%inod_global)
!
      end subroutine deallocate_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_ele(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%ie, ucd%iele_global)
!
      end subroutine deallocate_ucd_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%d_ucd)
!
      end subroutine deallocate_ucd_phys_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_phys_name(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%num_comp, ucd%phys_name)
!
      end subroutine deallocate_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call deallocate_ucd_phys_name(ucd)
      call deallocate_ucd_phys_data(ucd)
!
      end subroutine deallocate_ucd_data
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_ucd_mesh(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call deallocate_ucd_data(ucd)
      call deallocate_ucd_ele(ucd)
      call deallocate_ucd_node(ucd)
!
      end subroutine deallocate_ucd_mesh
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_parallel_ucd_mesh(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call deallocate_ucd_data(ucd)
      call deallocate_ucd_ele(ucd)
      call deallocate_ucd_node(ucd)
      call dealloc_merged_ucd_nod_stack(ucd)
      call dealloc_merged_ucd_ele_stack(ucd)
!
      end subroutine deallocate_parallel_ucd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_node(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%xx, ucd%inod_global)
!
      end subroutine disconnect_ucd_node
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_phys_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%d_ucd)
!
      end subroutine disconnect_ucd_phys_data
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_phys_name(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%phys_name, ucd%num_comp)
!
      end subroutine disconnect_ucd_phys_name
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_data(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call disconnect_ucd_phys_data(ucd)
      call disconnect_ucd_phys_name(ucd)
!
      end subroutine disconnect_ucd_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine disconnect_ucd_mesh(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call disconnect_ucd_data(ucd)
      call deallocate_ucd_ele(ucd)
      call disconnect_ucd_node(ucd)
!
      end subroutine disconnect_ucd_mesh
!
! -----------------------------------------------------------------------
!
      subroutine disconnect_merged_ucd_mesh(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      call disconnect_ucd_data(ucd)
      call deallocate_ucd_ele(ucd)
      call disconnect_ucd_node(ucd)
      call unlink_merged_ucd_nod_stack(ucd)
      call unlink_merged_ucd_ele_stack(ucd)
!
      end subroutine disconnect_merged_ucd_mesh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine unlink_merged_ucd_nod_stack(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%istack_merged_nod)
      nullify(ucd%istack_merged_intnod)
!
      end subroutine unlink_merged_ucd_nod_stack
!
! -----------------------------------------------------------------------
!
      subroutine unlink_merged_ucd_ele_stack(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      nullify(ucd%istack_merged_ele)
!
      end subroutine unlink_merged_ucd_ele_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_merged_ucd_nod_stack(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%istack_merged_nod)
      deallocate(ucd%istack_merged_intnod)
!
      end subroutine dealloc_merged_ucd_nod_stack
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_merged_ucd_ele_stack(ucd)
!
      type(ucd_data), intent(inout) :: ucd
!
!
      deallocate(ucd%istack_merged_ele)
!
      end subroutine dealloc_merged_ucd_ele_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_istack_ucd_component(ucd)
!
      type(ucd_data), intent(inout) :: ucd
      integer(kind = kint) :: inum
!
!
      ucd%ntot_comp = 0
      do inum = 1, ucd%num_field
        ucd%ntot_comp = ucd%ntot_comp + ucd%num_comp(inum)
      end do
!
      end subroutine cal_istack_ucd_component
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_new_ucd_field_name(new_field_name,              &
     &          ncomp_new_field, tmp, ucd)
!
      use copy_between_two_fields
!
      character(len=kchara), intent(in)  :: new_field_name
      integer(kind = kint), intent(in) :: ncomp_new_field
      type(ucd_data), intent(inout) :: tmp, ucd
!
!
      tmp%phys_name(1:ucd%num_field) = ucd%phys_name(1:ucd%num_field)
      tmp%num_comp(1:ucd%num_field) =  ucd%num_comp(1:ucd%num_field)
!
      call copy_fields_2_fields(ucd%ntot_comp,                          &
     &          ione, ucd%nnod, ucd%ntot_comp, ucd%d_ucd,               &
     &          ione, tmp%nnod, tmp%ntot_comp, tmp%d_ucd)
!
      call deallocate_ucd_data(ucd)
!
      ucd%num_field = ucd%num_field + 1
      call allocate_ucd_phys_name(ucd)
!
      ucd%phys_name(1:tmp%num_field) = tmp%phys_name(1:tmp%num_field)
      ucd%num_comp(1:tmp%num_field) =  tmp%num_comp(1:tmp%num_field)
!
      ucd%ntot_comp = ucd%ntot_comp + ncomp_new_field
      ucd%num_comp(ucd%num_field) =   ncomp_new_field
      ucd%phys_name(ucd%num_field) =  new_field_name
!
      call allocate_ucd_phys_data(ucd)
!
      end subroutine append_new_ucd_field_name
!
! -----------------------------------------------------------------------
!
      subroutine append_new_ucd_field_data(ncomp_new_field, d_tmp,      &
     &          tmp, ucd)
!
      use copy_between_two_fields
!
      type(ucd_data), intent(inout) :: tmp, ucd
      integer(kind = kint), intent(in) :: ncomp_new_field
      real(kind = kreal) :: d_tmp(ucd%nnod,ncomp_new_field)
!
!
      call copy_fields_2_fields(tmp%ntot_comp,                          &
     &          ione, tmp%nnod, tmp%ntot_comp, tmp%d_ucd,               &
     &          ione, ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
      call copy_fields_2_fields(ncomp_new_field,                        &
     &          ione, ucd%nnod, ncomp_new_field, d_tmp(1,1),            &
     &          (tmp%ntot_comp+1), ucd%nnod, ucd%ntot_comp, ucd%d_ucd)
!
      end subroutine append_new_ucd_field_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_read_ucd_data(ucd_b)
!
      type(ucd_data), intent(in) :: ucd_b
!
!
      write(*,*) 'ucd_b%nnod', ucd_b%nnod
      write(*,*) 'xx_1', ucd_b%xx(1,1:3)
      write(*,*) 'xx_2', ucd_b%xx(2,1:3)
      write(*,*) 'xx_3', ucd_b%xx(3,1:3)
      write(*,*) 'xx_1', ucd_b%xx(ucd_b%nnod-2,1:3)
      write(*,*) 'xx_2', ucd_b%xx(ucd_b%nnod-1,1:3)
      write(*,*) 'xx_3', ucd_b%xx(ucd_b%nnod,  1:3)
!
      write(*,*) 'ucd_b%nele', ucd_b%nele
      write(*,*) 'ie_1', ucd_b%ie(1,1:3)
      write(*,*) 'ie_2', ucd_b%ie(2,1:3)
      write(*,*) 'ie_3', ucd_b%ie(3,1:3)
      write(*,*) 'ie_1', ucd_b%ie(ucd_b%nele-2,1:3)
      write(*,*) 'ie_2', ucd_b%ie(ucd_b%nele-1,1:3)
      write(*,*) 'ie_3', ucd_b%ie(ucd_b%nele,  1:3)
!
      write(*,*) 'ucd_b%num_field', ucd_b%num_field, ucd_b%ntot_comp
      write(*,*) 'ucd_b%num_comp', ucd_b%num_comp
      write(*,*) 'ucd_b%phys_name: ', ucd_b%phys_name
      write(*,*) 'ucd_b%d_ucd', ucd_b%d_ucd(1:3,1)
!
      end subroutine check_read_ucd_data
!
! -----------------------------------------------------------------------
!
      subroutine compare_read_ucd_data(ucd_b, ucd_z)
!
      type(ucd_data), intent(in) :: ucd_b, ucd_z
!
      integer(kind = kint_gl) :: i
      integer(kind = kint) :: nd
!
!
      if(ucd_z%nnod_4_ele .ne. ucd_b%nnod_4_ele) write(*,*)             &
     &    'Wrong element type', ucd_z%nnod_4_ele, ucd_b%nnod_4_ele
      if(ucd_z%nele .ne. ucd_b%nele) write(*,*)                         &
     &    'Wrong num of element', ucd_z%nele, ucd_b%nele
      write(*,*) 'Check connectivity'
      do nd = 1, ucd_z%nnod_4_ele
        do i = 1, ucd_z%nele
          if(ucd_b%ie(i,nd) .ne. ucd_z%ie(i,nd)) then
            write(*,*) 'Wrong connectivity at ', nd, i,                 &
     &                ucd_z%ie(i,nd), ucd_b%ie(i,nd)
          end if
        end do
      end do
!
      if(ucd_z%nnod .ne. ucd_b%nnod) write(*,*)                         &
     &    'Wrong num of node', ucd_z%nnod, ucd_b%nnod
      write(*,*) 'Check position'
      do nd = 1, 3
        do i = 1, ucd_z%nnod
          if(ucd_b%xx(i,nd) .ne. ucd_z%xx(i,nd)) then
            write(*,*) 'Wrong position at ', nd, i,                     &
     &                ucd_z%xx(i,nd), ucd_b%xx(i,nd)
          end if
        end do
      end do
!
      if(ucd_z%ntot_comp .ne. ucd_b%ntot_comp) write(*,*)               &
     &    'Wrong num of total comps.', ucd_z%ntot_comp, ucd_b%ntot_comp
      write(*,*) 'Check field'
      do nd = 1, ucd_z%ntot_comp
        do i = 1, ucd_z%nnod
          if(ucd_z%d_ucd(i,nd) .ne. ucd_b%d_ucd(i,nd)) then
            write(*,*) 'Wrong field at ', nd, i,                        &
     &                ucd_z%d_ucd(i,nd), ucd_b%d_ucd(i,nd)
          end if
        end do
      end do
!
      end subroutine compare_read_ucd_data
!
! -----------------------------------------------------------------------
!
      end module t_ucd_data
