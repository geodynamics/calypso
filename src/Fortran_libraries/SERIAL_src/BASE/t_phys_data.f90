!>@file   t_phys_data.f90
!!@brief  module t_phys_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!> @brief Structure of field data
!!
!!@verbatim
!!      subroutine alloc_phys_name_type(fld)
!!      subroutine alloc_phys_data_type(num, fld)
!!        integer(kind = kint), intent(in) :: num
!!
!!      subroutine dealloc_phys_name_type(fld)
!!      subroutine dealloc_phys_data_type(fld)
!!
!!      subroutine link_field_name_type(org_fld, new_fld)
!!      subroutine link_field_data_type(org_fld, new_fld)
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!
!!      subroutine disconnect_phys_name_type(fld)
!!      subroutine disconnect_phys_data_type(fld)
!!        type(phys_data), intent(inout) :: fld
!!
!!      subroutine check_nodal_field_name_type(fld)
!!      subroutine check_nodal_data_type(my_rank, fld, nnod,            &
!!     &          numdir, i_field)
!!        integer(kind = kint), intent(in) :: my_rank
!!        integer(kind = kint), intent(in) :: nnod, numdir, i_field
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module t_phys_data
!
      use m_precision
!
      use t_phys_address
!
      implicit  none
! 
!>       Structure for field data
      type phys_data
!>       number of field
        integer (kind=kint) :: num_phys
!>       total number of component
        integer (kind=kint) :: ntot_phys
!>       number of component for each field
        integer (kind=kint), pointer :: num_component(:)
!>       end address for each field
        integer (kind=kint), pointer :: istack_component(:)
!>       FEM order of each field
        integer (kind=kint), pointer :: iorder_eletype(:)
!
!>       field name
        character (len=kchara), pointer :: phys_name(:)
!
!>       field data
        real (kind=kreal), pointer ::   d_fld(:,:)
!>       update flag for field data
        integer (kind=kint), pointer :: iflag_update(:)
!
!>        number of field for visualizer
        integer (kind=kint) :: num_phys_viz
!>        total number of component for visualizer
        integer (kind=kint) :: ntot_phys_viz
!
!>        flag to get average and RMS data
        integer (kind=kint), pointer:: iflag_monitor(:)
      end type phys_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_phys_name_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      allocate( fld%phys_name(fld%num_phys) )
      allocate( fld%num_component(fld%num_phys) )
      allocate( fld%istack_component(0:fld%num_phys) )
      allocate( fld%iorder_eletype(fld%num_phys) )
      allocate( fld%iflag_monitor(fld%num_phys) )
!
      if(fld%num_phys .gt. 0) then
        fld%phys_name = ''
        fld%num_component =    0
        fld%istack_component = 0
        fld%iorder_eletype =   0
        fld%iflag_monitor =    0
      end if
!
      end subroutine alloc_phys_name_type
!
!  --------------------------------------------------------------------
!
      subroutine alloc_phys_data_type(num, fld)
!
      integer(kind = kint), intent(in) :: num
      type(phys_data), intent(inout) :: fld
!
!
      allocate( fld%d_fld(num,fld%ntot_phys) )
      allocate( fld%iflag_update(fld%ntot_phys) )
      if(num*fld%ntot_phys .gt. 0)  then
        fld%d_fld =        0.0d0
        fld%iflag_update = 0
      end if
!
       end subroutine alloc_phys_data_type
!
! --------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_phys_name_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      deallocate( fld%phys_name, fld%iorder_eletype, fld%iflag_monitor)
      deallocate( fld%num_component, fld%istack_component )
!
      end subroutine dealloc_phys_name_type
!
!  --------------------------------------------------------------------
!
      subroutine dealloc_phys_data_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      deallocate( fld%d_fld, fld%iflag_update )
!
      end subroutine dealloc_phys_data_type
!
!  --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_field_name_type(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data), intent(inout) :: new_fld
!
!
      new_fld%num_phys =  org_fld%num_phys
      new_fld%ntot_phys = org_fld%ntot_phys
!
      new_fld%num_phys_viz =  org_fld%num_phys_viz
      new_fld%ntot_phys_viz = org_fld%ntot_phys_viz
!
      new_fld%num_component =>    org_fld%num_component
      new_fld%istack_component => org_fld%istack_component
      new_fld%iorder_eletype =>   org_fld%iorder_eletype
      new_fld%iflag_monitor =>    org_fld%iflag_monitor
      new_fld%phys_name =>        org_fld%phys_name
!
      end subroutine link_field_name_type
!
! -------------------------------------------------------------------
!
      subroutine link_field_data_type(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data), intent(inout) :: new_fld
!
!
      call link_field_name_type(org_fld, new_fld)
      new_fld%d_fld =>        org_fld%d_fld
      new_fld%iflag_update => org_fld%iflag_update
!
      end subroutine link_field_data_type
!
! -------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine disconnect_phys_name_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
       nullify(fld%phys_name, fld%iorder_eletype, fld%iflag_monitor)
       nullify(fld%num_component, fld%istack_component)
!
      end subroutine disconnect_phys_name_type
!
!  --------------------------------------------------------------------
!
      subroutine disconnect_phys_data_type(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      nullify(fld%d_fld, fld%iflag_update)
!
      end subroutine disconnect_phys_data_type
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine check_nodal_field_name_type(fld)
!
      type(phys_data), intent(in) :: fld
!
!
      integer(kind = kint) :: i
!
      write(*,*) 'fld%num_phys ',fld%num_phys
      write(*,*) 'fld%num_phys_viz ',fld%num_phys_viz
      write(*,*) 'id#, num_component, stack_component, field_name '
      do i = 1, fld%num_phys
        write(*,'(3i6,2x,a2,a)') i, fld%num_component(i),               &
     &         fld%istack_component(i), '  ', trim(fld%phys_name(i))
      end do
!
      end subroutine check_nodal_field_name_type
!
!   ---------------------------------------------------------------------
!
      subroutine check_nodal_data_type(my_rank, fld, nnod,              &
     &          numdir, i_field)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, numdir, i_field
      type(phys_data), intent(in) :: fld
      integer(kind = kint) :: inod, nd
!
      write(50+my_rank,*) 'inod, nodal field: ', i_field, numdir
      do inod = 1, nnod
        write(50+my_rank,'(i10,1p10e25.14)')                            &
     &         inod, (fld%d_fld(inod,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_nodal_data_type
!
!  --------------------------------------------------------------------
!
      end module t_phys_data
