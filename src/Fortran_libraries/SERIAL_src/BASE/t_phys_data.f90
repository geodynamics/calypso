!>@file   t_phys_data.f90
!!        module t_phys_data
!!
!!@author H. Matsui
!!@date Programmed in ????
!!      Modified March, 2020
!!
!> @brief Structure of field data
!!
!!@verbatim
!!      subroutine alloc_phys_name(fld)
!!      subroutine alloc_phys_data(num, fld)
!!        integer(kind = kint), intent(in) :: num
!!
!!      subroutine dealloc_phys_name(fld)
!!      subroutine dealloc_phys_data(fld)
!!
!!      integer(kind = kint) function field_id_by_address(fld, i_ref)
!!      character(len=kchara) function field_name_by_address(fld, i_ref)
!!      integer(kind=kint) function field_comp_by_address(fld, i_ref)
!!        type(phys_data), intent(in) :: fld
!!
!!      subroutine dup_phys_name(org_fld, new_fld)
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
!!
!!      subroutine check_all_field_data(id_rank, fld)
!!      subroutine check_nodal_field_name(id_output, fld)
!!      subroutine check_nodal_data(id_output, fld, numdir, i_field)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: numdir, i_field
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
!>       number of data points
        integer (kind=kint) :: n_point
!>       number of field
        integer (kind=kint) :: num_phys
!>       total number of component
        integer (kind=kint) :: ntot_phys
!>       number of component for each field
        integer (kind=kint), allocatable :: num_component(:)
!>       end address for each field
        integer (kind=kint), allocatable :: istack_component(:)
!>       FEM order of each field
        integer (kind=kint), allocatable :: iorder_eletype(:)
!
!>       field name
        character (len=kchara), allocatable :: phys_name(:)
!
!>       field data
        real (kind=kreal), allocatable ::   d_fld(:,:)
!>       update flag for field data
        integer (kind=kint), allocatable :: iflag_update(:)
!
!>        number of field for visualizer
        integer (kind=kint) :: num_phys_viz
!>        total number of component for visualizer
        integer (kind=kint) :: ntot_phys_viz
!
!>        flag to get average and RMS data
        logical, allocatable:: flag_monitor(:)
      end type phys_data
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_phys_name(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      if(allocated(fld%phys_name)) return
!
      allocate( fld%phys_name(fld%num_phys) )
      allocate( fld%num_component(fld%num_phys) )
      allocate( fld%istack_component(0:fld%num_phys) )
      allocate( fld%iorder_eletype(fld%num_phys) )
      allocate( fld%flag_monitor(fld%num_phys) )
!
      if(fld%num_phys .gt. 0) then
        fld%phys_name = ''
        fld%num_component =    0
        fld%istack_component = 0
        fld%iorder_eletype =   1
        fld%flag_monitor =  .FALSE.
      end if
!
      end subroutine alloc_phys_name
!
!  --------------------------------------------------------------------
!
      subroutine alloc_phys_data(num, fld)
!
      integer(kind = kint), intent(in) :: num
      type(phys_data), intent(inout) :: fld
!
!
      if(allocated(fld%d_fld)) return
!
      fld%n_point = num
      allocate( fld%d_fld(fld%n_point,fld%ntot_phys) )
      allocate( fld%iflag_update(fld%ntot_phys) )
      if(fld%n_point*fld%ntot_phys .gt. 0)  then
        fld%d_fld =        0.0d0
        fld%iflag_update = 0
      end if
!
       end subroutine alloc_phys_data
!
! --------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine dealloc_phys_name(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      if(allocated(fld%phys_name) .eqv. .FALSE.) return
!
      deallocate( fld%phys_name, fld%iorder_eletype, fld%flag_monitor)
      deallocate( fld%num_component, fld%istack_component )
!
      end subroutine dealloc_phys_name
!
!  --------------------------------------------------------------------
!
      subroutine dealloc_phys_data(fld)
!
      type(phys_data), intent(inout) :: fld
!
!
      if(allocated(fld%d_fld) .eqv. .FALSE.) return
      deallocate( fld%d_fld, fld%iflag_update )
!
      end subroutine dealloc_phys_data
!
!
      integer(kind = kint) function field_id_by_address(fld, i_ref)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_ref
!
      integer(kind = kint) :: i
!
      field_id_by_address = 0
      do i = 1, fld%num_phys
        if(fld%istack_component(i-1) .eq. i_ref-1) then
          field_id_by_address = i
          exit
        end if
      end do
!
      end function field_id_by_address
!
! -----------------------------------------------------------------------
!
      character(len=kchara) function field_name_by_address(fld, i_ref)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_ref
!
      integer(kind = kint) :: i
!
      i = field_id_by_address(fld, i_ref)
      if(i .gt. 0) then
        field_name_by_address = fld%phys_name(i)
      else
        write(field_name_by_address,'(a)') 'MISSING'
      end if
!
      end function field_name_by_address
!
! -----------------------------------------------------------------------
!
      integer(kind=kint) function field_comp_by_address(fld, i_ref)
!
      type(phys_data), intent(in) :: fld
      integer(kind = kint), intent(in) :: i_ref
!
      integer(kind = kint) :: i
!
      i = field_id_by_address(fld, i_ref)
      if(i .gt. 0) then
        field_comp_by_address = fld%istack_component(i)                 &
     &                         - fld%istack_component(i-1)
      else
        field_comp_by_address = 0
      end if
!
      end function field_comp_by_address
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dup_phys_name(org_fld, new_fld)
!
      type(phys_data), intent(in) :: org_fld
      type(phys_data), intent(inout) :: new_fld
      integer(kind = kint) :: i
!
      new_fld%num_phys =      org_fld%num_phys
      new_fld%ntot_phys =     org_fld%ntot_phys
      new_fld%num_phys_viz =  org_fld%num_phys_viz
      new_fld%ntot_phys_viz = org_fld%ntot_phys_viz
      call alloc_phys_name(new_fld)
!
      new_fld%istack_component(0) = 0
!$omp parallel do
      do i = 1, new_fld%num_phys
        new_fld%phys_name(i) =         org_fld%phys_name(i)
        new_fld%num_component(i) =     org_fld%num_component(i)
        new_fld%istack_component(i) =  org_fld%istack_component(i)
        new_fld%iorder_eletype(i) =    org_fld%iorder_eletype(i)
        new_fld%flag_monitor(i) =      org_fld%flag_monitor(i)
      end do
!$omp end parallel do
!
      end subroutine dup_phys_name
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_all_field_data(id_rank, fld)
!
      integer, intent(in) :: id_rank
      type(phys_data), intent(in) :: fld
!
      integer(kind = kint) :: id
!
      id = 50 + id_rank
!
      call check_nodal_field_name(id, fld)
      call check_nodal_data(id, fld, fld%ntot_phys, ione)
!
      end subroutine check_all_field_data
!
!  --------------------------------------------------------------------
!
      subroutine check_nodal_field_name(id_output, fld)
!
      integer(kind = kint), intent(in) :: id_output
      type(phys_data), intent(in) :: fld
!
!
      integer(kind = kint) :: i
!
      write(id_output,*) 'fld%num_phys ',fld%num_phys
      write(id_output,*) 'fld%num_phys_viz ',fld%num_phys_viz
      write(id_output,*)                                                &
     &      'id#, num_component, stack_component, field_name '
      do i = 1, fld%num_phys
        write(id_output,'(3i6,2x,a2,a)') i, fld%num_component(i),       &
     &         fld%istack_component(i), '  ', trim(fld%phys_name(i))
      end do
!
      end subroutine check_nodal_field_name
!
!   ---------------------------------------------------------------------
!
      subroutine check_nodal_data(id_output, fld, numdir, i_field)
!
      integer(kind = kint), intent(in) :: id_output, numdir, i_field
      type(phys_data), intent(in) :: fld
      integer(kind = kint) :: inod, nd
!
      write(id_output,*) 'inod, nodal field: ', i_field, numdir
      do inod = 1, fld%n_point
        write(id_output,'(i16,1p10e25.14)')                             &
     &         inod, (fld%d_fld(inod,i_field+nd-1),nd=1, numdir)
      end do
!
      end subroutine check_nodal_data
!
!  --------------------------------------------------------------------
!
      end module t_phys_data
