!>@file   t_group_data.f90
!!@brief  module t_group_data
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!> @brief Structure of group data
!
!!@verbatim
!!      subroutine allocate_grp_type(grp)
!!      subroutine allocate_grp_type_num(grp)
!!      subroutine allocate_grp_type_item(grp)
!!      subroutine allocate_grp_type_smp(grp)
!!      subroutine allocate_surf_grp_type(sf_grp)
!!      subroutine allocate_sf_grp_type_num(sf_grp)
!!      subroutine allocate_sf_grp_type_item(sf_grp)
!!      subroutine allocate_sf_grp_type_smp(grp)
!!
!!      subroutine deallocate_grp_type(grp)
!!      subroutine deallocate_grp_type_num(grp)
!!      subroutine deallocate_grp_type_item(grp)
!!      subroutine deallocate_grp_type_smp(grp)
!!
!!      subroutine deallocate_sf_grp_type(sf_grp)
!!      subroutine deallocate_sf_grp_type_num(sf_grp)
!!      subroutine deallocate_sf_grp_type_item(sf_grp)
!!      subroutine deallocate_sf_grp_type_smp(grp)
!!
!!      subroutine check_group_type_data(my_rank, grp)
!!      subroutine check_surf_grp_type_data(my_rank, sf_grp)
!!      subroutine check_grp_4_sheard_para(my_rank, grp)
!!      subroutine check_surf_grp_4_sheard_para(my_rank, sf_grp)
!!      subroutine compare_group_types(my_rank, grp_ref, grp)
!!      subroutine compare_surface_grp_types(my_rank, sf_grp_ref, sf_grp)
!!        integer(kind = kint), intent(in) :: my_rank
!!        type(group_data), intent(in) :: grp
!!        type(surface_group_data), intent(in) :: sf_grp
!!@endverbatim
!
      module t_group_data
!
      use m_precision
!
      implicit  none
!
!>  Structure for node and element group
      type group_data
!>      number of group
        integer (kind=kint) :: num_grp
!>      total number of for group
        integer (kind=kint) :: num_item
!
!>      number of in each group
        integer (kind=kint), allocatable :: nitem_grp(:)
!>      end address of each group
        integer (kind=kint), allocatable :: istack_grp(:)
!>      local ID for group
        integer (kind=kint), allocatable :: item_grp(:)
!
!>      group name
        character (len=kchara), allocatable :: grp_name(:)
!
!>      number of group for SMP process
        integer( kind=kint )  ::  num_grp_smp
!>      end address of each group for SMP process
        integer( kind=kint ), allocatable :: istack_grp_smp(:)
!>      maximum number of group for SMP process
        integer( kind=kint )  ::  max_grp_smp
      end type group_data
!
!
!>  Structure for surfacet group
      type surface_group_data
!>      number of surface group
        integer (kind=kint) :: num_grp
!>      total number of surface for surface group
        integer (kind=kint) :: num_item
!
!>      number of surface in each surface group
        integer (kind=kint), allocatable :: nitem_grp(:)
!>      end address of each surface group
        integer (kind=kint), allocatable :: istack_grp(:)
!>      local surface ID for surface group
!>      surf_item(1,:):  local element ID
!>      surf_item(2,:):  surface ID for each element
        integer (kind=kint), allocatable :: item_sf_grp(:,:)
!
!>      surface group name
        character (len=kchara), allocatable :: grp_name(:)
!
!>      number of surface group for SMP process
        integer( kind=kint )  ::  num_grp_smp
!>      end address of each surface group for SMP process
        integer( kind=kint ), allocatable :: istack_grp_smp(:)
!>      maximum number of surface group for SMP process
        integer( kind=kint )  ::  max_grp_smp
      end type surface_group_data
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_grp_type(grp)
!
      type(group_data), intent(inout) :: grp
!
      call allocate_grp_type_num(grp)
      call allocate_grp_type_item(grp)
!
      end subroutine allocate_grp_type
!
! ----------------------------------------------------------------------
!
      subroutine allocate_grp_type_num(grp)
!
      type(group_data), intent(inout) :: grp
!
      allocate(grp%grp_name(grp%num_grp))
      allocate(grp%nitem_grp(grp%num_grp))
      allocate(grp%istack_grp(0:grp%num_grp))
!
      if (grp%num_grp .gt. 0) grp%nitem_grp =  0
      grp%istack_grp = 0
!
      end subroutine allocate_grp_type_num
!
! ----------------------------------------------------------------------
!
      subroutine allocate_grp_type_item(grp)
!
      type(group_data), intent(inout) :: grp
!
      grp%num_item = grp%istack_grp(grp%num_grp)
!
      allocate(grp%item_grp(grp%num_item))
      if (grp%num_item .gt. 0) grp%item_grp =  0
!
      end subroutine allocate_grp_type_item
!
! ----------------------------------------------------------------------
!
      subroutine allocate_grp_type_smp(grp)
!
      type(group_data), intent(inout) :: grp
!
      allocate(grp%istack_grp_smp(0:grp%num_grp_smp))
      grp%istack_grp_smp =  0
!
      end subroutine allocate_grp_type_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_surf_grp_type(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      call allocate_sf_grp_type_num(sf_grp)
      call allocate_sf_grp_type_item(sf_grp)
!
      end subroutine allocate_surf_grp_type
!
!-----------------------------------------------------------------------
!
      subroutine allocate_sf_grp_type_num(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      allocate(sf_grp%grp_name(sf_grp%num_grp))
      allocate(sf_grp%nitem_grp(sf_grp%num_grp))
      allocate(sf_grp%istack_grp(0:sf_grp%num_grp))
!
      if (sf_grp%num_grp .gt. 0) sf_grp%nitem_grp =  0
      sf_grp%istack_grp = 0
!
      end subroutine allocate_sf_grp_type_num
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sf_grp_type_item(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      sf_grp%num_item = sf_grp%istack_grp(sf_grp%num_grp)
!
      allocate(sf_grp%item_sf_grp(2,sf_grp%num_item))
      if (sf_grp%num_item .gt. 0) sf_grp%item_sf_grp =  0
!
      end subroutine allocate_sf_grp_type_item
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sf_grp_type_smp(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      allocate(sf_grp%istack_grp_smp(0:sf_grp%num_grp_smp))
      sf_grp%istack_grp_smp =  0
!
      end subroutine allocate_sf_grp_type_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_grp_type(grp)
!
      type(group_data), intent(inout) :: grp
!
      call deallocate_grp_type_item(grp)
      call deallocate_grp_type_num(grp)
!
      end subroutine deallocate_grp_type
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_grp_type_num(grp)
!
      type(group_data), intent(inout) :: grp
!
      deallocate(grp%grp_name)
      deallocate(grp%nitem_grp)
      deallocate(grp%istack_grp)
!
      end subroutine deallocate_grp_type_num
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_grp_type_item(grp)
!
      type(group_data), intent(inout) :: grp
!
      deallocate(grp%item_grp)
!
      end subroutine deallocate_grp_type_item
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_grp_type_smp(grp)
!
      type(group_data), intent(inout) :: grp
!
      deallocate(grp%istack_grp_smp)
!
      end subroutine deallocate_grp_type_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_sf_grp_type(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      call deallocate_sf_grp_type_num(sf_grp)
      call deallocate_sf_grp_type_item(sf_grp)
!
      end subroutine deallocate_sf_grp_type
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sf_grp_type_num(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      deallocate(sf_grp%grp_name)
      deallocate(sf_grp%nitem_grp)
      deallocate(sf_grp%istack_grp)
!
      end subroutine deallocate_sf_grp_type_num
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sf_grp_type_item(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      deallocate(sf_grp%item_sf_grp)
!
      end subroutine deallocate_sf_grp_type_item
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_sf_grp_type_smp(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      deallocate(sf_grp%istack_grp_smp)
!
      end subroutine deallocate_sf_grp_type_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_group_type_data(my_rank, grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: grp
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_grp',  grp%num_grp
      write(50+my_rank,*) 'num_item', grp%num_item
      write(50+my_rank,*) 'istack_grp', grp%istack_grp
      do i = 1, grp%num_grp
        write(50+my_rank,*) trim(grp%grp_name(i))
        ist = grp%istack_grp(i-1)+1
        ied = grp%istack_grp(i)
        write(50+my_rank,'(5i16)') grp%item_grp(ist:ied)
      end do
!
      end subroutine check_group_type_data
!
! -----------------------------------------------------------------------
!
      subroutine check_surf_grp_type_data(my_rank, sf_grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(surface_group_data), intent(inout) :: sf_grp
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_grp',  sf_grp%num_grp
      write(50+my_rank,*) 'num_item', sf_grp%num_item
      write(50+my_rank,*) 'istack_grp', sf_grp%istack_grp
      do i = 1, sf_grp%num_grp
        write(50+my_rank,*) trim(sf_grp%grp_name(i))
        ist = sf_grp%istack_grp(i-1)+1
        ied = sf_grp%istack_grp(i)
        write(50+my_rank,'(5i16)') sf_grp%item_sf_grp(1,ist:ied)
        write(50+my_rank,'(5i16)') sf_grp%item_sf_grp(2,ist:ied)
      end do
!
      end subroutine check_surf_grp_type_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine compare_group_types(my_rank, grp_ref, grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: grp_ref
      type(group_data), intent(in) :: grp
!
      integer(kind = kint) :: i
!
!
      if(grp_ref%num_grp .ne. grp%num_grp) write(*,*)                   &
     &   'num_bc', my_rank, grp_ref%num_grp, grp%num_grp
      if(grp_ref%num_item .ne. grp%num_item) write(*,*)                 &
     &   'num_nod_bc', my_rank, grp_ref%num_item, grp%num_item
      do i = 1, grp%num_grp
        if(grp_ref%grp_name(i) .ne. grp%grp_name(i))                    &
     &       write(*,*) 'bc_name(i)', my_rank, i,                       &
     &       grp_ref%grp_name(i), grp%grp_name(i)
        if(grp_ref%istack_grp(i) .ne. grp%istack_grp(i))                &
     &       write(*,*) 'bc_istack(i)', my_rank, i,                     &
     &       grp_ref%istack_grp(i), grp%istack_grp(i)
      end do
      do i = 1, grp%num_item
        if(grp_ref%item_grp(i) .ne. grp%item_grp(i))                    &
     &       write(*,*) 'bc_item(i)', my_rank, i,                       &
     &       grp_ref%item_grp(i), grp%item_grp(i)
      end do
!
      end subroutine compare_group_types
!
!-----------------------------------------------------------------------
!
      subroutine compare_surface_grp_types(my_rank, sf_grp_ref, sf_grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(surface_group_data), intent(in) :: sf_grp_ref
      type(surface_group_data), intent(in) :: sf_grp
!
      integer(kind = kint) :: i
!
!
      if(sf_grp_ref%num_grp .ne. sf_grp%num_grp) write(*,*)             &
     &   'num_surf', my_rank, sf_grp_ref%num_grp, sf_grp%num_grp
      if(sf_grp_ref%num_item .ne. sf_grp%num_item) write(*,*)           &
     &   'num_surf_bc', my_rank, sf_grp_ref%num_item, sf_grp%num_item
      do i = 1, sf_grp%num_grp
        if(sf_grp_ref%grp_name(i) .ne. sf_grp%grp_name(i))              &
     &       write(*,*) 'surf_name(i)', my_rank, i,                     &
     &       sf_grp_ref%grp_name(i), sf_grp%grp_name(i)
        if(sf_grp_ref%istack_grp(i) .ne. sf_grp%istack_grp(i))          &
     &       write(*,*) 'surf_istack(i)', my_rank, i,                   &
     &       sf_grp_ref%istack_grp(i), sf_grp%istack_grp(i)
      end do
      do i = 1, sf_grp%num_item
        if(sf_grp_ref%item_sf_grp(1,i) .ne. sf_grp%item_sf_grp(1,i)     &
     &  .or. sf_grp_ref%item_sf_grp(2,i) .ne. sf_grp%item_sf_grp(2,i))  &
     &       write(*,*) 'surf_item(:,i)', my_rank, i,                   &
     &       sf_grp_ref%item_sf_grp(1,i), sf_grp%item_sf_grp(1,i),      &
     &       sf_grp_ref%item_sf_grp(2,i), sf_grp%item_sf_grp(2,i)
      end do
!
      end subroutine compare_surface_grp_types
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_grp_4_sheard_para(my_rank, grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(in) :: grp
!
!
      write(*,*) 'PE: ', my_rank, 'num_bc ', grp%num_grp
      write(*,*) 'PE: ', my_rank, 'num_bc_smp ', grp%num_grp_smp
      write(*,*) 'PE: ', my_rank, 'ibc_smp_stack ', grp%istack_grp_smp
!
      end subroutine check_grp_4_sheard_para
!
!-----------------------------------------------------------------------
!
      subroutine check_surf_grp_4_sheard_para(my_rank, sf_grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(surface_group_data), intent(in) :: sf_grp
!
!
      write(*,*) 'PE: ', my_rank, 'num_bc ', sf_grp%num_grp
      write(*,*) 'PE: ', my_rank, 'num_bc_smp ', sf_grp%num_grp_smp
      write(*,*) 'PE: ', my_rank, 'ibc_smp_stack ',                     &
     &          sf_grp%istack_grp_smp
!
      end subroutine check_surf_grp_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module t_group_data
!
