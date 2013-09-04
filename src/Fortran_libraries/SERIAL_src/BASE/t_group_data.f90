!t_group_data.f90
!      module t_group_data
!
!> @brief Structure of group data
!
!>    Substitution of 
!>@n        (module m_node_group)
!>@n        (module m_element_group)
!>@n        (module m_surface_group)
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_grp_type_num(grp)
!      subroutine allocate_grp_type_item(grp)
!      subroutine allocate_grp_type_smp(grp)
!      subroutine allocate_sf_grp_type_num(sf_grp)
!      subroutine allocate_sf_grp_type_item(sf_grp)
!      subroutine allocate_sf_grp_type_smp(grp)
!
!      subroutine deallocate_grp_type_num(grp)
!      subroutine deallocate_grp_type_item(grp)
!      subroutine deallocate_grp_type_smp(grp)
!      subroutine deallocate_sf_grp_type_num(sf_grp)
!      subroutine deallocate_sf_grp_type_item(sf_grp)
!      subroutine deallocate_sf_grp_type_smp(grp)
!
!      subroutine link_group_type(grp_org, grp_new)
!      subroutine link_surf_group_type(sf_grp_org, sf_grp_new)
!      subroutine unlink_group_type(grp)
!      subroutine unlink_surf_group_type(sf_grp)
!
!      subroutine check_group_type_data(my_rank, grp)
!      subroutine check_surf_grp_type_data(my_rank, sf_grp)
!        integer(kind = kint), intent(in) :: my_rank
!        type(group_data), intent(in) :: grp
!        type(surface_group_data), intent(in) :: sf_grp
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
        integer (kind=kint), pointer :: nitem_grp(:)
!>      end address of each group
        integer (kind=kint), pointer :: istack_grp(:)
!>      local ID for group
        integer (kind=kint), pointer :: item_grp(:)
!
!>      group name
        character (len=kchara), pointer :: grp_name(:)
!
!>      number of group for SMP process
        integer( kind=kint )  ::  num_grp_smp
!>      end address of each group for SMP process
        integer( kind=kint ), pointer :: istack_grp_smp(:)
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
        integer (kind=kint), pointer :: nitem_grp(:)
!>      end address of each surface group
        integer (kind=kint), pointer :: istack_grp(:)
!>      local surface ID for surface group
!>      surf_item(1,:):  local element ID
!>      surf_item(2,:):  surface ID for each element
        integer (kind=kint), pointer :: item_sf_grp(:,:)
!
!>      surface group name
        character (len=kchara), pointer :: grp_name(:)
!
!>      number of surface group for SMP process
        integer( kind=kint )  ::  num_grp_smp
!>      end address of each surface group for SMP process
        integer( kind=kint ), pointer :: istack_grp_smp(:)
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
!  ---------------------------------------------------------------------
!
      subroutine link_group_type(grp_org, grp_new)
!
      type(group_data), intent(in) :: grp_org
      type(group_data), intent(inout) :: grp_new
!
      grp_new%num_grp =  grp_org%num_grp
      grp_new%num_item = grp_org%num_item
!
      grp_new%grp_name =>   grp_org%grp_name
      grp_new%istack_grp => grp_org%istack_grp
      grp_new%item_grp =>   grp_org%item_grp
!
      end subroutine link_group_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_surf_group_type(sf_grp_org, sf_grp_new)
!
      type(surface_group_data), intent(in) :: sf_grp_org
      type(surface_group_data), intent(inout) :: sf_grp_new
!
      sf_grp_new%num_grp =  sf_grp_org%num_grp
      sf_grp_new%num_item = sf_grp_org%num_item
!
      sf_grp_new%grp_name =>   sf_grp_org%grp_name
      sf_grp_new%istack_grp => sf_grp_org%istack_grp
      sf_grp_new%item_sf_grp => sf_grp_org%item_sf_grp
!
      end subroutine link_surf_group_type
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_group_type(grp)
!
      type(group_data), intent(inout) :: grp
!
      grp%num_grp =  0
      grp%num_item = 0
!
      nullify(grp%grp_name, grp%istack_grp)
      nullify(grp%item_grp)
!
      end subroutine unlink_group_type
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_surf_group_type(sf_grp)
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      sf_grp%num_grp =  0
      sf_grp%num_item = 0
!
      nullify(sf_grp%grp_name, sf_grp%istack_grp)
      nullify(sf_grp%item_sf_grp)
!
      end subroutine unlink_surf_group_type
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_group_type_data(my_rank, grp)
!
      integer(kind = kint), intent(in) :: my_rank
      type(group_data), intent(inout) :: grp
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_grp',  grp%num_grp
      write(50+my_rank,*) 'num_item', grp%num_item
      write(50+my_rank,*) 'istack_grp', grp%istack_grp
      do i = 1, grp%num_grp
        write(50+my_rank,*) trim(grp%grp_name(i))
        ist = grp%istack_grp(i-1)+1
        ied = grp%istack_grp(i)
        write(50+my_rank,'(5i10)') grp%item_grp(ist:ied)
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
        write(50+my_rank,'(5i10)') sf_grp%item_sf_grp(1,ist:ied)
        write(50+my_rank,'(5i10)') sf_grp%item_sf_grp(2,ist:ied)
      end do
!
      end subroutine check_surf_grp_type_data
!
! -----------------------------------------------------------------------
!
      end module t_group_data
!
