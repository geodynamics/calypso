!t_surface_group_connect.f90
!      module t_surface_group_connect
!
!     Written by H. Matsui on Dec., 2008
!
!> @brief Structure of connectivity data for surface group items
!
!> Substitution of
!> @n      (module m_surface_group_connect)
!
!      subroutine alloc_num_surf_grp_nod(sf_grp, sf_nod)
!      subroutine alloc_num_surf_grp_nod_smp(sf_grp, sf_nod)
!      subroutine alloc_item_surf_grp_nod(sf_nod)
!      subroutine alloc_vect_surf_grp_nod(sf_nod)
!        type(surface_group_data), intent(in) :: sf_grp
!        type(surface_node_grp_data), intent(in) :: sf_nod
!
!      subroutine dealloc_num_surf_grp_nod(sf_nod)
!      subroutine dealloc_num_surf_grp_nod_smp(sf_nod)
!      subroutine dealloc_item_surf_grp_nod(sf_nod)
!      subroutine dealloc_vect_surf_grp_nod(sf_nod)
!        type(surface_node_grp_data), intent(in) :: sf_nod
!
      module t_surface_group_connect
!
      use m_precision
!
      implicit  none
!
!
!> Structure of connectivity data for surface group items
      type surface_node_grp_data
!>   total number of node for surface group
        integer(kind=kint) :: ntot_node_sf_grp
!>   number of node for each surface group
        integer(kind=kint), pointer :: nnod_sf_grp(:)
!>   end number of node for each surface group
        integer(kind=kint), pointer :: inod_stack_sf_grp(:)
!
!>      end address of each element group for SMP process
        integer( kind=kint ), pointer :: istack_surf_nod_smp(:)
!>      maximum number of element group for SMP process
        integer( kind=kint )  ::  max_sf_nod_4_smp
!
!>   local node ID for surface group
        integer(kind=kint), pointer :: inod_surf_grp(:)
!>   local node ID on opposite surface
        integer(kind=kint), pointer :: surf_node_n(:)
!>   number of surface for each node on surface group
        integer(kind=kint), pointer :: num_sf_4_nod(:)
!>   normal vector at each node for surface group
        real(kind=kreal),   pointer :: surf_norm_nod(:,:)
!>   multiply coefs at each node for surface group
        real(kind=kreal),   pointer :: coef_sf_nod(:)
      end type surface_node_grp_data
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_surf_grp_nod(sf_grp, sf_nod)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      allocate ( sf_nod%nnod_sf_grp(sf_grp%num_grp) )
      allocate ( sf_nod%inod_stack_sf_grp(0:sf_grp%num_grp) )
      if(sf_grp%num_grp .gt. 0) sf_nod%nnod_sf_grp = 0
      sf_nod%inod_stack_sf_grp = 0
!
      end subroutine alloc_num_surf_grp_nod
!
!-----------------------------------------------------------------------
!
      subroutine alloc_num_surf_grp_nod_smp(sf_grp, sf_nod)
!
      use t_group_data
!
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
       allocate( sf_nod%istack_surf_nod_smp(0:sf_grp%num_grp_smp))
       sf_nod%istack_surf_nod_smp = 0
!
      end subroutine alloc_num_surf_grp_nod_smp
!
!-----------------------------------------------------------------------
!
      subroutine alloc_item_surf_grp_nod(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      allocate ( sf_nod%inod_surf_grp(sf_nod%ntot_node_sf_grp) )
      allocate ( sf_nod%surf_node_n (sf_nod%ntot_node_sf_grp) )
      allocate ( sf_nod%num_sf_4_nod(sf_nod%ntot_node_sf_grp) )
!
      if (sf_nod%ntot_node_sf_grp .gt. 0) then
        sf_nod%inod_surf_grp = 0
        sf_nod%surf_node_n =   0
        sf_nod%num_sf_4_nod =  0
      end if
!
      end subroutine alloc_item_surf_grp_nod
!
!-----------------------------------------------------------------------
!
      subroutine alloc_vect_surf_grp_nod(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      allocate ( sf_nod%surf_norm_nod   (sf_nod%ntot_node_sf_grp,3) )
      allocate ( sf_nod%coef_sf_nod     (sf_nod%ntot_node_sf_grp)   )
!
      if (sf_nod%ntot_node_sf_grp .gt. 0) then
        sf_nod%surf_norm_nod    = 0.0d0
        sf_nod%coef_sf_nod      = 0.0d0
      end if
!
      end subroutine alloc_vect_surf_grp_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_num_surf_grp_nod(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      deallocate ( sf_nod%inod_stack_sf_grp, sf_nod%nnod_sf_grp )
!
      end subroutine dealloc_num_surf_grp_nod
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_num_surf_grp_nod_smp(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
       deallocate( sf_nod%istack_surf_nod_smp )
!
      end subroutine dealloc_num_surf_grp_nod_smp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_item_surf_grp_nod(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      deallocate ( sf_nod%inod_surf_grp )
      deallocate ( sf_nod%surf_node_n   )
      deallocate ( sf_nod%num_sf_4_nod  )
!
      end subroutine dealloc_item_surf_grp_nod
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_vect_surf_grp_nod(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      deallocate ( sf_nod%surf_norm_nod    )
      deallocate ( sf_nod%coef_sf_nod      )
!
      end subroutine dealloc_vect_surf_grp_nod
!
!-----------------------------------------------------------------------
!
      end module t_surface_group_connect
