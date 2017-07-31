!>@file   t_surface_group_connect.f90
!!@brief  module t_surface_group_connect
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Structure of connectivity data for surface group items
!!
!!@verbatim
!!      subroutine alloc_num_surf_grp_nod(num_surf, sf_nod)
!!      subroutine alloc_num_surf_grp_nod_smp(num_surf_smp, sf_nod)
!!      subroutine alloc_item_surf_grp_nod(sf_nod)
!!      subroutine alloc_vect_surf_grp_nod(sf_nod)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_nod
!!
!!      subroutine dealloc_num_surf_grp_nod_smp(sf_nod)
!!      subroutine dealloc_surf_grp_nod(sf_nod)
!!        type(surface_node_grp_data), intent(in) :: sf_nod
!!
!!       subroutine check_surface_node_id(id_check, sf_nod)
!!      subroutine check_surf_nod_4_sheard_para                         &
!!     &         (id_check, num_surf, sf_nod)
!!@endverbatim
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
        integer(kind=kint), allocatable :: nnod_sf_grp(:)
!>   end number of node for each surface group
        integer(kind=kint), allocatable :: inod_stack_sf_grp(:)
!
!>      end address of each element group for SMP process
        integer( kind=kint ), allocatable :: istack_surf_nod_smp(:)
!>      maximum number of element group for SMP process
        integer( kind=kint )  ::  max_sf_nod_4_smp
!
!>   local node ID for surface group
        integer(kind=kint), allocatable :: inod_surf_grp(:)
!>   local node ID on opposite surface
        integer(kind=kint), allocatable :: surf_node_n(:)
!>   number of surface for each node on surface group
        integer(kind=kint), allocatable :: num_sf_4_nod(:)
!>   normal vector at each node for surface group
        real(kind=kreal),   allocatable :: surf_norm_nod(:,:)
!>   multiply coefs at each node for surface group
        real(kind=kreal),   allocatable :: coef_sf_nod(:)
      end type surface_node_grp_data
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_surf_grp_nod(num_surf, sf_nod)
!
      integer(kind = kint), intent(in) :: num_surf
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      allocate ( sf_nod%nnod_sf_grp(num_surf) )
      allocate ( sf_nod%inod_stack_sf_grp(0:num_surf) )
      if(num_surf .gt. 0) sf_nod%nnod_sf_grp = 0
      sf_nod%inod_stack_sf_grp = 0
!
      end subroutine alloc_num_surf_grp_nod
!
!-----------------------------------------------------------------------
!
      subroutine alloc_num_surf_grp_nod_smp(num_surf_smp, sf_nod)
!
      integer(kind = kint), intent(in) :: num_surf_smp
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
       allocate( sf_nod%istack_surf_nod_smp(0:num_surf_smp))
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
      subroutine dealloc_surf_grp_nod(sf_nod)
!
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      deallocate ( sf_nod%inod_surf_grp )
      deallocate ( sf_nod%surf_node_n, sf_nod%num_sf_4_nod  )
      deallocate (sf_nod%surf_norm_nod, sf_nod%coef_sf_nod)
      deallocate ( sf_nod%inod_stack_sf_grp, sf_nod%nnod_sf_grp )
!
      end subroutine dealloc_surf_grp_nod
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_surface_node_id(id_check, sf_nod)
!
       integer(kind = kint), intent(in) :: id_check
      type(surface_node_grp_data), intent(in) :: sf_nod
       integer(kind = kint) :: inum
!
!
       write(id_check+50,*) 'inod_stack_sf_grp',                        &
     &                   sf_nod%inod_stack_sf_grp
       do inum = 1, sf_nod%ntot_node_sf_grp
         write(id_check+50,*) inum, sf_nod%inod_surf_grp(inum),         &
     &          sf_nod%surf_node_n(inum), sf_nod%num_sf_4_nod(inum)
       end do
!
       end subroutine check_surface_node_id
!
!-----------------------------------------------------------------------
!
      subroutine check_surf_nod_4_sheard_para                           &
     &         (id_check, num_surf, sf_nod)
!
       use m_machine_parameter
!
      integer(kind = kint), intent(in) :: id_check, num_surf
      type(surface_node_grp_data), intent(in) :: sf_nod
!
      integer(kind = kint) :: isurf, ist, ied
!
!
      do isurf = 1, num_surf
        ist = np_smp*(isurf-1)+1
        ied = np_smp*isurf
        write(50+id_check,*) isurf,sf_nod%istack_surf_nod_smp(ist:ied)
      end do
!
      end subroutine check_surf_nod_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module t_surface_group_connect
