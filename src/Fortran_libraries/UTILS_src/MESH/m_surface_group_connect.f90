!m_surface_group_connect.f90
!     module m_surface_group_connect
!
!> @brief connectivity data for surface group items
!
!     Writteg by H.Matsui on Aug., 2006
!
!      subroutine allocate_surf_id_4_sf_grp
!
!      subroutine allocate_edge_stack_4_sf_grp
!      subroutine allocate_edge_id_4_sf_grp
!
!      subroutine allocate_node_stack_4_sf_grp
!      subroutine allocate_surf_nod_param_smp
!
!      subroutine allocate_edge_stack_4_sf_grp
!      subroutine deallocate_edge_id_4_sf_grp
!
!      subroutine allocate_surf_nod
!      subroutine allocate_vect_4_node_on_surf
!      subroutine deallocate_surf_nod
!      subroutine deallocate_vect_4_node_on_surf
!      subroutine deallocate_surf_nod_param_smp
!
!      subroutine check_surface_node_id(id_check)
!
      module m_surface_group_connect
!
      use m_precision
!
      implicit  none
!
!
      integer(kind=kint), allocatable, target :: isurf_grp(:)
!<   local surface ID for surface group
!
      integer(kind=kint) :: ntot_edge_sf_grp
!<   total number of edge for surface group
      integer(kind=kint), allocatable, target :: nedge_sf_grp(:)
!<   number of edge for each surface group
      integer(kind=kint), allocatable, target :: iedge_stack_sf_grp(:)
!<   end number of edge for each surface group
      integer(kind=kint), allocatable, target :: iedge_surf_grp(:)
!<   local edge ID for surface group
!
      integer(kind=kint) :: ntot_node_sf_grp
!<   total number of node for surface group
      integer(kind=kint), allocatable, target :: nnod_sf_grp(:)
!<   number of node for each surface group
      integer(kind=kint), allocatable, target :: inod_stack_sf_grp(:)
!<   end number of node for each surface group
      integer(kind=kint), allocatable, target :: inod_surf_grp(:)
!<   local node ID for surface group
!
      integer(kind=kint), allocatable, target :: surf_node_n(:)
!<   local node ID on opposite surface
      integer(kind=kint), allocatable, target :: num_sf_4_nod(:)
!<   number of surface for each node on surface group
!
      real(kind=kreal),   allocatable, target :: surf_norm_nod(:,:)
!<   normal vector at each node for surface group
      real(kind=kreal),   allocatable, target :: coef_sf_nod(:)
!<   multiply coefs at each node for surface group
!
      integer(kind=kint), allocatable, target :: isurf_grp_n(:)
!<   local surface ID for opposite side of surface group
!
!
      integer( kind=kint ), allocatable :: isurf_nod_smp_stack(:)
!<      end address of each element group for SMP process
      integer( kind=kint )  ::  max_sf_nod_4_smp
!<      maximum number of element group for SMP process
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_id_4_sf_grp
!
      use m_surface_group
!
      allocate(isurf_grp(num_surf_bc))
      allocate(isurf_grp_n(num_surf_bc))
      isurf_grp = 0
      isurf_grp_n = 0
!
      end subroutine allocate_surf_id_4_sf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_edge_stack_4_sf_grp
!
      use m_surface_group
!
      allocate(nedge_sf_grp(num_surf))
      allocate(iedge_stack_sf_grp(0:num_surf))
!
      nedge_sf_grp = 0
      iedge_stack_sf_grp = 0
!
      end subroutine allocate_edge_stack_4_sf_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_edge_id_4_sf_grp
!
      allocate(iedge_surf_grp(ntot_edge_sf_grp))
      iedge_surf_grp = 0
!
      end subroutine allocate_edge_id_4_sf_grp
!
!-----------------------------------------------------------------------
!
      subroutine allocate_node_stack_4_sf_grp
!
      use m_surface_group
!
      allocate(nnod_sf_grp(num_surf))
      allocate(inod_stack_sf_grp(0:num_surf))
!
      nnod_sf_grp = 0
      inod_stack_sf_grp = 0
!
      end subroutine allocate_node_stack_4_sf_grp
!
!-----------------------------------------------------------------------
!
       subroutine allocate_surf_nod_param_smp
!
      use m_surface_group
!
       allocate( isurf_nod_smp_stack(0:num_surf_smp))
       isurf_nod_smp_stack = 0
!
       end subroutine allocate_surf_nod_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_id_4_sf_grp
!
      use m_surface_group
!
      deallocate(isurf_grp)
      deallocate(isurf_grp_n)
!
      end subroutine deallocate_surf_id_4_sf_grp
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_edge_id_4_sf_grp
!
      deallocate(nedge_sf_grp, iedge_stack_sf_grp)
      deallocate(iedge_surf_grp)
!
      end subroutine deallocate_edge_id_4_sf_grp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine allocate_surf_nod
!
       allocate ( inod_surf_grp(ntot_node_sf_grp) )
       allocate ( surf_node_n(ntot_node_sf_grp) )
       allocate ( num_sf_4_nod(ntot_node_sf_grp) )
       inod_surf_grp = 0
       surf_node_n =   0
       num_sf_4_nod =  0
!
      end subroutine allocate_surf_nod
!
!-----------------------------------------------------------------------
!
      subroutine allocate_vect_4_node_on_surf
!
       allocate ( surf_norm_nod(ntot_node_sf_grp,3) )
       allocate ( coef_sf_nod(ntot_node_sf_grp) )
!
       surf_norm_nod    = 0.0d0
       coef_sf_nod      = 0.0d0
!
      end subroutine allocate_vect_4_node_on_surf
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_nod
!
       deallocate ( inod_surf_grp )
       deallocate ( surf_node_n )
       deallocate ( num_sf_4_nod )
       deallocate ( inod_stack_sf_grp, nnod_sf_grp )
!
      end subroutine deallocate_surf_nod
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_vect_4_node_on_surf
!
       deallocate ( surf_norm_nod )
       deallocate ( coef_sf_nod )
!
      end subroutine deallocate_vect_4_node_on_surf
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_surf_nod_param_smp
!
       deallocate(isurf_nod_smp_stack)
!
       end subroutine deallocate_surf_nod_param_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine check_surface_node_id(id_check)
!
       use m_surface_group
!
       integer(kind = kint), intent(in) :: id_check
       integer(kind = kint) :: inum
!
!
       write(id_check,*) 'inod_stack_sf_grp', inod_stack_sf_grp
       do inum = 1, ntot_node_sf_grp
       write(id_check,*) inum, inod_surf_grp(inum), surf_node_n(inum),  &
     &                     num_sf_4_nod(inum)
       end do
!
       end subroutine check_surface_node_id
!
!-----------------------------------------------------------------------
!
      subroutine check_surf_nod_4_sheard_para(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
       write(*,*) 'PE: ', my_rank,                                      &
     &           'isurf_nod_smp_stack ', isurf_nod_smp_stack
!
      end subroutine check_surf_nod_4_sheard_para
!
!-----------------------------------------------------------------------
!
      end module m_surface_group_connect
