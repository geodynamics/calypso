!
!      module ordering_sph_mesh_to_rtp
!
!     Written by H. Matsui on March, 2013
!
!!      subroutine s_ordering_sph_mesh_for_rtp                          &
!!     &         (nidx_rtp, ip_r, ip_t, stk_lc1d, sph_gl1d, stbl,       &
!!     &          node, ele, nod_grp, nod_comm)
!!        type(sph_1d_index_stack), intent(in) :: stk_lc1d
!!        type(sph_1d_global_index), intent(in) :: sph_gl1d
!!        type(comm_table_make_sph), intent(in) :: stbl
!!        type(node_data), intent(inout) :: node
!!        type(element_data), intent(inout) :: ele
!!        type(group_data), intent(inout) :: nod_grp
!!        type(communication_table), intent(inout) :: nod_comm
!
      module ordering_sph_mesh_to_rtp
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), allocatable, private :: inod_old2new(:)
      integer(kind = kint_gl), allocatable, private :: id_global_org(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_ordering_sph_mesh_for_rtp                            &
     &         (nidx_rtp, ip_r, ip_t, stk_lc1d, sph_gl1d, stbl,         &
     &          node, ele, nod_grp, nod_comm)
!
      use t_geometry_data
      use t_comm_table
      use t_group_data
      use t_sph_mesh_1d_connect
      use t_sph_1d_global_index
!
      use cal_sph_node_addresses
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: ip_r, ip_t
      type(sph_1d_index_stack), intent(in) :: stk_lc1d
      type(sph_1d_global_index), intent(in) :: sph_gl1d
      type(comm_table_make_sph), intent(in) :: stbl
!
      type(node_data), intent(inout) :: node
      type(element_data), intent(inout) :: ele
      type(group_data), intent(inout) :: nod_grp
      type(communication_table), intent(inout) :: nod_comm
!
      integer(kind = kint) :: k, l, m
      integer(kind = kint) :: kr, k_gl, l_gl
      integer(kind = kint) :: k_lc, l_lc
      integer(kind = kint) :: inod, inod_org
      integer(kind = kint) :: iele, k1
      integer(kind = kint) :: inum
!
!
      allocate(inod_old2new(node%numnod))
      allocate(id_global_org(node%numnod))
      inod_old2new =  0
      id_global_org = 0
!
      do inum = 1, nod_comm%ntot_import
        inod_org = nod_comm%item_import(inum)
        if(inod_old2new(inod_org) .gt. 0) write(*,*) 'wrong!!',     &
     &                                       inum, inod_org
        inod_old2new(inod_org) = node%internal_node + inum
      end do
!
      inod = 0
      do m = 1, nidx_rtp(3)
        do l = 1, nidx_rtp(2)
          l_gl = l + stk_lc1d%istack_idx_local_rtp_t(ip_t-1)
          l_lc = stbl%irev_sph_t(l_gl,ip_t)
          do k = 1, nidx_rtp(1)
            kr = k + stk_lc1d%istack_idx_local_rtp_r(ip_r-1)
            k_gl = sph_gl1d%idx_global_rtp_r(kr)
            k_lc = stbl%irev_sph_r(k_gl,ip_r)
!
            inod = k + (l-1)*nidx_rtp(1)                                &
     &               + (m-1)*nidx_rtp(1)*nidx_rtp(2)
            inod_org                                                    &
     &           = sph_shell_node_id(ip_r, ip_t, k_lc, l_lc, m, stbl)
!
            if(inod_old2new(inod_org) .gt. 0) write(*,*) 'wrong!!',     &
     &                                       inod, k_lc, l_lc, m
            inod_old2new(inod_org) = inod
          end do
        end do
      end do
!
      inod = nidx_rtp(1)*nidx_rtp(2)*nidx_rtp(3)
      do inod_org = 1, node%numnod
        if(inod_old2new(inod_org) .eq. 0) then
          inod = inod + 1
          inod_old2new(inod_org) = inod
        end if
      end do
!
!
      do k1 = 1, ele%nnod_4_ele
        do iele = 1, ele%numele
          inod_org = ele%ie(iele,k1)
          ele%ie(iele,k1) = inod_old2new(inod_org)
        end do
      end do
!
      do inum = 1, nod_grp%num_item
        inod_org = nod_grp%item_grp(inum)
        nod_grp%item_grp(inum) = inod_old2new(inod_org)
      end do
!
      do inum = 1, nod_comm%ntot_import
        inod_org = nod_comm%item_import(inum)
        nod_comm%item_import(inum) = inod_old2new(inod_org)
      end do
!
      do inum = 1, nod_comm%ntot_export
        inod_org = nod_comm%item_export(inum)
        nod_comm%item_export(inum) = inod_old2new(inod_org)
      end do
!
      do inod = 1, node%numnod
        id_global_org(inod) = node%inod_global(inod)
        node%xx(inod,1) = node%rr(inod)
        node%xx(inod,2) = node%theta(inod)
        node%xx(inod,3) = node%phi(inod)
      end do
!
      do inod_org = 1, node%numnod
        inod = inod_old2new(inod_org)
!
        node%inod_global(inod) = id_global_org(inod_org)
        node%rr(inod) =          node%xx(inod_org,1)
        node%theta(inod) =       node%xx(inod_org,2)
        node%phi(inod) =         node%xx(inod_org,3)
      end do
!
      deallocate(inod_old2new, id_global_org)
!
      end subroutine s_ordering_sph_mesh_for_rtp
!
! -----------------------------------------------------------------------
!
      end module ordering_sph_mesh_to_rtp
