!>@file   find_belonged_process.f90
!!@brief  module find_belonged_process
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2021
!
!>@brief Find belonged pe for each node, element, surface, and edge
!!
!!@verbatim
!!      integer(kind = kint) function                                   &
!!     &                    set_each_interior_flag(my_rank, ip_ref)
!!        integer, intent(in) :: my_rank
!!        integer(kind = kint), intent(in) :: ip_ref
!!
!!      subroutine find_belonged_pe_each_ele(numnod, ip_node,           &
!!     &          ie_one, ip_ref, k_ref)
!!         integer(kind = kint), intent(in) :: numnod
!!         integer(kind = kint), intent(in) :: ip_node(numnod)
!!         integer(kind = kint), intent(in) :: ie_one
!!         integer(kind = kint), intent(inout) :: ip_ref
!!         integer(kind = kint), intent(inout) :: k_ref
!!       subroutine find_belonged_pe_each_surf(numnod, ip_node,         &
!!      &          ie_surf_one, nnod_same, ip_ref, k_ref)
!!         integer(kind = kint), intent(in) :: numnod
!!         integer(kind = kint), intent(in) :: ip_node(numnod)
!!         integer(kind = kint), intent(in) :: ie_surf_one(num_linear_sf)
!!         integer(kind = kint), intent(inout) :: nnod_same
!!         integer(kind = kint), intent(inout) :: ip_ref
!!         integer(kind = kint), intent(inout) :: k_ref
!!       subroutine find_belonged_pe_each_edge(numnod, ip_node,         &
!!      &          ie_edge_one, nnod_same, ip_ref, k_ref)
!!         integer(kind = kint), intent(in)                             &
!!     &              :: ie_edge_one(num_linear_edge)
!!         integer(kind = kint), intent(in) :: numnod
!!         integer(kind = kint), intent(in) :: ip_node(numnod)
!!         integer(kind = kint), intent(inout) :: nnod_same
!!         integer(kind = kint), intent(inout) :: ip_ref
!!         integer(kind = kint), intent(inout) :: k_ref
!!@endverbatim
      module find_belonged_process
!
      use m_geometry_constants
      use t_geometry_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    set_each_interior_flag(my_rank, ip_ref)
!
      integer, intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ip_ref
!
      set_each_interior_flag = 0
      if(ip_ref .eq. my_rank) set_each_interior_flag = 1
!
      end function set_each_interior_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_node                                &
     &         (my_rank, node, nod_comm, ip_node)
!
      use t_comm_table
!
      integer, intent(in) :: my_rank
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(inout) :: ip_node(node%numnod)
!
      integer(kind = kint) :: inod, ist, ied, i, ip
!
!
!$omp parallel do
      do inod = 1, node%internal_node
        ip_node(inod) = my_rank
      end do
!$omp end parallel do
!
      do ip = 1, nod_comm%num_neib
        ist = nod_comm%istack_import(ip-1) + 1
        ied = nod_comm%istack_import(ip)
!$omp parallel do private(i,inod)
        do i = ist, ied
          inod = nod_comm%item_import(i)
          ip_node(inod) = nod_comm%id_neib(ip)
        end do
!$omp end parallel do
      end do
!
      end subroutine find_belonged_pe_4_node
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_each_ele(numnod, ip_node,             &
     &          ie_one, ip_ref, k_ref)
!
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint), intent(in) :: ie_one
!
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
!
       ip_ref = ip_node(ie_one)
       k_ref = 1
 !
       end subroutine find_belonged_pe_each_ele
!
! ----------------------------------------------------------------------
!
       subroutine find_belonged_pe_each_surf(numnod, ip_node,           &
      &          ie_surf_one, nnod_same, ip_ref, k_ref)
!
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
       integer(kind = kint), intent(in) :: ie_surf_one(num_linear_sf)
!
       integer(kind = kint), intent(inout) :: nnod_same
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
       integer(kind = kint) :: ip1, ip2, ip3, ip4
!
!
       ip1 = ip_node(ie_surf_one(1))
       ip2 = ip_node(ie_surf_one(2))
       ip3 = ip_node(ie_surf_one(3))
       ip4 = ip_node(ie_surf_one(4))
!
       if(ip1.eq.ip2 .and. ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 4
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip3 .and. ip2.eq.ip4) then
         nnod_same = 3
         ip_ref = ip2
         k_ref = 2
       else if(ip1.eq.ip3 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip2 .and. ip1.eq.ip4) then
         nnod_same = 3
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip2 .and. ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
!
       else if(ip1.eq.ip2 .and. ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = min(ip1, ip3)
         k_ref = 1
         if(ip_ref .eq. ip4) k_ref = 3
       else if(ip2.eq.ip3 .and. ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = min(ip1, ip2)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
       else if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip3) then
         nnod_same = 2
         ip_ref = ip2
         k_ref = 2
       else if(ip3.eq.ip4) then
         nnod_same = 2
         ip_ref = ip3
         k_ref = 3
       else if(ip4.eq.ip1) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip1.eq.ip3) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else if(ip2.eq.ip4) then
         nnod_same = 2
         ip_ref = ip2
         k_ref = 2
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         ip_ref = min(ip3, ip_ref)
         ip_ref = min(ip4, ip_ref)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
         if(ip_ref .eq. ip3) k_ref = 3
         if(ip_ref .eq. ip4) k_ref = 4
       end if
!
       end subroutine find_belonged_pe_each_surf
!
! ----------------------------------------------------------------------
!
       subroutine find_belonged_pe_each_edge(numnod, ip_node,           &
      &          ie_edge_one, nnod_same, ip_ref, k_ref)
!
       integer(kind = kint), intent(in) :: ie_edge_one(num_linear_edge)
       integer(kind = kint), intent(in) :: numnod
       integer(kind = kint), intent(in) :: ip_node(numnod)
!
       integer(kind = kint), intent(inout) :: nnod_same
       integer(kind = kint), intent(inout) :: ip_ref
       integer(kind = kint), intent(inout) :: k_ref
!
       integer(kind = kint) :: ip1, ip2
!
       ip1 = ip_node(ie_edge_one(1))
       ip2 = ip_node(ie_edge_one(2))
!
       if(ip1.eq.ip2) then
         nnod_same = 2
         ip_ref = ip1
         k_ref = 1
       else
         nnod_same = 1
         ip_ref = min(ip1, ip2)
         k_ref = 1
         if(ip_ref .eq. ip2) k_ref = 2
       end if
!
       end subroutine find_belonged_pe_each_edge
!
! ----------------------------------------------------------------------
!
      end module find_belonged_process