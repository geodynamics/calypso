!>@file   t_element_double_number.f90
!!       module t_element_double_number
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Data for merged UCD file output
!!
!!@verbatim
!!      subroutine alloc_ele_double_number(numnod, dbl_id)
!!      subroutine dealloc_ele_double_number(dbl_id)
!!        type(element_double_number), intent(inout) :: dbl_id
!!
!!      subroutine find_belonged_pe_4_ele                               &
!!     &         (inod_dbl, numele, ie, iele_dbl)
!!        integer, intent(in) :: my_rank
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: numele
!!        integer(kind = kint), intent(in) :: ie(numele,1)
!!        type(element_double_number), intent(inout) :: iele_dbl
!!      subroutine find_belonged_pe_4_surf(my_rank, inod_dbl,           &
!!     &          numsurf, nnod_4_surf, ie_surf,                        &
!!     &          internal_surf, interior_surf, isurf_dbl)
!!        integer, intent(in) :: my_rank
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
!!        integer(kind = kint), intent(in):: ie_surf(numsurf,nnod_4_surf)
!!        type(element_double_number), intent(inout) :: isurf_dbl
!!        integer(kind = kint), intent(inout) :: internal_surf
!!        integer(kind = kint), intent(inout) :: interior_surf(numsurf)
!!      subroutine find_belonged_pe_4_edge(my_rank, inod_dbl,           &
!!     &          numedge, nnod_4_edge, ie_edge,                        &
!!     &          internal_edge, interior_edge, iedge_dbl)
!!        integer, intent(in) :: my_rank
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        integer(kind = kint), intent(in) :: numedge, nnod_4_edge
!!        integer(kind = kint), intent(in):: ie_edge(numedge,nnod_4_edge)
!!        type(edge_data), intent(inout) :: edge
!!        type(element_double_number), intent(inout) :: iedge_dbl
!!        integer(kind = kint), intent(inout) :: internal_edge
!!        integer(kind = kint), intent(inout) :: interior_edge(numedge)
!!@endverbatim
!
      module t_element_double_number
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_para_double_numbering
!
      implicit none
!
!>      Structure of double numbering
      type element_double_number
!>        number of node for each subdomain
        integer(kind = kint) :: num_dbl
!>        Reference node ID of each element to find  home domain
        integer(kind = kint), allocatable :: k_ref(:)
!>        belonged subdomains ID for each node
        integer(kind = kint), allocatable :: irank(:)
      end type element_double_number
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ele_double_number(numnod, dbl_id)
!
      integer(kind = kint), intent(in) :: numnod
      type(element_double_number), intent(inout) :: dbl_id
!
!
      dbl_id%num_dbl = numnod
      allocate(dbl_id%k_ref(dbl_id%num_dbl))
      allocate(dbl_id%irank(dbl_id%num_dbl))
      if(dbl_id%num_dbl .gt. 0) then
        dbl_id%k_ref = 0
        dbl_id%irank =  0
      end if
!
      end subroutine alloc_ele_double_number
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ele_double_number(dbl_id)
!
      type(element_double_number), intent(inout) :: dbl_id
!
!
      deallocate(dbl_id%k_ref, dbl_id%irank)
!
      end subroutine dealloc_ele_double_number
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_ele                                 &
     &         (inod_dbl, numele, ie, iele_dbl)
!
      use find_belonged_process
!
      type(node_ele_double_number), intent(in) :: inod_dbl
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: ie(numele,1)
!
      type(element_double_number), intent(inout) :: iele_dbl
!
      integer(kind = kint) :: iele, ie_one
!
!
!$omp parallel workshare
      iele_dbl%irank(1:numele) = -1
      iele_dbl%k_ref(1:numele) = 0
!$omp end parallel workshare
!
!%omp parallel do private(iele,ie_one)
      do iele = 1, numele
        ie_one = ie(iele,1)
        call find_belonged_pe_each_ele                                  &
     &     (inod_dbl%num_dbl, inod_dbl%irank, ie_one,                   &
     &      iele_dbl%irank(iele), iele_dbl%k_ref(iele))
      end do
!%omp end parallel do
!
      end subroutine find_belonged_pe_4_ele
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_surf(my_rank, inod_dbl,             &
     &          numsurf, nnod_4_surf, ie_surf,                          &
     &          internal_surf, interior_surf, isurf_dbl)
!
      use t_surface_data
      use find_belonged_process
!
      integer, intent(in) :: my_rank
      type(node_ele_double_number), intent(in) :: inod_dbl
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
!
      type(element_double_number), intent(inout) :: isurf_dbl
      integer(kind = kint), intent(inout) :: internal_surf
      integer(kind = kint), intent(inout) :: interior_surf(numsurf)
!
      integer(kind = kint) :: isurf, nnod_same, icou
      integer(kind = kint) :: ie_surf_one(num_linear_sf)
!
!
!$omp parallel workshare
      isurf_dbl%irank(1:numsurf) = -1
      isurf_dbl%k_ref(1:numsurf) =    0
!$omp end parallel workshare
!
!%omp parallel do private(isurf,ie_surf_one,nnod_same)
      do isurf = 1, numsurf
        ie_surf_one(1:num_linear_sf) = ie_surf(isurf,1:num_linear_sf)
        call find_belonged_pe_each_surf                                 &
     &     (inod_dbl%num_dbl, inod_dbl%irank, ie_surf_one, nnod_same,   &
     &      isurf_dbl%irank(isurf), isurf_dbl%k_ref(isurf))
!
        interior_surf(isurf)                                            &
     &     = set_each_interior_flag(my_rank, isurf_dbl%irank(isurf))
      end do
!%omp end parallel do
!
      icou = 0
!%omp parallel do private(isurf) reduction(+:icou)
      do isurf = 1, numsurf
        if(isurf_dbl%irank(isurf) .eq. my_rank) icou = icou + 1
      end do
!%omp end parallel do
      internal_surf = icou
!
      end subroutine find_belonged_pe_4_surf
!
! ----------------------------------------------------------------------
!
      subroutine find_belonged_pe_4_edge(my_rank, inod_dbl,             &
     &          numedge, nnod_4_edge, ie_edge,                          &
     &          internal_edge, interior_edge, iedge_dbl)
!
      use t_edge_data
      use find_belonged_process
!
      integer, intent(in) :: my_rank
      type(node_ele_double_number), intent(in) :: inod_dbl
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
!
      type(element_double_number), intent(inout) :: iedge_dbl
      integer(kind = kint), intent(inout) :: internal_edge
      integer(kind = kint), intent(inout) :: interior_edge(numedge)
!
      integer(kind = kint) :: iedge, nnod_same, icou
      integer(kind = kint) :: ie_edge_one(num_linear_edge)
!
!$omp parallel workshare
      iedge_dbl%irank(1:numedge) = -1
      iedge_dbl%k_ref(1:numedge) =  0
!$omp end parallel workshare
!
!%omp parallel do private(iedge,ie_edge_one,nnod_same)
      do iedge = 1, numedge
        ie_edge_one(1) = ie_edge(iedge,1)
        ie_edge_one(2) = ie_edge(iedge,2)
        call find_belonged_pe_each_edge                                 &
     &     (inod_dbl%num_dbl, inod_dbl%irank, ie_edge_one, nnod_same,   &
     &      iedge_dbl%irank(iedge), iedge_dbl%k_ref(iedge))
!
        interior_edge(iedge)                                            &
     &     = set_each_interior_flag(my_rank, iedge_dbl%irank(iedge))
      end do
!%omp end parallel do
!
      icou = 0
!%omp parallel do private(iedge) reduction(+:icou)
      do iedge = 1, numedge
        if(iedge_dbl%irank(iedge) .eq. my_rank) icou = icou + 1
      end do
!%omp end parallel do
      internal_edge = icou
!
      end subroutine find_belonged_pe_4_edge
!
! ----------------------------------------------------------------------
!
      end module t_element_double_number
