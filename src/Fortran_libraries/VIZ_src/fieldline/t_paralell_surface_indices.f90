!>@file   t_paralell_surface_indices.f90
!!@brief  module t_paralell_surface_indices
!!
!!@author H. Matsui
!!@date Programmed in June, 2006
!
!>@brief Data structuresa for visualizers
!!
!!@verbatim
!!      subroutine init_para_surf_indices(mesh, ele_comm, surf_comm,    &
!!     &                                  iele_dbl, isurf_dbl,          &
!!     &                                  para_surf, m_SR)
!!      subroutine dealloc_para_surf_indices(para_surf)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(communication_table), intent(in) :: ele_comm
!!        type(communication_table), intent(in) :: surf_comm
!!        type(node_ele_double_number), intent(in) :: iele_dbl
!!        type(node_ele_double_number), intent(in) :: isurf_dbl
!!        type(paralell_surface_indices), intent(inout) :: para_surf
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module t_paralell_surface_indices
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_comm_table
      use t_mesh_SR
      use t_para_double_numbering
!
!>      Structure of data for visualization
      type paralell_surface_indices
        integer(kind = kint), allocatable :: isf_4_ele_dbl(:,:,:)
        integer(kind = kint), allocatable :: iele_4_surf_dbl(:,:,:)
      end type paralell_surface_indices
!
      private :: alloc_para_surf_indices
      private :: set_iele_4_surf_double_index
      private :: set_isf_4_ele_double_index
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_para_surf_indices(mesh, ele_comm, surf_comm,      &
     &                                  iele_dbl, isurf_dbl,            &
     &                                  para_surf, m_SR)
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: surf_comm
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(node_ele_double_number), intent(in) :: isurf_dbl
!
      type(paralell_surface_indices), intent(inout) :: para_surf
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call alloc_para_surf_indices(mesh%ele, mesh%surf, para_surf)
      call set_isf_4_ele_double_index(mesh%ele, mesh%surf,              &
     &    isurf_dbl, ele_comm, para_surf%isf_4_ele_dbl, m_SR)
      call set_iele_4_surf_double_index(mesh%surf, iele_dbl, surf_comm, &
     &    para_surf%iele_4_surf_dbl, m_SR)
!
      end subroutine init_para_surf_indices
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_para_surf_indices(para_surf)
      type(paralell_surface_indices), intent(inout) :: para_surf
!
      deallocate(para_surf%isf_4_ele_dbl, para_surf%iele_4_surf_dbl)
!
      end subroutine dealloc_para_surf_indices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_para_surf_indices(ele, surf, para_surf)
!
      use t_geometry_data
      use t_surface_data
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(paralell_surface_indices), intent(inout) :: para_surf
!
      allocate(para_surf%isf_4_ele_dbl(ele%numele,nsurf_4_ele,2))
      allocate(para_surf%iele_4_surf_dbl(surf%numsurf,2,3))
!
      if(ele%numele .gt. 0) then
!$omp parallel workshare
        para_surf%isf_4_ele_dbl = 0
!$omp end parallel workshare
      end if
      if(surf%numsurf .gt. 0) then
!$omp parallel workshare
        para_surf%iele_4_surf_dbl = 0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_para_surf_indices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_iele_4_surf_double_index                           &
     &         (surf, iele_dbl, surf_comm, iele_4_surf_dbl, m_SR)
!
      use m_geometry_constants
      use reverse_SR_int
      use solver_SR_type
!
      type(surface_data), intent(in) :: surf
      type(node_ele_double_number), intent(in) :: iele_dbl
      type(communication_table), intent(in) :: surf_comm
!
      integer(kind = kint), intent(inout)                               &
     &    :: iele_4_surf_dbl(surf%numsurf,2,3)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: isurf, iele, inum, k1, k2, ip, ist, ied
      integer(kind = kint), allocatable :: iflag_backside(:)
      integer(kind = kint), allocatable :: iflag_backside_check(:,:,:)
!
!
!$omp parallel do private(isurf,iele)
      do isurf = 1, surf%numsurf
        iele =             surf%iele_4_surf(isurf,1,1)
        iele_4_surf_dbl(isurf,1,1) = iele_dbl%irank(iele)
        iele_4_surf_dbl(isurf,1,2) = iele_dbl%index(iele)
        iele_4_surf_dbl(isurf,1,3) = surf%iele_4_surf(isurf,1,2)
        iele =             surf%iele_4_surf(isurf,2,1)
        iele_4_surf_dbl(isurf,2,1) = iele_dbl%irank(iele)
        iele_4_surf_dbl(isurf,2,2) = iele_dbl%index(iele)
        iele_4_surf_dbl(isurf,2,3) = surf%iele_4_surf(isurf,2,2)
      end do
!$omp end parallel do
!

      allocate(iflag_backside(surf_comm%ntot_import))
      allocate(iflag_backside_check(surf_comm%ntot_export,2,3))
!
      do k2 = 1, 3
        do k1 = 1, 2
          do inum = 1, surf_comm%ntot_import
            isurf = surf_comm%item_import(inum)
            iflag_backside(inum) = iele_4_surf_dbl(isurf,k1,k2)
          end do

          call comm_items_send_recv                                     &
     &     (surf_comm%num_neib, surf_comm%id_neib,                      &
     &      surf_comm%istack_import, iflag_backside(1),                 &
     &      surf_comm%num_neib, surf_comm%id_neib,                      &
     &      surf_comm%istack_export, izero,                             &
     &      iflag_backside_check(1,k1,k2), m_SR%SR_sig)
        end do
      end do

      do ip  = 1, surf_comm%num_neib
        ist = surf_comm%istack_export(ip-1) + 1
        ied = surf_comm%istack_export(ip)
        do inum = ist, ied
          isurf = surf_comm%item_export(inum)
          if(iele_4_surf_dbl(isurf,2,3) .eq. 0                          &
     &        .and. iflag_backside_check(inum,2,3) .gt. 0) then
            if(     iele_4_surf_dbl(isurf,1,1)                          &
     &                .eq. iflag_backside_check(inum,1,1)               &
     &        .and. iele_4_surf_dbl(isurf,1,2)                          &
     &                .eq. iflag_backside_check(inum,1,2)               &
     &        .and. iele_4_surf_dbl(isurf,1,3)                          &
     &                .eq. iflag_backside_check(inum,1,3)) then
              iele_4_surf_dbl(isurf,2,1:3)                              &
     &                 = iflag_backside_check(inum,2,1:3)
            else
              iele_4_surf_dbl(isurf,2,1:3)                              &
     &                 = iflag_backside_check(inum,1,1:3)
            end if
          end if
        end do
      end do
!
      deallocate(iflag_backside)
      deallocate(iflag_backside_check)
!
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,1,1))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,1,2))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,1,3))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,2,1))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,2,2))
      call SOLVER_SEND_RECV_int_type(surf%numsurf, surf_comm,           &
     &    m_SR%SR_sig, m_SR%SR_i, iele_4_surf_dbl(1,2,3))
!
      end subroutine set_iele_4_surf_double_index
!
!  ---------------------------------------------------------------------
!
      subroutine set_isf_4_ele_double_index                             &
     &         (ele, surf, isurf_dbl, ele_comm, isf_4_ele_dbl, m_SR)
!
      use  solver_SR_type
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(node_ele_double_number), intent(in) :: isurf_dbl
      type(communication_table), intent(in) :: ele_comm
!
      integer(kind = kint), intent(inout)                               &
     &    :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: isurf, iele, k1
!
!
      do k1 = 1, nsurf_4_ele
!$omp parallel do private(isurf,iele)
        do iele = 1, ele%numele
          isurf = abs(surf%isf_4_ele(iele,k1))
          
          isf_4_ele_dbl(iele,k1,1) = isurf_dbl%irank(isurf)
          isf_4_ele_dbl(iele,k1,2) = isurf_dbl%index(isurf)             &
    &                             * (surf%isf_4_ele(iele,k1) / isurf)

        end do
!$omp end parallel do
      end do
!
      do k1 = 1, nsurf_4_ele
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,            &
     &      m_SR%SR_sig, m_SR%SR_i, isf_4_ele_dbl(1,k1,1))
        call SOLVER_SEND_RECV_int_type(ele%numele, ele_comm,            &
     &      m_SR%SR_sig, m_SR%SR_i, isf_4_ele_dbl(1,k1,2))
      end do
!
      end subroutine set_isf_4_ele_double_index
!
!  ---------------------------------------------------------------------
!
      end module t_paralell_surface_indices
