!
!     module copy_mesh_from_type
!
!      written by H. Matsui on June, 2007
!
!>@file   copy_mesh_from_type.f90
!!@brief  module copy_mesh_from_type
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2010
!
!>@brief Copy FEM mesh data from structure to 1st mesh module
!!
!!@verbatim
!!      subroutine set_mesh_from_type(mesh, group)
!!      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!!      subroutine set_geometry_data_from_type(mesh)
!!      subroutine compare_geometry_type_vs_1st(my_rank, mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) :: group
!!@endverbatim
!
      module copy_mesh_from_type
!
      use m_precision
!
      implicit  none
!
!
      private :: set_geometry_data_from_type
      private :: copy_node_comm_tbl_from_type
      private :: copy_node_geometry_from_type
      private :: copy_element_connect_from_type
      private :: compare_node_type_vs_1st
      private :: compare_element_type_vs_1st
      private :: compare_node_comm_type_vs_1st
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_mesh_from_type(mesh, group)
!
      use t_mesh_data
      use copy_group_data_from_type
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call set_geometry_data_from_type(mesh)
      call group_data_from_type(group)
!
      end subroutine set_mesh_from_type
!
!  ---------------------------------------------------------------------
!
      subroutine compare_mesh_type_vs_1st(my_rank, mesh, group)
!
      use t_mesh_data
      use copy_group_data_from_type
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
!
!
      call compare_geometry_type_vs_1st(my_rank, mesh)
      call compare_group_type_vs_1st(my_rank, group)
!
      end subroutine compare_mesh_type_vs_1st
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_geometry_data_from_type(mesh)
!
      use m_geometry_data
      use t_mesh_data
!
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call copy_node_comm_tbl_from_type(mesh%nod_comm)
      call copy_node_geometry_from_type(mesh%node)
      call copy_element_connect_from_type(mesh%ele)
!
      call allocate_element_geometry
!
      end subroutine set_geometry_data_from_type
!
!-----------------------------------------------------------------------
!
      subroutine compare_geometry_type_vs_1st(my_rank, mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(mesh_geometry),    intent(inout) :: mesh
!
!
      call compare_node_type_vs_1st(my_rank, mesh%node)
      call compare_element_type_vs_1st(my_rank, mesh%ele)
      call compare_node_comm_type_vs_1st(my_rank, mesh%nod_comm)
!
      end subroutine compare_geometry_type_vs_1st
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_from_type(nod_comm)
!
      use m_nod_comm_table
      use copy_communication_table
      use t_comm_table
!
      type(communication_table), intent(inout) :: nod_comm
!
!
      num_neib = nod_comm%num_neib
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    nod_comm%id_neib, nod_comm%istack_import,                     &
     &    nod_comm%istack_export)
      call copy_num_import_export(num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item                                      &
     &   (ntot_import, ntot_export, item_import, item_export,           &
     &    nod_comm%item_import, nod_comm%item_export)
!
      call deallocate_type_comm_tbl(nod_comm)
!
      end subroutine copy_node_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_geometry_from_type(node)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      type(node_data), intent(inout) :: node
!
      integer(kind = kint) :: inod
!
!
      numnod =        node%numnod
      internal_node = node%internal_node
!
      call allocate_node_geometry
!
!$omp parallel do
      do inod = 1, numnod
        inod_global(inod) = node%inod_global(inod)
        xx(inod,1) = node%xx(inod,1)
        xx(inod,2) = node%xx(inod,2)
        xx(inod,3) = node%xx(inod,3)
      end do
!$omp end parallel do
!
      call deallocate_node_geometry_type(node)
!
      end subroutine copy_node_geometry_from_type
!
!------------------------------------------------------------------
!
      subroutine copy_element_connect_from_type(ele)
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      type(element_data), intent(inout) :: ele
!
!
      integer(kind = kint) :: iele, k1
!
!
      numele =            ele%numele
      first_ele_type = ele%first_ele_type
!
      if (first_ele_type .eq. 332) then
        nnod_4_ele =  num_t_quad
        nnod_4_surf = num_quad_sf
        nnod_4_edge = num_quad_edge
      else if (first_ele_type .eq. 331) then
        nnod_4_ele =  num_t_linear
        nnod_4_surf = num_linear_sf
        nnod_4_edge = num_linear_edge
      else if (first_ele_type .eq. 333) then
        nnod_4_ele =  num_t_lag
        nnod_4_surf = num_lag_sf
        nnod_4_edge = num_quad_edge
      end if
!
      call allocate_element_connection
!
!$omp parallel private(k1)
      do k1 = 1, nnod_4_ele
!$omp do private(iele)
        do iele = 1, numele
          ie(iele,k1) = ele%ie(iele,k1)
        end do
!$omp end do nowait
      end do
!
!$omp do
      do iele = 1, numele
        iele_global(iele) = ele%iele_global(iele)
        elmtyp(iele) =      ele%elmtyp(iele)
        nodelm(iele) =      ele%nodelm(iele)
      end do
!$omp end do
!$omp end parallel
!
      call deallocate_ele_connect_type(ele)
!
      end subroutine copy_element_connect_from_type
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine compare_node_type_vs_1st(my_rank, node)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(node_data), intent(in) :: node
!
      integer(kind = kint) :: i
      real(kind = kreal) :: err
!
!
      if(node%numnod .ne. numnod) write(*,*) 'numnod',                  &
     &      my_rank, node%numnod, numnod
      if(node%internal_node .ne. internal_node) write(*,*)              &
     &      'numnod', my_rank, node%internal_node, internal_node
      do i = 1, numnod
        err = sqrt((node%xx(i,1) - xx(i,1))**2                          &
     &           + (node%xx(i,2) - xx(i,2))**2                          &
     &           + (node%xx(i,3) - xx(i,3))**2)
        if(node%inod_global(i) .ne. inod_global(i))                     &
     &       write(*,*) 'inod_global(i)', my_rank, i,                   &
     &       node%inod_global(i), inod_global(i)
        if(err .gt. 1d-11) write(*,*) 'xx', my_rank, err, i,            &
     &       node%xx(i,1:3), xx(i,1:3)
      end do
!
      end subroutine compare_node_type_vs_1st
!
!------------------------------------------------------------------
!
      subroutine compare_element_type_vs_1st(my_rank, ele)
!
      use m_geometry_parameter
      use m_geometry_data
      use t_geometry_data
!
      integer(kind = kint), intent(in)  :: my_rank
      type(element_data), intent(inout) :: ele
!
!
      integer(kind = kint) :: i, k1, iflag
!
!
      if(ele%numele .ne. numele) write(*,*) 'numele',                   &
     &      my_rank, ele%numele, numele
      if(ele%first_ele_type .ne. first_ele_type) write(*,*)             &
     &       'first_ele_type', my_rank, ele%first_ele_type,             &
     &       first_ele_type
      if(ele%nnod_4_ele .ne. nnod_4_ele) write(*,*) 'nnod_4_ele',       &
     &      my_rank, ele%nnod_4_ele, nnod_4_ele
!
      do i = 1, numele
        iflag = 0
        do k1 = 1, nnod_4_ele
          if(ele%ie(i,k1) .ne. ie(i,k1)) iflag = 1
        end do
        if(ele%iele_global(i) .ne. iele_global(i))                      &
     &       write(*,*) 'iele_global(i)', my_rank, i,                   &
     &       ele%iele_global(i), iele_global(i)
        if(ele%elmtyp(i) .ne. elmtyp(i)) write(*,*) 'elmtyp(i)',        &
     &       my_rank, i, ele%elmtyp(i), elmtyp(i)
        if(ele%nodelm(i) .ne. nodelm(i)) write(*,*) 'nodelm(i)',        &
     &       my_rank, i, ele%nodelm(i), nodelm(i)
        if(iflag .gt. 0) then
          do k1 = 1, nnod_4_ele
            if(ele%nodelm(i) .ne. nodelm(i)) write(*,*) 'ie(i,k)',      &
     &         my_rank, i, k1, ele%ie(i,k1), ie(i,k1)
          end do
        end if
      end do
!
      end subroutine compare_element_type_vs_1st
!
!------------------------------------------------------------------
!
      subroutine compare_node_comm_type_vs_1st(my_rank, nod_comm)
!
      use m_nod_comm_table
      use copy_communication_table
      use t_comm_table
!
      integer(kind = kint), intent(in)  :: my_rank
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint) :: i
!
!
      if(nod_comm%num_neib .ne. num_neib) write(*,*) 'num_neib',        &
     &      my_rank, nod_comm%num_neib, num_neib
      if(nod_comm%ntot_export .ne. ntot_export)                         &
     &      write(*,*) 'ntot_export',                                   &
     &      my_rank, nod_comm%ntot_export, ntot_export
      if(nod_comm%ntot_import .ne. ntot_import)                         &
     &      write(*,*) 'ntot_import',                                   &
     &      my_rank, nod_comm%ntot_import, ntot_import
      do i = 1, num_neib
        if(nod_comm%id_neib(i) .ne. id_neib(i))                         &
     &       write(*,*) 'id_neib(i)', my_rank, i,                       &
     &       nod_comm%id_neib(i), id_neib(i)
        if(nod_comm%istack_import(i) .ne. istack_import(i))             &
     &       write(*,*) 'istack_import(i)', my_rank, i,                 &
     &       nod_comm%istack_import(i), istack_import(i)
        if(nod_comm%istack_export(i) .ne. istack_export(i))             &
     &       write(*,*) 'istack_export(i)', my_rank, i,                 &
     &       nod_comm%istack_export(i), istack_export(i)
      end do
      do i = 1, ntot_export
        if(nod_comm%item_export(i) .ne. item_export(i))                 &
     &       write(*,*) 'item_export(i)', my_rank, i,                   &
     &       nod_comm%item_export(i), item_export(i)
      end do
      do i = 1, ntot_import
        if(nod_comm%item_import(i) .ne. item_import(i))                 &
     &       write(*,*) 'item_import(i)', my_rank, i,                   &
     &       nod_comm%item_import(i), item_import(i)
      end do
!
      end subroutine compare_node_comm_type_vs_1st
!
!-----------------------------------------------------------------------
!
      end module copy_mesh_from_type
