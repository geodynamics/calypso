!>@file   t_psf_geometry_list.f90
!!@brief  module t_psf_geometry_list
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Structure for cross sectioning data
!!
!!@verbatim
!!      subroutine alloc_num_psf_search_list(np_smp, list)
!!      subroutine alloc_psf_search_list(list)
!!      subroutine alloc_mark_ele_psf(search)
!!      subroutine dealloc_num_psf_search_list(list)
!!      subroutine dealloc_psf_search_list(list)
!!      subroutine dealloc_mark_ele_psf(search)
!!
!!      subroutine alloc_ref_field_4_psf(node, psf_list)
!!       type(node_data), intent(in) :: node
!!      subroutine alloc_nnod_psf(np_smp, edge, psf_list)
!!        type(edge_data), intent(in) :: edge
!!      subroutine alloc_inod_psf(psf_list)
!!      subroutine alloc_nnod_grp_psf(np_smp, node, psf_g_list)
!!       type(node_data), intent(in) :: node
!!      subroutine alloc_inod_grp_psf(psf_g_list)
!!      subroutine dealloc_ref_field_4_psf(psf_list)
!!      subroutine dealloc_nnod_psf(psf_list)
!!      subroutine dealloc_inod_psf(psf_list)
!!      subroutine dealloc_inod_grp_psf(psf_g_list)
!!@endverbatim
!
      module t_psf_geometry_list
!
      use m_precision
      use m_constants
!
      implicit none
!
!
!>      Structure for search list for surfacing
      type sect_search_list
!>        Number of node/edge/surface/element for searching
        integer(kind = kint) :: num_search
!>        SMP stack for searching
        integer(kind = kint), allocatable :: istack_search_smp(:)
!>        Local node/edge/surface/element ID for searching
        integer(kind = kint), allocatable :: id_search(:)
      end type sect_search_list
!
!>      Structure for search list for sectioning
      type psf_search_lists
!>        Structure for search list for element sectioning
        type(sect_search_list) :: elem_list
!>        Structure for search list for surface sectioning
        type(sect_search_list) :: surf_list
!>        Structure for search list for edge sectioning
        type(sect_search_list) :: edge_list
!>        Structure for search list for node sectioning
        type(sect_search_list) :: node_list
!
!>        marking for element generation
        integer(kind = kint), allocatable :: mark_e(:)
      end type psf_search_lists
!
!
!>      Structure for cross sectioning list
      type sectioning_list
!
!>        reference field for sectioning
        real(kind = kreal), allocatable :: ref_fld(:)
!
!>        Number of interior nodes for sections on edge
        integer(kind = kint) :: internod_on_edge
!>        Number of interior nodes for sections on edge
        integer(kind = kint) :: externod_on_edge
!>        SMP stack for sections on edge
        integer(kind = kint), allocatable :: istack_inter_n_on_e_smp(:)
!>        SMP stack for sections on edge
        integer(kind = kint), allocatable :: istack_exter_n_on_e_smp(:)
!
!>        ID for node on edge
        integer(kind = kint_gl), allocatable :: id_n_on_e(:)
!
!>        Edge ID for sections on internal node
        integer(kind = kint), allocatable :: iedge_int_nod(:)
!>        Interpolation coefficients for internal node on edge
        real(kind = kreal), allocatable :: coef_int_edge(:,:)
!
!>        Edge ID for sections on external node
        integer(kind = kint), allocatable :: iedge_ext_nod(:)
!>        Interpolation coefficients for external node on edge
        real(kind = kreal), allocatable :: coef_ext_edge(:,:)
      end type sectioning_list
!
!>      Structure for cross sectioning by surface group list
      type grp_section_list
!>        ID for node on node
        integer(kind = kint_gl), allocatable :: id_n_on_n(:)
!
!>        Number of interior nodes for sections on node
        integer(kind = kint) :: internod_on_nod
!>        Number of interior nodes for sections on node
        integer(kind = kint) :: externod_on_nod
!>        SMP stack for sections on node
        integer(kind = kint), allocatable :: istack_inter_n_on_n_smp(:)
!>        SMP stack for sections on edge
        integer(kind = kint), allocatable :: istack_exter_n_on_n_smp(:)
!
!>        Node ID for sections on intenal node
        integer(kind = kint), allocatable :: inod_int_nod(:)
!>        Node ID for sections on external node
        integer(kind = kint), allocatable :: inod_ext_nod(:)
      end type grp_section_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_psf_search_list(np_smp, list)
!
      integer(kind = kint), intent(in) :: np_smp
      type(sect_search_list), intent(inout) :: list
!
!
      allocate( list%istack_search_smp(0:np_smp) )
      list%istack_search_smp = 0
!
      end subroutine alloc_num_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_psf_search_list(list)
!
      type(sect_search_list), intent(inout) :: list
!
!
      allocate( list%id_search(list%num_search) )
      if(list%num_search .gt. 0) list%id_search = 0
!
      end subroutine alloc_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_mark_ele_psf(search)
!
      type(psf_search_lists), intent(inout) :: search
!
!
      allocate( search%mark_e(search%elem_list%num_search) )
      if(search%elem_list%num_search .gt. 0) search%mark_e = 0
!
      end subroutine alloc_mark_ele_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_psf_search_list(list)
!
      type(sect_search_list), intent(inout) :: list
!
!
      deallocate(list%istack_search_smp)
!
      end subroutine dealloc_num_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_psf_search_list(list)
!
      type(sect_search_list), intent(inout) :: list
!
!
      deallocate(list%id_search)
!
      end subroutine dealloc_psf_search_list
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mark_ele_psf(search)
!
      type(psf_search_lists), intent(inout) :: search
!
!
      deallocate(search%mark_e)
!
      end subroutine dealloc_mark_ele_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_ref_field_4_psf(node, psf_list)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(sectioning_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%ref_fld(node%numnod) )
      if(node%numnod .gt. 0) psf_list%ref_fld = 0.0d0
!
      end subroutine alloc_ref_field_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_nnod_psf(np_smp, edge, psf_list)
!
      use t_edge_data
!
      integer(kind= kint), intent(in) :: np_smp
      type(edge_data), intent(in) :: edge
      type(sectioning_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%istack_inter_n_on_e_smp(0:np_smp))
      allocate(psf_list%istack_exter_n_on_e_smp(0:np_smp))
      allocate(psf_list%id_n_on_e(edge%numedge))
!
      psf_list%istack_inter_n_on_e_smp = 0
      psf_list%istack_exter_n_on_e_smp = 0
      if(edge%numedge .gt. 0)  psf_list%id_n_on_e = 0
!
      end subroutine alloc_nnod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_inod_psf(psf_list)
!
      type(sectioning_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%iedge_int_nod(psf_list%internod_on_edge))
      allocate(psf_list%coef_int_edge(psf_list%internod_on_edge,2))
      allocate(psf_list%iedge_ext_nod(psf_list%externod_on_edge))
      allocate(psf_list%coef_ext_edge(psf_list%externod_on_edge,2))
!
      if(psf_list%internod_on_edge .gt. 0) psf_list%iedge_int_nod = 0
      if(psf_list%internod_on_edge .gt. 0) psf_list%coef_int_edge= zero
      if(psf_list%externod_on_edge .gt. 0) psf_list%iedge_ext_nod = 0
      if(psf_list%externod_on_edge .gt. 0) psf_list%coef_ext_edge= zero
!
      end subroutine alloc_inod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_nnod_grp_psf(np_smp, node, psf_g_list)
!
      use t_geometry_data
!
      integer(kind= kint), intent(in) :: np_smp
      type(node_data), intent(in) :: node
      type(grp_section_list), intent(inout) :: psf_g_list
!
!
      allocate(psf_g_list%istack_inter_n_on_n_smp(0:np_smp))
      allocate(psf_g_list%istack_exter_n_on_n_smp(0:np_smp))
      allocate(psf_g_list%id_n_on_n(node%numnod))
!
      psf_g_list%istack_inter_n_on_n_smp = 0
      psf_g_list%istack_exter_n_on_n_smp = 0
      if(node%numnod .gt. 0) psf_g_list%id_n_on_n = 0
!
      end subroutine alloc_nnod_grp_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_inod_grp_psf(psf_g_list)
!
      type(grp_section_list), intent(inout) :: psf_g_list
!
!
      allocate(psf_g_list%inod_int_nod(psf_g_list%internod_on_nod))
      allocate(psf_g_list%inod_ext_nod(psf_g_list%externod_on_nod))
!
      if(psf_g_list%internod_on_nod.gt.0) psf_g_list%inod_int_nod = 0
      if(psf_g_list%externod_on_nod.gt.0) psf_g_list%inod_ext_nod = 0
!
      end subroutine alloc_inod_grp_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ref_field_4_psf(psf_list)
!
      type(sectioning_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%ref_fld)
!
      end subroutine dealloc_ref_field_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_nnod_psf(psf_list)
!
      type(sectioning_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%istack_inter_n_on_e_smp)
      deallocate(psf_list%istack_exter_n_on_e_smp)
      deallocate(psf_list%id_n_on_e)
!
      end subroutine dealloc_nnod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_psf(psf_list)
!
      type(sectioning_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%iedge_int_nod, psf_list%coef_int_edge)
      deallocate(psf_list%iedge_ext_nod, psf_list%coef_ext_edge)
!
      end subroutine dealloc_inod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_grp_psf(psf_g_list)
!
      type(grp_section_list), intent(inout) :: psf_g_list
!
!
      deallocate(psf_g_list%istack_inter_n_on_n_smp)
      deallocate(psf_g_list%inod_int_nod, psf_g_list%inod_ext_nod)
      deallocate(psf_g_list%id_n_on_n)
      deallocate(psf_g_list%istack_exter_n_on_n_smp)
!
      end subroutine dealloc_inod_grp_psf
!
!  ---------------------------------------------------------------------
!
      end module t_psf_geometry_list
