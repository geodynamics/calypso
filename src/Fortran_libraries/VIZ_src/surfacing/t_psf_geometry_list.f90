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
!!      subroutine alloc_ref_field_4_psf(numnod, psf_list)
!!      subroutine alloc_nnod_psf(np_smp, numnod, numedge, psf_list)
!!      subroutine alloc_inod_psf(psf_list)
!!      subroutine dealloc_ref_field_4_psf(psf_list)
!!      subroutine dealloc_nnod_psf(psf_list)
!!      subroutine dealloc_inod_psf(psf_list)
!!@endverbatim
!
      module t_psf_geometry_list
!
      use m_precision
!
      implicit none
!
!
!>      Structure for search list for surfacing
      type sect_search_list
!>        Number of node/edge/surface/element for searching
        integer(kind = kint) :: num_search
!>        SMP stack for searching
        integer(kind = kint), pointer :: istack_search_smp(:)
!>        Local node/edge/surface/element ID for searching
        integer(kind = kint), pointer :: id_search(:)
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
        integer(kind = kint), pointer :: mark_e(:)
      end type psf_search_lists
!
!
!>      Structure for cross sectioning list
      type sectiong_list
!
!>        reference field for sectioning
        real(kind = kreal), pointer :: ref_fld(:)
!
!>        Number of nodes for sections on node
        integer(kind = kint) :: nnod_on_nod
!>        SMP stack for sections on node
        integer(kind = kint), pointer :: istack_n_on_n_smp(:)
!
!>        Node ID for sections on node
        integer(kind = kint), pointer :: inod_4_nod(:)
!>        ID for node on node
        integer(kind = kint), pointer :: id_n_on_n(:)
!
!
!>        Number of nodes for sections on edge
        integer(kind = kint) :: nnod_on_edge
!>        SMP stack for sections on edge
        integer(kind = kint), pointer :: istack_n_on_e_smp(:)
!
!>        Edge ID for sections on node
        integer(kind = kint), pointer :: iedge_4_nod(:)
!>        ID for node on edge
        integer(kind = kint), pointer :: id_n_on_e(:)
!
!>        Interpolation coefficients for node on edge
        real(kind = kreal), pointer :: coef_on_edge(:,:)
      end type sectiong_list
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
      subroutine alloc_ref_field_4_psf(numnod, psf_list)
!
      integer(kind= kint), intent(in) :: numnod
      type(sectiong_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%ref_fld(numnod) )
      if(numnod .gt. 0) psf_list%ref_fld = 0.0d0
!
      end subroutine alloc_ref_field_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_nnod_psf(np_smp, numnod, numedge, psf_list)
!
      integer(kind= kint), intent(in) :: np_smp, numnod, numedge
      type(sectiong_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%istack_n_on_n_smp(0:np_smp))
      allocate(psf_list%istack_n_on_e_smp(0:np_smp))
      allocate(psf_list%id_n_on_n(numnod))
      allocate(psf_list%id_n_on_e(numedge))
!
      psf_list%istack_n_on_n_smp = 0
      psf_list%istack_n_on_e_smp = 0
      if(numnod .gt. 0) then
        psf_list%id_n_on_n = 0
      end if
      if(numedge .gt. 0) then
        psf_list%id_n_on_e = 0
      end if
!
      end subroutine alloc_nnod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_inod_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      allocate(psf_list%inod_4_nod(psf_list%nnod_on_nod))
!
      allocate(psf_list%iedge_4_nod(psf_list%nnod_on_edge))
      allocate(psf_list%coef_on_edge(psf_list%nnod_on_edge,2))
!
      if(psf_list%nnod_on_nod .gt. 0) then
        psf_list%inod_4_nod = 0
      end if
!
      if(psf_list%nnod_on_edge .gt. 0) then
        psf_list%iedge_4_nod = 0
        psf_list%coef_on_edge = 0.0d0
      end if
!
      end subroutine alloc_inod_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ref_field_4_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
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
      type(sectiong_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%istack_n_on_n_smp)
      deallocate(psf_list%istack_n_on_e_smp)
      deallocate(psf_list%id_n_on_n)
      deallocate(psf_list%id_n_on_e)
!
      end subroutine dealloc_nnod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_psf(psf_list)
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      deallocate(psf_list%inod_4_nod)
      deallocate(psf_list%iedge_4_nod, psf_list%coef_on_edge)
!
      end subroutine dealloc_inod_psf
!
!  ---------------------------------------------------------------------
!
      end module t_psf_geometry_list
