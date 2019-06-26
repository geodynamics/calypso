!>@file   set_node_for_sections.f90
!!@brief  module set_node_for_sections
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Find nodes to make sectioning data
!!
!!@verbatim
!!      subroutine count_node_on_edge_4_psf                             &
!!     &         (numedge, nnod_4_edge, ie_edge, interior_edge,         &
!!     &          ie_edge, edge_search, psf_list)
!!      subroutine set_node_on_edge_4_psf(numedge, nnod_4_edge,         &
!!     &          ie_edge, interior_edge, edge_search, psf_list)
!!@endverbatim
!
      module set_node_for_sections
!
      use m_precision
      use t_psf_geometry_list
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_on_edge_4_psf                               &
     &         (numedge, nnod_4_edge, ie_edge, interior_edge,           &
     &          edge_search, psf_list)
!
      use m_constants
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: num_item(np_smp)
      integer(kind = kint) :: inter_item(np_smp), exter_item(np_smp)
      integer(kind = kint) :: ip, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum
      real(kind= kreal) :: c0
!
!
      num_item(1:np_smp) =   0
      inter_item(1:np_smp) = 0
      exter_item(1:np_smp) = 0
!$omp parallel do private(ist,ied,inum,iedge,inod1,inod2,c0)
      do ip = 1, np_smp
        ist = edge_search%istack_search_smp(ip-1) + 1
        ied = edge_search%istack_search_smp(ip)
        do inum = ist, ied
          iedge = edge_search%id_search(inum)
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          c0 = psf_list%ref_fld(inod1) * psf_list%ref_fld(inod2)
          if(c0 .le. zero) then
            num_item(ip) = num_item(ip) + 1
            if(interior_edge(iedge) .gt. 0) then
              inter_item(ip) = inter_item(ip) + 1
            else
              exter_item(ip) = exter_item(ip) + 1
            end if
          end if
        end do
      end do
!$omp end parallel do
!
      psf_list%istack_inter_n_on_e_smp(0) = 0
      psf_list%istack_exter_n_on_e_smp(0) = 0
      do ip = 1, np_smp
        psf_list%istack_inter_n_on_e_smp(ip)                            &
     &        = psf_list%istack_inter_n_on_e_smp(ip-1) + inter_item(ip)
        psf_list%istack_exter_n_on_e_smp(ip)                            &
     &        = psf_list%istack_exter_n_on_e_smp(ip-1) + exter_item(ip)
      end do
!
      psf_list%internod_on_edge                                         &
     &        = psf_list%istack_inter_n_on_e_smp(np_smp)
      psf_list%externod_on_edge                                         &
     &        = psf_list%istack_exter_n_on_e_smp(np_smp)
      psf_list%totalnod_on_edge                                         &
     &        = psf_list%internod_on_edge + psf_list%externod_on_edge
!
      end subroutine count_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_on_edge_4_psf(numedge, nnod_4_edge,           &
     &          ie_edge, interior_edge, edge_search, psf_list)
!
      use m_machine_parameter
      use m_constants
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: interior_edge(numedge)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: ip, icou, jcou, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum
      real(kind= kreal) :: c0
!
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,inum,icou,jcou,iedge,inod1,inod2,c0)
      do ip = 1, np_smp
        icou = psf_list%istack_inter_n_on_e_smp(ip-1)
        jcou = psf_list%istack_exter_n_on_e_smp(ip-1)
        ist = edge_search%istack_search_smp(ip-1) + 1
        ied = edge_search%istack_search_smp(ip)
        do inum = ist, ied
          iedge = edge_search%id_search(inum)
          inod1 = abs( ie_edge(iedge,1) )
          inod2 = abs( ie_edge(iedge,2) )
          c0 = psf_list%ref_fld(inod1) * psf_list%ref_fld(inod2)
          if(c0 .le. zero) then
            if(interior_edge(iedge) .gt. 0) then
              icou = icou + 1
              psf_list%iedge_int_nod(icou) = iedge
            else
              jcou = jcou + 1
              psf_list%iedge_ext_nod(jcou) = iedge
            end if
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      end module set_node_for_sections
