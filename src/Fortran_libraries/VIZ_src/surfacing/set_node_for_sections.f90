!>@file   set_node_for_sections.f90
!!@brief  module set_node_for_sections
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Find nodes to make sectioning data
!!
!!@verbatim
!!      subroutine count_node_at_node_psf                               &
!!     &         (internal_node, nod_search, psf_list)
!!      subroutine set_node_at_node_psf                                 &
!!     &         (numnod, internal_node, nod_search, psf_list)
!!
!!      subroutine count_node_on_edge_4_psf                             &
!!     &         (internal_node, numedge, nnod_4_edge,                  &
!!     &          ie_edge, edge_search, psf_list)
!!      subroutine set_node_on_edge_4_psf(internal_node, numedge,       &
!!     &          nnod_4_edge, ie_edge, edge_search, psf_list)
!!
!!      subroutine set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge,      &
!!     &          ie_edge, edge_search, psf_list)
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
      subroutine count_node_at_node_psf                                 &
     &         (internal_node, nod_search, psf_list)
!
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: internal_node
      type(sect_search_list), intent(in) :: nod_search
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: num_item(np_smp)
      integer(kind = kint) :: inter_item(np_smp), exter_item(np_smp)
      integer(kind = kint) :: ip, inum, inod, ist, ied
      real(kind= kreal), parameter :: zero = 0.0d0
!
!
      num_item(1:np_smp) =   0
      inter_item(1:np_smp) = 0
      exter_item(1:np_smp) = 0
!$omp parallel do private(ist,ied,inum,inod)
      do ip = 1, np_smp
        ist = nod_search%istack_search_smp(ip-1) + 1
        ied = nod_search%istack_search_smp(ip)
        do inum = ist, ied
          inod = nod_search%id_search(inum)
          if (psf_list%ref_fld(inod) .eq. zero) then
            num_item(ip) = num_item(ip) + 1
            if(inod .le. internal_node) then
              inter_item(ip) = inter_item(ip) + 1
            else
              exter_item(ip) = exter_item(ip) + 1
            end if
          end if
        end do
      end do
!$omp end parallel do
!
      psf_list%istack_inter_n_on_n_smp(0) = 0
      psf_list%istack_exter_n_on_n_smp(0) = 0
      do ip = 1, np_smp
        psf_list%istack_inter_n_on_n_smp(ip)                            &
     &        = psf_list%istack_inter_n_on_n_smp(ip-1) + inter_item(ip)
        psf_list%istack_exter_n_on_n_smp(ip)                            &
     &        = psf_list%istack_exter_n_on_n_smp(ip-1) + exter_item(ip)
      end do
!
      psf_list%internod_on_nod                                          &
     &        = psf_list%istack_inter_n_on_n_smp(np_smp)
      psf_list%externod_on_nod                                          &
     &        = psf_list%istack_exter_n_on_n_smp(np_smp)
!
      end subroutine count_node_at_node_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_psf                                   &
     &         (numnod, internal_node, nod_search, psf_list)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      type(sect_search_list), intent(in) :: nod_search
!
      type(sectioning_list), intent(inout) :: psf_list
!
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, icou, jcou, ij
!
!
!$omp workshare
      psf_list%id_n_on_n(1:numnod) = 0
!$omp end workshare
!
!$omp parallel do private(ist,ied,inum,inod,icou,jcou,ij)
      do ip = 1, np_smp
        icou = psf_list%istack_inter_n_on_n_smp(ip-1)
        jcou = psf_list%istack_exter_n_on_n_smp(ip-1)
        ist = nod_search%istack_search_smp(ip-1) + 1
        ied = nod_search%istack_search_smp(ip)
        do inum = ist, ied
          inod = nod_search%id_search(inum)
          if (psf_list%ref_fld(inod) .eq. zero) then
            if(inod .le. internal_node) then
              icou = icou + 1
              psf_list%inod_int_nod(icou) = inod
              psf_list%id_n_on_n(inod) =    icou
            else
              jcou = jcou + 1
              psf_list%inod_ext_nod(jcou) = inod
              psf_list%id_n_on_n(inod) =    jcou                        &
     &                                   + psf_list%internod_on_nod     &
     &                                   + psf_list%internod_on_edge
            end if
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_node_at_node_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_node_on_edge_4_psf                               &
     &         (internal_node, numedge, nnod_4_edge,                    &
     &          ie_edge, edge_search, psf_list)
!
      use m_constants
      use m_machine_parameter
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
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
          if (c0 .lt. zero) then
            num_item(ip) = num_item(ip) + 1
            if(inod1 .le. internal_node) then
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
!
      end subroutine count_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_on_edge_4_psf(internal_node, numedge,         &
     &          nnod_4_edge, ie_edge, edge_search, psf_list)
!
      use m_machine_parameter
      use m_constants
!
      integer(kind = kint), intent(in) :: internal_node
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: ip, icou, jcou, ij, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum
      real(kind= kreal) :: c0, diff
!
!
      psf_list%id_n_on_e(1:numedge) =    izero
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,inum,icou,jcou,ij,iedge,inod1,inod2,c0,diff)
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
          if ( c0 .lt. zero) then
            if(inod1 .le. internal_node) then
              icou = icou + 1
              diff = psf_list%ref_fld(inod2)-psf_list%ref_fld(inod1)
              psf_list%iedge_int_nod(icou) = iedge
!
              psf_list%id_n_on_e(iedge) = icou                          &
     &                                   + psf_list%internod_on_nod
            else
              jcou = jcou + 1
              diff = psf_list%ref_fld(inod2)-psf_list%ref_fld(inod1)
              psf_list%iedge_ext_nod(jcou) = iedge
!
              ij = jcou + psf_list%internod_on_edge
!
              psf_list%id_n_on_e(iedge) = icou                          &
     &                                   + psf_list%internod_on_nod     &
     &                                   + psf_list%externod_on_nod     &
     &                                   + psf_list%internod_on_edge
            end if
          end if
        end do
      end do
!$omp end parallel do
!
!       write(40+my_rank,*) 'inum_e, psf_list%id_n_on_e(inum)'
!      do inum = 1, numedge
!         if ( psf_list%id_n_on_e(inum).ne.0 )                          &
!     &     write(40+my_rank,*) inum, psf_list%id_n_on_e(inum)
!      end do
!
      end subroutine set_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge,        &
     &          ie_edge, edge_search, psf_list)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectioning_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: ip, ist, ied, iedge, jp
      integer(kind = kint) :: inod1, inod2, inum
!
!
!$omp parallel do private(ist,ied,inum,iedge,inod1,inod2,jp)
      do ip = 1, np_smp
        ist = edge_search%istack_search_smp(ip-1) + 1
        ied = edge_search%istack_search_smp(ip)
        do inum = ist, ied
          iedge = edge_search%id_search(inum)
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          if(psf_list%id_n_on_n(inod1) .gt. 0) then
            psf_list%id_n_on_e(iedge) = psf_list%id_n_on_n(inod1)
          else if(psf_list%id_n_on_n(inod2) .gt. 0) then
            psf_list%id_n_on_e(iedge) = psf_list%id_n_on_n(inod2)
          end if
        end do
      end do
!$omp end parallel do
!
!       write(40+my_rank,*) 'inum_n, psf_list%id_n_on_e(inum)'
!      do inum = 1, numedge
!         if ( psf_list%id_n_on_e(inum).ne.0 ) write(40+my_rank,*)      &
!     &                                inum, psf_list%id_n_on_e(inum)
!      end do
!
      end subroutine set_nod_on_nod_4_edge_psf
!
!  ---------------------------------------------------------------------
!
      end module set_node_for_sections
