!set_node_for_sections.f90
!      module set_node_for_sections
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine count_node_at_node_psf(nod_search, psf_list)
!!      subroutine set_node_at_node_psf(numnod, nod_search, psf_list)
!!
!!      subroutine count_node_on_edge_4_psf(numedge, nnod_4_edge,       &
!!     &          ie_edge, edge_search, psf_list)
!!      subroutine set_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,&
!!     &          istack_nod_smp, edge_search, psf_list)
!!
!!      subroutine set_nod_on_nod_4_edge_psf(numedge, nnod_4_edge,      &
!!     &          ie_edge, istack_nod_smp, edge_search, psf_list)
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
      subroutine count_node_at_node_psf(nod_search, psf_list)
!
      use m_machine_parameter
!
      type(sect_search_list), intent(in) :: nod_search
!
      type(sectiong_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: nnod_on_nod_smp(np_smp)
      integer(kind = kint) :: ip, inum, inod, ist, ied
      real(kind= kreal), parameter :: zero = 0.0d0
!
!
      nnod_on_nod_smp(1:np_smp) = 0
!$omp parallel do private(ist,ied,inum,inod)
      do ip = 1, np_smp
        ist = nod_search%istack_search_smp(ip-1) + 1
        ied = nod_search%istack_search_smp(ip)
        do inum = ist, ied
          inod = nod_search%id_search(inum)
          if (psf_list%ref_fld(inod) .eq. zero)                         &
     &                nnod_on_nod_smp(ip) = nnod_on_nod_smp(ip) + 1
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        psf_list%istack_n_on_n_smp(ip)                                  &
     &        = psf_list%istack_n_on_n_smp(ip-1) + nnod_on_nod_smp(ip)
      end do
!
      end subroutine count_node_at_node_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_psf(numnod, nod_search, psf_list)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numnod
      type(sect_search_list), intent(in) :: nod_search
!
      type(sectiong_list), intent(inout) :: psf_list
!
!
      integer(kind = kint) :: ip, ist,ied,inum,inod,icou
!
!
      psf_list%id_n_on_n(1:numnod) = izero
!
!$omp parallel do private(ist,ied,inum,inod,icou)
      do ip = 1, np_smp
        icou = psf_list%istack_n_on_n_smp(ip-1)
        ist = nod_search%istack_search_smp(ip-1) + 1
        ied = nod_search%istack_search_smp(ip)
        do inum = ist, ied
          inod = nod_search%id_search(inum)
          if (psf_list%ref_fld(inod) .eq. zero) then
            icou = icou + 1
            psf_list%inod_4_nod(icou) = inod
            psf_list%id_n_on_n(inod) = icou
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
      subroutine count_node_on_edge_4_psf(numedge, nnod_4_edge,         &
     &          ie_edge, edge_search, psf_list)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectiong_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: nnod_on_edge_smp(np_smp)
      integer(kind = kint) :: ip, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum
      real(kind= kreal) :: c0
!
!
      nnod_on_edge_smp(1:np_smp) = 0
!$omp parallel do private(ist,ied,inum,iedge,inod1,inod2,c0)
      do ip = 1, np_smp
        ist = edge_search%istack_search_smp(ip-1) + 1
        ied = edge_search%istack_search_smp(ip)
        do inum = ist, ied
          iedge = edge_search%id_search(inum)
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          c0 = psf_list%ref_fld(inod1) * psf_list%ref_fld(inod2)
          if ( c0 .lt. zero) then
            nnod_on_edge_smp(ip) = nnod_on_edge_smp(ip) + 1
          end if
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        psf_list%istack_n_on_e_smp(ip)                                  &
     &     = psf_list%istack_n_on_e_smp(ip-1) + nnod_on_edge_smp(ip)
      end do
!
      end subroutine count_node_on_edge_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_on_edge_4_psf(numedge, nnod_4_edge, ie_edge,  &
     &          istack_nod_smp, edge_search, psf_list)
!
      use m_machine_parameter
      use m_constants
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectiong_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: ip, icou, ist, ied, iedge
      integer(kind = kint) :: inod1, inod2, inum, num
      real(kind= kreal) :: c0, diff
!
!
      psf_list%id_n_on_e(1:numedge) =    izero
!
!$omp parallel do private(ist,ied,inum,icou,iedge,inod1,inod2,c0,diff)
      do ip = 1, np_smp
        icou = psf_list%istack_n_on_e_smp(ip-1)
        ist = edge_search%istack_search_smp(ip-1) + 1
        ied = edge_search%istack_search_smp(ip)
        do inum = ist, ied
          iedge = edge_search%id_search(inum)
          inod1 = abs( ie_edge(iedge,1) )
          inod2 = abs( ie_edge(iedge,2) )
          c0 = psf_list%ref_fld(inod1) * psf_list%ref_fld(inod2)
          if ( c0 .lt. zero) then
            icou = icou + 1
            psf_list%iedge_4_nod(icou) = iedge
            diff = psf_list%ref_fld(inod2)-psf_list%ref_fld(inod1)
!
            psf_list%coef_on_edge(icou,1)                               &
     &                      =  psf_list%ref_fld(inod2) / diff
            psf_list%coef_on_edge(icou,2)                               &
     &                       = -psf_list%ref_fld(inod1) / diff
          end if
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(num,inum,icou,iedge)
      do ip = 1, np_smp
        num = psf_list%istack_n_on_e_smp(ip)                            &
     &       - psf_list%istack_n_on_e_smp(ip-1)
        do inum = 1, num
          icou = inum + psf_list%istack_n_on_e_smp(ip-1)
          iedge = psf_list%iedge_4_nod(icou)
          psf_list%id_n_on_e(iedge) = inum                              &
     &                      + psf_list%istack_n_on_n_smp(ip)            &
     &                      - psf_list%istack_n_on_n_smp(ip-1)          &
     &                      + istack_nod_smp(ip-1)
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
     &          ie_edge, istack_nod_smp, edge_search, psf_list)
!
      use m_constants
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numedge, nnod_4_edge
      integer(kind = kint), intent(in) :: ie_edge(numedge,nnod_4_edge)
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      type(sect_search_list), intent(in) :: edge_search
!
      type(sectiong_list), intent(inout) :: psf_list
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
!
          do jp = 1, np_smp
            if(psf_list%id_n_on_n(inod1)                                &
     &                          .gt. psf_list%istack_n_on_n_smp(jp-1)   &
     &      .and. psf_list%id_n_on_n(inod1)                             &
     &                          .le. psf_list%istack_n_on_n_smp(jp)     &
     &       )then
              psf_list%id_n_on_e(iedge) = psf_list%id_n_on_n(inod1)     &
     &                          - psf_list%istack_n_on_n_smp(jp-1)      &
     &                          + istack_nod_smp(jp-1)
!              write(40+my_rank,*) ip, jp, iedge,                       &
!     &           psf_list%id_n_on_e(iedge), psf_list%id_n_on_n(inod1), &
!     &           istack_nod_smp(jp-1)
            end if
          end do
!
        end do
!
        do inum = ist, ied
          iedge = edge_search%id_search(inum)
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
!
          do jp = 1, np_smp
            if (    psf_list%id_n_on_n(inod1).eq.izero                  &
     &        .and. psf_list%id_n_on_n(inod2)                           &
     &               .gt. psf_list%istack_n_on_n_smp(jp-1)              &
     &        .and. psf_list%id_n_on_n(inod2)                           &
     &               .le. psf_list%istack_n_on_n_smp(jp  )) then
              psf_list%id_n_on_e(iedge) = psf_list%id_n_on_n(inod2)     &
     &                          - psf_list%istack_n_on_n_smp(jp-1)      &
     &                          + istack_nod_smp(jp-1)
!              write(40+my_rank,*) ip, jp, iedge,                       &
!     &           psf_list%id_n_on_e(iedge), psf_list%id_n_on_n(inod2), &
!     &           istack_nod_smp(jp-1)
            end if
          end do
!
        end do
!
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
