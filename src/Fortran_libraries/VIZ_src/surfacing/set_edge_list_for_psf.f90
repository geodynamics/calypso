!set_edge_list_for_psf.f90
!      module set_edge_list_for_psf
!
!        programmed by H.Matsui on june, 2006
!
!      subroutine allocate_work_4_mark_edge_psf(numedge)
!      subroutine deallocate_work_4_mark_edge_psf
!
!      subroutine mark_edge_list_4_psf(numsurf, numedge, iedge_4_sf,    &
!     &          surf_search)
!
!      subroutine count_edge_list_4_psf(iedge_smp_stack, edge_search)
!      subroutine set_edge_list_4_psf(iedge_smp_stack, edge_search)
!
      module set_edge_list_for_psf
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), allocatable:: imark_edge(:)
      integer(kind = kint), allocatable:: imark_edge_smp(:,:)
!
      integer(kind = kint), allocatable:: nedge_search_smp(:)
!
      private :: imark_edge, imark_edge_smp
      private :: nedge_search_smp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_mark_edge_psf(numedge)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numedge
!
      allocate(imark_edge(numedge))
      allocate(imark_edge_smp(numedge,np_smp))
      allocate(nedge_search_smp(np_smp))
!
      imark_edge(1:numedge) = 0
      imark_edge_smp(1:numedge,1:np_smp) = 0
      nedge_search_smp(1:np_smp) = 0
!
      end subroutine allocate_work_4_mark_edge_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_mark_edge_psf
!
      deallocate(imark_edge, imark_edge_smp)
      deallocate(nedge_search_smp)
!
      end subroutine deallocate_work_4_mark_edge_psf
!
!  ---------------------------------------------------------------------
!
      subroutine mark_edge_list_4_psf(numsurf, numedge, iedge_4_sf,     &
     &          surf_search)
!
      use m_machine_parameter
      use m_geometry_constants
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: numsurf, numedge
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_4_sf(numsurf,nedge_4_surf)
      type(sect_search_list), intent(in) :: surf_search
!
      integer(kind = kint) :: ip, inum, isurf, ist, ied
      integer(kind = kint) :: iedge, iedge1, iedge2, iedge3, iedge4
!
!
!$omp parallel do
      do iedge = 1, numedge
        imark_edge(iedge) = 1
      end do
!$omp end parallel do
!$omp parallel do
      do ip = 1, np_smp
        imark_edge_smp(1:numedge,ip) = 1
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,isurf,iedge1,iedge,iedge3,iedge4,ist,ied)
      do ip = 1, np_smp
        ist = surf_search%istack_search_smp(ip-1) + 1
        ied = surf_search%istack_search_smp(ip)
        do inum = ist, ied
          isurf = surf_search%id_search(inum)
            iedge1 = abs( iedge_4_sf(isurf,1) )
            iedge2 = abs( iedge_4_sf(isurf,2) )
            iedge3 = abs( iedge_4_sf(isurf,3) )
            iedge4 = abs( iedge_4_sf(isurf,4) )
            imark_edge_smp(iedge1,ip) = 0
            imark_edge_smp(iedge2,ip) = 0
            imark_edge_smp(iedge3,ip) = 0
            imark_edge_smp(iedge4,ip) = 0
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
!$omp parallel do
        do iedge = 1, numedge
          imark_edge(iedge) = imark_edge(iedge)                         &
     &                       * imark_edge_smp(iedge,ip)
        end do
!$omp end parallel do
      end do
!
!$omp parallel do
      do iedge = 1, numedge
        imark_edge(iedge) = (1 - imark_edge(iedge))
      end do
!$omp end parallel do
!
      end subroutine mark_edge_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine count_edge_list_4_psf(iedge_smp_stack, edge_search)
!
      use m_machine_parameter
      use t_psf_geometry_list
!
      integer(kind=kint), intent(in) :: iedge_smp_stack(0:np_smp)
      type(sect_search_list), intent(inout) :: edge_search
!
      integer(kind=kint) :: ip, iedge, ist, ied
!
!
      nedge_search_smp(1:np_smp) = 0
!$omp parallel do private(iedge,ist,ied)
      do ip = 1, np_smp
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
          nedge_search_smp(ip)                                          &
     &            = nedge_search_smp(ip) + imark_edge(iedge)
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        edge_search%istack_search_smp(ip)                               &
     &     = edge_search%istack_search_smp(ip-1) + nedge_search_smp(ip)
      end do
      edge_search%num_search = edge_search%istack_search_smp(np_smp)
!
      end subroutine count_edge_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_edge_list_4_psf(iedge_smp_stack, edge_search)
!
      use m_machine_parameter
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: iedge_smp_stack(0:np_smp)
      type(sect_search_list), intent(inout) :: edge_search
!
      integer(kind = kint) :: ip, iedge, ist, ied, icou
!
!$omp parallel do private(iedge,ist,ied,icou)
      do ip = 1, np_smp
        icou = edge_search%istack_search_smp(ip-1)
        ist = iedge_smp_stack(ip-1) + 1
        ied = iedge_smp_stack(ip)
        do iedge = ist, ied
          if( imark_edge(iedge) .gt. 0) then
            icou = icou + 1
            edge_search%id_search(icou) = iedge
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_edge_list_4_psf
!
!  ---------------------------------------------------------------------
!
      end module set_edge_list_for_psf
