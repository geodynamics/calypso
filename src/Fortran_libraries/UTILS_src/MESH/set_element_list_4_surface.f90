!set_element_list_4_surface.f90
!      module set_element_list_4_surface
!
!     written by H. Matsui
!
!      subroutine set_ele_list_4_surf(nele, nsurf, nsurf_4_ele,         &
!     &          isf_4_ele, iele_4_surf)
!
!      subroutine count_ele_list_4_edge(nele, nedge, nedge_4_ele,       &
!     &           iedge_4_ele, nt_ele4eg, num_iele_4_edge,              &
!     &           istack_iele_4_edge)
!      subroutine set_ele_list_4_edge(nele, nedge, nedge_4_ele,         &
!     &          iedge_4_ele, nt_ele4eg, num_iele_4_edge,               &
!     &          istack_iele_4_edge, iele_4_edge)
!
      module set_element_list_4_surface
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_ele_list_4_surf(nele, nsurf, nsurf_4_ele,          &
     &          isf_4_ele, iele_4_surf)
!
      integer(kind = kint), intent(in) :: nele, nsurf, nsurf_4_ele
      integer(kind = kint), intent(in) :: isf_4_ele(nele,nsurf_4_ele)
      integer(kind = kint), intent(inout) :: iele_4_surf(nsurf,2,2)
      
      integer(kind = kint) :: iele, isurf, k1, k2
!
!$omp parallel
      do k1 = 1, nsurf_4_ele
!$omp do private(iele,isurf,k2)
        do iele = 1, nele
          isurf = abs(isf_4_ele(iele,k1))
          k2 = 1 + (1 - sign(1,isf_4_ele(iele,k1))) / 2
          iele_4_surf(isurf,k2,1) = iele
          iele_4_surf(isurf,k2,2) = k1
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine set_ele_list_4_surf
!
! ------------------------------------------------------
!
      subroutine count_ele_list_4_edge(nele, nedge, nedge_4_ele,        &
     &           iedge_4_ele, nt_ele4eg, num_iele_4_edge,               &
     &           istack_iele_4_edge)
!
      integer(kind = kint), intent(in) :: nele, nedge, nedge_4_ele
      integer(kind = kint), intent(in) :: iedge_4_ele(nele,nedge_4_ele)
      integer(kind = kint), intent(inout) :: nt_ele4eg
      integer(kind = kint), intent(inout) :: istack_iele_4_edge(0:nedge)
      integer(kind = kint), intent(inout) :: num_iele_4_edge(nedge)
!
      integer(kind = kint) :: iele, iedge, k1
!
      num_iele_4_edge(1:nedge) = 0
!$omp parallel
      do k1 = 1, nedge_4_ele
!$omp do private(iele,iedge)
        do iele = 1, nele
          iedge = abs(iedge_4_ele(iele,k1))
          num_iele_4_edge(iedge) = num_iele_4_edge(iedge) + 1
        end do
!$omp end do
      end do
!$omp end parallel
!
      istack_iele_4_edge(0) = 0
      do iedge = 1, nedge
        istack_iele_4_edge(iedge) = istack_iele_4_edge(iedge-1)         &
     &                             + num_iele_4_edge(iedge)
      end do
      nt_ele4eg = istack_iele_4_edge(nedge)
!
      end subroutine count_ele_list_4_edge
!
! ------------------------------------------------------
!
      subroutine set_ele_list_4_edge(nele, nedge, nedge_4_ele,          &
     &          iedge_4_ele, nt_ele4eg, num_iele_4_edge,                &
     &          istack_iele_4_edge, iele_4_edge)
!
      integer(kind = kint), intent(in) :: nele, nedge, nedge_4_ele
      integer(kind = kint), intent(in) :: iedge_4_ele(nele,nedge_4_ele)
      integer(kind = kint), intent(in) :: nt_ele4eg
      integer(kind = kint), intent(in) :: istack_iele_4_edge(0:nedge)
      integer(kind = kint), intent(inout) :: num_iele_4_edge(nedge)
      integer(kind = kint), intent(inout) :: iele_4_edge(nt_ele4eg,2)
!
      integer(kind = kint) :: iele, iedge, icou, k1, k2
!
      num_iele_4_edge(1:nedge) = 0
!$omp parallel
      do k1 = 1, nedge_4_ele
!$omp do private(iele,iedge,icou,k2)
        do iele = 1, nele
          iedge = abs(iedge_4_ele(iele,k1))
          k2 = k1 * sign(1,iedge_4_ele(iele,k1))
!
          num_iele_4_edge(iedge) = num_iele_4_edge(iedge) + 1
          icou = istack_iele_4_edge(iedge-1) + num_iele_4_edge(iedge)
          iele_4_edge(icou,1) = iele
          iele_4_edge(icou,2) = k2
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine set_ele_list_4_edge
!
! ------------------------------------------------------
!
      end module set_element_list_4_surface
