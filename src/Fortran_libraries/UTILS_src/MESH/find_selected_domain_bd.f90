!find_selected_domain_bd
!      module find_selected_domain_bd
!
!      subroutine allocate_imark_4_surface(numsurf)
!
!!      subroutine mark_selected_domain_bd(numele, numsurf, isf_4_ele,  &
!!     &          iflag_used_ele, imark_sf)
!!      subroutine count_selected_domain_bd(numsurf, imark_sf, nsurf_bd)
!!      subroutine s_find_selected_domain_bd                            &
!!     &         (numele, numsurf, iele_4_surf, imark_sf,               &
!!     &          iflag_used_ele, nsurf_bd, isurf_bd_item)
!
      module find_selected_domain_bd
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine mark_selected_domain_bd(numele, numsurf, isf_4_ele,    &
     &          iflag_used_ele, imark_sf)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: isf_4_ele(numele, nsurf_4_ele)
      integer(kind = kint), intent(in) :: iflag_used_ele(numele)
!
      integer(kind = kint), intent(inout) :: imark_sf(numsurf)
!
      integer(kind = kint) :: iele, k1, isurf
!
!
!
!$omp parallel workshare
      imark_sf(1:numsurf) = 0
!$omp end parallel workshare
!
      do k1 = 1, nsurf_4_ele
        do iele = 1, numele
          if(iflag_used_ele(iele) .gt. 0) then
            isurf = abs(isf_4_ele(iele,k1))
            imark_sf(isurf) = imark_sf(isurf) + 1
          end if
        end do
      end do
!
      end subroutine mark_selected_domain_bd
!
!  ---------------------------------------------------------------------
!
      subroutine count_selected_domain_bd(numsurf, imark_sf, nsurf_bd)
!
      integer(kind = kint), intent(in) :: numsurf
      integer(kind = kint), intent(in) :: imark_sf(numsurf)
      integer(kind = kint), intent(inout) :: nsurf_bd
!
      integer(kind = kint) :: isurf
!
      nsurf_bd = 0
      do isurf = 1, numsurf
        if(imark_sf(isurf) .eq. 1) nsurf_bd = nsurf_bd + 1
      end do
!
      end subroutine count_selected_domain_bd
!
!  ---------------------------------------------------------------------
!
      subroutine s_find_selected_domain_bd                              &
     &         (numele, numsurf, iele_4_surf, imark_sf,                 &
     &          iflag_used_ele, nsurf_bd, isurf_bd_item)
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind = kint), intent(in) :: imark_sf(numsurf)
      integer(kind = kint), intent(in) :: iflag_used_ele(numele)
      integer(kind = kint), intent(in) :: nsurf_bd
      integer(kind = kint), intent(inout) :: isurf_bd_item(2,nsurf_bd)
!
      integer(kind = kint) :: icou, isurf, iele
!
!
      icou = 0
      do isurf = 1, numsurf
        if(imark_sf(isurf) .eq. 1) then
          icou = icou + 1
          iele = iele_4_surf(isurf,1,1)
          if(iflag_used_ele(iele) .gt. 0) then
            isurf_bd_item(1,icou) = iele
            isurf_bd_item(2,icou) = iele_4_surf(isurf,1,2)
          else
            isurf_bd_item(1,icou) = iele_4_surf(isurf,2,1)
            isurf_bd_item(2,icou) = iele_4_surf(isurf,2,2)
          end if
        end if
      end do
!
      end subroutine s_find_selected_domain_bd
!
!  ---------------------------------------------------------------------
!
      end module find_selected_domain_bd
