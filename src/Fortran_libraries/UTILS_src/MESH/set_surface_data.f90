!
!      module set_surface_data
!
!      Written by H. Matsui
!
!      subroutine count_all_surfaces(numele, numsurf)
!      subroutine set_all_surfaces(numele, numsurf, nnod_4_ele,         &
!     &          nnod_4_surf, ie, node_on_sf, ie_surf, isf_4_ele)
!
!      subroutine set_surf_rotation_flag(numele, numsurf, nnod_4_ele,   &
!     &          nnod_4_surf, ie, ie_surf, isf_4_ele, isf_rot_ele)
!
!      subroutine count_part_surface(nele_grp, numsurf_part)
!      subroutine set_part_surface(numele, nele_grp,                    &
!     &          numsurf_part, isf_4_ele, isf_part)
!
      module set_surface_data
!
      use m_precision
!
      use m_geometry_constants
      use m_surface_hash
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine count_all_surfaces(numele, numsurf)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(inout) :: numsurf
!
      integer(kind = kint) :: k1
!
!
      numsurf = 0
      do k1 = 1, nsurf_4_ele*numele
        if (isurf_flag(k1) .gt. 0) numsurf = numsurf + 1
      end do
!
      end subroutine count_all_surfaces
!
!------------------------------------------------------------------
!
      subroutine set_all_surfaces(numele, numsurf, nnod_4_ele,          &
     &          nnod_4_surf, ie, node_on_sf, ie_surf, isf_4_ele)
!
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &                  :: node_on_sf(nnod_4_surf,nsurf_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &      :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(inout)                               &
     &      :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: k1, k2
      integer(kind = kint) :: i, iele, is, isurf
      integer(kind = kint) :: j, jele, js
!
!
      isurf = 0
      do k1 = 1, nsurf_4_ele*numele
        if (isurf_flag(k1) .gt. 0) then
          isurf = isurf + 1
!
          iele = isurf_hash(k1,1)
          is =   isurf_hash(k1,2)
          isf_4_ele(iele,is) = isurf
          do i = 1, nnod_4_surf
            j = node_on_sf(i,is)
            ie_surf(isurf,i) = ie(iele,j)
          end do
!
        end if
      end do
!
!
!
      do k1 = 1, nsurf_4_ele*numele
!
        if (isurf_flag(k1) .lt. 0) then
!
          k2 = -isurf_flag(k1)
          iele = isurf_hash(k1,1)
          is =   isurf_hash(k1,2)
          jele = isurf_hash(k2,1)
          js =   isurf_hash(k2,2)
          isf_4_ele(iele,is) = -isf_4_ele(jele,js)
!
        end if
      end do
!
      end subroutine set_all_surfaces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_surf_rotation_flag(numele, numsurf, nnod_4_ele,    &
     &          nnod_4_surf, ie, ie_surf, isf_4_ele, isf_rot_ele)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &                      :: isf_rot_ele(numele,nsurf_4_ele)
!
      integer(kind = kint) :: k1, isurf, k, iele
!
!
!$omp parallel
      do k1 = 1, nsurf_4_ele
!$omp do private(iele,isurf,k)
        do iele = 1, numele
          isurf = abs( isf_4_ele(iele,k1) )
          if (isurf .eq. isf_4_ele(iele,k1)) then
            isf_rot_ele(iele,k1) = 0
          else
            k = node_on_sf_4(1,k1)
            if     (ie(iele,k) .eq. ie_surf(isurf,1) ) then
              isf_rot_ele(iele,k1) = 1
            else if(ie(iele,k) .eq. ie_surf(isurf,2) ) then
              isf_rot_ele(iele,k1) = 2
            else if(ie(iele,k) .eq. ie_surf(isurf,3) ) then
              isf_rot_ele(iele,k1) = 3
            else if(ie(iele,k) .eq. ie_surf(isurf,4) ) then
              isf_rot_ele(iele,k1) = 4
            end if
          end if
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine set_surf_rotation_flag
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_part_surface(nele_grp, numsurf_part)
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(inout) :: numsurf_part
!
      integer(kind = kint) :: k1
!
!
      do k1 = 1, nsurf_4_ele*nele_grp
        if (isurf_flag(k1) .eq. 0) numsurf_part = numsurf_part + 1
      end do
!
      end subroutine count_part_surface
!
!------------------------------------------------------------------
!
      subroutine set_part_surface(numele, nele_grp,                     &
     &          numsurf_part, isf_4_ele, isf_part)
!
      integer(kind = kint), intent(in) :: numele, nele_grp
      integer(kind = kint), intent(in) :: numsurf_part
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      integer(kind = kint), intent(inout) :: isf_part(numsurf_part)
!!
      integer(kind = kint) :: k1
      integer(kind = kint) :: iele, is, inum
!
!
      inum = 0
      do k1 = 1, nsurf_4_ele*nele_grp
        if (isurf_flag(k1).eq.0) then
          iele = isurf_hash(k1,1)
          is =   isurf_hash(k1,2)
          inum = inum + 1
          isf_part(inum) = isf_4_ele(iele,is)
        end if
      end do
!
      end subroutine set_part_surface
!
!------------------------------------------------------------------
!
      end module set_surface_data
