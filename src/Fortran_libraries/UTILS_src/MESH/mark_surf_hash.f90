!mark_surf_hash.f90
!      module mark_surf_hash
!
!      Written by H. Matsui
!
!!      subroutine mark_all_surfaces(numele, nnod_4_ele, ie,            &
!!     &          ntot_id, ntot_list, istack_surf_hash,                 &
!!     &          iend_surf_hash, isurf_hash, isurf_flag)
!!
!!      subroutine mark_independent_surface(numele, nnod_4_ele, ie,     &
!!     &          ntot_id, ntot_list, istack_surf_hash, iend_surf_hash, &
!!     &          isurf_hash, isurf_flag)
!!      subroutine mark_external_surface                                &
!!     &         (internal_node, numele, nnod_4_ele, ie,                &
!!     &          ntot_id, ntot_list, istack_surf_hash, iend_surf_hash, &
!!     &          isurf_hash, isurf_flag)
!
      module mark_surf_hash
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mark_all_surfaces(numele, nnod_4_ele, ie,              &
     &          ntot_id, ntot_list, istack_surf_hash,                   &
     &          iend_surf_hash, isurf_hash, isurf_flag)
!
      use compare_indices
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint_gl), intent(in) :: ntot_id, ntot_list
      integer(kind = kint_gl), intent(in) :: iend_surf_hash
      integer(kind = kint_gl), intent(in)                               &
     &                        :: istack_surf_hash(0:ntot_id)
      integer(kind = kint), intent(in) :: isurf_hash(ntot_list,2)
!
      integer(kind = kint_gl), intent(inout) :: isurf_flag(ntot_list)
!
      integer(kind = kint) :: inod(4), jnod(4)
      integer(kind = kint) :: iflag_inside
      integer(kind = kint_gl) :: ist, ied, k1, k2
      integer(kind = kint_gl) :: ihash
!
!
      isurf_flag = 0
!!$omp parallel do private(ihash,ist,ied,k1,k2,inod,jnod,iflag_inside)
      do ihash = 1, iend_surf_hash
        ist = istack_surf_hash(ihash-1)+1
        ied = istack_surf_hash(ihash)
!
        if (ied .eq. ist) then
          isurf_flag(ist) = ist
        else if (ied .gt. ist) then
!
          do k1 = ist, ied
            if(isurf_flag(k1) .ne. 0) cycle
            isurf_flag(k1) = k1
!
            call set_4nodes_id_on_surf                                  &
     &         (isurf_hash(k1,1), isurf_hash(k1,2),                     &
     &          numele, nnod_4_ele, ie, inod)
!
            if(     inod(1).eq.inod(2) .and. inod(1).eq.inod(3)         &
     &        .and. inod(1).eq.inod(4)) then
              do k2 = k1+1, ied
                call set_4nodes_id_on_surf                              &
     &             (isurf_hash(k2,1), isurf_hash(k2,2),                 &
     &              numele, nnod_4_ele, ie, jnod)
                if(     inod(1).eq.jnod(1) .and. inod(2).eq.jnod(2)     &
     &            .and. inod(3).eq.jnod(3) .and. inod(4).eq.jnod(4))    &
     &            isurf_flag(k2) = -k1
              end do
!
            else if((inod(1).eq.inod(2) .and. inod(3).eq.inod(4))       &
     &        .or.  (inod(2).eq.inod(3) .and. inod(4).eq.inod(1))) then
              do k2 = k1+1, ied
                call set_4nodes_id_on_surf                              &
     &             (isurf_hash(k2,1), isurf_hash(k2,2),                 &
     &              numele, nnod_4_ele, ie, jnod)
                iflag_inside = check_4_on_3(inod(1), inod(2), inod(3),  &
     &                        jnod(1), jnod(2), jnod(3), jnod(4))
                if (iflag_inside .eq. 1) isurf_flag(k2) = -k1
              end do
!
            else
              do k2 = k1+1, ied
                call set_4nodes_id_on_surf                              &
     &             (isurf_hash(k2,1), isurf_hash(k2,2),                 &
     &              numele, nnod_4_ele, ie, jnod)
!
                iflag_inside = check_4_on_3(inod(1), inod(2), inod(3),  &
     &                        jnod(1), jnod(2), jnod(3), jnod(4))
                if (iflag_inside .eq. 1) then
                  isurf_flag(k2) = -k1
                  exit
                end if
              end do
            end if
!
          end do
        end if
!
      end do
!!$omp end parallel do
!
      end subroutine mark_all_surfaces
!
!------------------------------------------------------------------
!
      subroutine mark_independent_surface(numele, nnod_4_ele, ie,       &
     &          ntot_id, ntot_list, istack_surf_hash, iend_surf_hash,   &
     &          isurf_hash, isurf_flag)
!
      use compare_indices
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint_gl), intent(in) :: ntot_id, ntot_list
      integer(kind = kint_gl), intent(in) :: iend_surf_hash
      integer(kind = kint_gl), intent(in)                               &
     &                        :: istack_surf_hash(0:ntot_id)
      integer(kind = kint), intent(in) :: isurf_hash(ntot_list,2)
!
      integer(kind = kint_gl), intent(inout) :: isurf_flag(ntot_list)
!
      integer(kind = kint) :: inod(4), jnod(4)
      integer(kind = kint) :: iflag_inside
      integer(kind = kint_gl) :: ist, ied, k1, k2
      integer(kind = kint_gl) :: ihash
!
!
      isurf_flag = 0
!!$omp parallel do private(ihash,ist,ied,k1,k2,inod,jnod,iflag_inside)
      do ihash = 1, iend_surf_hash
        ist = istack_surf_hash(ihash-1)+1
        ied = istack_surf_hash(ihash)
!
        if (ied .gt. ist) then
          do k1 = ist, ied
            if( isurf_flag(k1) .ne. 0 ) cycle
!
            call set_4nodes_id_on_surf                                  &
     &         (isurf_hash(k1,1), isurf_hash(k1,2),                     &
     &          numele, nnod_4_ele, ie, inod)
!
            if(     inod(1).eq.inod(2) .and. inod(1).eq.inod(3)         &
     &        .and. inod(1).eq.inod(4)) then
              isurf_flag(k1) =  1
              do k2 = k1+1, ied
                call set_4nodes_id_on_surf                              &
     &             (isurf_hash(k2,1), isurf_hash(k2,2),                 &
     &              numele, nnod_4_ele, ie, jnod)
                if(     inod(1).eq.jnod(1) .and. inod(2).eq.jnod(2)     &
     &            .and. inod(3).eq.jnod(3) .and. inod(4).eq.jnod(4))    &
     &            isurf_flag(k2) = 1
              end do
!
            else if((inod(1).eq.inod(2) .and. inod(3).eq.inod(4))       &
     &        .or.  (inod(2).eq.inod(3) .and. inod(4).eq.inod(1))) then
              isurf_flag(k1) =  1
              do k2 = k1+1, ied
                call set_4nodes_id_on_surf                              &
     &             (isurf_hash(k2,1), isurf_hash(k2,2),                 &
     &              numele, nnod_4_ele, ie, jnod)
                iflag_inside = check_4_on_3(inod(1), inod(2), inod(3),  &
     &                        jnod(1), jnod(2), jnod(3), jnod(4))
                if (iflag_inside .eq. 1) isurf_flag(k2) = iflag_inside
              end do
!
            else
              do k2 = k1+1, ied
                call set_4nodes_id_on_surf                              &
     &             (isurf_hash(k2,1), isurf_hash(k2,2),                 &
     &              numele, nnod_4_ele, ie, jnod)
!
                iflag_inside = check_4_on_3(inod(1), inod(2), inod(3),  &
     &                      jnod(1), jnod(2), jnod(3), jnod(4))
                if (iflag_inside .eq. 1) then
                  isurf_flag(k1) =  iflag_inside
                  isurf_flag(k2) =  iflag_inside
                  exit
                end if
              end do
!
            end if
          end do
        end if
!
      end do
!!$omp end parallel do
!
      end subroutine mark_independent_surface
!
!------------------------------------------------------------------
!
      subroutine mark_external_surface                                  &
     &         (internal_node, numele, nnod_4_ele, ie,                  &
     &          ntot_id, ntot_list, istack_surf_hash, iend_surf_hash,   &
     &          isurf_hash, isurf_flag)
!
      integer(kind = kint), intent(in) :: internal_node, numele
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint_gl), intent(in) :: ntot_id, ntot_list
      integer(kind = kint_gl), intent(in) :: iend_surf_hash
      integer(kind = kint_gl), intent(in)                               &
     &                        :: istack_surf_hash(0:ntot_id)
      integer(kind = kint), intent(in) :: isurf_hash(ntot_list,2)
!
      integer(kind = kint_gl), intent(inout) :: isurf_flag(ntot_list)
!
      integer(kind = kint) :: iele, is
      integer(kind = kint) :: inod(4)
      integer(kind = kint_gl) :: ihash
      integer(kind = kint_gl) :: ist, ied, k1
!
!
!!$omp parallel do private(ihash,ist,ied,k1,iele,is,inod)
      do ihash = 1, iend_surf_hash
        ist = istack_surf_hash(ihash-1)+1
        ied = istack_surf_hash(ihash)
!
        do k1 = ist, ied
          if(isurf_flag(k1) .ne. 0) cycle
!
          iele = isurf_hash(k1,1)
          is =   isurf_hash(k1,2)
!
          call set_4nodes_id_on_surf                                    &
     &         (iele, is, numele, nnod_4_ele, ie, inod)
!
          if ( ie(iele,1).gt.internal_node ) then
                isurf_flag(k1) = 2
          else if ( (inod(1) .le. internal_node)                        &
     &         .or. (inod(2) .le. internal_node)                        &
     &         .or. (inod(3) .le. internal_node)                        &
     &         .or. (inod(4) .le. internal_node) ) then
            isurf_flag(k1) = 3
          end if
        end do
!
      end do
!!$omp end parallel do
!
      end subroutine mark_external_surface
!
!------------------------------------------------------------------
!
      subroutine set_4nodes_id_on_surf                                  &
     &         (iele, is, numele, nnod_4_ele, ie, inod)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: iele, is
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: inod(4)
!
      integer(kind = kint) :: is1, is2, is3, is4
!
!
      is1 = node_on_sf_4(1,is)
      is2 = node_on_sf_4(2,is)
      is3 = node_on_sf_4(3,is)
      is4 = node_on_sf_4(4,is)
!
      inod(1) = ie(iele,is1)
      inod(2) = ie(iele,is2)
      inod(3) = ie(iele,is3)
      inod(4) = ie(iele,is4)
!
      end subroutine set_4nodes_id_on_surf
!
!------------------------------------------------------------------
!
      end module mark_surf_hash
