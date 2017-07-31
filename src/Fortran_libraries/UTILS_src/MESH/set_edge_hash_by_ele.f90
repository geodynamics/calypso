!>@file   set_edge_hash_by_ele.f90
!!@brief  module set_edge_hash_by_ele
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Hash table using sum of local node ID
!!
!!@verbatim
!!      subroutine const_edge_hash_4_ele                                &
!!     &         (numnod, numele, nnod_4_ele, nnod_4_edge, ie,          &
!!     &          inum_edge_hash, istack_edge_hash,                     &
!!     &          iend_edge_hash, iedge_hash, iedge_flag)
!!      subroutine const_part_edge_hash_4_ele                           &
!!     &         (numnod, numele, numele_part, nnod_4_ele, nnod_4_edge, &
!!     &          ie, iele_part, inum_edge_hash, istack_edge_hash,      &
!!     &          iend_edge_hash, iedge_hash, iedge_flag)
!!@endverbatim
!
      module set_edge_hash_by_ele
!
      use m_precision
!
      use m_geometry_constants
!
      implicit none
!
      private :: count_edge_hash_4_ele, count_part_edge_hash_4_ele
      private :: set_edge_hash_4_ele, set_part_edge_hash_4_ele
      private :: mark_all_edges_by_ele
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine const_edge_hash_4_ele                                  &
     &         (numnod, numele, nnod_4_ele, nnod_4_edge, ie,            &
     &          inum_edge_hash, istack_edge_hash,                       &
     &          iend_edge_hash, iedge_hash, iedge_flag)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: iend_edge_hash
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: inum_edge_hash(nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_hash(nedge_4_ele*numele,2)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_flag(nedge_4_ele*numele)
!
!
      call count_edge_hash_4_ele                                        &
     &   (numnod, numele, nnod_4_ele, nnod_4_edge, ie,                  &
     &    inum_edge_hash, istack_edge_hash, iend_edge_hash)
!
      call set_edge_hash_4_ele                                          &
     &   (numnod, numele, nnod_4_ele, nnod_4_edge, ie,                  &
     &    inum_edge_hash, istack_edge_hash, iedge_hash)
!
      call mark_all_edges_by_ele                                        &
     &   (numnod, numele, nnod_4_ele, nnod_4_edge, ie,                  &
     &    istack_edge_hash, iend_edge_hash, iedge_hash, iedge_flag)
!
      end subroutine const_edge_hash_4_ele
!
!------------------------------------------------------------------
!
      subroutine const_part_edge_hash_4_ele                             &
     &         (numnod, numele, numele_part, nnod_4_ele, nnod_4_edge,   &
     &          ie, iele_part, inum_edge_hash, istack_edge_hash,        &
     &          iend_edge_hash, iedge_hash, iedge_flag)
!
      integer(kind = kint), intent(in) :: numnod, numele, numele_part
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: iele_part(numele_part)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: iend_edge_hash
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: inum_edge_hash(nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_hash(nedge_4_ele*numele,2)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_flag(nedge_4_ele*numele)
!
!
      call count_part_edge_hash_4_ele(numnod, numele, numele_part,      &
     &    nnod_4_ele, nnod_4_edge, ie, iele_part,                       &
     &    inum_edge_hash, istack_edge_hash, iend_edge_hash)
!
      call set_part_edge_hash_4_ele(numnod, numele, numele_part,        &
     &    nnod_4_ele, nnod_4_edge, ie, iele_part,                       &
     &    inum_edge_hash, istack_edge_hash, iedge_hash)
!
      call mark_all_edges_by_ele                                        &
     &   (numnod, numele, nnod_4_ele, nnod_4_edge, ie,                  &
     &    istack_edge_hash, iend_edge_hash, iedge_hash, iedge_flag)
!
      end subroutine const_part_edge_hash_4_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_edge_hash_4_ele                                  &
     &         (numnod, numele, nnod_4_ele, nnod_4_edge, ie,            &
     &          inum_edge_hash, istack_edge_hash, iend_edge_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: iend_edge_hash
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: inum_edge_hash(nnod_4_edge*numnod)
!
      integer(kind = kint) :: iele, is1, is2, k1
      integer(kind = kint) :: ihash
!
!
! Count numbers
      do iele = 1, numele
        do k1 = 1, nedge_4_ele
          is1 = node_on_edge_l(1,k1)
          is2 = node_on_edge_l(2,k1)
          ihash = ie(iele,is1) + ie(iele,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
        end do
      end do
!
! Set stacks
      istack_edge_hash = 0
      do ihash = 1, nnod_4_edge*numnod
        istack_edge_hash(ihash) = istack_edge_hash(ihash-1)             &
     &                               + inum_edge_hash(ihash)
        if (istack_edge_hash(ihash) .le. (nedge_4_ele*numele) ) then
          iend_edge_hash = ihash
        end if
      end do
!
      end subroutine count_edge_hash_4_ele
!
!------------------------------------------------------------------
!
      subroutine set_edge_hash_4_ele                                    &
     &         (numnod, numele, nnod_4_ele, nnod_4_edge, ie,            &
     &          inum_edge_hash, istack_edge_hash, iedge_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: inum_edge_hash(nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_hash(nedge_4_ele*numele,2)
!
      integer(kind = kint) :: iele, is1, is2, k1
      integer(kind = kint) :: ihash, icou
!
!
! Set ID
      inum_edge_hash = 0
      do iele = 1, numele
        do k1 = 1, nedge_4_ele
          is1 = node_on_edge_l(1,k1)
          is2 = node_on_edge_l(2,k1)
          ihash = ie(iele,is1) + ie(iele,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
          icou = istack_edge_hash(ihash-1) + inum_edge_hash(ihash)
          iedge_hash(icou,1) = iele
          iedge_hash(icou,2) = k1
        end do
      end do
!
      end subroutine set_edge_hash_4_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_part_edge_hash_4_ele                             &
     &         (numnod, numele, numele_part,                            &
     &          nnod_4_ele, nnod_4_edge, ie, iele_part,                 &
     &          inum_edge_hash, istack_edge_hash, iend_edge_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele, numele_part
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: iele_part(numele_part)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: inum_edge_hash(nnod_4_edge*numnod)
      integer(kind = kint), intent(inout) :: iend_edge_hash
!
      integer(kind = kint) :: inum, iele, is1, is2, k1
      integer(kind = kint) :: ihash
!
!
! Count numbers
      do inum = 1, numele_part
        iele = abs( iele_part(inum) )
        do k1 = 1, nedge_4_ele
          is1 = node_on_edge_l(1,k1)
          is2 = node_on_edge_l(2,k1)
          ihash = ie(iele,is1) + ie(iele,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
        end do
      end do
!
! Set stacks
      istack_edge_hash = 0
      do ihash = 1, nnod_4_edge*numnod
        istack_edge_hash(ihash) = istack_edge_hash(ihash-1)             &
     &                               + inum_edge_hash(ihash)
        if (istack_edge_hash(ihash) .le. (nedge_4_ele*numele_part) )    &
     &   then
          iend_edge_hash = ihash
        end if
      end do
!
      end subroutine count_part_edge_hash_4_ele
!
!------------------------------------------------------------------
!
      subroutine set_part_edge_hash_4_ele(numnod, numele, numele_part,  &
     &          nnod_4_ele, nnod_4_edge, ie, iele_part,                 &
     &          inum_edge_hash, istack_edge_hash, iedge_hash)
!
      integer(kind = kint), intent(in) :: numnod, numele, numele_part
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: iele_part(numele_part)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: inum_edge_hash(nnod_4_edge*numnod)
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_hash(nedge_4_ele*numele,2)
!
      integer(kind = kint) :: inum, iele, is1, is2, k1
      integer(kind = kint) :: ihash, icou
!
!
! Set ID
      inum_edge_hash = 0
      do inum = 1, numele_part
        iele = abs( iele_part(inum) )
        do k1 = 1, nedge_4_ele
          is1 = node_on_edge_l(1,k1)
          is2 = node_on_edge_l(2,k1)
          ihash = ie(iele,is1) + ie(iele,is2)
!
          inum_edge_hash(ihash) = inum_edge_hash(ihash) + 1
          icou = istack_edge_hash(ihash-1) + inum_edge_hash(ihash)
          iedge_hash(icou,1) = iele
          iedge_hash(icou,2) = k1
        end do
      end do
!
      end subroutine set_part_edge_hash_4_ele
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mark_all_edges_by_ele                                  &
     &       (numnod, numele, nnod_4_ele, nnod_4_edge, ie,              &
     &        istack_edge_hash, iend_edge_hash, iedge_hash, iedge_flag)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: nnod_4_ele, nnod_4_edge
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(in)                                  &
     &                     :: istack_edge_hash(0:nnod_4_edge*numnod)
      integer(kind = kint), intent(in)                                  &
     &                     :: iedge_hash(nedge_4_ele*numele,2)
      integer(kind = kint), intent(in) :: iend_edge_hash
!
      integer(kind = kint), intent(inout)                               &
     &                     :: iedge_flag(nedge_4_surf*numele)
!
      integer(kind = kint) :: inod(2), jnod(2)
      integer(kind = kint) :: ihash
      integer(kind = kint) :: ist, ied, k1, k2
!      integer(kind= kint_gl) :: i1_gl, i2_gl
!
!
      iedge_flag = 0
!!$omp parallel do private(ihash,ist,ied,k1,k2,inod,jnod)
      do ihash = 1, iend_edge_hash
        ist = istack_edge_hash(ihash-1)+1
        ied = istack_edge_hash(ihash)
        if (ied .eq. ist) then
          iedge_flag(ist) = ist
        else if (ied .gt. ist) then
          do k1 = ist, ied
            if(iedge_flag(k1) .eq. 0) then
              iedge_flag(k1) = k1
              call set_2nodes_id_on_edge                                &
     &            (iedge_hash(k1,1), iedge_hash(k1,2),                  &
     &             numele, nnod_4_ele, ie, inod)
              do k2 = k1+1, ied
                call set_2nodes_id_on_edge                              &
     &            (iedge_hash(k2,1), iedge_hash(k2,2),                  &
     &             numele, nnod_4_ele, ie, jnod)
                if ( (inod(2)-inod(1)) .eq. (jnod(2)-jnod(1)) ) then
                  iedge_flag(k2) = k1
                else if( (inod(2)-inod(1)) .eq. (jnod(1)-jnod(2))) then
                  iedge_flag(k2) =-k1
                end if
              end do
            end if
          end do
        end if
      end do
!!$omp end parallel do
!
      end subroutine mark_all_edges_by_ele
!
!------------------------------------------------------------------
!
      subroutine set_2nodes_id_on_edge                                  &
     &         (iele, is, numele, nnod_4_ele, ie, inod)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: iele, is
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
!
      integer(kind = kint), intent(inout) :: inod(2)
!
      integer(kind = kint) :: is1, is2
!
!
      is1 = node_on_edge_l(1,is)
      is2 = node_on_edge_l(2,is)
!
      inod(1) = ie(iele,is1)
      inod(2) = ie(iele,is2)
!
      end subroutine set_2nodes_id_on_edge
!
!------------------------------------------------------------------
!
      end module set_edge_hash_by_ele
