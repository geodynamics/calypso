!>@file   set_element_export_item.f90
!!@brief  module set_element_export_item
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine s_set_element_export_item                            &
!!     &         (txt, numnod, numele, internal_flag, x_ele,            &
!!     &          iele_stack_4_node, iele_4_node, x_ref_ele,            &
!!     &          num_neib, istack_import, item_import,                 &
!!     &          num_neib_e, istack_export_e, inod_export_l,           &
!!     &          xe_export, item_export_e)
!!      subroutine element_export_item_in_ext                           &
!!     &         (txt, numnod, numele, inod_global, internal_flag,      &
!!     &          x_ele, iele_stack_4_node, iele_4_node, x_ref_ele,     &
!!     &          num_neib, istack_export, item_export,                 &
!!     &          num_neib_e, istack_export_e, inod_export_e,           &
!!     &          xe_export, item_export_e)
!!@endverbatim
!!
      module set_element_export_item
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!>      many number
      integer(kind = kint), parameter, private :: many = 64
!
      private :: search_target_element
      private :: search_target_element2, search_target_element3
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_element_export_item                              &
     &         (txt, numnod, numele, internal_flag, x_ele,              &
     &          iele_stack_4_node, iele_4_node, x_ref_ele,              &
     &          num_neib, istack_import, item_import,                   &
     &          num_neib_e, istack_export_e, inod_export_l,             &
     &          xe_export, item_export_e)
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_4_node(iele_stack_4_node(numnod))
      real(kind = kreal), intent(in)                                    &
     &        :: x_ref_ele(iele_stack_4_node(numnod))
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint), intent(in)                                  &
     &        :: inod_export_l(istack_export_e(num_neib_e))
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, iflag
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jnod
      integer(kind = kint) :: kst, num
      real(kind = kreal) :: dist_min
!
!
      do ip = 1, num_neib
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        jst = istack_import(ip-1) + 1
        jed = istack_import(ip)
!
        do inum = ist, ied
          inod = inod_export_l(inum)
          if(inod .le. 0) cycle
!
          iflag = 0
          dist_min = 1.0d30
!
          do jnum = jst, jed
            jnod = item_import(jnum)
!
            if(inod .eq. jnod) then
              kst = iele_stack_4_node(jnod-1) + 1
              num = iele_stack_4_node(jnod) - iele_stack_4_node(jnod-1)
              call search_target_element(numele, internal_flag, x_ele,  &
     &            num, iele_4_node(kst), x_ref_ele(kst),                &
     &            xe_export(3*inum-2), item_export_e(inum),             &
     &            dist_min, iflag, inum)
              exit
            end if
          end do
!          if(iflag .eq. 0) write(*,*)                                  &
!     &           'Missing imported ', trim(txt), ' by external: ',     &
!     &                     my_rank, inum, item_export_e(inum),         &
!     &                     xe_export(3*inum-2:3*inum), dist_min
        end do
      end do
!
      end subroutine s_set_element_export_item
!
!-----------------------------------------------------------------------
!
      subroutine element_export_item_in_ext                             &
     &         (txt, numnod, numele, inod_global, internal_flag,        &
     &          x_ele, iele_stack_4_node, iele_4_node, x_ref_ele,       &
     &          num_neib, istack_export, item_export,                   &
     &          num_neib_e, istack_export_e, inod_export_e,             &
     &          xe_export, item_export_e)
!
      character(len=kchara), intent(in) :: txt
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_4_node(iele_stack_4_node(numnod))
      real(kind = kreal), intent(in)                                    &
     &        :: x_ref_ele(iele_stack_4_node(numnod))
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint_gl), intent(in)                               &
     &        :: inod_export_e(istack_export_e(num_neib_e))
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, iflag
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum, jnod
      integer(kind = kint) :: kst, num
      integer(kind = kint_gl) :: inod_gl
      real(kind = kreal) :: dist_min
!
!
      do ip = 1, num_neib
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        jst = istack_export(ip-1) + 1
        jed = istack_export(ip)
!
        do inum = ist, ied
          inod_gl = inod_export_e(inum)
          if(item_export_e(inum) .gt. 0) cycle
!
          iflag = 0
          dist_min = 1.0d30
!
          do jnum = jst, jed
            jnod = item_export(jnum)
!
            if(inod_gl .eq. inod_global(jnod)) then
              kst = iele_stack_4_node(jnod-1) + 1
              num = iele_stack_4_node(jnod) - iele_stack_4_node(jnod-1)
              call search_target_element(numele, internal_flag, x_ele,  &
     &            num, iele_4_node(kst), x_ref_ele(kst),                &
     &            xe_export(3*inum-2), item_export_e(inum),             &
     &            dist_min, iflag, inum)
              exit
            end if
          end do
          if(iflag .eq. 0) write(*,*)                                   &
     &           'Missing imported ', trim(txt), ' by internal: ',      &
     &                     my_rank, inum, item_export_e(inum),          &
     &                     xe_export(3*inum-2:3*inum), dist_min
        end do
      end do
!
      end subroutine element_export_item_in_ext
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine search_target_element(numele, internal_flag, x_ele,    &
     &          nele_4_node, iele_4_node, x_ref_ele,                    &
     &          xe_export, item_export_e, dist_min, iflag, inum)
!
      integer(kind = kint), intent(in) :: numele, inum
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
      real(kind = kreal), intent(in)   :: x_ref_ele(nele_4_node)
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
!
      if(nele_4_node .lt. 1) then
        return
      else if(nele_4_node .ge. many) then
        call search_target_element3(numele, internal_flag, x_ele,       &
     &      nele_4_node, iele_4_node, x_ref_ele, xe_export,             &
     &      item_export_e, dist_min, iflag, inum)
      else
        call search_target_element2(numele, internal_flag, x_ele,       &
     &       nele_4_node, iele_4_node, xe_export,                       &
     &       item_export_e, dist_min, iflag, inum)
      end if
!
      end subroutine search_target_element
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine search_target_element3(numele, internal_flag, x_ele,   &
     &          nele_4_node, iele_4_node, x_ref_ele, xe_export,         &
     &          item_export_e, dist_min, iflag, inum)
!
      integer(kind = kint), intent(in) :: numele, inum
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
      real(kind = kreal), intent(in)   :: x_ref_ele(nele_4_node)
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
      integer(kind = kint) :: kst, ked, knum, kele
      integer(kind = kint) :: kkst, kknum, kk
      real(kind = kreal) :: dist
!
!
      kst = 1
      ked = nele_4_node
      knum = (ked-kst+1) / 2
      do
        kele = iele_4_node(knum)
        if(internal_flag(kele) .gt. 0) then
          dist = sqrt((xe_export(1) - x_ele(kele,1))**2                 &
     &              + (xe_export(2) - x_ele(kele,2))**2                 &
     &              + (xe_export(3) - x_ele(kele,3))**2)
!
          if(dist .le. TINY) then
            item_export_e = kele
            iflag = 1
            exit
          end if
          dist_min = min(dist_min,dist)
        end if
!
        if(xe_export(1) .lt. x_ref_ele(knum)) then
          ked = knum
          if(ked .le. kst) exit
          knum = ked - (ked-kst+1) / 2
        else if(xe_export(1) .gt. x_ref_ele(knum)) then
          kst = knum
          if(ked .le. kst) exit
          knum = kst + (ked-kst+1) / 2
        else
          kkst = knum
          kknum = 1
          do kk = knum, 1, -1
            kkst = kk
            if(x_ref_ele(kk) .ne. x_ref_ele(knum)) exit
          end do
          do kk = knum, nele_4_node
            kknum = kk - kkst + 1
            if(x_ref_ele(kk) .ne. x_ref_ele(knum)) exit
          end do
!
          call search_target_element2(numele, internal_flag, x_ele,     &
     &        kknum, iele_4_node(kkst), xe_export,                      &
     &        item_export_e, dist_min, iflag, inum)
          exit
        end if
        if((ked - kst) .lt. many) then
          call search_target_element2(numele, internal_flag, x_ele,     &
     &        many, iele_4_node(kst), xe_export,                        &
     &        item_export_e, dist_min, iflag, inum)
          exit
        end if
      end do
!
      end subroutine search_target_element3
!
!-----------------------------------------------------------------------
!
      subroutine search_target_element2(numele, internal_flag, x_ele,   &
     &          nele_4_node, iele_4_node, xe_export,                    &
     &          item_export_e, dist_min, iflag, inum)
!
      integer(kind = kint), intent(in) :: numele, inum
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
      integer(kind = kint) :: knum, kele
      real(kind = kreal) :: dist
!
!
      do knum = 1, nele_4_node
        kele = iele_4_node(knum)
        if(internal_flag(kele) .eq. 0) cycle
        dist = sqrt((xe_export(1) - x_ele(kele,1))**2                   &
     &            + (xe_export(2) - x_ele(kele,2))**2                   &
     &            + (xe_export(3) - x_ele(kele,3))**2)
!
        if(dist .le. TINY) then
          item_export_e = kele
          iflag = 1
          exit
        end if
        dist_min = min(dist_min,dist)
      end do
!
      end subroutine search_target_element2
!
!-----------------------------------------------------------------------
!
      end module set_element_export_item
