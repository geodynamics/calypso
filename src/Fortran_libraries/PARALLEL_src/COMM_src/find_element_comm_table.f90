!>@file   find_element_comm_table.f90
!!@brief  module find_element_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine count_element_import_num(numnod, iele_stack_ht_node, &
!!     &          num_neib, id_neib, istack_import, item_import,        &
!!     &          num_neib_e, id_neib_e, num_import_e, istack_import_e, &
!!     &          ntot_import_e)
!!      subroutine set_element_import_item                              &
!!     &         (numnod, internal_node, numele, nnod_4_ele, ie,        &
!!     &          inod_global, x_ele, iele_stack_ht_node, iele_ht_node, &
!!     &          inod_local, num_neib, istack_import, item_import,     &
!!     &          num_neib_e, istack_import_e, item_import_e,           &
!!     &          inod_import_e, inod_import_l, xe_import)
!!
!!      subroutine set_element_export_item                              &
!!     &         (txt, numnod, numele, inod_global,                     &
!!     &          internal_flag, x_ele, iele_stack_4_node, iele_4_node, &
!!     &          num_neib, istack_import, item_import,                 &
!!     &          istack_export, item_export, num_neib_e,               &
!!     &          istack_export_e, inod_export_e, inod_export_l,        &
!!     &          xe_export, item_export_e)
!!@endverbatim
!!
      module find_element_comm_table
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!>      small number
      real(kind = kreal) :: tiny = 1.0d-11
!
      private :: tiny
      private :: search_target_element
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_element_import_num(numnod, iele_stack_ht_node,   &
     &          num_neib, id_neib, istack_import, item_import,          &
     &          num_neib_e, id_neib_e, num_import_e, istack_import_e,   &
     &          ntot_import_e)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: iele_stack_ht_node(0:numnod)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &              :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(inout) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(inout) :: ntot_import_e
      integer(kind = kint), intent(inout) :: num_import_e(num_neib_e)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: ist, ied, inum, inod
!
      num_import_e = 0
      istack_import_e(0) = 0
      do ip = 1, num_neib
        id_neib_e(ip) = id_neib(ip)
        ist = istack_import(ip-1) + 1
        ied = istack_import(ip)
        do inum = ist, ied
          inod = item_import(inum)
          num_import_e(ip) = num_import_e(ip)                           &
     &                      + iele_stack_ht_node(inod  )                &
     &                      - iele_stack_ht_node(inod-1)
        end do
        istack_import_e(ip) = istack_import_e(ip-1) + num_import_e(ip)
      end do
      ntot_import_e = istack_import_e(num_neib)
!
      end subroutine count_element_import_num
!
!-----------------------------------------------------------------------
!
      subroutine set_element_import_item                                &
     &         (numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_global, x_ele, iele_stack_ht_node, iele_ht_node,   &
     &          inod_local, num_neib, istack_import, item_import,       &
     &          num_neib_e, istack_import_e, item_import_e,             &
     &          inod_import_e, inod_import_l, xe_import)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
      integer(kind = kint), intent(in) :: iele_stack_ht_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_ht_node(iele_stack_ht_node(numnod))
      integer(kind = kint), intent(in) :: inod_local(numnod)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &              :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_import_e(istack_import_e(num_neib_e))
      integer(kind = kint_gl), intent(inout)                            &
     &        :: inod_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_import_l(istack_import_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &        :: xe_import(3*istack_import_e(num_neib_e))
!
      integer(kind = kint) :: ip, icou
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, num, jnum, jele
      integer(kind = kint) :: k1, jnod, minimum, nele
!
!
      do ip = 1, num_neib
        ist = istack_import(ip-1) + 1
        ied = istack_import(ip)
        icou = istack_import_e(ip-1)
        do inum = ist, ied
          inod = item_import(inum)
          jst = iele_stack_ht_node(inod-1)
          num = iele_stack_ht_node(inod  ) - jst
          do jnum = 1, num
            icou = icou + 1
            jele = iele_ht_node(jst+jnum)
            item_import_e(icou) = jele
!
            inod_import_e(icou) = inod_global(inod)
            inod_import_l(icou) = 0
            xe_import(3*icou-2) = x_ele(jele,1)
            xe_import(3*icou-1) = x_ele(jele,2)
            xe_import(3*icou  ) = x_ele(jele,3)
!
            minimum = num
            do k1 = 1, nnod_4_ele
              jnod = ie(jele,k1)
              if(jnod .gt. internal_node) cycle
              nele = iele_stack_ht_node(jnod)                           &
     &              - iele_stack_ht_node(jnod-1)
              if(nele .lt. minimum) then
                minimum = nele
                inod_import_l(icou) = inod_local(jnod)
              end if
            end do
!
          end do
        end do
      end do
!
      end subroutine  set_element_import_item
!
!-----------------------------------------------------------------------
!
      subroutine count_element_import_num_e(numnod, iele_stack_ht_node,   &
     &          num_neib, id_neib, istack_import, item_import,          &
     &          num_neib_e, id_neib_e, num_import_e, istack_import_e,   &
     &          ntot_import_e)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: iele_stack_ht_node(0:numnod)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &              :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(inout) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(inout) :: ntot_import_e
      integer(kind = kint), intent(inout) :: num_import_e(num_neib_e)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: ist, ied, inum, inod
!
      num_import_e = 0
      istack_import_e(0) = 0
      do ip = 1, num_neib
        id_neib_e(ip) = id_neib(ip)
        ist = istack_import(ip-1) + 1
        ied = istack_import(ip)
        do inum = ist, ied
          inod = item_import(inum)
          num_import_e(ip) = num_import_e(ip)                           &
     &                      + iele_stack_ht_node(inod  )                &
     &                      - iele_stack_ht_node(inod-1)
        end do
        istack_import_e(ip) = istack_import_e(ip-1) + num_import_e(ip)
      end do
      ntot_import_e = istack_import_e(num_neib)
      write(*,*) 'ntot_import_e', ntot_import_e, my_rank
!
      end subroutine count_element_import_num_e
!
!-----------------------------------------------------------------------
!
      subroutine set_element_import_item_e                              &
     &         (numnod, internal_node, numele, nnod_4_ele, ie,          &
     &          inod_global, x_ele, iele_stack_ht_node, iele_ht_node,   &
     &          inod_local, num_neib, istack_import, item_import,       &
     &          num_neib_e, istack_import_e, item_import_e,             &
     &          inod_import_e, inod_import_l, xe_import)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
      integer(kind = kint), intent(in) :: iele_stack_ht_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_ht_node(iele_stack_ht_node(numnod))
      integer(kind = kint), intent(in) :: inod_local(numnod)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &              :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_import_e(istack_import_e(num_neib_e))
      integer(kind = kint_gl), intent(inout)                            &
     &        :: inod_import_e(istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_import_l(istack_import_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &        :: xe_import(3*istack_import_e(num_neib_e))
!
      integer(kind = kint) :: ip, icou
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, num, jnum, jele
      integer(kind = kint) :: k1, jnod, minimum, nele
!
!
      do ip = 1, numnod
        ist = iele_stack_ht_node(ip-1) + 1
        ied = iele_stack_ht_node(ip)
        do inum = ist, ied
           if(iele_ht_node(inum) .le. 0 .or. iele_ht_node(inum) .gt. numele) write(*,*)   &
     &        'Wrong iele_ht_node at ', inum, ip, iele_ht_node(inum)
        end do
      end do
      call calypso_mpi_barrier
      write(*,*) 'element list checked'
!
      do ip = 1, num_neib
        ist = istack_import(ip-1) + 1
        ied = istack_import(ip)
        icou = istack_import_e(ip-1)
        do inum = ist, ied
          inod = item_import(inum)
          jst = iele_stack_ht_node(inod-1)
          num = iele_stack_ht_node(inod  ) - jst
          do jnum = 1, num
            icou = icou + 1
            jele = iele_ht_node(jst+jnum)
            item_import_e(icou) = jele
!
            inod_import_e(icou) = inod_global(inod)
            inod_import_l(icou) = 0
            xe_import(3*icou-2) = x_ele(jele,1)
            xe_import(3*icou-1) = x_ele(jele,2)
            xe_import(3*icou  ) = x_ele(jele,3)
!
            minimum = num
            do k1 = 1, nnod_4_ele
              jnod = ie(jele,k1)
              if(jnod .gt. internal_node) cycle
              nele = iele_ht_node(jnod) - iele_ht_node(jnod-1)
              if(nele .lt. minimum) then
                minimum = nele
                inod_import_l(icou) = inod_local(jnod)
              end if
            end do
!
          end do
        end do
      end do
      call calypso_mpi_barrier
      write(*,*) 'last iclu', istack_import_e(num_neib_e), icou
!
      end subroutine  set_element_import_item_e
!
!-----------------------------------------------------------------------
!
      subroutine set_element_export_item                                &
     &         (txt, numnod, numele, inod_global,                       &
     &          internal_flag, x_ele, iele_stack_4_node, iele_4_node,   &
     &          num_neib, istack_import, item_import,                   &
     &          istack_export, item_export, num_neib_e,                 &
     &          istack_export_e, inod_export_e, inod_export_l,          &
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
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_import(istack_import(num_neib))
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint), intent(in)                                  &
     &        :: inod_export_l(istack_export_e(num_neib_e))
      integer(kind = kint_gl), intent(in)                               &
     &        :: inod_export_e(istack_export_e(num_neib_e))
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, iflag
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, jed, jnum, jnod
      integer(kind = kint_gl) :: inod_gl
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
              call search_target_element(jnod, numnod, numele,          &
     &            internal_flag, x_ele, iele_stack_4_node, iele_4_node, &
     &            xe_export(3*inum-2), item_export_e(inum),             &
     &            dist_min, iflag)
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
              call search_target_element(jnod, numnod, numele,          &
     &            internal_flag, x_ele, iele_stack_4_node, iele_4_node, &
     &            xe_export(3*inum-2), item_export_e(inum),             &
     &            dist_min, iflag)
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
      end subroutine set_element_export_item
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine search_target_element(jnod, numnod, numele,            &
     &          internal_flag, x_ele, iele_stack_4_node, iele_4_node,   &
     &          xe_export, item_export_e, dist_min, iflag)
!
      integer(kind = kint), intent(in) :: jnod
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint), intent(in) :: internal_flag(numele)
      real(kind = kreal), intent(in) :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_4_node(iele_stack_4_node(numnod))
!
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      integer(kind = kint), intent(inout) :: iflag
      real(kind = kreal), intent(inout) :: dist_min
!
      integer(kind = kint) :: kst, ked, knum, kele
      real(kind = kreal) :: dx(3), dist
!
!
      kst = iele_stack_4_node(jnod-1) + 1
      ked = iele_stack_4_node(jnod)
!      if(ked-kst .gt. 8) write(50+my_rank,*)                           &
!     &                  'kst, ked', my_rank, jnod, ked-kst
      do knum = kst, ked
        kele = iele_4_node(knum)
        if(internal_flag(kele) .eq. 0) cycle
        dx(1) = abs(xe_export(1) - x_ele(kele,1))
        dx(2) = abs(xe_export(2) - x_ele(kele,2))
        dx(3) = abs(xe_export(3) - x_ele(kele,3))
        dist = sqrt(dx(1)**2+dx(2)**2+dx(3)**2)
        if(dx(1).le.tiny .and. dx(2).le.tiny .and. dx(3).le.tiny) then
          item_export_e = kele
          iflag = 1
          exit
        end if
        dist_min = min(dist_min,dist)
        if(dist .lt. dist_min)  dist_min = dist
      end do
!
      end subroutine search_target_element
!
!-----------------------------------------------------------------------
!
      end module find_element_comm_table
      