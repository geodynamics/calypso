!>@file   const_element_comm_table.f90
!!@brief  module const_element_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine allocate_element_rev_imports(ntot_import_e)
!!      subroutine allocate_element_rev_exports(ntot_export_e)
!!      subroutine deallocate_element_rev_list
!!
!!      subroutine count_element_import_num(numnod, iele_stack_ht_node, &
!!     &          num_neib, id_neib, istack_import, item_import,        &
!!     &          num_neib_e, id_neib_e, num_import_e, istack_import_e, &
!!     &          ntot_import_e)
!!      subroutine set_element_import_item(numnod, numele,              &
!!     &          inod_global, x_ele, iele_stack_ht_node, iele_ht_node, &
!!     &          num_neib, istack_import, item_import,                 &
!!     &          num_neib_e, istack_import_e, item_import_e)
!!
!!      subroutine element_num_reverse_SR(num_neib_e, id_neib_e,        &
!!     &          num_import_e, num_export_e, istack_export_e,          &
!!     &          ntot_export_e)
!!      subroutine element_position_reverse_SR(num_neib_e, id_neib_e,   &
!!     &          istack_import_e, istack_export_e)
!!
!!      subroutine set_element_export_item                              &
!!     &         (txt, numnod, numele, inod_global,                     &
!!     &          internal_flag, x_ele, iele_stack_4_node, iele_4_node, &
!!     &          num_neib, istack_export, item_export, num_neib_e,     &
!!     &          istack_export_e, item_export_e)
!!@endverbatim
!!
      module const_element_comm_table
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_solver_SR
!
      implicit none
!
!>      global node ID for element import table
      integer(kind = kint_gl), allocatable :: inod_import_e(:)
!>      global node ID for element export table
      integer(kind = kint_gl), allocatable :: inod_export_e(:)
!
!>      element position for element import table
      real(kind = kreal), allocatable :: xe_import(:)
!>      element position for element import table
      real(kind = kreal), allocatable :: xe_export(:)
!
!>      small number
      real(kind = kreal) :: tiny = 1.0d-11
!
      private :: inod_import_e, xe_import
      private :: inod_export_e, xe_export
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_element_rev_imports(ntot_import_e)
!
      integer(kind = kint), intent(in) :: ntot_import_e
!
!
      allocate(inod_import_e(ntot_import_e))
      allocate(xe_import(3*ntot_import_e))
      if(ntot_import_e .gt. 0) inod_import_e = 0
      if(ntot_import_e .gt. 0) xe_import = 0.0d0
!
      end subroutine allocate_element_rev_imports
!
!------------------------------------------------------------------
!
      subroutine allocate_element_rev_exports(ntot_export_e)
!
      integer(kind = kint), intent(in) :: ntot_export_e
!
!
      allocate(inod_export_e(ntot_export_e))
      allocate(xe_export(3*ntot_export_e))
      if(ntot_export_e .gt. 0) inod_export_e = 0
      if(ntot_export_e .gt. 0) xe_export = 0.0d0
!
      end subroutine allocate_element_rev_exports
!
!------------------------------------------------------------------
!
      subroutine deallocate_element_rev_list
!
!
      deallocate(inod_import_e, xe_import)
      deallocate(inod_export_e, xe_export)
!
      end subroutine deallocate_element_rev_list
!
!-----------------------------------------------------------------------
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
      subroutine set_element_import_item(numnod, numele,                &
     &          inod_global, x_ele, iele_stack_ht_node, iele_ht_node,   &
     &          num_neib, istack_import, item_import,                   &
     &          num_neib_e, istack_import_e, item_import_e)
!
      integer(kind = kint), intent(in) :: numnod, numele
      integer(kind = kint_gl), intent(in) :: inod_global(numnod)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
      integer(kind = kint), intent(in) :: iele_stack_ht_node(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &        :: iele_ht_node(iele_stack_ht_node(numnod))
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
!
      integer(kind = kint) :: ip, icou
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst, num, jnum, jele
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
            inod_import_e(icou) = inod_global(inod)
            xe_import(3*icou-2) = x_ele(jele,1)
            xe_import(3*icou-1) = x_ele(jele,2)
            xe_import(3*icou  ) = x_ele(jele,3)
         end do
        end do
      end do
!
      end subroutine  set_element_import_item
!
!-----------------------------------------------------------------------
!
      subroutine element_num_reverse_SR(num_neib_e, id_neib_e,          &
     &          num_import_e, num_export_e, istack_export_e,            &
     &          ntot_export_e)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: num_import_e(num_neib_e)
!
      integer(kind = kint), intent(inout) :: ntot_export_e
      integer(kind = kint), intent(inout) :: num_export_e(num_neib_e)
      integer(kind = kint), intent(inout)                               &
     &        :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, num_neib_e
        call MPI_ISEND (num_import_e(ip), ione, CALYPSO_INTEGER,        &
     &                  id_neib_e(ip), 0, CALYPSO_COMM,                 &
     &                  req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib_e
        call MPI_IRECV (num_export_e(ip), ione, CALYPSO_INTEGER,        &
     &                 id_neib_e(ip), 0, CALYPSO_COMM,                  &
     &                  req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL(num_neib_e, req2(1), sta2(1,1), ierr_MPI)
      call MPI_WAITALL(num_neib_e, req1(1), sta1(1,1), ierr_MPI)
!
      do ip = 1, num_neib_e
        istack_export_e(ip) = istack_export_e(ip-1) + num_export_e(ip)
      end do
      ntot_export_e = istack_export_e(num_neib_e)
!
      end subroutine  element_num_reverse_SR
!
!-----------------------------------------------------------------------
!
      subroutine element_position_reverse_SR(num_neib_e, id_neib_e,     &
     &          istack_import_e, istack_export_e)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
!
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint) :: ip, ist, num
!
!      do ip = 1, istack_import_e(num_neib_e)
!        write(*,*) ip, inod_import_e(ip), xe_import(3*ip-2:3*ip)
!      end do
!
      do ip = 1, num_neib_e
        ist = istack_import_e(ip-1)
        num = istack_import_e(ip  ) - istack_import_e(ip-1)
        call MPI_ISEND (inod_import_e(ist+1), num, CALYPSO_GLOBAL_INT,  &
     &                  id_neib_e(ip), 0, CALYPSO_COMM,                 &
     &                  req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib_e
        ist = istack_export_e(ip-1)
        num = istack_export_e(ip  ) - istack_export_e(ip-1)
        call MPI_IRECV (inod_export_e(ist+1), num, CALYPSO_GLOBAL_INT,  &
     &                 id_neib_e(ip), 0, CALYPSO_COMM,                  &
     &                 req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL(num_neib_e, req2(1), sta2(1,1), ierr_MPI)
      call MPI_WAITALL(num_neib_e, req1(1), sta1(1,1), ierr_MPI)
!
!
      do ip = 1, num_neib_e
        ist = 3*istack_import_e(ip-1)
        num = 3*(istack_import_e(ip  ) - istack_import_e(ip-1))
        call MPI_ISEND (xe_import(ist+1), num, CALYPSO_REAL,            &
     &                  id_neib_e(ip), 0, CALYPSO_COMM,                 &
     &                  req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib_e
        ist = 3* istack_export_e(ip-1)
        num = 3*(istack_export_e(ip  ) - istack_export_e(ip-1))
        call MPI_IRECV (xe_export(ist+1), num, CALYPSO_REAL,            &
     &                 id_neib_e(ip), 0, CALYPSO_COMM,                  &
     &                 req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL(num_neib_e, req2(1), sta2(1,1), ierr_MPI)
      call MPI_WAITALL(num_neib_e, req1(1), sta1(1,1), ierr_MPI)
!
      end subroutine element_position_reverse_SR
!
!-----------------------------------------------------------------------
!
      subroutine set_element_export_item                                &
     &         (txt, numnod, numele, inod_global,                       &
     &          internal_flag, x_ele, iele_stack_4_node, iele_4_node,   &
     &          num_neib, istack_export, item_export, num_neib_e,       &
     &          istack_export_e, item_export_e)
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
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &        :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
!
      integer(kind = kint) :: ip, iflag
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum, jnod
      integer(kind = kint) :: kst, ked, knum, kele, kele_min
      integer(kind = kint_gl) :: inod_gl
      real(kind = kreal) :: dx(3), dist, dist_min
!
!
      do ip = 1, num_neib
        jst = istack_export(ip-1) + 1
        jed = istack_export(ip)
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        do inum = ist, ied
          inod_gl = inod_export_e(inum)
          iflag = 0
          dist_min = 1.0d30
!
          do jnum = jst, jed
            jnod = item_export(jnum)
!
            if(inod_export_e(inum) .eq. inod_global(jnod)) then
              kst = iele_stack_4_node(jnod-1) + 1
              ked = iele_stack_4_node(jnod)
              do knum = kst, ked
                kele = iele_4_node(knum)
                if(internal_flag(kele) .eq. 0) cycle
                dx(1) = abs(xe_export(3*inum-2) - x_ele(kele,1))
                dx(2) = abs(xe_export(3*inum-1) - x_ele(kele,2))
                dx(3) = abs(xe_export(3*inum  ) - x_ele(kele,3))
                dist = sqrt(dx(1)**2+dx(2)**2+dx(3)**2)
                if(dx(1).le.tiny .and. dx(2).le.tiny                    &
     &            .and. dx(3).le.tiny) then
                  item_export_e(inum) = kele
                  iflag = 1
                  exit
                end if
                dist_min = min(dist_min,dist)
                if(dist .lt. dist_min) then
                  dist_min = dist
                  kele_min = kele
                end if
              end do
              exit
            end if
          end do
          if(iflag .eq. 0)                                             &
     &           write(*,*) 'Missing imported ', trim(txt), ': ',      &
     &                     my_rank, kele, inum, xe_export(3*inum-2:3*inum), dist_min
        end do
      end do
!
      end subroutine set_element_export_item
!
!-----------------------------------------------------------------------
!
      end module const_element_comm_table
      