!>@file   set_element_comm_table.f90
!!@brief  module set_element_comm_table
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2021
!
!>@brief  Routines to Set communication table items for elements
!!
!!@verbatim
!!      subroutine count_element_import_num(nod_comm, iele_dbl,         &
!!     &          num_neib_e, id_neib_e, num_import_e, istack_import_e, &
!!     &          ntot_import_e)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(element_double_number), intent(in) :: iele_dbl
!!      subroutine set_element_import_item(inod_dbl, iele_dbl,          &
!!     &          numele, nnod_4_ele, ie, x_ele, isum_ele,              &
!!     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,&
!!     &          inod_lc_import, ipe_lc_import, isum_import, xe_import)
!!        type(node_ele_double_number), intent(in) :: inod_dbl
!!        type(element_double_number), intent(in) :: iele_dbl
!!
!!      subroutine set_element_export_item                              &
!!     &         (txt, neib_e, numele, nnod_4_ele, x_ele,               &
!!     &          num_neib_e, istack_export_e, inod_lc_export,          &
!!     &          ipe_lc_export, xe_export, item_export_e, fail_tbl)
!!        type(element_around_node), intent(in) :: neib_e
!!        type(failed_table), intent(inout) :: fail_tbl
!!@endverbatim
!!
      module set_element_comm_table
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
      integer(kind = kint), parameter, private :: ISMALL = 17
!
      private :: find_ele_export_from_small, find_ele_export_from_large
      private :: dist_ele_position_to_export
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_element_import_num(nod_comm, iele_dbl,           &
     &          num_neib_e, id_neib_e, num_import_e, istack_import_e,   &
     &          ntot_import_e)
!
      use t_comm_table
      use t_element_double_number
!
      type(communication_table), intent(in) :: nod_comm
      type(element_double_number), intent(in) :: iele_dbl
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(inout) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(inout) :: ntot_import_e
      integer(kind = kint), intent(inout) :: num_import_e(num_neib_e)
      integer(kind = kint), intent(inout)                               &
     &              :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint) :: ip, inum, iele
      integer(kind = kint), allocatable :: num_import_tmp(:)
!
!
      allocate(num_import_tmp(nprocs))
!
!$omp parallel workshare
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      do iele = 1, iele_dbl%num_dbl
        ip = iele_dbl%irank(iele)
        if(ip .ne. my_rank) then
          num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
        end if
      end do
!
      istack_import_e(0) = 0
      do inum = 1, nod_comm%num_neib
        ip = nod_comm%id_neib(inum)
        id_neib_e(inum) =    ip
        num_import_e(inum) = num_import_tmp(ip+1)
        istack_import_e(inum) = istack_import_e(inum-1)                 &
     &                         + num_import_e(inum)
      end do
      ntot_import_e = istack_import_e(nod_comm%num_neib)
!
      deallocate(num_import_tmp)
!
      end subroutine count_element_import_num
!
!-----------------------------------------------------------------------
!
      subroutine set_element_import_item(inod_dbl, iele_dbl,            &
     &          numele, nnod_4_ele, ie, x_ele, isum_ele,                &
     &          num_neib_e, id_neib_e, istack_import_e, item_import_e,  &
     &          inod_lc_import, ipe_lc_import, isum_import, xe_import)
!
      use t_para_double_numbering
      use t_element_double_number
!
      type(node_ele_double_number), intent(in) :: inod_dbl
      type(element_double_number), intent(in) :: iele_dbl
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: isum_ele(numele)
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: id_neib_e(num_neib_e)
      integer(kind = kint), intent(in) :: istack_import_e(0:num_neib_e)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_import_e(istack_import_e(num_neib_e))
      real(kind = kreal), intent(inout)                                 &
     &        :: xe_import(3*istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: isum_import(istack_import_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: inod_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(inout)                               &
     &        :: ipe_lc_import(istack_import_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint) :: ip, icou, iele
      integer(kind = kint) :: inum, inod, k1
!
      integer(kind = kint), allocatable :: ip_rev_tmp(:)
      integer(kind = kint), allocatable :: num_import_tmp(:)
!
!
      allocate(ip_rev_tmp(nprocs))
      allocate(num_import_tmp(nprocs))
!
!$omp parallel workshare
      ip_rev_tmp(1:nprocs) =     0
      num_import_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
!$omp parallel do private(ip)
      do inum = 1, num_neib_e
        ip = id_neib_e(inum)
        ip_rev_tmp(ip+1) = inum
      end do
!$omp end parallel do
!
      do iele = 1, iele_dbl%num_dbl
        ip =   iele_dbl%irank(iele)
        if(ip .ne. my_rank) then
          inum = ip_rev_tmp(ip+1)
          num_import_tmp(ip+1) = num_import_tmp(ip+1) + 1
          icou = istack_import_e(inum-1) + num_import_tmp(ip+1)
!
          item_import_e(icou) = iele
        end if
      end do
!
      deallocate(ip_rev_tmp, num_import_tmp)
!
!$omp parallel do private(icou,iele,k1,inod)
      do icou = 1, istack_import_e(num_neib_e)
        iele = item_import_e(icou)
!
        isum_import(icou) =   isum_ele(iele)
        xe_import(3*icou-2) = x_ele(iele,1)
        xe_import(3*icou-1) = x_ele(iele,2)
        xe_import(3*icou  ) = x_ele(iele,3)
        do k1 = 1, nnod_4_ele
          inod = ie(iele,k1)
          inod_lc_import(icou,k1) = inod_dbl%index(inod)
          ipe_lc_import(icou,k1) =  inod_dbl%irank(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine  set_element_import_item
!
!-----------------------------------------------------------------------
!
      subroutine set_element_export_item                                &
     &         (neib_e, numele, nnod_4_ele, x_ele, isum_ele,            &
     &          num_neib_e, istack_export_e, inod_lc_export,            &
     &          ipe_lc_export, isum_export, xe_export,                  &
     &          item_export_e, fail_tbl)
!
      use t_next_node_ele_4_node
      use t_failed_export_list
      use calypso_mpi_int
      use quicksort
!
      type(element_around_node), intent(in) :: neib_e
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: isum_ele(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer(kind = kint), intent(in) :: num_neib_e
      integer(kind = kint), intent(in) :: istack_export_e(0:num_neib_e)
!
      real(kind = kreal), intent(in)                                    &
     &        :: xe_export(3*istack_export_e(num_neib_e))
      integer(kind = kint), intent(inout)                               &
     &        :: isum_export(istack_export_e(num_neib_e))
      integer(kind = kint), intent(in)                                  &
     &        :: inod_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
      integer(kind = kint), intent(in)                                  &
     &        :: ipe_lc_export(istack_export_e(num_neib_e),nnod_4_ele)
!
      integer(kind = kint), intent(inout)                               &
     &        :: item_export_e(istack_export_e(num_neib_e))
      type(failed_table), intent(inout) :: fail_tbl
!
      integer(kind = kint) :: ip, icou, num_gl
      integer(kind = kint) :: ist, ied, inum, inod
      integer(kind = kint) :: jst
      integer(kind = kint) :: k1, kk
      real(kind = kreal) :: dist_min
!
      integer(kind = kint) :: inod_sf_lc
      integer(kind = kint) :: n_search(nnod_4_ele)
      integer(kind = kint) :: idx_sort(nnod_4_ele)
      type(failed_item) :: fail_comm_t
!
!
      icou = 0
      do ip = 1, num_neib_e
        ist = istack_export_e(ip-1) + 1
        ied = istack_export_e(ip)
        do inum = ist, ied
          do k1 = 1, nnod_4_ele
            idx_sort(k1) = k1
            inod_sf_lc = inod_lc_export(inum,k1)
            if(ipe_lc_export(inum,k1) .eq. my_rank) then
              n_search(k1) = neib_e%istack_4_node(inod_sf_lc)           &
     &                      - neib_e%istack_4_node(inod_sf_lc-1)
            else
              n_search(k1) = 0
            end if
          end do
          call quicksort_w_index                                        &
     &       (nnod_4_ele, n_search, ione, nnod_4_ele, idx_sort)
!
          dist_min = 1.0d30
          do k1 = 1, nnod_4_ele
            kk = idx_sort(k1)
            if(ipe_lc_export(inum,kk) .ne. my_rank) cycle
!
            inod = inod_lc_export(inum,kk)
            jst = neib_e%istack_4_node(inod-1)
            if(neib_e%nele_4_node(inod) .le. ISMALL) then
              call find_ele_export_from_small(numele, x_ele,            &
     &            neib_e%nele_4_node(inod), neib_e%iele_4_node(jst+1),  &
     &            xe_export(3*inum-2), item_export_e(inum), dist_min)
            else
              call find_ele_export_from_large(numele, isum_ele, x_ele,  &
     &            neib_e%nele_4_node(inod), neib_e%iele_4_node(jst+1),  &
     &            isum_export(inum), xe_export(3*inum-2),               &
     &            item_export_e(inum), dist_min)
            end if
            if(dist_min .eq. zero) exit
          end do
!
          if(dist_min .ne. zero) then
            icou = icou + 1
            call set_failed_export(inum, item_export_e(inum), dist_min, &
     &                             fail_comm_t)
            call append_failed_export(fail_comm_t, fail_tbl)
          end if
        end do
      end do
!
      call calypso_mpi_barrier
      call calypso_mpi_allreduce_one_int(icou, num_gl, MPI_SUM)
!
      if(num_gl.gt.0 .and. my_rank .eq. 0) write(*,*)                    &
     &   'Failed export by set_element_export_item', num_gl
!
      end subroutine set_element_export_item
!
!-----------------------------------------------------------------------
!
      subroutine find_ele_export_from_small                             &
     &         (numele, x_ele, nele_4_node, iele_4_node,                &
     &          xe_export, item_export_e, dist_min)
!
      integer (kind=kint) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
!
      integer(kind = kint), intent(in) :: numele
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      real(kind = kreal), intent(inout) :: dist_min
!
      integer(kind = kint) :: jnum, jele
      real(kind= kreal) :: x_each(3)
!
!
      do jnum = 1, nele_4_node
        jele = iele_4_node(jnum)
        x_each(1:3) = x_ele(jele,1:3)
        call dist_ele_position_to_export(x_each, xe_export, dist_min)
!
        if(dist_min .eq. zero) then
          item_export_e = jele
          return
        end if
      end do
!
      end subroutine find_ele_export_from_small
!
!-----------------------------------------------------------------------
!
      subroutine find_ele_export_from_large(numele, isum_ele, x_ele,    &
     &          nele_4_node, iele_4_node, iref_sum, xe_export,          &
     &          item_export_e, dist_min)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: isum_ele(numele)
      real(kind = kreal), intent(in)  :: x_ele(numele,3)
!
      integer (kind=kint) :: nele_4_node
      integer(kind = kint), intent(in) :: iele_4_node(nele_4_node)
!
      integer(kind = kint), intent(in) :: iref_sum
      real(kind = kreal), intent(in) :: xe_export(3)
!
      integer(kind = kint), intent(inout) :: item_export_e
      real(kind = kreal), intent(inout) :: dist_min
!
      integer(kind = kint) :: ist, ied, imid, ilast, inum, jele, icou
      real(kind= kreal) :: x_each(3)
      logical :: flag_find
!
!
      flag_find = .FALSE.
      icou = 1
      ist = 1
      ied = nele_4_node
      imid = ied
      do
        ilast = imid
        jele = iele_4_node(imid)
!
        if(isum_ele(jele) .lt. iref_sum) then
          ist = imid
        else if(isum_ele(jele) .gt. iref_sum) then
          ied = imid
        else
          flag_find = .TRUE.
          exit
        end if
!
        imid = (ist + ied) / 2
        if(imid .eq. ilast) exit
        icou = icou + 1
      end do
!
      if(flag_find .EQV. .FALSE.) then
        dist_min = -1.0d0
        write(e_message,'(a)')                                          &
     &             'mathced sum of eleement ID is missimg'
        return
      end if
!
      dist_min = 1.0d17
      jele = iele_4_node(imid)
      x_each(1:3) = x_ele(jele,1:3)
      call dist_ele_position_to_export(x_each, xe_export, dist_min)
      if(dist_min .eq. zero) then
        item_export_e = jele
        return
      end if
!
      do inum = imid-1, 1, -1
        jele = iele_4_node(inum)
        if(isum_ele(jele) .ne. iref_sum) exit
!
        x_each(1:3) = x_ele(jele,1:3)
        call dist_ele_position_to_export(x_each, xe_export, dist_min)
        if(dist_min .eq. zero) then
          item_export_e = jele
          return
        end if
      end do
      do inum = imid+1, nele_4_node
        jele = iele_4_node(inum)
        if(isum_ele(jele) .ne. iref_sum) exit
!
        x_each(1:3) = x_ele(jele,1:3)
        call dist_ele_position_to_export(x_each, xe_export, dist_min)
        if(dist_min .eq. zero) then
          item_export_e = jele
          return
        end if
      end do
!
      end subroutine find_ele_export_from_large
!
! -----------------------------------------------------------------------
!
      subroutine dist_ele_position_to_export(x_each, xe_export,         &
     &                                       dist_min)
!
      real(kind = kreal), intent(in) :: x_each(3), xe_export(3)
      real(kind = kreal), intent(inout) :: dist_min
!
      real(kind = kreal) :: dist
!
!
      dist = sqrt((x_each(1) - xe_export(1))**2                         &
     &          + (x_each(2) - xe_export(2))**2                         &
     &          + (x_each(3) - xe_export(3))**2)
!
      if(dist .le. TINY) then
        dist_min = zero
      else
        dist_min = min(dist, dist_min)
      end if
!
      end subroutine dist_ele_position_to_export
!
! -----------------------------------------------------------------------
!
      end module set_element_comm_table
