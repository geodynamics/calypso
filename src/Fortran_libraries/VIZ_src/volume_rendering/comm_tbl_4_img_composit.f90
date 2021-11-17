!>@file   comm_tbl_4_img_composit.f90
!!@brief  module comm_tbl_4_img_composit
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Routies to construct communication table for image compostiion
!!
!!@verbatim
!!      subroutine count_comm_pe_pvr_composition                        &
!!     &         (nprocs, num_send_pixel_tmp, num_recv_pixel_tmp,       &
!!     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit)
!!      subroutine count_comm_tbl_pvr_composition(nprocs, my_rank,      &
!!     &          num_send_pixel_tmp, num_recv_pixel_tmp,               &
!!     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit, &
!!     &          ntot_send_pixel_composit, irank_send_pixel_composit,  &
!!     &          istack_send_pixel_composit, ntot_recv_pixel_composit, &
!!     &          irank_recv_pixel_composit, istack_recv_pixel_composit,&
!!     &          iself_pixel_composit)
!!      subroutine set_comm_tbl_pvr_composition                         &
!!     &         (num_pvr_ray, id_pixel_start, index_pvr_start,         &
!!     &          num_pixel_xy, irank_4_composit,                       &
!!     &          ncomm_send_pixel_composit, ntot_send_pixel_composit,  &
!!     &          irank_send_pixel_composit, istack_send_pixel_composit,&
!!     &          item_send_pixel_composit)
!!
!!      subroutine set_item_recv_tmp_composit(ntot_recv_pixel_composit, &
!!     &          item_recv_pixel_composit, irev_recv_pixel_composit)
!!      subroutine set_image_composition_stack(num_pixel_xy,            &
!!     &          item_4_composit, npixel_4_composit,                   &
!!     &          ntot_recv_pixel_composit, ipix_4_composit,            &
!!     &          istack_composition, idx_recv_pixel_composit)
!!      subroutine sort_recv_pixel_by_depth                             &
!!     &         (npixel_4_composit, ntot_recv_pixel_composit,          &
!!     &          depth_pixel_composit, istack_composition,             &
!!     &          idx_recv_pixel_composit, item_recv_pixel_composit)
!!@endverbatim
!!
      module comm_tbl_4_img_composit
!
      use m_precision
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_comm_pe_pvr_composition                          &
     &         (nprocs, num_send_pixel_tmp, num_recv_pixel_tmp,         &
     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit)
!
      integer, intent(in) :: nprocs
      integer(kind = kint), intent(in) :: num_send_pixel_tmp(nprocs)
      integer(kind = kint), intent(in) :: num_recv_pixel_tmp(nprocs)
!
      integer(kind = kint), intent(inout) :: ncomm_send_pixel_composit
      integer(kind = kint), intent(inout) :: ncomm_recv_pixel_composit
!
      integer :: ip
      integer(kind = kint) :: icou1, icou2
!
!
      icou1 = 0
      icou2 = 0
      do ip = 1, nprocs
        if(num_send_pixel_tmp(ip) .gt. 0) icou1 = icou1 + 1
        if(num_recv_pixel_tmp(ip) .gt. 0) icou2 = icou2 + 1
      end do
      ncomm_send_pixel_composit = icou1
      ncomm_recv_pixel_composit = icou2
!
      end subroutine count_comm_pe_pvr_composition
!
!  ---------------------------------------------------------------------
!
      subroutine count_comm_tbl_pvr_composition(nprocs, my_rank,        &
     &          num_send_pixel_tmp, num_recv_pixel_tmp,                 &
     &          ncomm_send_pixel_composit, ncomm_recv_pixel_composit,   &
     &          ntot_send_pixel_composit, irank_send_pixel_composit,    &
     &          istack_send_pixel_composit, ntot_recv_pixel_composit,   &
     &          irank_recv_pixel_composit, istack_recv_pixel_composit,  &
     &          iself_pixel_composit)
!
      integer, intent(in) :: nprocs, my_rank
      integer(kind = kint), intent(in) :: num_send_pixel_tmp(nprocs)
      integer(kind = kint), intent(in) :: num_recv_pixel_tmp(nprocs)
!
      integer(kind = kint), intent(in) :: ncomm_send_pixel_composit
      integer(kind = kint), intent(in) :: ncomm_recv_pixel_composit
!
      integer(kind = kint), intent(inout) :: iself_pixel_composit
      integer(kind = kint), intent(inout) :: ntot_send_pixel_composit
      integer(kind = kint), intent(inout)                               &
     &      :: irank_send_pixel_composit(ncomm_send_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_send_pixel_composit(0:ncomm_send_pixel_composit)
!
      integer(kind = kint), intent(inout) :: ntot_recv_pixel_composit
      integer(kind = kint), intent(inout)                               &
     &      :: irank_recv_pixel_composit(ncomm_recv_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: istack_recv_pixel_composit(0:ncomm_recv_pixel_composit)
!
      integer :: ip
      integer(kind = kint) :: icou1, icou2, i_rank
!
!
      icou1 = 0
      icou2 = 0
      iself_pixel_composit = 0
      istack_send_pixel_composit(icou1) = 0
      istack_recv_pixel_composit(icou2) = 0
      do ip = 1, nprocs
        i_rank = mod(my_rank+ip,nprocs)
        if(num_send_pixel_tmp(i_rank+1) .gt. 0) then
          icou1 = icou1 + 1
          irank_send_pixel_composit(icou1) = i_rank
          istack_send_pixel_composit(icou1)                             &
     &          = istack_send_pixel_composit(icou1-1)                   &
     &           + num_send_pixel_tmp(i_rank+1)
          if(i_rank .eq. my_rank) iself_pixel_composit = 1
        end if
        if(num_recv_pixel_tmp(i_rank+1) .gt. 0) then
          icou2 = icou2 + 1
          irank_recv_pixel_composit(icou2) = i_rank
          istack_recv_pixel_composit(icou2)                             &
     &          = istack_recv_pixel_composit(icou2-1)                   &
     &           + num_recv_pixel_tmp(i_rank+1)
          if(i_rank .eq. my_rank) iself_pixel_composit = 1
        end if
      end do
!
      ntot_send_pixel_composit                                          &
     &       = istack_send_pixel_composit(ncomm_send_pixel_composit)
      ntot_recv_pixel_composit                                          &
     &       = istack_recv_pixel_composit(ncomm_recv_pixel_composit)
!
      end subroutine count_comm_tbl_pvr_composition
!
!  ---------------------------------------------------------------------
!
      subroutine set_comm_tbl_pvr_composition                           &
     &         (num_pvr_ray, id_pixel_start, index_pvr_start,           &
     &          num_pixel_xy, irank_4_composit,                         &
     &          ncomm_send_pixel_composit, ntot_send_pixel_composit,    &
     &          irank_send_pixel_composit, istack_send_pixel_composit,  &
     &          item_send_pixel_composit)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(in) :: index_pvr_start(num_pvr_ray)
!
      integer(kind = kint), intent(in) :: ncomm_send_pixel_composit
      integer(kind = kint), intent(in) :: ntot_send_pixel_composit
      integer(kind = kint), intent(in)                                  &
     &      :: irank_send_pixel_composit(ncomm_send_pixel_composit)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_send_pixel_composit(0:ncomm_send_pixel_composit)
!
      integer(kind = kint), intent(inout)                               &
     &      :: item_send_pixel_composit(ntot_send_pixel_composit)
!
      integer(kind = kint) :: ip, jst, num
      integer(kind = kint) :: inum, icou, isrt, ipix, i_rank
!
!
      icou = 0
      do 
        isrt = index_pvr_start(icou+1)
        ipix =  id_pixel_start(isrt)
        i_rank = irank_4_composit(ipix)
        do ip = 1, ncomm_send_pixel_composit
          if(irank_send_pixel_composit(ip) .eq. i_rank) then
            jst = istack_send_pixel_composit(ip-1)
            num = istack_send_pixel_composit(ip) - jst
            do inum = 1, num
              icou = icou + 1
              isrt = index_pvr_start(icou)
              item_send_pixel_composit(inum+jst) = isrt
            end do
            exit
          end if
        end do
        if(icou .ge. num_pvr_ray) exit
      end do
!
      end subroutine set_comm_tbl_pvr_composition
!
!  ---------------------------------------------------------------------
!
      subroutine set_item_recv_tmp_composit(ntot_recv_pixel_composit,   &
     &          item_recv_pixel_composit, irev_recv_pixel_composit)
!
      integer(kind = kint), intent(in) :: ntot_recv_pixel_composit
!
      integer(kind = kint), intent(inout)                               &
     &      :: item_recv_pixel_composit(ntot_recv_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: irev_recv_pixel_composit(ntot_recv_pixel_composit)
!
      integer(kind = kint) :: inum
!
!
!$omp parallel do
      do inum = 1, ntot_recv_pixel_composit
        item_recv_pixel_composit(inum) = inum
        irev_recv_pixel_composit(inum) = inum
      end do
!$omp end parallel do
!
      end subroutine set_item_recv_tmp_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_image_composition_stack(num_pixel_xy,              &
     &          item_4_composit, npixel_4_composit,                     &
     &          ntot_recv_pixel_composit, ipix_4_composit,              &
     &          istack_composition, idx_recv_pixel_composit)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: item_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: ntot_recv_pixel_composit
      integer(kind = kint), intent(in)                                  &
     &      :: ipix_4_composit(ntot_recv_pixel_composit)
      integer(kind = kint), intent(in) :: npixel_4_composit
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_composition(0:npixel_4_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: idx_recv_pixel_composit(ntot_recv_pixel_composit)
!
      integer(kind = kint), allocatable :: itmp_recv_pixel_composit(:)
      integer(kind = kint) :: inum, ipix
!
!
      allocate(itmp_recv_pixel_composit(ntot_recv_pixel_composit))
!
!$omp parallel do
      do inum = 1, ntot_recv_pixel_composit
        ipix = ipix_4_composit(inum)
        itmp_recv_pixel_composit(inum) = item_4_composit(ipix)
        idx_recv_pixel_composit(inum) = inum
      end do
!$omp end parallel do
!
      if(ntot_recv_pixel_composit .gt. 1) then
        call quicksort_w_index                                          &
     &     (ntot_recv_pixel_composit, itmp_recv_pixel_composit,         &
     &      ione, ntot_recv_pixel_composit, idx_recv_pixel_composit)
      end if
!
!$omp parallel workshare
      istack_composition(0:npixel_4_composit) = 0
!$omp end parallel workshare
      do inum = 1, ntot_recv_pixel_composit
        ipix = itmp_recv_pixel_composit(inum)
        istack_composition(ipix) = istack_composition(ipix) + 1
      end do
      do ipix = 1, npixel_4_composit
        istack_composition(ipix) = istack_composition(ipix-1)           &
     &                            + istack_composition(ipix)
      end do
!
      deallocate(itmp_recv_pixel_composit)
!
      end subroutine set_image_composition_stack
!
!  ---------------------------------------------------------------------
!
      subroutine sort_recv_pixel_by_depth                               &
     &         (npixel_4_composit, ntot_recv_pixel_composit,            &
     &          depth_pixel_composit, istack_composition,               &
     &          idx_recv_pixel_composit, item_recv_pixel_composit)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: ntot_recv_pixel_composit
      real(kind = kreal), intent(in)                                    &
     &      :: depth_pixel_composit(ntot_recv_pixel_composit)
      integer(kind = kint), intent(in) :: npixel_4_composit
!
      integer(kind = kint), intent(in)                                  &
     &      :: istack_composition(0:npixel_4_composit)
!
      integer(kind = kint), intent(inout)                               &
     &      :: idx_recv_pixel_composit(ntot_recv_pixel_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: item_recv_pixel_composit(ntot_recv_pixel_composit)
!
      real(kind = kreal), allocatable :: rwork_recv_pixel_composit(:)
      integer(kind = kint) :: inum, ipix, ist, ied, num, icou
!
!
      allocate(rwork_recv_pixel_composit(ntot_recv_pixel_composit))
!
!$omp parallel do private(inum,icou)
      do inum = 1, ntot_recv_pixel_composit
        icou = idx_recv_pixel_composit(inum)
        rwork_recv_pixel_composit(inum) = depth_pixel_composit(icou)
      end do
!$omp end parallel do
!
!$omp parallel do private(ipix,ist,ied,num)
      do ipix = 1, npixel_4_composit
        ist = istack_composition(ipix-1)
        ied = istack_composition(ipix)
        num = ied - ist
        if(num .gt. 1) then
          call quicksort_real_w_index                                   &
     &       (num, rwork_recv_pixel_composit(ist+1),                    &
     &        ione, num, idx_recv_pixel_composit(ist+1))
        end if
      end do
!$omp end parallel do
!
!$omp parallel do private(inum,icou)
      do inum = 1, ntot_recv_pixel_composit
        icou = idx_recv_pixel_composit(inum)
        item_recv_pixel_composit(icou) = inum
      end do
!$omp end parallel do
!
      deallocate(rwork_recv_pixel_composit)
!
      end subroutine sort_recv_pixel_by_depth
!
!  ---------------------------------------------------------------------
!
      end module comm_tbl_4_img_composit
