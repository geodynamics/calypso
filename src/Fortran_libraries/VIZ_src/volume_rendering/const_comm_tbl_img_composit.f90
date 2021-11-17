!>@file   const_comm_tbl_img_composit.f90
!!@brief  module const_comm_tbl_img_composit
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Routies to construct communication table for volume rendering
!!
!!@verbatim
!!      subroutine s_const_comm_tbl_img_output                          &
!!     &         (stencil_wk, irank_image_file, num_pixel_xy,           &
!!     &          npixel_4_composit, num_pixel_recv, img_output_tbl)
!!      subroutine s_const_comm_tbl_img_composit                        &
!!     &         (irank_image_file, irank_end_composit, num_pixel_xy,   &
!!    &          irank_4_composit, num_pvr_ray, id_pixel_start,        &
!!     &          img_composit_tbl)
!!      subroutine set_image_stacking_and_recv(num_pixel_xy,            &
!!     &          item_4_composit, npixel_4_composit, ipix_4_composit,  &
!!     &          depth_pixel_composit, istack_composition,             &
!!     &          img_composit_tbl)
!!        type(calypso_comm_table), intent(inout) :: img_composit_tbl
!!@endverbatim
!!
      module const_comm_tbl_img_composit
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_calypso_comm_table
!
      implicit  none
!
      private :: sort_index_pvr_start, count_num_send_pixel_tmp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_comm_tbl_img_output                            &
     &         (stencil_wk, irank_image_file, num_pixel_xy,             &
     &          npixel_4_composit, num_pixel_recv, img_output_tbl)
!
      use t_calypso_comm_table
      use t_stencil_buffer_work
      use comm_tbl_4_img_output
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: npixel_4_composit
      type(stencil_buffer_work), intent(in) :: stencil_wk
!
      integer(kind = kint), intent(inout) :: num_pixel_recv
      type(calypso_comm_table), intent(inout) :: img_output_tbl
!
!
      call count_export_pe_pvr_output                                   &
     &   (npixel_4_composit, img_output_tbl%nrank_export)
!
      call alloc_calypso_export_num(img_output_tbl)
      call count_export_item_pvr_output                                 &
     &   (irank_image_file, npixel_4_composit,                          &
     &    img_output_tbl%nrank_export, img_output_tbl%ntot_export,      &
     &    img_output_tbl%irank_export, img_output_tbl%istack_export)
!
      call alloc_calypso_export_item(img_output_tbl)
      call set_export_item_pvr_output                                   &
     &   (img_output_tbl%ntot_export, img_output_tbl%item_export)
!
!
      call count_import_pe_pvr_output                                   &
     &   (nprocs, my_rank, irank_image_file,                            &
     &    stencil_wk%istack_recv_image, img_output_tbl%nrank_import)
!
      call alloc_calypso_import_num(img_output_tbl)
      call count_import_item_pvr_output(nprocs, my_rank,                &
     &    irank_image_file, stencil_wk%istack_recv_image, num_pixel_xy, &
     &    stencil_wk%irank_4_composit, stencil_wk%item_recv_image,      &
     &    img_output_tbl%nrank_import, img_output_tbl%ntot_import,      &
     &    img_output_tbl%irank_import, img_output_tbl%istack_import,    &
     &    img_output_tbl%iflag_self_copy, num_pixel_recv)
!
      call alloc_calypso_import_item(num_pixel_recv, img_output_tbl)
      call set_import_item_pvr_output                                   &
     &    (num_pixel_xy, stencil_wk%item_recv_image,                    &
     &     img_output_tbl%ntot_import, num_pixel_recv,                  &
     &     img_output_tbl%item_import, img_output_tbl%irev_import)
!
      end subroutine s_const_comm_tbl_img_output
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_const_comm_tbl_img_composit                          &
     &         (irank_image_file, irank_end_composit, num_pixel_xy,     &
     &          irank_4_composit, num_pvr_ray, id_pixel_start,          &
     &          img_composit_tbl)
!
      use calypso_mpi_int
      use comm_tbl_4_img_composit
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: irank_end_composit
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
!
      type(calypso_comm_table), intent(inout) :: img_composit_tbl
!
      integer(kind = kint), allocatable :: index_pvr_start(:)
      integer(kind = kint), allocatable :: num_send_pixel_tmp(:)
      integer(kind = kint), allocatable :: num_recv_pixel_tmp(:)
!
      integer :: i_rank
!
!
      allocate(index_pvr_start(num_pvr_ray))
      call sort_index_pvr_start                                         &
     &   (num_pvr_ray, id_pixel_start, index_pvr_start)
!
      allocate(num_send_pixel_tmp(nprocs))
      allocate(num_recv_pixel_tmp(nprocs))
!$omp parallel workshare
      num_send_pixel_tmp(1:nprocs) = 0
      num_recv_pixel_tmp(1:nprocs) = 0
!$omp end parallel workshare
!
      call count_num_send_pixel_tmp                                     &
     &   (num_pixel_xy, irank_4_composit, num_pvr_ray,                  &
     &    id_pixel_start, index_pvr_start, num_send_pixel_tmp)
!
      do i_rank = int(irank_image_file), int(irank_end_composit)
        call calypso_mpi_gather_one_int                                 &
     &     (num_send_pixel_tmp(i_rank+1), num_recv_pixel_tmp, i_rank)
      end do
!
      call count_comm_pe_pvr_composition                                &
     &   (nprocs, num_send_pixel_tmp, num_recv_pixel_tmp,               &
     &    img_composit_tbl%nrank_export, img_composit_tbl%nrank_import)
!
      call alloc_calypso_import_num(img_composit_tbl)
      call alloc_calypso_export_num(img_composit_tbl)
!
      call count_comm_tbl_pvr_composition(nprocs, my_rank,              &
     &   num_send_pixel_tmp, num_recv_pixel_tmp,                        &
     &   img_composit_tbl%nrank_export, img_composit_tbl%nrank_import,  &
     &   img_composit_tbl%ntot_export, img_composit_tbl%irank_export,   &
     &   img_composit_tbl%istack_export, img_composit_tbl%ntot_import,  &
     &   img_composit_tbl%irank_import, img_composit_tbl%istack_import, &
     &   img_composit_tbl%iflag_self_copy)
!
      call alloc_calypso_export_item(img_composit_tbl)
      call set_comm_tbl_pvr_composition(num_pvr_ray, id_pixel_start,    &
     &   index_pvr_start, num_pixel_xy, irank_4_composit,               &
     &   img_composit_tbl%nrank_export, img_composit_tbl%ntot_export,   &
     &   img_composit_tbl%irank_export, img_composit_tbl%istack_export, &
     &   img_composit_tbl%item_export)
!
      call alloc_calypso_import_item                                    &
     &   (img_composit_tbl%ntot_import, img_composit_tbl)
      call set_item_recv_tmp_composit(img_composit_tbl%ntot_import,     &
     &   img_composit_tbl%item_import, img_composit_tbl%irev_import)
!
      deallocate(num_send_pixel_tmp, num_recv_pixel_tmp)
      deallocate(index_pvr_start)
!
      end subroutine s_const_comm_tbl_img_composit
!
!  ---------------------------------------------------------------------
!
      subroutine set_image_stacking_and_recv(num_pixel_xy,              &
     &          item_4_composit, npixel_4_composit, ipix_4_composit,    &
     &          depth_pixel_composit, istack_composition,               &
     &          img_composit_tbl)
!
      use comm_tbl_4_img_composit
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: item_4_composit(num_pixel_xy)
!
      type(calypso_comm_table), intent(inout) :: img_composit_tbl
!
      integer(kind = kint), intent(in)                                  &
     &      :: ipix_4_composit(img_composit_tbl%ntot_import)
      real(kind = kreal), intent(in)                                    &
     &      :: depth_pixel_composit(img_composit_tbl%ntot_import)
      integer(kind = kint), intent(in) :: npixel_4_composit
!
      integer(kind = kint), intent(inout)                               &
     &      :: istack_composition(0:npixel_4_composit)
!
!
      call set_image_composition_stack                                  &
     &   (num_pixel_xy, item_4_composit, npixel_4_composit,             &
     &    img_composit_tbl%ntot_import, ipix_4_composit,                &
     &    istack_composition, img_composit_tbl%irev_import)
!
      call sort_recv_pixel_by_depth                                     &
     &    (npixel_4_composit, img_composit_tbl%ntot_import,             &
     &     depth_pixel_composit, istack_composition,                    &
     &     img_composit_tbl%irev_import, img_composit_tbl%item_import)
!
      end subroutine set_image_stacking_and_recv
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sort_index_pvr_start                                   &
     &         (num_pvr_ray, id_pixel_start, index_pvr_start)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(inout)                               &
     &              :: index_pvr_start(num_pvr_ray)
!
      integer(kind = kint), allocatable :: iref_pvr_start(:)
      integer(kind = kint) :: inum
!
!
      allocate(iref_pvr_start(num_pvr_ray))
!
      do inum = 1, num_pvr_ray
        index_pvr_start(inum) = inum
        iref_pvr_start(inum) = id_pixel_start(inum)
      end do
!
      if(num_pvr_ray .gt. 1) then
        call quicksort_w_index(num_pvr_ray, iref_pvr_start,             &
     &      ione, num_pvr_ray, index_pvr_start)
      end if
      deallocate(iref_pvr_start)
!
      end subroutine sort_index_pvr_start
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_send_pixel_tmp                               &
     &         (num_pixel_xy, irank_4_composit, num_pvr_ray,            &
     &          id_pixel_start, index_pvr_start, num_send_pixel_tmp)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in)                                  &
     &                     :: irank_4_composit(num_pixel_xy)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      integer(kind = kint), intent(in) :: index_pvr_start(num_pvr_ray)
!
      integer(kind = kint), intent(inout) :: num_send_pixel_tmp(nprocs)
!
      integer(kind = kint) :: inum, isrt, ipix, ip
!
!
!$omp parallel workshare
      num_send_pixel_tmp(1:nprocs) = 0
!$omp end parallel workshare
      do inum = 1, num_pvr_ray
        isrt = index_pvr_start(inum)
        ipix =  id_pixel_start(isrt)
        ip = irank_4_composit(ipix) + 1
        num_send_pixel_tmp(ip) = num_send_pixel_tmp(ip) + 1
      end do
!
      end subroutine count_num_send_pixel_tmp
!
!  ---------------------------------------------------------------------
!
      end module const_comm_tbl_img_composit
