!>@file   set_pvr_stencil_buffer.f90
!!@brief  module set_pvr_stencil_buffer
!!
!!@author H. Matsui
!!@date Programmed on  Oct., 2016
!
!>@brief  Set communication table and imagae comsition stack
!!
!!@verbatim
!!      subroutine s_set_pvr_stencil_buffer                             &
!!     &         (irank_image_file, irank_end_composit, num_pixel_xy,   &
!!     &          elps_PVR, pvr_start, stencil_wk, num_pixel_recv,      &
!!     &          img_output_tbl, img_composit_tbl, img_stack,          &
!!     &          SR_sig, SR_r, SR_i)
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(pvr_ray_start_type), intent(in) :: pvr_start
!!        type(stencil_buffer_work), intent(in)  :: stencil_wk
!!        type(calypso_comm_table), intent(inout) :: img_output_tbl
!!        type(calypso_comm_table), intent(inout) :: img_composit_tbl
!!        type(pvr_image_stack_table), intent(inout) :: img_stack
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
!!@endverbatim
!!
      module set_pvr_stencil_buffer
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_calypso_comm_table
      use t_pvr_ray_startpoints
      use t_pvr_image_stack_table
      use t_stencil_buffer_work
      use t_solver_SR
      use t_solver_SR_int
!
      implicit  none
!
      character(len=kchara), parameter, private                         &
     &                      :: check_fhead = 'pvr_composition_check'
      integer(kind = kint), parameter, private :: id_file = 49
!
      private :: num_parallel_stencil_buffer
      private :: set_global_pixel_4_composit
      private :: check_composit_communication
      private :: check_img_output_communication
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_stencil_buffer                               &
     &         (irank_image_file, irank_end_composit, num_pixel_xy,     &
     &          elps_PVR, pvr_start, stencil_wk, num_pixel_recv,        &
     &          img_output_tbl, img_composit_tbl, img_stack,            &
     &          SR_sig, SR_r, SR_i)
!
      use m_work_time
      use quicksort
      use calypso_SR_type
      use const_comm_tbl_img_composit
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: irank_image_file
      integer(kind = kint), intent(in) :: irank_end_composit
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(elapsed_lables), intent(in) :: elps_PVR
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(stencil_buffer_work), intent(in)  :: stencil_wk
!
      integer(kind = kint), intent(inout) :: num_pixel_recv
      type(calypso_comm_table), intent(inout) :: img_output_tbl
      type(calypso_comm_table), intent(inout) :: img_composit_tbl
      type(pvr_image_stack_table), intent(inout) :: img_stack
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      character(len=kchara) :: fname_tmp, file_name
!
!
!      write(*,*) 'num_parallel_stencil_buffer'
      img_stack%npixel_4_composit                                       &
     &           = num_parallel_stencil_buffer(stencil_wk)
!
!
!      write(*,*) 's_const_comm_tbl_img_output'
      if(elps_PVR%flag_elapsed)                                         &
     &       call start_elapsed_time(elps_PVR%ist_elapsed+8)
      call s_const_comm_tbl_img_output                                  &
     &   (stencil_wk, irank_image_file, num_pixel_xy,                   &
     &    img_stack%npixel_4_composit, num_pixel_recv, img_output_tbl)
      if(elps_PVR%flag_elapsed)                                         &
     &       call end_elapsed_time(elps_PVR%ist_elapsed+8)
!
!
!      write(*,*) 'set_global_pixel_4_composit'
      call alloc_pvr_ipixel_4_composit(num_pixel_xy, img_stack)
      call set_global_pixel_4_composit                                  &
     &   (stencil_wk, img_stack%npixel_4_composit, num_pixel_xy,        &
     &    img_stack%ipixel_4_composit, img_stack%item_4_composit)
!
!
!
!      write(*,*) 's_const_comm_tbl_img_composit'
      if(elps_PVR%flag_elapsed)                                         &
     &       call start_elapsed_time(elps_PVR%ist_elapsed+9)
      call s_const_comm_tbl_img_composit                                &
     &   (irank_image_file, irank_end_composit,                         &
     &    num_pixel_xy, stencil_wk%irank_4_composit,                    &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    img_composit_tbl)
      if(elps_PVR%flag_elapsed)                                         &
     &       call end_elapsed_time(elps_PVR%ist_elapsed+9)
!
!
      call alloc_depth_pixel_composit(pvr_start%num_pvr_ray,            &
     &    img_composit_tbl%ntot_import, img_stack)
!
!$omp parallel workshare
      img_stack%depth_pvr_ray_start(1:pvr_start%num_pvr_ray)            &
     &      = - pvr_start%xx4_pvr_ray_start(3,1:pvr_start%num_pvr_ray)
!$omp end parallel workshare
!
      if(elps_PVR%flag_elapsed)                                         &
     &       call start_elapsed_time(elps_PVR%ist_elapsed+10)
      call calypso_SR_type_int(0, img_composit_tbl,                     &
     &    pvr_start%num_pvr_ray, img_composit_tbl%ntot_import,          &
     &    pvr_start%id_pixel_start, img_stack%ipix_4_composit,          &
     &    SR_sig, SR_i)
      if(elps_PVR%flag_elapsed)                                         &
     &       call end_elapsed_time(elps_PVR%ist_elapsed+10)
!
      if(elps_PVR%flag_elapsed)                                         &
     &       call start_elapsed_time(elps_PVR%ist_elapsed+11)
      call calypso_SR_type_1(0, img_composit_tbl,                       &
     &   pvr_start%num_pvr_ray, img_composit_tbl%ntot_import,           &
     &   img_stack%depth_pvr_ray_start, img_stack%depth_pixel_composit, &
     &   SR_sig, SR_r)
      if(elps_PVR%flag_elapsed)                                         &
     &       call end_elapsed_time(elps_PVR%ist_elapsed+11)
!
      call alloc_pvr_image_stack_table(img_stack)
      call set_image_stacking_and_recv                                  &
     &   (num_pixel_xy, img_stack%item_4_composit,                      &
     &    img_stack%npixel_4_composit, img_stack%ipix_4_composit,       &
     &    img_stack%depth_pixel_composit, img_stack%istack_composition, &
     &    img_composit_tbl)
!
      if(i_debug .gt. 0) then
        fname_tmp = add_int_suffix(my_rank, check_fhead)
        file_name = add_dat_extension(fname_tmp)
        open(id_file, file = file_name)
        call check_img_output_communication(id_file,                    &
     &      img_stack, img_output_tbl, stencil_wk,                      &
     &      num_pixel_xy, num_pixel_recv, SR_sig, SR_i)
!
        call check_composit_communication(id_file,                      &
     &      pvr_start, img_composit_tbl, img_stack)
        close(id_file)
!
        call dealloc_pvr_ipixel_4_composit(img_stack)
        call dealloc_depth_pixel_composit(img_stack)
      end if
!
      end subroutine s_set_pvr_stencil_buffer
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    num_parallel_stencil_buffer(stencil_wk)
!
      type(stencil_buffer_work), intent(in) :: stencil_wk
!
      integer(kind = kint) :: ip, ist, ipix, num
!
!
      num_parallel_stencil_buffer = 0
      do ip = 1, nprocs
        ist = stencil_wk%istack_recv_image(ip-1)
        num = stencil_wk%istack_recv_image(ip) - ist
        if(num .le. 0) cycle
!
        ipix = stencil_wk%item_recv_image(ist+1)
        if(stencil_wk%irank_4_composit(ipix) .eq. my_rank) then
          num_parallel_stencil_buffer = num
          exit
        end if
      end do
!
      end function num_parallel_stencil_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine set_global_pixel_4_composit                            &
     &         (stencil_wk, npixel_4_composit, num_pixel_xy,            &
     &          ipixel_4_composit, item_4_composit)
!
      type(stencil_buffer_work), intent(in) :: stencil_wk
      integer(kind = kint), intent(in) :: npixel_4_composit
      integer(kind = kint), intent(in) :: num_pixel_xy
!
      integer(kind = kint), intent(inout)                               &
     &      :: ipixel_4_composit(npixel_4_composit)
      integer(kind = kint), intent(inout)                               &
     &      :: item_4_composit(num_pixel_xy)
!
      integer(kind = kint) :: ip, inum, ipix, ist
!
!
!$omp parallel workshare
      item_4_composit(1:num_pixel_xy) = 0
!$omp end parallel workshare
!
      do ip = 1, nprocs
        ist = stencil_wk%istack_recv_image(ip-1)
        ipix = stencil_wk%item_recv_image(ist+1)
        if(stencil_wk%irank_4_composit(ipix) .eq. my_rank) then
          do inum = 1, npixel_4_composit
            ipix = stencil_wk%item_recv_image(ist+inum)
            ipixel_4_composit(inum) = ipix
            item_4_composit(ipix) = inum
          end do
          exit
        end if
      end do
!
      end subroutine set_global_pixel_4_composit
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_composit_communication                           &
     &         (id_file, pvr_start, img_composit_tbl, img_stack)
!
      integer(kind = kint), intent(in) :: id_file
      type(pvr_ray_start_type), intent(in) :: pvr_start
      type(calypso_comm_table), intent(in) :: img_composit_tbl
      type(pvr_image_stack_table), intent(in) :: img_stack
!
      integer(kind = kint) :: inum, ipix, icou, ist, num, ip
!
!
      write(id_file,*) 'nrank_export', img_composit_tbl%nrank_export
      do ip = 1, img_composit_tbl%nrank_export
        ist = img_composit_tbl%istack_export(ip-1)
        num = img_composit_tbl%istack_export(ip) - ist
        write(id_file,*) 'img_composit_tbl%irank_export',               &
     &        ip, img_composit_tbl%irank_export(ip), ist, num
        do inum = 1, num
          icou = img_composit_tbl%item_export(ist+inum)
          write(id_file,*) inum, icou,                                  &
     &                pvr_start%id_pixel_start(icou),                   &
     &                pvr_start%xx4_pvr_ray_start(3,icou)
        end do
      end do
!
      write(id_file,*) 'img_composit_tbl%ntot_import',                  &
     &       img_composit_tbl%ntot_import, img_stack%npixel_4_composit
      do ipix = 1, img_stack%npixel_4_composit
        ist = img_stack%istack_composition(ipix-1)
        num = img_stack%istack_composition(ipix) - ist
        write(id_file,*) 'img_composit_tbl%item_import', ist, num
        do inum = 1, num
          icou = img_composit_tbl%item_import(ist+inum)
          write(id_file,*) inum, ipix, icou,                            &
     &        img_stack%ipix_4_composit(icou),                          &
     &        img_stack%depth_pixel_composit(icou)
        end do
      end do
      close(id_file)
!
      end subroutine check_composit_communication
!
!  ---------------------------------------------------------------------
!
      subroutine check_img_output_communication(id_file,                &
     &          img_stack, img_output_tbl, stencil_wk,                  &
     &          num_pixel_xy, num_pixel_recv, SR_sig, SR_i)
!
      use calypso_SR_type
!
      integer(kind = kint), intent(in) :: id_file
      type(pvr_image_stack_table), intent(in) :: img_stack
      type(calypso_comm_table), intent(in) :: img_output_tbl
      type(stencil_buffer_work), intent(in)  :: stencil_wk
      integer(kind = kint), intent(in) :: num_pixel_xy, num_pixel_recv
!
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_int_buffer), intent(inout) :: SR_i
!
      integer(kind = kint), allocatable :: ipixel_check(:)
      integer(kind = kint) :: ipix
!
!
      allocate(ipixel_check(num_pixel_recv))
      call calypso_SR_type_int(0, img_output_tbl,                       &
     &    img_stack%npixel_4_composit, num_pixel_recv,                  &
     &    img_stack%ipixel_4_composit, ipixel_check, SR_sig, SR_i)
!
      write(id_file,*) 'ipixel_check', num_pixel_recv, num_pixel_xy
      do ipix = 1, num_pixel_recv
        write(id_file,*)                                                &
             ipix, ipixel_check(ipix), stencil_wk%irev_recv_image(ipix)
      end do
      deallocate(ipixel_check)
!
      end subroutine check_img_output_communication
!
!  ---------------------------------------------------------------------
!
      end module set_pvr_stencil_buffer
