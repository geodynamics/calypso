!>@file   t_ctl_data_view_transfers.f90
!!@brief  module t_ctl_data_view_transfers
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!>@brief Control inputs for multiple PVR view parameter
!!
!!@verbatim
!!      subroutine alloc_multi_modeview_ctl(mul_mats_c)
!!      subroutine dealloc_multi_modeview_ctl(mul_mats_c)
!!      subroutine init_multi_modeview_ctl(hd_block, mul_mats_c)
!!
!!      subroutine read_mul_view_transfer_ctl                           &
!!     &         (id_control, hd_block, mul_mats_c, c_buf)
!!        type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_mul_view_transfer_ctl                          &
!!     &         (id_control, hd_block, mul_mats_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(multi_modelview_ctl), intent(in) :: mul_mats_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine append_mul_view_trans_ctl(idx_in, hd_block,          &
!!     &                                     mul_mats_c)
!!      subroutine delete_mul_view_trans_ctl(idx_in, mul_mats_c)
!!        integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!        type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!!      subroutine dup_mul_view_trans_ctl(org_mul_mats_c,               &
!!     &                                  new_mul_mats_c)
!!        type(multi_modelview_ctl), intent(in) :: org_mul_mats_c
!!        type(multi_modelview_ctl), intent(inout) :: new_mul_mats_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array view_transform_ctl
!!      file  view_transform_ctl  control_view
!!
!!      begin view_transform_ctl
!!        ..
!!      end
!!    end array view_transform_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_view_transfers
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_4_view_transfer
      use skip_comment_f
!
      implicit  none
!
!
!>        Structure of modelview parameters or file names to load
      type multi_modelview_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'isosurface_ctl'
!
!>         Number of modelview parameter block
        integer(kind = kint) :: num_modelviews_c = 0
!>        File name for external control file
        character(len=kchara), allocatable :: fname_mat_ctl(:)
!>         Lists of view parameters
        type(modeview_ctl), allocatable :: matrices(:)
      end type multi_modelview_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_multi_modeview_ctl(mul_mats_c)
!
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!
!
     if(allocated(mul_mats_c%matrices)) then
        call dealloc_mul_view_trans_ctl                                 &
     &     (mul_mats_c%num_modelviews_c, mul_mats_c%matrices)
        deallocate(mul_mats_c%matrices, mul_mats_c%fname_mat_ctl)
      end if
!
      mul_mats_c%num_modelviews_c = 0
!
      end subroutine dealloc_multi_modeview_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_multi_modeview_ctl(mul_mats_c)
!
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!
!
      allocate(mul_mats_c%matrices(mul_mats_c%num_modelviews_c))
      allocate(mul_mats_c%fname_mat_ctl(mul_mats_c%num_modelviews_c))
!
      end subroutine alloc_multi_modeview_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_multi_modeview_ctl(hd_block, mul_mats_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!
!
      mul_mats_c%block_name =       hd_block
      mul_mats_c%num_modelviews_c = 0
!
      end subroutine init_multi_modeview_ctl
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_mul_view_transfer_ctl                             &
     &         (id_control, hd_block, mul_mats_c, c_buf)
!
      use ctl_file_pvr_modelview_IO
      use ctl_data_view_transfer_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(mul_mats_c%matrices)) return
      call alloc_multi_modeview_ctl(mul_mats_c)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)                             &
     &        .or. check_begin_flag(c_buf, hd_block)) then
          n_append = mul_mats_c%num_modelviews_c
          call append_mul_view_trans_ctl(n_append, hd_block,            &
     &                                   mul_mats_c)
!
          call sel_read_ctl_modelview_file                              &
     &       (id_control, hd_block, mul_mats_c%num_modelviews_c,        &
     &        mul_mats_c%fname_mat_ctl(mul_mats_c%num_modelviews_c),    &
     &        mul_mats_c%matrices(mul_mats_c%num_modelviews_c), c_buf)
        end if
      end do
!
      end subroutine read_mul_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_mul_view_transfer_ctl                            &
     &         (id_control, hd_block, mul_mats_c, level)
!
      use ctl_file_pvr_modelview_IO
      use ctl_data_view_transfer_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(multi_modelview_ctl), intent(in) :: mul_mats_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, mul_mats_c%num_modelviews_c
        write(*,'(3a,i4)', ADVANCE='NO') '!  ', trim(hd_block),         &
     &                                   ' No. ', i
!
        call sel_write_ctl_modelview_file(id_control, hd_block,         &
     &      mul_mats_c%fname_mat_ctl(i), mul_mats_c%matrices(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_mul_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_mul_view_trans_ctl(idx_in, hd_block,            &
     &                                     mul_mats_c)
!
      use ctl_data_view_transfer_IO
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!
      type(multi_modelview_ctl) :: tmp_mul_qmats
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0 .or. idx_in.gt.mul_mats_c%num_modelviews_c) return
!
      tmp_mul_qmats%num_modelviews_c = mul_mats_c%num_modelviews_c
      call alloc_multi_modeview_ctl(tmp_mul_qmats)
!
      do i = 1, tmp_mul_qmats%num_modelviews_c
        call dup_view_transfer_ctl(mul_mats_c%matrices(i),              &
     &                             tmp_mul_qmats%matrices(i))
        tmp_mul_qmats%fname_mat_ctl(i) = mul_mats_c%fname_mat_ctl(i)
      end do
!
      call dealloc_multi_modeview_ctl(mul_mats_c)
      mul_mats_c%num_modelviews_c = tmp_mul_qmats%num_modelviews_c + 1
      call alloc_multi_modeview_ctl(mul_mats_c)
!
      do i = 1, idx_in
        call dup_view_transfer_ctl(tmp_mul_qmats%matrices(i),           &
     &                             mul_mats_c%matrices(i))
        mul_mats_c%fname_mat_ctl(i) = tmp_mul_qmats%fname_mat_ctl(i)
      end do
!
      call init_view_transfer_ctl_label(hd_block,                       &
     &                                  mul_mats_c%matrices(idx_in+1))
      mul_mats_c%fname_mat_ctl(idx_in+1) = 'NO_FILE'
!
      do i = idx_in+1, tmp_mul_qmats%num_modelviews_c
        call dup_view_transfer_ctl(tmp_mul_qmats%matrices(i),           &
     &                             mul_mats_c%matrices(i+1))
        mul_mats_c%fname_mat_ctl(i+1) = tmp_mul_qmats%fname_mat_ctl(i)
      end do
!
      call dealloc_multi_modeview_ctl(tmp_mul_qmats)
!
      end subroutine append_mul_view_trans_ctl
!
! -----------------------------------------------------------------------
!
      subroutine delete_mul_view_trans_ctl(idx_in, mul_mats_c)
!
      integer(kind = kint), intent(in) :: idx_in
      type(multi_modelview_ctl), intent(inout) :: mul_mats_c
!
      type(multi_modelview_ctl) :: tmp_mul_qmats
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.mul_mats_c%num_modelviews_c) return
!
      tmp_mul_qmats%num_modelviews_c = mul_mats_c%num_modelviews_c
      call alloc_multi_modeview_ctl(tmp_mul_qmats)
!
      do i = 1, tmp_mul_qmats%num_modelviews_c
        call dup_view_transfer_ctl(mul_mats_c%matrices(i),              &
     &                             tmp_mul_qmats%matrices(i))
        tmp_mul_qmats%fname_mat_ctl(i) = mul_mats_c%fname_mat_ctl(i)
      end do
!
      call dealloc_multi_modeview_ctl(mul_mats_c)
      mul_mats_c%num_modelviews_c = tmp_mul_qmats%num_modelviews_c + 1
      call alloc_multi_modeview_ctl(mul_mats_c)
!
      do i = 1, idx_in-1
        call dup_view_transfer_ctl(tmp_mul_qmats%matrices(i),           &
     &                             mul_mats_c%matrices(i))
        mul_mats_c%fname_mat_ctl(i) = tmp_mul_qmats%fname_mat_ctl(i)
      end do
      do i = idx_in, mul_mats_c%num_modelviews_c
        call dup_view_transfer_ctl(tmp_mul_qmats%matrices(i+1),         &
     &                             mul_mats_c%matrices(i))
        mul_mats_c%fname_mat_ctl(i) = tmp_mul_qmats%fname_mat_ctl(i+1)
      end do
!
      call dealloc_multi_modeview_ctl(tmp_mul_qmats)
!
      end subroutine delete_mul_view_trans_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_mul_view_trans_ctl(org_mul_mats_c,                 &
     &                                  new_mul_mats_c)
!
      type(multi_modelview_ctl), intent(in) :: org_mul_mats_c
      type(multi_modelview_ctl), intent(inout) :: new_mul_mats_c
!
      integer(kind = kint) :: i
!
      new_mul_mats_c%block_name = org_mul_mats_c%block_name
      new_mul_mats_c%num_modelviews_c                                   &
     &     = org_mul_mats_c%num_modelviews_c
      call alloc_multi_modeview_ctl(new_mul_mats_c)
!
      do i = 1, new_mul_mats_c%num_modelviews_c
        call dup_view_transfer_ctl(org_mul_mats_c%matrices(i),          &
     &                             new_mul_mats_c%matrices(i))
        new_mul_mats_c%fname_mat_ctl(i)                                 &
     &     = org_mul_mats_c%fname_mat_ctl(i)
      end do
!
      end subroutine dup_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mul_view_trans_ctl(num_mat, matrices)
!
      integer(kind = kint), intent(in) :: num_mat
      type(modeview_ctl), intent(inout) :: matrices(num_mat)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_mat
        call dealloc_view_transfer_ctl(matrices(i))
      end do
!
      end subroutine dealloc_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_view_transfers
