!>@file   t_ctl_data_view_transfers.f90
!!@brief  module t_ctl_data_view_transfers
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!>@brief Control inputs for multiple PVR view parameter
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine dealloc_multi_modeview_ctl(mul_mats_c)
!!      subroutine read_mul_view_transfer_ctl                           &
!!     &         (id_control, hd_block, mul_mats_c, c_buf)
!!        type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine bcast_mul_view_trans_ctl(mul_mats_c)
!!        type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!!
!!      subroutine append_mul_view_trans_ctl(mul_mats_c)
!!        type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!!      subroutine dup_mul_view_trans_ctl(org_mul_mats_c,               &
!!     &                                  new_mul_mats_c)
!!        type(multi_modeview_ctl), intent(in) :: org_mul_mats_c
!!        type(multi_modeview_ctl), intent(inout) :: new_mul_mats_c
!!      subroutine copy_mul_view_trans_ctl                              &
!!     &         (num_mat, org_mat_ctl, new_mat_ctl)
!!        type(modeview_and_fname_ctl), intent(in)                      &
!!     &                       :: org_mat_ctl(num_mat)
!!        type(modeview_and_fname_ctl), intent(inout)                   &
!!     &                       :: new_mat_ctl(num_mat)
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
      use calypso_mpi
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
!>        Structure of modelview parameter or file name to load
      type modeview_and_fname_ctl
!>         file name for external view parameter control
        character(len = kchara) :: fname_modelviews_ctl
!>         Lists of view parameters
        type(modeview_ctl) :: matrices
      end type modeview_and_fname_ctl
!
!
!>        Structure of modelview parameters or file names to load
      type multi_modeview_ctl
!>         Number of modelview parameter block
        integer(kind = kint) :: num_modelviews_c = 0
!>        Structure of modelview parameter or file name to load
       type(modeview_and_fname_ctl), allocatable :: mat_file_ctl(:)
      end type multi_modeview_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_multi_modeview_ctl(mul_mats_c)
!
      type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!
!
     if(allocated(mul_mats_c%mat_file_ctl)) then
        call dealloc_mul_view_trans_ctl                                 &
     &     (mul_mats_c%num_modelviews_c, mul_mats_c%mat_file_ctl)
        deallocate(mul_mats_c%mat_file_ctl)
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
      type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!
!
      allocate(mul_mats_c%mat_file_ctl(mul_mats_c%num_modelviews_c))
!
      end subroutine alloc_multi_modeview_ctl
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_mul_view_transfer_ctl                             &
     &         (id_control, hd_block, mul_mats_c, c_buf)
!
      use read_control_pvr_modelview
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(multi_modeview_ctl), intent(inout) :: mul_mats_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: icou
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(mul_mats_c%mat_file_ctl)) return
      mul_mats_c%num_modelviews_c = 0
      call alloc_multi_modeview_ctl(mul_mats_c)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_file_flag(c_buf, hd_block)) then
          call append_mul_view_trans_ctl(mul_mats_c)
          icou = mul_mats_c%num_modelviews_c
          mul_mats_c%mat_file_ctl(icou)%fname_modelviews_ctl            &
     &         = third_word(c_buf)
          write(*,'(2a,i4)', ADVANCE='NO') trim(hd_block),              &
     &                           ' No. ', mul_mats_c%num_modelviews_c
          write(*,'(a)', ADVANCE='NO') ' is read from file... '
          call read_control_modelview_file(id_control+2,                &
     &        mul_mats_c%mat_file_ctl(icou)%fname_modelviews_ctl,       &
     &        mul_mats_c%mat_file_ctl(icou)%matrices)
        end if
        if(check_begin_flag(c_buf, hd_block)) then
          call append_mul_view_trans_ctl(mul_mats_c)
          icou = mul_mats_c%num_modelviews_c
          mul_mats_c%mat_file_ctl(icou)%fname_modelviews_ctl            &
     &         = 'NO_FILE'
          call read_view_transfer_ctl(id_control, hd_block,             &
     &        mul_mats_c%mat_file_ctl(icou)%matrices, c_buf)
          write(*,'(2a,i4)', ADVANCE='NO') trim(hd_block),              &
     &                           ' No. ', mul_mats_c%num_modelviews_c
          write(*,*) ' is included'
        end if
      end do
!
      end subroutine read_mul_view_transfer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_mul_view_trans_ctl(mul_mats_c)
!
      use bcast_control_arrays
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_dup_view_transfer_ctl
!
      type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!
      integer(kind = kint) :: i, num
!
!
      call calypso_mpi_bcast_one_int(mul_mats_c%num_modelviews_c, 0)
      if(mul_mats_c%num_modelviews_c .gt. 0 .and. my_rank .gt. 0) then
        num = mul_mats_c%num_modelviews_c
        allocate(mul_mats_c%mat_file_ctl(num))
      end if
!
      do i = 1, mul_mats_c%num_modelviews_c
        call calypso_mpi_bcast_character                                &
     &     (mul_mats_c%mat_file_ctl(i)%fname_modelviews_ctl,            &
     &      cast_long(kchara), 0)
!
        call bcast_view_transfer_ctl                                    &
     &     (mul_mats_c%mat_file_ctl(i)%matrices)
      end do
!
      end subroutine bcast_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_mul_view_trans_ctl(mul_mats_c)
!
      type(multi_modeview_ctl), intent(inout) :: mul_mats_c
!
      type(multi_modeview_ctl) :: tmp_mul_qmats
!
!
      tmp_mul_qmats%num_modelviews_c = mul_mats_c%num_modelviews_c
      call alloc_multi_modeview_ctl(tmp_mul_qmats)
      call copy_mul_view_trans_ctl(tmp_mul_qmats%num_modelviews_c,      &
     &    mul_mats_c%mat_file_ctl, tmp_mul_qmats%mat_file_ctl)
!
      call dealloc_multi_modeview_ctl(mul_mats_c)
!
      mul_mats_c%num_modelviews_c = tmp_mul_qmats%num_modelviews_c + 1
      call alloc_multi_modeview_ctl(mul_mats_c)
!
      call copy_mul_view_trans_ctl(tmp_mul_qmats%num_modelviews_c,      &
     &    tmp_mul_qmats%mat_file_ctl, mul_mats_c%mat_file_ctl(1))
!
      call dealloc_multi_modeview_ctl(tmp_mul_qmats)
!
      end subroutine append_mul_view_trans_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_mul_view_trans_ctl(org_mul_mats_c,                 &
     &                                  new_mul_mats_c)
!
      type(multi_modeview_ctl), intent(in) :: org_mul_mats_c
      type(multi_modeview_ctl), intent(inout) :: new_mul_mats_c
!
!
      new_mul_mats_c%num_modelviews_c                                   &
     &     = org_mul_mats_c%num_modelviews_c
      call alloc_multi_modeview_ctl(new_mul_mats_c)
      call copy_mul_view_trans_ctl(org_mul_mats_c%num_modelviews_c,     &
     &                             org_mul_mats_c%mat_file_ctl,         &
     &                             new_mul_mats_c%mat_file_ctl)
!
      end subroutine dup_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_mul_view_trans_ctl                                &
     &         (num_mat, org_mat_ctl, new_mat_ctl)
!
      use bcast_dup_view_transfer_ctl
!
      integer(kind = kint), intent(in) :: num_mat
      type(modeview_and_fname_ctl), intent(in)                          &
     &                             :: org_mat_ctl(num_mat)
      type(modeview_and_fname_ctl), intent(inout)                       &
     &                             :: new_mat_ctl(num_mat)
!
      integer(kind = kint) :: i
!
      do i = 1, num_mat
        call dup_view_transfer_ctl(org_mat_ctl(i)%matrices,             &
     &                             new_mat_ctl(i)%matrices)
        new_mat_ctl(i)%fname_modelviews_ctl                             &
     &     = org_mat_ctl(i)%fname_modelviews_ctl
      end do
!
      end subroutine copy_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_mul_view_trans_ctl(num_mat, mat_file_ctl)
!
      integer(kind = kint), intent(in) :: num_mat
      type(modeview_and_fname_ctl), intent(inout)                       &
     &                             :: mat_file_ctl(num_mat)
!
      integer(kind = kint) :: i
!
!
      do i = 1, num_mat
        call dealloc_view_transfer_ctl(mat_file_ctl(i)%matrices)
      end do
!
      end subroutine dealloc_mul_view_trans_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_view_transfers
