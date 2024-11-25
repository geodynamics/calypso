!>@file   t_control_data_pvr_tracers.f90
!!@brief  module t_control_data_pvr_tracers
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_tracers_ctl                                &
!!     &         (id_control, hd_block, pvr_tracers_c, c_buf)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_pvr_tracers_ctl                               &
!!     &         (id_control, hd_block, pvr_tracers_c, level)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(pvr_tracers_ctl), intent(in) :: pvr_tracers_c
!!         integer(kind = kint), intent(inout) :: level
!!      subroutine alloc_pvr_tracers_ctl(pvr_tracers_c)
!!      subroutine dealloc_pvr_tracers_ctl(pvr_tracers_c)
!!      subroutine init_pvr_tracerss_ctl(hd_block, pvr_tracers_c)
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine dup_pvr_tracers_ctl(org_pvr_iso_c, new_pvr_isos_c)
!!        type(pvr_tracers_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_tracers_ctl), intent(inout) :: new_pvr_isos_c
!!
!!      subroutine append_pvr_tracers_ctl(idx_in, hd_block,             &
!!     &                                  pvr_tracers_c)
!!      subroutine delete_pvr_tracers_ctl(idx_in, pvr_tracers_c)
!!        integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array isosurface_ctl
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_tracers
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_ctl_data_pvr_tracer
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_tracers_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'tracers_ctl'
!
        integer(kind = kint) :: num_pvr_tracer_ctl = 0
        type(pvr_tracer_ctl), allocatable :: pvr_trc_c(:)
      end type pvr_tracers_ctl
!
      private :: reset_pvr_tracers_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_tracers_ctl                                   &
     &         (id_control, hd_block, pvr_tracers_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(pvr_tracers_c%pvr_trc_c)) return
      call alloc_pvr_tracers_ctl(pvr_tracers_c)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          n_append = pvr_tracers_c%num_pvr_tracer_ctl
          call append_pvr_tracers_ctl(n_append, hd_block,               &
     &                                pvr_tracers_c)
          call read_pvr_tracer_ctl(id_control, hd_block,                &
     &       pvr_tracers_c%pvr_trc_c(pvr_tracers_c%num_pvr_tracer_ctl), &
     &       c_buf)
        end if
      end do
!
      end subroutine read_pvr_tracers_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_pvr_tracers_ctl                                  &
     &         (id_control, hd_block, pvr_tracers_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracers_ctl), intent(in) :: pvr_tracers_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(pvr_tracers_c%num_pvr_tracer_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, pvr_tracers_c%num_pvr_tracer_ctl
        call write_pvr_tracer_ctl(id_control, hd_block,                 &
     &      pvr_tracers_c%pvr_trc_c(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     hd_block)
!
      end subroutine write_pvr_tracers_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pvr_tracers_ctl(pvr_tracers_c)
!
      type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
!
!
      if(allocated(pvr_tracers_c%pvr_trc_c)) then
        call reset_pvr_tracers_ctl(pvr_tracers_c)
        deallocate(pvr_tracers_c%pvr_trc_c)
      end if
!
      pvr_tracers_c%num_pvr_tracer_ctl = 0
!
      end subroutine dealloc_pvr_tracers_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_tracers_ctl(pvr_tracers_c)
!
      type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
      integer(kind = kint) :: num
!
      num = pvr_tracers_c%num_pvr_tracer_ctl
      allocate(pvr_tracers_c%pvr_trc_c(num))
      call reset_pvr_tracers_ctl(pvr_tracers_c)
!
      end subroutine alloc_pvr_tracers_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_pvr_tracerss_ctl(hd_block, pvr_tracers_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
!
!
      pvr_tracers_c%block_name = hd_block
      pvr_tracers_c%num_pvr_tracer_ctl = 0
!
      end subroutine init_pvr_tracerss_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine append_pvr_tracers_ctl(idx_in, hd_block,               &
     &                                  pvr_tracers_c)
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
!
      type(pvr_tracers_ctl) :: pvr_trcs_tmp
      integer(kind = kint) :: i
!
!
      if(idx_in.lt.0                                                    &
     &      .or. idx_in.gt.pvr_tracers_c%num_pvr_tracer_ctl) return
!
      pvr_trcs_tmp%num_pvr_tracer_ctl                                   &
     &      = pvr_tracers_c%num_pvr_tracer_ctl
      call alloc_pvr_tracers_ctl(pvr_trcs_tmp)
      do i = 1, pvr_trcs_tmp%num_pvr_tracer_ctl
        call dup_pvr_tracer_ctl(pvr_tracers_c%pvr_trc_c(i),             &
     &                          pvr_trcs_tmp%pvr_trc_c(i))
      end do
!
      call dealloc_pvr_tracers_ctl(pvr_tracers_c)
      pvr_tracers_c%num_pvr_tracer_ctl                                  &
     &      = pvr_trcs_tmp%num_pvr_tracer_ctl + 1
      call alloc_pvr_tracers_ctl(pvr_tracers_c)
!
      do i = 1, idx_in
        call dup_pvr_tracer_ctl(pvr_trcs_tmp%pvr_trc_c(i),              &
     &                          pvr_tracers_c%pvr_trc_c(i))
      end do
      call init_pvr_tracer_ctl_label(hd_block,                          &
     &                               pvr_tracers_c%pvr_trc_c(idx_in+1))
      do i = idx_in+1, pvr_trcs_tmp%num_pvr_tracer_ctl
        call dup_pvr_tracer_ctl(pvr_trcs_tmp%pvr_trc_c(i),              &
     &                          pvr_tracers_c%pvr_trc_c(i+1))
      end do
!
      call dealloc_pvr_tracers_ctl(pvr_trcs_tmp)
!
      end subroutine append_pvr_tracers_ctl
!
! -----------------------------------------------------------------------
!
      subroutine delete_pvr_tracers_ctl(idx_in, pvr_tracers_c)
!
      integer(kind = kint), intent(in) :: idx_in
      type(pvr_tracers_ctl), intent(inout) :: pvr_tracers_c
!
      type(pvr_tracers_ctl) :: pvr_trcs_tmp
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0                                                    &
     &      .or. idx_in.gt.pvr_tracers_c%num_pvr_tracer_ctl) return
!
      pvr_trcs_tmp%num_pvr_tracer_ctl                                   &
     &      = pvr_tracers_c%num_pvr_tracer_ctl
      call alloc_pvr_tracers_ctl(pvr_trcs_tmp)
      do i = 1, pvr_trcs_tmp%num_pvr_tracer_ctl
        call dup_pvr_tracer_ctl(pvr_tracers_c%pvr_trc_c(i),             &
     &                          pvr_trcs_tmp%pvr_trc_c(i))
      end do
!
      call dealloc_pvr_tracers_ctl(pvr_tracers_c)
      pvr_tracers_c%num_pvr_tracer_ctl                                  &
     &      = pvr_trcs_tmp%num_pvr_tracer_ctl - 1
      call alloc_pvr_tracers_ctl(pvr_tracers_c)
!
      do i = 1, idx_in-1
        call dup_pvr_tracer_ctl(pvr_trcs_tmp%pvr_trc_c(i),              &
     &                          pvr_tracers_c%pvr_trc_c(i))
      end do
      do i = idx_in, pvr_trcs_tmp%num_pvr_tracer_ctl
        call dup_pvr_tracer_ctl(pvr_trcs_tmp%pvr_trc_c(i+1),            &
     &                          pvr_tracers_c%pvr_trc_c(i))
      end do
!
      call dealloc_pvr_tracers_ctl(pvr_trcs_tmp)
!
      end subroutine delete_pvr_tracers_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dup_pvr_tracers_ctl(org_pvr_trcs_c, new_pvr_trcs_c)
!
      type(pvr_tracers_ctl), intent(in) :: org_pvr_trcs_c
      type(pvr_tracers_ctl), intent(inout) :: new_pvr_trcs_c
!
      integer(kind = kint) :: i
!
!
      new_pvr_trcs_c%block_name = org_pvr_trcs_c%block_name
      new_pvr_trcs_c%num_pvr_tracer_ctl                                 &
     &     = org_pvr_trcs_c%num_pvr_tracer_ctl
      call alloc_pvr_tracers_ctl(new_pvr_trcs_c)
      do i = 1, org_pvr_trcs_c%num_pvr_tracer_ctl
        call dup_pvr_tracer_ctl(org_pvr_trcs_c%pvr_trc_c(i),            &
     &                          new_pvr_trcs_c%pvr_trc_c(i))
      end do
!
      end subroutine dup_pvr_tracers_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_tracers_ctl(pvr_isos_ctl)
!
      type(pvr_tracers_ctl), intent(inout) :: pvr_isos_ctl
!
      integer(kind = kint) :: i
!
      do i = 1, pvr_isos_ctl%num_pvr_tracer_ctl
        call reset_pvr_tracer_ctl(pvr_isos_ctl%pvr_trc_c(i))
      end do
!
      end subroutine reset_pvr_tracers_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_tracers
