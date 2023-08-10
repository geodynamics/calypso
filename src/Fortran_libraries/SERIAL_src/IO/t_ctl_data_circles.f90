!>@file   t_ctl_data_circles.f90
!!        module t_ctl_data_circles
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!>@brief control date for data ouput on circles
!!
!!@verbatim
!!      subroutine read_data_on_circles_ctl                             &
!!     &         (id_control, hd_block, smonitor_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_data_on_circles_ctl                            &
!!     &         (id_control, smonitor_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine append_data_on_circles_ctl(idx_in, hd_block,         &
!!     &                                      smonitor_ctl)
!!      subroutine delete_data_on_circles_ctl(idx_in, smonitor_ctl)
!!        integer(kind = kint), intent(in) :: idx_in
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  array fields_on_circle_ctl
!!    begin fields_on_circle_ctl
!!      field_on_circle_prefix         'monitor/dbench_field'
!!      spectr_on_circle_prefix        'monitor/dbench_spectr'
!!      field_on_circle_format         'gzip'
!!
!!      pick_circle_coord_ctl         spherical
!!      nphi_mid_eq_ctl               500
!!      pick_cylindrical_radius_ctl   0.75
!!      pick_vertical_position_ctl    0.6
!!    end fields_on_circle_ctl
!!  end array fields_on_circle_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_circles
!
      use m_precision
!
      use t_read_control_elements
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_mid_equator
      use skip_comment_f
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_data_on_circles_ctl                               &
     &         (id_control, hd_block, smonitor_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(allocated(smonitor_ctl%meq_ctl)) return
      smonitor_ctl%num_circ_ctl = 0
      smonitor_ctl%d_circ_name = hd_block
      call alloc_data_on_circles_ctl(smonitor_ctl)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          n_append = smonitor_ctl%num_circ_ctl
          call append_data_on_circles_ctl(n_append, hd_block,           &
     &                                    smonitor_ctl)
          write(*,'(3a,i4,a)') 'Control for ', trim(hd_block), ' No. ', &
     &        smonitor_ctl%num_circ_ctl, ' is included'
          call read_mid_eq_monitor_ctl(id_control, hd_block,            &
     &        smonitor_ctl%meq_ctl(smonitor_ctl%num_circ_ctl), c_buf)
        end if
      end do
!
      end subroutine read_data_on_circles_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_data_on_circles_ctl                              &
     &         (id_control, smonitor_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(sph_monitor_control), intent(in) :: smonitor_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 smonitor_ctl%d_circ_name)
      do i = 1, smonitor_ctl%num_circ_ctl
        call write_mid_eq_monitor_ctl(id_control,                       &
     &                                smonitor_ctl%meq_ctl(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     smonitor_ctl%d_circ_name)
!
      end subroutine write_data_on_circles_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine append_data_on_circles_ctl(idx_in, hd_block,           &
     &                                      smonitor_ctl)
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      type(mid_equator_control), allocatable :: tmp_meq_c(:)
      integer(kind = kint) :: i, num_tmp
!
!
      num_tmp = smonitor_ctl%num_circ_ctl
      allocate(tmp_meq_c(num_tmp))
      do i = 1, num_tmp
        call dup_mid_equator_control(smonitor_ctl%meq_ctl(i),           &
                                     tmp_meq_c(i))
      end do
!
      call dealloc_data_on_circles_ctl(smonitor_ctl)
      smonitor_ctl%num_circ_ctl = num_tmp + 1
      call alloc_data_on_circles_ctl(smonitor_ctl)
!
      do i = 1, idx_in
        call dup_mid_equator_control(tmp_meq_c(i),                      &
     &                               smonitor_ctl%meq_ctl(i))
      end do
      call init_mid_eq_monitor_ctl_label(hd_block,                      &
     &    smonitor_ctl%meq_ctl(idx_in+1))
      do i = idx_in+1, num_tmp
        call dup_mid_equator_control(tmp_meq_c(i),                      &
     &                               smonitor_ctl%meq_ctl(i+1))
      end do
      deallocate(tmp_meq_c)
!
      end subroutine append_data_on_circles_ctl
!
! -----------------------------------------------------------------------
!
      subroutine delete_data_on_circles_ctl(idx_in, smonitor_ctl)
!
      integer(kind = kint), intent(in) :: idx_in
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      type(mid_equator_control), allocatable :: tmp_meq_c(:)
      integer(kind = kint) :: i, num_tmp
!
!
      if(idx_in.le.0 .or. idx_in.gt.smonitor_ctl%num_vspec_ctl) return

      num_tmp = smonitor_ctl%num_circ_ctl
      allocate(tmp_meq_c(num_tmp))
      do i = 1, num_tmp
        call dup_mid_equator_control(smonitor_ctl%meq_ctl(i),           &
                                     tmp_meq_c(i))
      end do
!
      call dealloc_data_on_circles_ctl(smonitor_ctl)
      smonitor_ctl%num_circ_ctl = num_tmp - 1
      call alloc_data_on_circles_ctl(smonitor_ctl)
!
      do i = 1, idx_in-1
        call dup_mid_equator_control(tmp_meq_c(i),                      &
     &                               smonitor_ctl%meq_ctl(i))
      end do
      do i = idx_in, smonitor_ctl%num_vspec_ctl
        call dup_mid_equator_control(tmp_meq_c(i+1),                    &
     &                               smonitor_ctl%meq_ctl(i))
      end do
      deallocate(tmp_meq_c)
!
      end subroutine delete_data_on_circles_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_circles
