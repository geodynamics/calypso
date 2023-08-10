!>@file   ctl_data_volume_spectr_IO.f90
!!        module ctl_data_volume_spectr_IO
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_volume_spectr_ctl                               &
!!     &         (id_control, hd_block, smonitor_ctl, c_buf)
!!      subroutine write_volume_spectr_ctl                              &
!!     &         (id_control, smonitor_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      subroutine append_volume_spectr_ctls(idx_in, hd_block,          &
!!     &                                     smonitor_ctl)
!!      subroutine delete_volume_spectr_ctls(idx_in, smonitor_ctl)
!!         integer(kind = kint), intent(in) :: idx_in
!!        character(len=kchara), intent(in) :: hd_block
!!         type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!    array volume_spectrum_ctl
!!      ...
!!    end array volume_spectrum_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module ctl_data_volume_spectr_IO
!
      use m_precision
!
      use t_read_control_elements
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_4_sph_monitor
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
      subroutine read_volume_spectr_ctl                                 &
     &         (id_control, hd_block, smonitor_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: n_append
!
!
      if(smonitor_ctl%num_vspec_ctl .gt. 0) return
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      smonitor_ctl%num_vspec_ctl = 0
      call alloc_volume_spectr_control(smonitor_ctl)
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        if(check_begin_flag(c_buf, hd_block)) then
          n_append = smonitor_ctl%num_vspec_ctl
          call append_volume_spectr_ctls(n_append, hd_block,            &
     &                                   smonitor_ctl)
          call read_each_vol_spectr_ctl(id_control, hd_block,           &
     &        smonitor_ctl%v_pwr(smonitor_ctl%num_vspec_ctl), c_buf)
        end if
      end do
!
      end subroutine read_volume_spectr_ctl
!
!  ---------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_volume_spectr_ctl                                &
     &         (id_control, smonitor_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(sph_monitor_control), intent(in) :: smonitor_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(smonitor_ctl%num_vspec_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level,               &
     &                                 smonitor_ctl%v_pwr_name)
      do i = 1, smonitor_ctl%num_vspec_ctl
          call write_each_vol_spectr_ctl                                &
     &       (id_control, smonitor_ctl%v_pwr(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level,           &
     &                                     smonitor_ctl%v_pwr_name)
!
      end subroutine write_volume_spectr_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_volume_spectr_ctls(idx_in, hd_block,            &
     &                                     smonitor_ctl)
!
      integer(kind = kint), intent(in) :: idx_in
      character(len=kchara), intent(in) :: hd_block
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      type(volume_spectr_control), allocatable :: tmp_vpwr(:)
      integer(kind = kint) :: i, num_tmp
!
!
      if(idx_in.lt.0 .or. idx_in.gt.smonitor_ctl%num_vspec_ctl) return
!
      num_tmp = smonitor_ctl%num_vspec_ctl
      allocate(tmp_vpwr(num_tmp))
      do i = 1, num_tmp
        call copy_volume_spectr_control(smonitor_ctl%v_pwr(i),          &
     &                                  tmp_vpwr(i))
      end do
!
      call dealloc_volume_spectr_control(smonitor_ctl)
      smonitor_ctl%num_vspec_ctl = num_tmp + 1
      call alloc_volume_spectr_control(smonitor_ctl)
!
      do i = 1, idx_in
        call copy_volume_spectr_control(tmp_vpwr(i),                    &
     &                                  smonitor_ctl%v_pwr(i))
      end do
      call init_each_vol_spectr_labels(hd_block,                        &
     &                                 smonitor_ctl%v_pwr(idx_in+1))
      do i = idx_in+1, num_tmp
        call copy_volume_spectr_control(tmp_vpwr(i),                    &
     &                                  smonitor_ctl%v_pwr(i+1))
      end do
      deallocate(tmp_vpwr)
!
      end subroutine append_volume_spectr_ctls
!
! -----------------------------------------------------------------------
!
      subroutine delete_volume_spectr_ctls(idx_in, smonitor_ctl)
!
      integer(kind = kint), intent(in) :: idx_in
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      integer(kind = kint) :: num_tmp
      type(volume_spectr_control), allocatable :: tmp_vpwr(:)
      integer(kind = kint) :: i
!
!
      if(idx_in.le.0 .or. idx_in.gt.smonitor_ctl%num_vspec_ctl) return

      num_tmp = smonitor_ctl%num_vspec_ctl
      allocate(tmp_vpwr(num_tmp))
      do i = 1, num_tmp
        call copy_volume_spectr_control(smonitor_ctl%v_pwr(i),          &
     &                                  tmp_vpwr(i))
      end do
!
      call dealloc_volume_spectr_control(smonitor_ctl)
      smonitor_ctl%num_vspec_ctl = num_tmp - 1
      call alloc_volume_spectr_control(smonitor_ctl)
!
      do i = 1, idx_in-1
        call copy_volume_spectr_control(tmp_vpwr(i),                    &
     &                                  smonitor_ctl%v_pwr(i))
      end do
      do i = idx_in, smonitor_ctl%num_vspec_ctl
        call copy_volume_spectr_control(tmp_vpwr(i+1),                  &
     &                                  smonitor_ctl%v_pwr(i))
      end do
      deallocate(tmp_vpwr)
!
      end subroutine delete_volume_spectr_ctls
!
! -----------------------------------------------------------------------
!
      end module ctl_data_volume_spectr_IO
