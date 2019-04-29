!>@file   t_ctl_data_4_sph_monitor.f90
!!        module t_ctl_data_4_sph_monitor
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_sph_monitoring_ctl(hd_block, iflag, smonitor_ctl)
!!      subroutine dealloc_sph_monitoring_ctl(smonitor_ctl)
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_monitor_ctl
!!    volume_average_prefix        'sph_ave_volume'
!!    volume_pwr_spectr_prefix     'sph_pwr_volume'
!!
!!    nusselt_number_prefix        'Nusselt'
!!!
!!    array volume_spectrum_ctl      2
!!      ...
!!    end array volume_spectrum_ctl
!!
!!    begin layered_spectrum_ctl
!!      ...
!!    end   layered_spectrum_ctl
!!
!!    begin gauss_coefficient_ctl
!!      ...
!!    end   gauss_coefficient_ctl
!!
!!    begin pickup_spectr_ctl
!!      ...
!!    end   pickup_spectr_ctl
!!
!!    begin mid_equator_monitor_ctl
!!      ...
!!    end   mid_equator_monitor_ctl
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_4_sph_monitor
!
      use m_precision
!
      use t_control_elements
      use t_read_control_arrays
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_pick_sph_spectr
      use skip_comment_f
!
      implicit  none
!
!
      type sph_monitor_control
        integer(kind = kint) :: num_vspec_ctl = 0
        type(volume_spectr_control), allocatable :: v_pwr(:)
!
        type(layerd_spectr_control) :: lp_ctl
!
        type(gauss_spectr_control) :: g_pwr
!
!>        Structure for spectr data pickup
        type(pick_spectr_control) :: pspec_ctl
!
        type(mid_equator_control) :: meq_ctl
!
!
!>        Structure for layered spectrum file prefix
        type(read_character_item) :: volume_average_prefix
!
!>        Structure for layered spectrum file prefix
        type(read_character_item) :: volume_pwr_spectr_prefix
!
!>        Structure for picked spectrum file prefix
        type(read_character_item) :: Nusselt_file_prefix
      end type sph_monitor_control
!
!
!    label for entry
!
      character(len=kchara), parameter                                  &
     &            :: hd_vol_spec_block =   'volume_spectrum_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_layer_spec_block = 'layered_spectrum_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_gauss_spec_block = 'gauss_coefficient_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_sph_ctl =     'pickup_spectr_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_mid_eq_monitor_ctl = 'mid_equator_monitor_ctl'
      integer(kind = kint) :: i_vol_spectr_ctl =   0
      integer(kind = kint) :: i_layer_spectr_ctl = 0
      integer(kind = kint) :: i_gauss_pwr_ctl = 0
      integer(kind = kint) :: i_pick_sph_ctl =  0
      integer(kind = kint) :: i_mid_eq_monitor_ctl =  0
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &           :: hd_voume_ave_head = 'volume_average_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_voume_rms_head = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_Nusselt_file_head = 'nusselt_number_prefix'
!
!
      private :: hd_vol_spec_block, i_vol_spectr_ctl
      private :: hd_layer_spec_block, i_layer_spectr_ctl
      private :: hd_gauss_spec_block, i_gauss_pwr_ctl
      private :: hd_pick_sph_ctl, i_pick_sph_ctl
      private :: hd_mid_eq_monitor_ctl, i_mid_eq_monitor_ctl
      private :: hd_Nusselt_file_head
      private :: hd_voume_ave_head, hd_voume_rms_head
!
      private :: read_volume_spectr_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_monitoring_ctl(hd_block, iflag, smonitor_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
        call read_gauss_spectr_ctl                                      &
     &     (hd_gauss_spec_block, i_gauss_pwr_ctl, smonitor_ctl%g_pwr)
        call read_pickup_spectr_ctl                                     &
     &     (hd_pick_sph_ctl, i_pick_sph_ctl, smonitor_ctl%pspec_ctl)
        call read_layerd_spectr_ctl                                     &
     &     (hd_layer_spec_block, i_layer_spectr_ctl,                    &
     &      smonitor_ctl%lp_ctl)
        call read_mid_eq_monitor_ctl                                    &
     &     (hd_mid_eq_monitor_ctl, i_mid_eq_monitor_ctl,                &
     &      smonitor_ctl%meq_ctl)
!
        call find_control_array_flag                                    &
     &     (hd_vol_spec_block, smonitor_ctl%num_vspec_ctl)
        if(smonitor_ctl%num_vspec_ctl .gt. 0) then
          call read_volume_spectr_ctl(smonitor_ctl)
        end if
!
        call read_chara_ctl_type(hd_Nusselt_file_head,                  &
     &      smonitor_ctl%Nusselt_file_prefix)
        call read_chara_ctl_type(hd_voume_ave_head,                     &
     &      smonitor_ctl%volume_average_prefix)
        call read_chara_ctl_type(hd_voume_rms_head,                     &
     &      smonitor_ctl%volume_pwr_spectr_prefix)
      end do
!
      end subroutine read_sph_monitoring_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_volume_spectr_ctl(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      integer(kind = kint) :: iflag
!
!
      if (i_vol_spectr_ctl .gt. 0) return
      allocate(smonitor_ctl%v_pwr(smonitor_ctl%num_vspec_ctl))
!
      do
        call load_ctl_label_and_line
        call find_control_end_array_flag(hd_vol_spec_block,             &
     &      smonitor_ctl%num_vspec_ctl, i_vol_spectr_ctl)
        if(i_vol_spectr_ctl .ge. smonitor_ctl%num_vspec_ctl) exit
!
        if(right_begin_flag(hd_vol_spec_block) .gt. 0) then
          i_vol_spectr_ctl = i_vol_spectr_ctl + 1
          iflag = 0
          call read_each_vol_spectr_ctl(hd_vol_spec_block, iflag,       &
     &        smonitor_ctl%v_pwr(i_vol_spectr_ctl))
        end if
      end do
!
      end subroutine read_volume_spectr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sph_monitoring_ctl(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
!
      call dealloc_num_spec_layer_ctl(smonitor_ctl%lp_ctl)
      call dealloc_pick_spectr_control(smonitor_ctl%pspec_ctl)
      call dealloc_gauss_spectr_control(smonitor_ctl%g_pwr)
      call reset_mid_equator_control(smonitor_ctl%meq_ctl)
!
      smonitor_ctl%volume_average_prefix%iflag = 0
      smonitor_ctl%volume_pwr_spectr_prefix%iflag = 0
      smonitor_ctl%Nusselt_file_prefix%iflag = 0
!
      if(smonitor_ctl%num_vspec_ctl .le. 0) return
      deallocate(smonitor_ctl%v_pwr)
      smonitor_ctl%num_vspec_ctl = 0
!
      end subroutine dealloc_sph_monitoring_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_monitor
