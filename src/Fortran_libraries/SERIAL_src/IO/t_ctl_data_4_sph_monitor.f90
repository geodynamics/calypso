!>@file   t_ctl_data_4_sph_monitor.f90
!!        module t_ctl_data_4_sph_monitor
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_sph_monitoring_ctl                              &
!!     &         (id_control, hd_block, smonitor_ctl, c_buf)
!!      subroutine dealloc_sph_monitoring_ctl(smonitor_ctl)
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_monitor_ctl
!!    volume_average_prefix        'sph_ave_volume'
!!    volume_pwr_spectr_prefix     'sph_pwr_volume'
!!    volume_pwr_spectr_format     'gzip'
!!
!!    nusselt_number_prefix        'Nusselt'
!!    nusselt_number_format        'gzip'
!!
!!    heat_Nusselt_number_prefix        'Nusselt_temp'
!!    comp_Nusselt_number_prefix        'Nusselt_comp'
!!    heat_Nusselt_number_format        'gzip'
!!    comp_Nusselt_number_format        'gzip'
!!
!!    typical_scale_prefix         'typical_scale'
!!    typical_scale_format         'gzip'
!!!
!!    array volume_spectrum_ctl
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
!!
!!    begin sph_dipolarity_ctl
!!      ...
!!    end sph_dipolarity_ctl
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_4_sph_monitor
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_sph_layer_spectr
      use t_ctl_data_pick_sph_spectr
      use t_ctl_data_gauss_coefs
      use t_mid_equator_control
      use t_ctl_data_sph_dipolarity
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
!>        Structure for dipolarity setting
        type(sph_dipolarity_control) :: fdip_ctl
!
!>        Structure for volume average file prefix
        type(read_character_item) :: volume_average_prefix
!
!>        Structure for volume spectrum file prefix
        type(read_character_item) :: volume_pwr_spectr_prefix
!
!>        Structure for volume spectrum file format
        type(read_character_item) :: volume_pwr_spectr_format
!
!>        Structure for Nusselt number file prefix
        type(read_character_item) :: heat_Nusselt_file_prefix
!>        Structure for Nusselt number file prefix
        type(read_character_item) :: comp_Nusselt_file_prefix
!
!>        Structure for Nusselt number file prefix
        type(read_character_item) :: heat_Nusselt_file_format
!>        Structure for Nusselt number file prefix
        type(read_character_item) :: comp_Nusselt_file_format
!
!>        Structure for typical scale file prefix
        type(read_character_item) :: typ_scale_file_prefix_ctl
!
!>        Structure for typical scale file format
        type(read_character_item) :: typ_scale_file_format_ctl
!
        integer (kind = kint) :: i_sph_monitor = 0
      end type sph_monitor_control
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &            :: hd_vol_spec_block =   'volume_spectrum_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_layer_spec_block = 'layered_spectrum_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_gauss_spec_block = 'gauss_coefficient_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_pick_sph_ctl =     'pickup_spectr_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_sph_dipolarity_ctl = 'sph_dipolarity_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_mid_eq_monitor_ctl = 'mid_equator_monitor_ctl'
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_voume_ave_head = 'volume_average_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_voume_rms_head = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_voume_rms_format = 'volume_pwr_spectr_format'
      character(len=kchara), parameter, private                         &
     &           :: hd_Nusselt_file_head = 'nusselt_number_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_Nusselt_file_fmt = 'nusselt_number_format'
      character(len=kchara), parameter, private                         &
     &           :: hd_typ_scale_file_head = 'typical_scale_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_typ_scale_file_format = 'typical_scale_format'
!
       character(len=kchara), parameter, private                        &
     &    :: hd_heat_Nusselt_file_head = 'heat_Nusselt_number_prefix'
       character(len=kchara), parameter, private                        &
     &    :: hd_comp_Nusselt_file_head = 'comp_Nusselt_number_prefix'
       character(len=kchara), parameter, private                        &
     &    :: hd_heat_Nusselt_file_fmt = 'heat_Nusselt_number_format'
       character(len=kchara), parameter, private                        &
     &    :: hd_comp_Nusselt_file_fmt = 'comp_Nusselt_number_format'
!
      private :: read_volume_spectr_ctl
      private :: append_volume_spectr_ctls
      private :: alloc_volume_spectr_control
      private :: dealloc_volume_spectr_control
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_monitoring_ctl                                &
     &         (id_control, hd_block, smonitor_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(smonitor_ctl%i_sph_monitor  .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_gauss_spectr_ctl(id_control, hd_gauss_spec_block,     &
     &                             smonitor_ctl%g_pwr, c_buf)
        call read_pickup_spectr_ctl(id_control, hd_pick_sph_ctl,        &
     &                              smonitor_ctl%pspec_ctl, c_buf)
        call read_layerd_spectr_ctl(id_control, hd_layer_spec_block,    &
     &                              smonitor_ctl%lp_ctl, c_buf)
        call read_sph_dipolarity_ctl(id_control,                        &
     &      hd_sph_dipolarity_ctl, smonitor_ctl%fdip_ctl, c_buf)
        call read_mid_eq_monitor_ctl(id_control, hd_mid_eq_monitor_ctl, &
     &      smonitor_ctl%meq_ctl, c_buf)
!
        call read_volume_spectr_ctl                                     &
     &     (id_control, hd_vol_spec_block, smonitor_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_Nusselt_file_head,           &
     &      smonitor_ctl%heat_Nusselt_file_prefix)
        call read_chara_ctl_type(c_buf, hd_Nusselt_file_fmt,            &
     &      smonitor_ctl%heat_Nusselt_file_format)
!
        call read_chara_ctl_type(c_buf, hd_heat_Nusselt_file_head,      &
     &      smonitor_ctl%heat_Nusselt_file_prefix)
        call read_chara_ctl_type(c_buf, hd_heat_Nusselt_file_fmt,       &
     &      smonitor_ctl%heat_Nusselt_file_format)
        call read_chara_ctl_type(c_buf, hd_comp_Nusselt_file_head,      &
     &      smonitor_ctl%comp_Nusselt_file_prefix)
        call read_chara_ctl_type(c_buf, hd_comp_Nusselt_file_fmt,       &
     &      smonitor_ctl%comp_Nusselt_file_format)
!
        call read_chara_ctl_type(c_buf, hd_typ_scale_file_head,         &
     &      smonitor_ctl%typ_scale_file_prefix_ctl)
        call read_chara_ctl_type(c_buf, hd_typ_scale_file_format,       &
     &      smonitor_ctl%typ_scale_file_format_ctl)
        call read_chara_ctl_type(c_buf, hd_voume_ave_head,              &
     &      smonitor_ctl%volume_average_prefix)
        call read_chara_ctl_type(c_buf, hd_voume_rms_head,              &
     &      smonitor_ctl%volume_pwr_spectr_prefix)
        call read_chara_ctl_type(c_buf, hd_voume_rms_format,            &
     &      smonitor_ctl%volume_pwr_spectr_format)
      end do
      smonitor_ctl%i_sph_monitor = 1
!
      end subroutine read_sph_monitoring_ctl
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
      type(volume_spectr_control) :: read_vpwr
!
!
      if(check_array_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(smonitor_ctl%num_vspec_ctl .gt. 0) return
      read_vpwr%i_vol_spectr_ctl = 0
      smonitor_ctl%num_vspec_ctl = 0
      call alloc_volume_spectr_control(smonitor_ctl)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        call read_each_vol_spectr_ctl(id_control, hd_block,             &
     &                                read_vpwr, c_buf)
        if(read_vpwr%i_vol_spectr_ctl .gt. 0) then
          call append_volume_spectr_ctls(read_vpwr, smonitor_ctl)
          read_vpwr%i_vol_spectr_ctl = 0
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
      integer(kind = kint) :: i
!
!
      smonitor_ctl%i_sph_monitor = 0
!
      call dealloc_num_spec_layer_ctl(smonitor_ctl%lp_ctl)
      call dealloc_pick_spectr_control(smonitor_ctl%pspec_ctl)
      call dealloc_gauss_spectr_control(smonitor_ctl%g_pwr)
      call reset_mid_equator_control(smonitor_ctl%meq_ctl)
      call dealloc_sph_dipolarity_ctl(smonitor_ctl%fdip_ctl)
!
      smonitor_ctl%volume_average_prefix%iflag =     0
      smonitor_ctl%volume_pwr_spectr_prefix%iflag =  0
      smonitor_ctl%volume_pwr_spectr_format%iflag =  0
      smonitor_ctl%heat_Nusselt_file_prefix%iflag =  0
      smonitor_ctl%heat_Nusselt_file_format%iflag =  0
      smonitor_ctl%comp_Nusselt_file_prefix%iflag =  0
      smonitor_ctl%comp_Nusselt_file_format%iflag =  0
      smonitor_ctl%typ_scale_file_prefix_ctl%iflag = 0
      smonitor_ctl%typ_scale_file_format_ctl%iflag = 0
!
      if(smonitor_ctl%num_vspec_ctl .le. 0) return
!
      do i = 1, smonitor_ctl%num_vspec_ctl
       call reset_volume_spectr_control(smonitor_ctl%v_pwr(i))
      end do
      call dealloc_volume_spectr_control(smonitor_ctl)
!
      end subroutine dealloc_sph_monitoring_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_volume_spectr_ctls(add_vpwr, smonitor_ctl)
!
      type(volume_spectr_control), intent(inout) :: add_vpwr
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      integer(kind = kint) :: num_tmp = 0
      type(volume_spectr_control), allocatable :: tmp_vpwr(:)
!
!
      num_tmp = smonitor_ctl%num_vspec_ctl
      allocate(tmp_vpwr(num_tmp))
      call copy_volume_spectr_ctls                                      &
     &   (num_tmp, smonitor_ctl%v_pwr, tmp_vpwr)
!
      call dealloc_volume_spectr_control(smonitor_ctl)
      smonitor_ctl%num_vspec_ctl = num_tmp + 1
      call alloc_volume_spectr_control(smonitor_ctl)
!
      call copy_volume_spectr_ctls                                      &
     &   (num_tmp, tmp_vpwr, smonitor_ctl%v_pwr(1))
      deallocate(tmp_vpwr)
!
      call copy_volume_spectr_control                                   &
     &   (add_vpwr, smonitor_ctl%v_pwr(smonitor_ctl%num_vspec_ctl))
      call reset_volume_spectr_control(add_vpwr)
!
      end subroutine append_volume_spectr_ctls
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_volume_spectr_control(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      allocate(smonitor_ctl%v_pwr(smonitor_ctl%num_vspec_ctl))
!
      end subroutine alloc_volume_spectr_control
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_volume_spectr_control(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
      if(allocated(smonitor_ctl%v_pwr)) deallocate(smonitor_ctl%v_pwr)
      smonitor_ctl%num_vspec_ctl = 0
!
      end subroutine dealloc_volume_spectr_control
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_monitor
