!>@file   ctl_data_sph_monitor_IO.f90
!!        module ctl_data_sph_monitor_IO
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
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_monitoring_ctl                             &
!!     &         (id_control, hd_block, smonitor_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_monitor_control), intent(in) :: smonitor_ctl
!!        integer(kind = kint), intent(inout) :: level
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
!!    degree_spectra_switch         'On'
!!    order_spectra_switch          'On'
!!    diff_lm_spectra_switch        'On'
!!    axisymmetric_power_switch     'On'
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
!!
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
!!    array fields_on_circle_ctl
!!      ...
!!    array fields_on_circle_ctl
!!
!!    begin sph_dipolarity_ctl
!!      ...
!!    end sph_dipolarity_ctl
!!
!!    begin dynamo_benchmark_data_ctl
!!      ...
!!    end dynamo_benchmark_data_ctl
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module ctl_data_sph_monitor_IO
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_sph_layer_spectr
      use t_ctl_data_pick_sph_spectr
      use t_ctl_data_gauss_coefs
      use t_ctl_data_mid_equator
      use t_ctl_data_sph_dipolarity
      use t_ctl_data_4_sph_monitor
      use skip_comment_f
!
      implicit  none
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
     &            :: hd_field_on_circle_ctl = 'fields_on_circle_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_dynamobench_ctl = 'dynamo_benchmark_data_ctl'
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &           :: hd_voume_ave_head = 'volume_average_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_voume_rms_head = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter, private                         &
     &           :: hd_voume_rms_format = 'volume_pwr_spectr_format'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_degree_spectr_switch = 'degree_spectra_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_order_spectr_switch = 'order_spectra_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_diff_lm_spectr_switch = 'diff_lm_spectra_switch'
      character(len=kchara), parameter, private                         &
     &           :: hd_axis_spectr_switch = 'axisymmetric_power_switch'
!
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
!     Deprecated labels
       character(len=kchara), parameter, private                         &
     &            :: hd_mid_eq_monitor_ctl = 'mid_equator_monitor_ctl'
!
      private :: read_volume_spectr_ctl, write_volume_spectr_ctl
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
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
        call read_ctl_data_dynamobench(id_control,                      &
     &      hd_dynamobench_ctl, smonitor_ctl%dbench_ctl, c_buf)
!
        call read_data_on_circles_ctl(id_control,                       &
     &      hd_field_on_circle_ctl, smonitor_ctl%circ_ctls, c_buf)
        call read_data_on_circles_ctl(id_control,                       &
     &      hd_mid_eq_monitor_ctl,  smonitor_ctl%circ_ctls, c_buf)
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
!
        call read_chara_ctl_type(c_buf, hd_degree_spectr_switch,        &
     &      smonitor_ctl%degree_v_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_order_spectr_switch,         &
     &      smonitor_ctl%order_v_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_diff_lm_spectr_switch,       &
     &      smonitor_ctl%diff_v_lm_spectra_switch)
        call read_chara_ctl_type(c_buf, hd_axis_spectr_switch,          &
     &      smonitor_ctl%axis_v_power_switch)
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
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
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
!  ---------------------------------------------------------------------
!
      subroutine write_sph_monitoring_ctl                               &
     &         (id_control, hd_block, smonitor_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_monitor_control), intent(in) :: smonitor_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(smonitor_ctl%i_sph_monitor .le. 0) return
!
      maxlen = len_trim(hd_Nusselt_file_head)
      maxlen = max(maxlen, len_trim(hd_Nusselt_file_fmt))
      maxlen = max(maxlen, len_trim(hd_heat_Nusselt_file_head))
      maxlen = max(maxlen, len_trim(hd_heat_Nusselt_file_fmt))
      maxlen = max(maxlen, len_trim(hd_comp_Nusselt_file_head))
      maxlen = max(maxlen, len_trim(hd_comp_Nusselt_file_fmt))
      maxlen = max(maxlen, len_trim(hd_typ_scale_file_head))
      maxlen = max(maxlen, len_trim(hd_typ_scale_file_format))
      maxlen = max(maxlen, len_trim(hd_voume_ave_head))
      maxlen = max(maxlen, len_trim(hd_voume_rms_head))
      maxlen = max(maxlen, len_trim(hd_degree_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_order_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_diff_lm_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_axis_spectr_switch))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_voume_ave_head, smonitor_ctl%volume_average_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_voume_rms_head, smonitor_ctl%volume_pwr_spectr_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_diff_lm_spectr_switch,                                     &
     &    smonitor_ctl%volume_pwr_spectr_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &   hd_degree_spectr_switch, smonitor_ctl%degree_v_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &   hd_order_spectr_switch, smonitor_ctl%order_v_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_diff_lm_spectr_switch,                                     &
     &    smonitor_ctl%diff_v_lm_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_axis_spectr_switch, smonitor_ctl%axis_v_power_switch)
!
      call write_volume_spectr_ctl(id_control, hd_vol_spec_block,       &
     &                             smonitor_ctl, level)
      call write_layerd_spectr_ctl(id_control, hd_layer_spec_block,     &
     &                             smonitor_ctl%lp_ctl, level)
!
      call write_pickup_spectr_ctl(id_control, hd_pick_sph_ctl,         &
     &                             smonitor_ctl%pspec_ctl, level)
      call write_gauss_spectr_ctl(id_control, hd_gauss_spec_block,      &
     &                            smonitor_ctl%g_pwr, level)
!
      call write_sph_dipolarity_ctl(id_control,                         &
     &    hd_sph_dipolarity_ctl, smonitor_ctl%fdip_ctl, level)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_Nusselt_file_head, smonitor_ctl%heat_Nusselt_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_Nusselt_file_fmt, smonitor_ctl%heat_Nusselt_file_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          hd_heat_Nusselt_file_head,              &
     &                          smonitor_ctl%heat_Nusselt_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          hd_heat_Nusselt_file_fmt,               &
     &                          smonitor_ctl%heat_Nusselt_file_format)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          hd_comp_Nusselt_file_head,              &
     &                          smonitor_ctl%comp_Nusselt_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          hd_comp_Nusselt_file_fmt,               &
     &                          smonitor_ctl%comp_Nusselt_file_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          hd_typ_scale_file_head,                 &
     &                          smonitor_ctl%typ_scale_file_prefix_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          hd_typ_scale_file_format,               &
     &                          smonitor_ctl%typ_scale_file_format_ctl)
!
      call write_ctl_data_dynamobench(id_control,                       &
     &    hd_dynamobench_ctl, smonitor_ctl%dbench_ctl, level)
      call write_data_on_circles_ctl(id_control,                        &
     &    hd_field_on_circle_ctl, smonitor_ctl%circ_ctls, level)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_monitoring_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_volume_spectr_ctl                                &
     &         (id_control, hd_block, smonitor_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(sph_monitor_control), intent(in) :: smonitor_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: i
!
!
      if(smonitor_ctl%num_vspec_ctl .le. 0) return
!
      level = write_array_flag_for_ctl(id_control, level, hd_block)
      do i = 1, smonitor_ctl%num_vspec_ctl
          call write_each_vol_spectr_ctl(id_control, hd_block,          &
     &     smonitor_ctl%v_pwr(i), level)
      end do
      level = write_end_array_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_volume_spectr_ctl
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_sph_monitor_IO
