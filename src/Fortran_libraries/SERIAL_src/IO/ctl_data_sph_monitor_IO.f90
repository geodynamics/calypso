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
!!      subroutine init_sph_monitoring_labels(hd_block, smonitor_ctl)
!!      subroutine read_sph_monitoring_ctl                              &
!!     &         (id_control, hd_block, smonitor_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_monitoring_ctl                             &
!!     &         (id_control, smonitor_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
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
!!    volume_work_spectr_prefix    'sph_work_convective'
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
     &            :: hd_vol_lor_wk = 'volume_work_spectr_prefix'
!
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
       character(len=kchara), parameter, private                        &
     &            :: hd_mid_eq_monitor_ctl = 'mid_equator_monitor_ctl'
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
      use ctl_data_volume_spectr_IO
      use t_ctl_data_circles
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(smonitor_ctl%i_sph_monitor .gt. 0) return
      call init_sph_monitoring_labels(hd_block, smonitor_ctl)
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
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
     &      hd_field_on_circle_ctl, smonitor_ctl, c_buf)
        call read_data_on_circles_ctl(id_control,                       &
     &      hd_mid_eq_monitor_ctl,  smonitor_ctl, c_buf)
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
        call read_chara_ctl_type(c_buf, hd_vol_lor_wk,                  &
     &      smonitor_ctl%volume_work_spectr_prefix)
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
!  ---------------------------------------------------------------------
!
      subroutine write_sph_monitoring_ctl                               &
     &         (id_control, smonitor_ctl, level)
!
      use write_control_elements
      use t_ctl_data_circles
      use ctl_data_volume_spectr_IO
!
      integer(kind = kint), intent(in) :: id_control
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
      maxlen = max(maxlen, len_trim(hd_vol_lor_wk))
      maxlen = max(maxlen, len_trim(hd_degree_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_order_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_diff_lm_spectr_switch))
      maxlen = max(maxlen, len_trim(hd_axis_spectr_switch))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 smonitor_ctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%volume_average_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%volume_pwr_spectr_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%volume_work_spectr_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%volume_pwr_spectr_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%degree_v_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%order_v_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%diff_v_lm_spectra_switch)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%axis_v_power_switch)
!
      call write_volume_spectr_ctl(id_control, smonitor_ctl, level)
      call write_layerd_spectr_ctl(id_control,                          &
     &                             smonitor_ctl%lp_ctl, level)
!
      call write_pickup_spectr_ctl(id_control,                          &
     &                             smonitor_ctl%pspec_ctl, level)
      call write_gauss_spectr_ctl(id_control,                           &
     &                            smonitor_ctl%g_pwr, level)
!
      call write_sph_dipolarity_ctl(id_control,                         &
     &                              smonitor_ctl%fdip_ctl, level)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%heat_Nusselt_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    smonitor_ctl%heat_Nusselt_file_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          smonitor_ctl%heat_Nusselt_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          smonitor_ctl%heat_Nusselt_file_format)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          smonitor_ctl%comp_Nusselt_file_prefix)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          smonitor_ctl%comp_Nusselt_file_format)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          smonitor_ctl%typ_scale_file_prefix_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          smonitor_ctl%typ_scale_file_format_ctl)
!
      call write_ctl_data_dynamobench(id_control,                       &
     &                                smonitor_ctl%dbench_ctl, level)
      call write_data_on_circles_ctl(id_control, smonitor_ctl, level)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                smonitor_ctl%block_name)
!
      end subroutine write_sph_monitoring_ctl
!
! -----------------------------------------------------------------------
!
      subroutine init_sph_monitoring_labels(hd_block, smonitor_ctl)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
!
      smonitor_ctl%block_name = trim(hd_block)
      smonitor_ctl%v_pwr_name = hd_vol_spec_block
      smonitor_ctl%d_circ_name = hd_field_on_circle_ctl
      call init_gauss_spectr_ctl_labels(hd_gauss_spec_block,            &
     &                                  smonitor_ctl%g_pwr)
      call init_pickup_spectr_ctl_labels(hd_pick_sph_ctl,               &
     &                                   smonitor_ctl%pspec_ctl)
      call init_layerd_spectr_ctl_labels(hd_layer_spec_block,           &
     &                                   smonitor_ctl%lp_ctl)
      call init_sph_dipolarity_ctl_label(hd_sph_dipolarity_ctl,         &
     &                                   smonitor_ctl%fdip_ctl)
      call init_ctl_data_dynamobench_label(hd_dynamobench_ctl,          &
     &                                     smonitor_ctl%dbench_ctl)
!
      end subroutine init_sph_monitoring_labels
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_sph_monitor_IO
