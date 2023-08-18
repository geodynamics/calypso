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
!!      subroutine dealloc_sph_monitoring_ctl(smonitor_ctl)
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!
!!      subroutine alloc_volume_spectr_control(smonitor_ctl)
!!      subroutine alloc_data_on_circles_ctl(smonitor_ctl)
!!      subroutine dealloc_sph_monitoring_ctl(smonitor_ctl)
!!      subroutine dealloc_data_on_circles_ctl(smonitor_ctl)
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
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
!!    array fields_on_circle_ctl
!!      ...
!!    array   fields_on_circle_ctl
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
      use t_ctl_data_mid_equator
      use t_ctl_data_dynamobench
      use t_ctl_data_sph_dipolarity
      use skip_comment_f
!
      implicit  none
!
!
      type sph_monitor_control
!>        Block name
        character(len=kchara) :: block_name = 'sph_monitor_ctl'
!
!>        array name for volume_spectr_control
        character(len=kchara) :: v_pwr_name = 'volume_spectrum_ctl'
!>        number of volume_spectr_control
        integer(kind = kint) :: num_vspec_ctl = 0
!>        array for volume_spectr_control
        type(volume_spectr_control), allocatable :: v_pwr(:)
!
!>        array name for fields_on_circle_ctl
        character(len=kchara) :: d_circ_name = 'fields_on_circle_ctl'
!         Nunber of circle data
        integer(kind = kint) :: num_circ_ctl = 0
!>        Structure for data on circle
        type(mid_equator_control), allocatable :: meq_ctl(:)
!
!         Structure for layerd spectr
        type(layerd_spectr_control) :: lp_ctl
!
!>        Structure for Gauss coefficient
        type(gauss_spectr_control) :: g_pwr
!
!>        Structure for spectr data pickup
        type(pick_spectr_control) :: pspec_ctl
!
!>        Structure for dynamo benchmark output
        type(dynamobench_control) :: dbench_ctl
!
!>        Structure for dipolarity setting
        type(sph_dipolarity_control) :: fdip_ctl
!
!>        Structure for volume average file prefix
        type(read_character_item) :: volume_average_prefix
!
!>        Structure for volume spectrum file prefix
        type(read_character_item) :: volume_pwr_spectr_prefix
!>        Structure for volume spectrum file format
        type(read_character_item) :: volume_pwr_spectr_format
!
!>        Structure for degree spectrum switch
        type(read_character_item) :: degree_v_spectra_switch
!>        Structure for order spectrum switch
        type(read_character_item) :: order_v_spectra_switch
!>        Structure for l-m spectrum switch
        type(read_character_item) :: diff_v_lm_spectra_switch
!>        Structure for axissymmetric data output switch
        type(read_character_item) :: axis_v_power_switch
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
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
      call reset_ctl_data_dynamobench(smonitor_ctl%dbench_ctl)
      call dealloc_sph_dipolarity_ctl(smonitor_ctl%fdip_ctl)
!
      smonitor_ctl%volume_average_prefix%iflag =     0
      smonitor_ctl%volume_pwr_spectr_prefix%iflag =  0
      smonitor_ctl%volume_pwr_spectr_format%iflag =  0
!
      smonitor_ctl%degree_v_spectra_switch%iflag =   0
      smonitor_ctl%order_v_spectra_switch%iflag =    0
      smonitor_ctl%diff_v_lm_spectra_switch%iflag =  0
      smonitor_ctl%axis_v_power_switch%iflag =       0
!
      smonitor_ctl%heat_Nusselt_file_prefix%iflag =  0
      smonitor_ctl%heat_Nusselt_file_format%iflag =  0
      smonitor_ctl%comp_Nusselt_file_prefix%iflag =  0
      smonitor_ctl%comp_Nusselt_file_format%iflag =  0
      smonitor_ctl%typ_scale_file_prefix_ctl%iflag = 0
      smonitor_ctl%typ_scale_file_format_ctl%iflag = 0
!
      do i = 1, smonitor_ctl%num_vspec_ctl
       call reset_volume_spectr_control(smonitor_ctl%v_pwr(i))
      end do
      call dealloc_volume_spectr_control(smonitor_ctl)
!
      do i = 1, smonitor_ctl%num_circ_ctl
        call reset_mid_equator_control(smonitor_ctl%meq_ctl(i))
      end do
      call dealloc_data_on_circles_ctl(smonitor_ctl)
!
      end subroutine dealloc_sph_monitoring_ctl
!
!  ---------------------------------------------------------------------
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
      subroutine alloc_data_on_circles_ctl(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      integer(kind = kint) :: i
!
!
      allocate(smonitor_ctl%meq_ctl(smonitor_ctl%num_circ_ctl))
!
      end subroutine alloc_data_on_circles_ctl
!
!  ---------------------------------------------------------------------
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
      subroutine dealloc_data_on_circles_ctl(smonitor_ctl)
!
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
!
!
      if(allocated(smonitor_ctl%meq_ctl))                               &
     &                    deallocate(smonitor_ctl%meq_ctl)
      smonitor_ctl%num_circ_ctl = 0
!
      end subroutine dealloc_data_on_circles_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_sph_monitor
