!>@file   m_ctl_data_4_pickup_sph.f90
!!        module m_ctl_data_4_pickup_sph
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine deallocate_num_pick_layer_ctl
!!
!!      subroutine deallocate_pick_sph_ctl
!!      subroutine deallocate_pick_sph_l_ctl
!!      subroutine deallocate_pick_sph_m_ctl
!!
!!      subroutine deallocate_pick_gauss_ctl
!!      subroutine deallocate_pick_gauss_l_ctl
!!      subroutine deallocate_pick_gauss_m_ctl
!!
!!      subroutine read_pickup_sph_ctl
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_monitor_ctl
!!    volume_average_prefix        'sph_ave_volume'
!!    volume_pwr_spectr_prefix     'sph_pwr_volume'
!!    layered_pwr_spectr_prefix    'sph_pwr_layer'
!!
!!    picked_sph_prefix            'sph_spectr/picked_mode'
!!    gauss_coefs_prefix           'sph_spectr/gauss_coefs'
!!
!!    gauss_coefs_radius_ctl        2.91
!!
!!   if num_pick_layer_ctl = 0 or negative:
!!           output all layer and volume average
!!
!!    array pick_layer_ctl  1
!!      pick_layer_ctl  62
!!    end array
!!
!!    array pick_sph_spectr_ctl  2
!!      pick_sph_spectr_ctl   2  -2
!!      pick_sph_spectr_ctl   2   2
!!    end array pick_sph_spectr_ctl
!!
!!    array pick_sph_degree_ctl  2
!!      pick_sph_degree_ctl   2
!!      pick_sph_degree_ctl   2
!!    end array pick_sph_degree_ctl
!!
!!    array pick_sph_order_ctl  2
!!      pick_sph_order_ctl  -2
!!      pick_sph_order_ctl   2
!!    end array pick_sph_order_ctl
!!
!!
!!    array pick_gauss_coefs_ctl  2
!!      pick_gauss_coefs_ctl   2  -2
!!      pick_gauss_coefs_ctl   2   2
!!    end array pick_gauss_coefs_ctl
!!
!!    array pick_gauss_coef_degree_ctl  2
!!      pick_gauss_coef_degree_ctl   2
!!      pick_gauss_coef_degree_ctl   2
!!    end array pick_gauss_coef_degree_ctl
!!
!!    array pick_gauss_coef_order_ctl  2
!!      pick_gauss_coef_order_ctl   -2
!!      pick_gauss_coef_order_ctl    2
!!    end array pick_gauss_coef_order_ctl
!!
!!    pick_circle_coord_ctl         spherical
!!    nphi_mid_eq_ctl               500
!!    pick_cylindrical_radius_ctl   0.75
!!    pick_vertical_position_ctl    0.6
!!  end sph_monitor_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_pickup_sph
!
      use m_precision
!
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
      character(len = kchara) :: volume_average_prefix
      character(len = kchara) :: volume_pwr_spectr_prefix
      character(len = kchara) :: layered_pwr_spectr_prefix
!
      character(len = kchara) :: picked_mode_head_ctl
      character(len = kchara) :: gauss_coefs_prefix
      real(kind = kreal) :: gauss_coefs_radius_ctl = 2.91
!
      integer(kind = kint) :: num_pick_layer_ctl = 0
      integer(kind = kint), allocatable :: id_pick_layer_ctl(:)
!
      integer(kind = kint) :: num_pick_sph_mode_ctl = 0
      integer(kind = kint), allocatable :: idx_pick_sph_mode_ctl(:,:)
!
      integer(kind = kint) :: num_pick_sph_l_ctl = 0
      integer(kind = kint), allocatable :: idx_pick_sph_l_ctl(:)
!
      integer(kind = kint) :: num_pick_sph_m_ctl = 0
      integer(kind = kint), allocatable :: idx_pick_sph_m_ctl(:)
!
!
      integer(kind = kint) :: num_pick_gauss_coefs_ctl = 0
      integer(kind = kint), allocatable :: idx_pick_gauss_mode_ctl(:,:)
!
      integer(kind = kint) :: num_pick_gauss_l_ctl = 0
      integer(kind = kint), allocatable :: idx_pick_gauss_l_ctl(:)
!
      integer(kind = kint) :: num_pick_gauss_m_ctl = 0
      integer(kind = kint), allocatable :: idx_pick_gauss_m_ctl(:)
!
      character(len = kchara) :: pick_circle_coord_ctl = 'spherical'
      integer(kind = kint) :: nphi_mid_eq_ctl
      real(kind = kreal) :: pick_s_ctl = 7.0d0/13.0d0 + 0.5d0
      real(kind = kreal) :: pick_z_ctl = 0.0d0
!
!    label for entry
!
      character(len=kchara), parameter                                  &
     &                     :: hd_pick_sph = 'sph_monitor_ctl'
      integer(kind = kint) :: i_pick_sph = 0
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &           :: hd_voume_ave_head = 'volume_average_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_voume_rms_head = 'volume_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_layer_rms_head = 'layered_pwr_spectr_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_picked_mode_head = 'picked_sph_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_gauss_coefs_head = 'gauss_coefs_prefix'
      character(len=kchara), parameter                                  &
     &           :: hd_gauss_coefs_r =    'gauss_coefs_radius_ctl'
!
      character(len=kchara), parameter                                  &
     &           :: hd_num_pick_layer = 'pick_layer_ctl'
!
      character(len=kchara), parameter                                  &
     &            :: hd_num_pick_sph =   'pick_sph_spectr_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_num_pick_l =     'pick_sph_degree_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_num_pick_m =     'pick_sph_order_ctl'
!
      character(len=kchara), parameter                                  &
     &            :: hd_num_pick_gauss =   'pick_gauss_coefs_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_num_pick_gauss_l = 'pick_gauss_coef_degree_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_num_pick_gauss_m = 'pick_gauss_coef_order_ctl'
!
      character(len=kchara), parameter                                  &
     &            :: hd_nphi_mid_eq = 'nphi_mid_eq_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_s_ctl = 'pick_cylindrical_radius_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_pick_z_ctl =  'pick_vertical_position_ctl'
      character(len=kchara), parameter                                  &
     &            :: hd_circle_coord = 'pick_circle_coord_ctl'
!
!
      integer (kind=kint) :: i_voume_ave_head =         0
      integer (kind=kint) :: i_voume_rms_head =         0
      integer (kind=kint) :: i_layer_rms_head =         0
      integer (kind=kint) :: i_picked_mode_head =       0
      integer (kind=kint) :: i_gauss_coefs_head =       0
      integer (kind=kint) :: i_gauss_coefs_r =          0
!
      integer (kind=kint) :: i_num_pick_layer =         0
!
      integer (kind=kint) :: i_num_pick_sph =           0
      integer (kind=kint) :: i_num_pick_l =             0
      integer (kind=kint) :: i_num_pick_m =             0
!
      integer (kind=kint) :: i_num_pick_gauss =         0
      integer (kind=kint) :: i_num_pick_gauss_l =       0
      integer (kind=kint) :: i_num_pick_gauss_m =       0
!
      integer (kind=kint) :: i_nphi_mid_eq =            0
      integer (kind=kint) :: i_pick_s_ctl =             0
      integer (kind=kint) :: i_pick_z_ctl =             0
      integer (kind=kint) :: i_circle_coord =           0
!
      private :: hd_pick_sph, i_pick_sph, hd_num_pick_layer
      private :: hd_gauss_coefs_head, hd_gauss_coefs_r
      private :: hd_picked_mode_head
      private :: hd_num_pick_sph, hd_num_pick_gauss
      private :: hd_num_pick_l, hd_num_pick_m
      private :: hd_num_pick_gauss_l, hd_num_pick_gauss_m
      private :: hd_voume_ave_head, hd_voume_rms_head
      private :: hd_layer_rms_head, hd_nphi_mid_eq
      private :: hd_pick_s_ctl, hd_pick_z_ctl
!
      private :: allocate_pick_layer_ctl, allocate_pick_sph_ctl
      private :: allocate_pick_sph_l_ctl, allocate_pick_sph_m_ctl
      private :: allocate_pick_gauss_ctl
      private :: allocate_pick_gauss_l_ctl, allocate_pick_gauss_m_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_layer_ctl
!
      allocate( id_pick_layer_ctl(num_pick_layer_ctl) )
!
      end subroutine allocate_pick_layer_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_ctl
!
      allocate( idx_pick_sph_mode_ctl(num_pick_sph_mode_ctl,2) )
!
      end subroutine allocate_pick_sph_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_l_ctl
!
      allocate( idx_pick_sph_l_ctl(num_pick_sph_l_ctl) )
!
      end subroutine allocate_pick_sph_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_sph_m_ctl
!
      allocate( idx_pick_sph_m_ctl(num_pick_sph_m_ctl) )
!
      end subroutine allocate_pick_sph_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_ctl
!
      allocate( idx_pick_gauss_mode_ctl(num_pick_gauss_coefs_ctl,2) )
!
      end subroutine allocate_pick_gauss_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_l_ctl
!
      allocate( idx_pick_gauss_l_ctl(num_pick_gauss_l_ctl) )
!
      end subroutine allocate_pick_gauss_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine allocate_pick_gauss_m_ctl
!
      allocate( idx_pick_gauss_m_ctl(num_pick_gauss_m_ctl) )
!
      end subroutine allocate_pick_gauss_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_num_pick_layer_ctl
!
      deallocate( id_pick_layer_ctl )
!
      end subroutine deallocate_num_pick_layer_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_ctl
!
      deallocate( idx_pick_sph_mode_ctl )
!
      end subroutine deallocate_pick_sph_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_l_ctl
!
      deallocate( idx_pick_sph_l_ctl )
!
      end subroutine deallocate_pick_sph_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_sph_m_ctl
!
      deallocate( idx_pick_sph_m_ctl )
!
      end subroutine deallocate_pick_sph_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss_ctl
!
      deallocate( idx_pick_gauss_mode_ctl )
!
      end subroutine deallocate_pick_gauss_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss_l_ctl
!
      deallocate( idx_pick_gauss_l_ctl )
!
      end subroutine deallocate_pick_gauss_l_ctl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_pick_gauss_m_ctl
!
      deallocate( idx_pick_gauss_m_ctl )
!
      end subroutine deallocate_pick_gauss_m_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_pickup_sph_ctl
!
!
      if(right_begin_flag(hd_pick_sph) .eq. 0) return
      if (i_pick_sph .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pick_sph, i_pick_sph)
        if(i_pick_sph .gt. 0) exit
!
!
        call find_control_array_flag(hd_num_pick_layer,                 &
     &      num_pick_layer_ctl)
        if(num_pick_layer_ctl.gt.0  .and. i_num_pick_layer.eq.0) then
          call allocate_pick_layer_ctl
          call read_control_array_int_list(hd_num_pick_layer,           &
     &        num_pick_layer_ctl, i_num_pick_layer, id_pick_layer_ctl)
        end if
!
!
        call find_control_array_flag(hd_num_pick_sph,                   &
     &      num_pick_sph_mode_ctl)
        if(num_pick_sph_mode_ctl.gt.0 .and. i_num_pick_sph.eq.0) then
          call allocate_pick_sph_ctl
          call read_control_array_int2_list(hd_num_pick_sph,            &
     &        num_pick_sph_mode_ctl, i_num_pick_sph,                    &
     &        idx_pick_sph_mode_ctl(1,1), idx_pick_sph_mode_ctl(1,2))
        end if
!
        call find_control_array_flag(hd_num_pick_l, num_pick_sph_l_ctl)
        if(num_pick_sph_l_ctl.gt.0  .and. i_num_pick_l.eq.0) then
          call allocate_pick_sph_l_ctl
          call read_control_array_int_list(hd_num_pick_l,               &
     &        num_pick_sph_l_ctl, i_num_pick_l, idx_pick_sph_l_ctl)
        end if
!
        call find_control_array_flag(hd_num_pick_m, num_pick_sph_m_ctl)
        if(num_pick_sph_m_ctl.gt.0  .and. i_num_pick_m.eq.0) then
          call allocate_pick_sph_m_ctl
          call read_control_array_int_list(hd_num_pick_m,               &
     &        num_pick_sph_m_ctl, i_num_pick_m, idx_pick_sph_m_ctl)
        end if
!
!
        call find_control_array_flag(hd_num_pick_gauss,                 &
     &      num_pick_gauss_coefs_ctl)
        if(num_pick_gauss_coefs_ctl.gt.0 .and. i_num_pick_gauss.eq.0)   &
     &      then
          call allocate_pick_gauss_ctl
          call read_control_array_int2_list(hd_num_pick_gauss,          &
     &        num_pick_gauss_coefs_ctl, i_num_pick_gauss,               &
     &        idx_pick_gauss_mode_ctl(1,1),                             &
     &        idx_pick_gauss_mode_ctl(1,2))
        end if
!
        call find_control_array_flag(hd_num_pick_gauss_l,               &
     &      num_pick_gauss_l_ctl)
        if(num_pick_gauss_l_ctl.gt.0  .and. i_num_pick_gauss_l.eq.0)    &
     &      then
          call allocate_pick_gauss_l_ctl
          call read_control_array_int_list(hd_num_pick_gauss_l,         &
     &        num_pick_gauss_l_ctl, i_num_pick_gauss_l,                 &
     &        idx_pick_gauss_l_ctl)
        end if
!
        call find_control_array_flag(hd_num_pick_gauss_m,               &
     &      num_pick_gauss_m_ctl)
        if(num_pick_gauss_m_ctl.gt.0  .and. i_num_pick_gauss_m.eq.0)    &
     &      then
          call allocate_pick_gauss_m_ctl
          call read_control_array_int_list(hd_num_pick_gauss_m,         &
     &        num_pick_gauss_m_ctl, i_num_pick_gauss_m,                 &
     &        idx_pick_gauss_m_ctl)
        end if
!
!
        call read_real_ctl_item(hd_gauss_coefs_r,                       &
     &          i_gauss_coefs_r, gauss_coefs_radius_ctl)
        call read_real_ctl_item(hd_pick_s_ctl,                          &
     &          i_pick_s_ctl, pick_s_ctl)
        call read_real_ctl_item(hd_pick_z_ctl,                          &
     &          i_pick_z_ctl, pick_z_ctl)
!
        call read_integer_ctl_item(hd_nphi_mid_eq,                      &
     &          i_nphi_mid_eq, nphi_mid_eq_ctl)
!
        call read_character_ctl_item(hd_gauss_coefs_head,               &
     &          i_gauss_coefs_head, gauss_coefs_prefix)
        call read_character_ctl_item(hd_picked_mode_head,               &
     &          i_picked_mode_head, picked_mode_head_ctl)
!
        call read_character_ctl_item(hd_voume_ave_head,                 &
     &          i_voume_ave_head, volume_average_prefix)
        call read_character_ctl_item(hd_voume_rms_head,                 &
     &          i_voume_rms_head, volume_pwr_spectr_prefix)
        call read_character_ctl_item(hd_layer_rms_head,                 &
     &          i_layer_rms_head, layered_pwr_spectr_prefix)
!
        call read_character_ctl_item(hd_circle_coord,                   &
     &          i_circle_coord, pick_circle_coord_ctl)
      end do
!
      end subroutine read_pickup_sph_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_pickup_sph
