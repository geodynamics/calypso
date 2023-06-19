!>@file   t_ctl_data_4_divide_sphere.f90
!!        module t_ctl_data_4_divide_sphere
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!>@brief  Control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine dealloc_ndomain_rtp_ctl(sdctl)
!!
!!      subroutine read_control_shell_domain                            &
!!     &         (id_control, hd_block, sdctl, c_buf)
!!        type(sphere_domain_control), intent(inout) :: sdctl
!!      subroutine write_control_shell_domain                           &
!!     &         (id_file, hd_block, sdctl, level)
!!
!!  ---------------------------------------------------------------------
!!    example of control data
!!
!!  begin num_domain_ctl
!!    ordering_set_ctl         Ver_2
!!
!!    inner_decomp_direction        radial or horizontal
!!
!!    rj_inner_loop_direction             radial or horizontal
!!    rlm_inner_loop_direction            radial or horizontal
!!    rtm_inner_loop_direction            radial or horizontal
!!    rtp_inner_loop_direction            radial or horizontal
!!
!!    rlm_order_distribution               cyclic_eq_transform
!!       (Flags:  cyclic_eq_transform, cyclic_eq_mode, or simple)
!!
!!    simple_radial_decomposition_ctl      On
!!
!!    num_radial_domain_ctl         2
!!    num_horizontal_domain_ctl     2
!!
!!    array  num_domain_sph_grid   2
!!      num_domain_sph_grid    radial       2   end
!!      num_domain_sph_grid   meridional    3   end
!!    end array num_domain_sph_grid
!!
!!    array num_domain_legendre   2
!!      num_domain_legendre   radial        2   end
!!      num_domain_legendre   zonal         3   end
!!    end array num_domain_legendre
!!
!!    array num_domain_spectr     1
!!      num_domain_spectr     modes         6   end
!!    end array num_domain_spectr
!!  end num_domain_ctl
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_4_divide_sphere
!
      use m_precision
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_charaint
!
      implicit  none
!
!
!
!>      control data structure for spherical shell parallelization
      type sphere_domain_control
!>        Orsering set control
        type(read_character_item) :: indices_ordering_set
!
!>        Direction of inner decomposition
        type(read_character_item) :: inner_decomp_ctl
!
!>        Direction of inner loop for f(r,j)
        type(read_character_item) :: rj_inner_loop_ctl
!>        Direction of inner loop for f(r,l,m)
        type(read_character_item) :: rlm_inner_loop_ctl
!>        Direction of inner loop for f(r,\theta,m)
        type(read_character_item) :: rtm_inner_loop_ctl
!>        Direction of inner loop for f(r,t,p)
        type(read_character_item) :: rtp_inner_loop_ctl
!
!>        Distribution of harmonics order for legendre transform
        type(read_character_item) :: rlm_distibution_ctl
!>        Simple radial decomposition_switch
        type(read_character_item) :: simple_r_decomp_ctl
!
!>        Number of subdomains in raidal direction for reduced definition
        type(read_integer_item) :: num_radial_domain_ctl
!
!>        Number of subdomains in horizontal directions
!!@n      for reduced definition
        type(read_integer_item) :: num_horiz_domain_ctl
!
!
!>        Structure for domain decompostion for spherical grid
!!@n        ndomain_sph_grid_ctl%c_tbl:  Direction of decomposition
!!@n        ndomain_sph_grid_ctl%ivec:   Number of subdomains
        type(ctl_array_ci) :: ndomain_sph_grid_ctl
!>        Structure for domain decompostion for Legendre transform
!!@n        ndomain_legendre_ctl%c_tbl:  Direction of decomposition
!!@n        ndomain_legendre_ctl%ivec:   Number of subdomains
        type(ctl_array_ci) :: ndomain_legendre_ctl
!>        Structure for domain decompostion for spherical harmonics
!!@n        ndomain_spectr_ctl%c_tbl:  Direction of decomposition
!!@n        ndomain_spectr_ctl%ivec:   Number of subdomains
        type(ctl_array_ci) :: ndomain_spectr_ctl
!
        integer(kind = kint) :: i_domains_sph = 0
      end type sphere_domain_control
!
!   labels for subdomain define for spherical shell
!
      character(len=kchara), parameter, private                         &
     &      :: hd_inner_decomp =       'inner_decomp_direction'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_rj_inner_loop =       'rj_inner_loop_direction'
      character(len=kchara), parameter, private                         &
     &      :: hd_rlm_inner_loop =      'rlm_inner_loop_direction'
      character(len=kchara), parameter, private                         &
     &      :: hd_rtm_inner_loop =      'rtm_inner_loop_direction'
      character(len=kchara), parameter, private                         &
     &      :: hd_rtp_inner_loop =      'rtp_inner_loop_direction'
      character(len=kchara), parameter, private                         &
     &      :: hd_indices_ordering_set = 'ordering_set_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_rlm_order_dist =      'rlm_order_distribution'
      character(len=kchara), parameter, private                         &
     &      :: hd_simple_r_decomp = 'simple_radial_decomposition_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_num_radial_domain =  'num_radial_domain_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_num_horiz_domain =  'num_horizontal_domain_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_ndomain_rtp =  'num_domain_sph_grid'
      character(len=kchara), parameter, private                         &
     &       :: hd_ndomain_rtm = 'num_domain_legendre'
      character(len=kchara), parameter, private                         &
     &       :: hd_ndomain_rj  = 'num_domain_spectr'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ndomain_rtp_ctl(sdctl)
!
      type(sphere_domain_control), intent(inout) :: sdctl
!
!
      call dealloc_control_array_c_i(sdctl%ndomain_sph_grid_ctl)
      call dealloc_control_array_c_i(sdctl%ndomain_legendre_ctl)
      call dealloc_control_array_c_i(sdctl%ndomain_spectr_ctl)
!
      end subroutine dealloc_ndomain_rtp_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_control_shell_domain                              &
     &         (id_control, hd_block, sdctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sphere_domain_control), intent(inout) :: sdctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sdctl%i_domains_sph .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_inner_decomp, sdctl%inner_decomp_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_rj_inner_loop,  sdctl%rj_inner_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_rlm_inner_loop, sdctl%rlm_inner_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_rtm_inner_loop, sdctl%rtm_inner_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_rtp_inner_loop, sdctl%rtp_inner_loop_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_indices_ordering_set, sdctl%indices_ordering_set)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_rlm_order_dist, sdctl%rlm_distibution_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_simple_r_decomp, sdctl%simple_r_decomp_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_radial_domain, sdctl%num_radial_domain_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_horiz_domain, sdctl%num_horiz_domain_ctl)
!
        call read_control_array_c_i(id_control,                         &
     &      hd_ndomain_rtp, sdctl%ndomain_sph_grid_ctl, c_buf)
        call read_control_array_c_i(id_control,                         &
     &      hd_ndomain_rtm, sdctl%ndomain_legendre_ctl, c_buf)
        call read_control_array_c_i(id_control,                         &
     &      hd_ndomain_rj, sdctl%ndomain_spectr_ctl, c_buf)
      end do
      sdctl%i_domains_sph = 1
!
      end subroutine read_control_shell_domain
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_shell_domain                             &
     &         (id_file, hd_block, sdctl, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: hd_block
      type(sphere_domain_control), intent(in) :: sdctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(sdctl%i_domains_sph .le. 0) return
!
      maxlen = len_trim(hd_inner_decomp)
      maxlen = max(maxlen, len_trim(hd_num_radial_domain))
      maxlen = max(maxlen, len_trim(hd_num_horiz_domain))
!
      level = write_begin_flag_for_ctl(id_file, level, hd_block)
!
      call write_chara_ctl_type(id_file, level, maxlen,                 &
     &    hd_inner_decomp, sdctl%inner_decomp_ctl)
!
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_num_radial_domain, sdctl%num_radial_domain_ctl)
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_num_horiz_domain, sdctl%num_horiz_domain_ctl)
!
      call write_control_array_c_i(id_file, level,                      &
     &    hd_ndomain_rtp, sdctl%ndomain_sph_grid_ctl)
      call write_control_array_c_i(id_file, level,                      &
     &    hd_ndomain_rtm, sdctl%ndomain_legendre_ctl)
      call write_control_array_c_i(id_file, level,                      &
     &    hd_ndomain_rj, sdctl%ndomain_spectr_ctl)
!
      level =  write_end_flag_for_ctl(id_file, level, hd_block)
!
      end subroutine write_control_shell_domain
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_4_divide_sphere
