!>@file   t_fline_seeds_list_ctl.f90
!!@brief  module t_fline_seeds_list_ctl
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each field line
!!
!!@verbatim
!!      subroutine init_fline_seeds_list_ctl(hd_block, fln_seeds)
!!      subroutine read_fline_seeds_list_ctl(id_control, hd_block,      &
!!     &                                     fln_seeds, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fline_seeds_list_ctl), intent(inout) :: fln_seeds
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_fline_seeds_list_ctl(id_control, fln_seeds,    &
!!     &                                      level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(fline_seeds_list_ctl), intent(in) :: fln_seeds
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_fline_seeds_list_ctl(fln_seeds)
!!      subroutine reset_fline_seeds_list_ctl(fln_seeds)
!!        type(fline_seeds_list_ctl), intent(inout) :: fln_seeds
!!      subroutine dup_fline_seeds_list_ctl(org_fln_seeds,              &
!!     &                                    new_fln_seeds)
!!        type(fline_seeds_list_ctl), intent(in) :: org_fln_seeds
!!        type(fline_seeds_list_ctl), intent(inout) :: new_fln_seeds
!!  ---------------------------------------------------------------------
!!     example of control for Kemo's field line
!!
!!  begin seed_lists_ctl
!!    array seed_point_ctl
!!      seed_point_ctl  0.0  0.0  0.0
!!    end array seed_point_ctl
!!
!!    array seed_geological_ctl
!!      seed_geological_ctl  1.03    36.5    140.0
!!    end array seed_geological_ctl
!!
!!    array seed_spherical_ctl
!!      seed_geological_ctl 0.75    -1.047    3.141592
!!    end array seed_spherical_ctl
!!
!!    array starting_gl_surface_id  10
!!      starting_gl_surface_id  12  3
!!    end array
!!  end seed_lists_ctl
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module t_fline_seeds_list_ctl
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_integer2
      use t_control_array_real3
      use calypso_mpi
!
      implicit  none
!
!
      type fline_seeds_list_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'seed_lists_ctl'
!
!>      Structure for seed points
!!@n      seed_point_ctl%vec1:  X-component of seed points
!!@n      seed_point_ctl%vec2:  Y-component of seed points
!!@n      seed_point_ctl%vec3:  Z-component of seed points
        type(ctl_array_r3) :: seed_point_ctl
!>      Structure for seed points
!!@n      seed_geological_ctl%vec1:  r-component of seed points
!!@n      seed_geological_ctl%vec2:  latitude of seed points in degree
!!@n      seed_geological_ctl%vec3:  longitude of seed points in degree
        type(ctl_array_r3) :: seed_geological_ctl
!>      Structure for seed points
!!@n      seed_spherical_ctl%vec1:  r-component of seed points
!!@n      seed_spherical_ctl%vec2:  theta-component of seed points
!!@n      seed_spherical_ctl%vec3:  phi-component of seed points
        type(ctl_array_r3) :: seed_spherical_ctl
!
!>      Structure for seed points on center of the surfaces
!!@n      seed_surface_ctl%int1:  element ID for seed points
!!@n      seed_surface_ctl%int2:  Surface ID for seed points
        type(ctl_array_i2) :: seed_surface_ctl
!
        integer (kind=kint) :: i_seeds_list_ctl = 0
      end type fline_seeds_list_ctl
!
!
      character(len=kchara), parameter, private                         &
     &      :: hd_xx_start_point =  'seed_point_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_geo_start_point = 'seed_geological_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_rtp_start_point = 'seed_spherical_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_start_global_surf = 'starting_gl_surface_id'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_fline_seeds_list_ctl(id_control, hd_block,        &
     &                                     fln_seeds, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(fline_seeds_list_ctl), intent(inout) :: fln_seeds
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
!
      if (fln_seeds%i_seeds_list_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_r3(id_control,                          &
     &      hd_xx_start_point, fln_seeds%seed_point_ctl, c_buf)
        call read_control_array_r3(id_control,                          &
     &      hd_geo_start_point, fln_seeds%seed_geological_ctl, c_buf)
        call read_control_array_r3(id_control,                          &
     &      hd_rtp_start_point, fln_seeds%seed_spherical_ctl, c_buf)
!
        call read_control_array_i2(id_control,                          &
     &      hd_start_global_surf, fln_seeds%seed_surface_ctl, c_buf)
      end do
      fln_seeds%i_seeds_list_ctl = 1 
!
      end subroutine read_fline_seeds_list_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_fline_seeds_list_ctl(id_control, fln_seeds,      &
     &                                      level)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(fline_seeds_list_ctl), intent(in) :: fln_seeds
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(fln_seeds%i_seeds_list_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 fln_seeds%block_name)
!
      call write_control_array_r3(id_control, level,                    &
     &                            fln_seeds%seed_point_ctl)
      call write_control_array_r3(id_control, level,                    &
     &                            fln_seeds%seed_geological_ctl)
      call write_control_array_r3(id_control, level,                    &
     &                            fln_seeds%seed_spherical_ctl)
      call write_control_array_i2 (id_control, level,                   &
     &                             fln_seeds%seed_surface_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                fln_seeds%block_name)
!
      end subroutine write_fline_seeds_list_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_fline_seeds_list_ctl(hd_block, fln_seeds)
!
      character(len=kchara), intent(in) :: hd_block
      type(fline_seeds_list_ctl), intent(inout) :: fln_seeds
!
!
      fln_seeds%block_name = hd_block
!
      call init_r3_ctl_array_label                                      &
     &   (hd_xx_start_point,  fln_seeds%seed_point_ctl)
      call init_r3_ctl_array_label                                      &
     &   (hd_geo_start_point, fln_seeds%seed_geological_ctl)
      call init_r3_ctl_array_label                                      &
     &   (hd_rtp_start_point, fln_seeds%seed_spherical_ctl)
!
      call init_int2_ctl_array_label                                    &
     &   (hd_start_global_surf, fln_seeds%seed_surface_ctl)
!
      end subroutine init_fline_seeds_list_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_seeds_list_ctl(fln_seeds)
!
      type(fline_seeds_list_ctl), intent(inout) :: fln_seeds
!
!
      call dealloc_control_array_i2(fln_seeds%seed_surface_ctl)
      call dealloc_control_array_r3(fln_seeds%seed_point_ctl)
      call dealloc_control_array_r3(fln_seeds%seed_geological_ctl)
      call dealloc_control_array_r3(fln_seeds%seed_spherical_ctl)
!
      end subroutine dealloc_fline_seeds_list_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_fline_seeds_list_ctl(fln_seeds)
!
      type(fline_seeds_list_ctl), intent(inout) :: fln_seeds
!
!
      fln_seeds%seed_point_ctl%icou =      0
      fln_seeds%seed_geological_ctl%icou = 0
      fln_seeds%seed_spherical_ctl%icou =  0
      fln_seeds%seed_surface_ctl%icou =    0
!
      end subroutine reset_fline_seeds_list_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dup_fline_seeds_list_ctl(org_fln_seeds,                &
     &                                    new_fln_seeds)
!
      type(fline_seeds_list_ctl), intent(in) :: org_fln_seeds
      type(fline_seeds_list_ctl), intent(inout) :: new_fln_seeds
!
!
      call dup_control_array_r3(org_fln_seeds%seed_point_ctl,           &
     &                          new_fln_seeds%seed_point_ctl)
      call dup_control_array_r3(org_fln_seeds%seed_geological_ctl,      &
     &                          new_fln_seeds%seed_geological_ctl)
      call dup_control_array_r3(org_fln_seeds%seed_spherical_ctl,       &
     &                          new_fln_seeds%seed_spherical_ctl)
!
      call dup_control_array_i2(org_fln_seeds%seed_surface_ctl,         &
     &                          new_fln_seeds%seed_surface_ctl)
!
      new_fln_seeds%block_name =        org_fln_seeds%block_name
      new_fln_seeds%i_seeds_list_ctl =  org_fln_seeds%i_seeds_list_ctl
!
      end subroutine dup_fline_seeds_list_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_fline_seeds_list_ctl
