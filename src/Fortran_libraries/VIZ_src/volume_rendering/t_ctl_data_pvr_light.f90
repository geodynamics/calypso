!>@file   t_ctl_data_pvr_light.f90
!!@brief  module t_ctl_data_pvr_light
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine init_lighting_ctl_label(hd_block, light)
!!      subroutine read_lighting_ctl(id_control, hd_block, light, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_light_ctl), intent(inout) :: light
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_lighting_ctl                                   &
!!     &         (id_control, hd_block, light, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_light_ctl), intent(in) :: light
!!        integer(kind = kint), intent(inout) :: level
!!      logical function cmp_pvr_light_ctl(light1, light2)
!!        type(pvr_light_ctl), intent(in) :: light1, light2
!!
!!      subroutine reset_pvr_light_flags(light)
!!      subroutine dealloc_pvr_light_crl(light)
!!        type(pvr_light_ctl), intent(inout) :: light
!!      subroutine dup_lighting_ctl(org_light, new_light)
!!        type(pvr_light_ctl), intent(in) :: org_light
!!        type(pvr_light_ctl), intent(inout) :: new_light
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of color control for Kemo's volume rendering
!!
!!  begin lighting_ctl
!!    array position_of_lights
!!      position_of_lights    0.0   0.0    0.0   end
!!      position_of_lights  -10.0   0.0  -10.0   end
!!      position_of_lights  -10.0   0.0    0.0   end
!!      position_of_lights    0.0  10.0    0.0   end
!!    end array position_of_lights
!!!
!!    array sph_position_of_lights
!!      sph_position_of_lights   10.0   0.0    0.0   end
!!      sph_position_of_lights   10.0  30.0  -45.0   end
!!      sph_position_of_lights   10.0  30.0   45.0   end
!!      sph_position_of_lights   10.0 -45.0  180.0   end
!!    end array sph_position_of_lights
!!!
!!    ambient_coef_ctl              0.5
!!    diffuse_coef_ctl              5.6
!!    specular_coef_ctl             0.8
!!  end lighting_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_light
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_real3
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_light_ctl
!>        Control block name
        character(len = kchara) :: block_name = 'lighting_ctl'
!
        type(read_real_item) :: ambient_coef_ctl
        type(read_real_item) :: diffuse_coef_ctl
        type(read_real_item) :: specular_coef_ctl
!
!>      Structure for light positions
!!@n      light_position_ctl%vec1:  X-component of light position
!!@n      light_position_ctl%vec2:  Y-component of light position
!!@n      light_position_ctl%vec3:  Z-component of light position
        type(ctl_array_r3) :: light_position_ctl
!>      Structure for light positions
!!@n      light_sph_posi_ctl%vec1:  r-component of light position
!!@n      light_sph_posi_ctl%vec2:  theta-component of light position
!!@n      light_sph_posi_ctl%vec3:  phi-component of light position
        type(ctl_array_r3) :: light_sph_posi_ctl
!
        integer (kind=kint) :: i_pvr_lighting = 0
      end type pvr_light_ctl
!
!
!     3rd level for lighting
!
      character(len=kchara) :: hd_ambient =  'ambient_coef_ctl'
      character(len=kchara) :: hd_diffuse =  'diffuse_coef_ctl'
      character(len=kchara) :: hd_specular = 'specular_coef_ctl'
      character(len=kchara) :: hd_light_xyz =  'position_of_lights'
      character(len=kchara) :: hd_light_sph =  'sph_position_of_lights'
!
      private :: hd_ambient, hd_diffuse, hd_specular
      private :: hd_light_xyz, hd_light_sph
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_lighting_ctl(id_control, hd_block, light, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_light_ctl), intent(inout) :: light
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(light%i_pvr_lighting .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_r3(id_control,                          &
     &      hd_light_xyz, light%light_position_ctl, c_buf)
        call read_control_array_r3(id_control,                          &
     &      hd_light_sph, light%light_sph_posi_ctl, c_buf)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_ambient, light%ambient_coef_ctl )
        call read_real_ctl_type                                         &
     &     (c_buf, hd_diffuse, light%diffuse_coef_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_specular, light%specular_coef_ctl)
      end do
      light%i_pvr_lighting = 1
!
      end subroutine read_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_lighting_ctl                                     &
     &         (id_control, hd_block, light, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_light_ctl), intent(in) :: light
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(light%i_pvr_lighting .le. 0) return
!
      maxlen = len_trim(hd_ambient)
      maxlen = max(maxlen, len_trim(hd_diffuse))
      maxlen = max(maxlen, len_trim(hd_specular))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_r3(id_control, level,                    &
     &    light%light_position_ctl)
      call write_control_array_r3(id_control, level,                    &
     &    light%light_sph_posi_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    light%ambient_coef_ctl )
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    light%diffuse_coef_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    light%specular_coef_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine init_lighting_ctl_label(hd_block, light)
!
      character(len=kchara), intent(in) :: hd_block
      type(pvr_light_ctl), intent(inout) :: light
!
!
      light%block_name = hd_block
        call init_r3_ctl_array_label                                    &
     &     (hd_light_xyz, light%light_position_ctl)
        call init_r3_ctl_array_label                                    &
     &     (hd_light_sph, light%light_sph_posi_ctl)
!
        call init_real_ctl_item_label                                   &
     &     (hd_ambient, light%ambient_coef_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_diffuse, light%diffuse_coef_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_specular, light%specular_coef_ctl)
!
      end subroutine init_lighting_ctl_label
!
!  ---------------------------------------------------------------------
!
      logical function cmp_pvr_light_ctl(light1, light2)
!
      type(pvr_light_ctl), intent(in) :: light1, light2
!
      cmp_pvr_light_ctl = .FALSE.
      if(light1%i_pvr_lighting .ne. light2%i_pvr_lighting) return
      if(cmp_no_case(trim(light1%block_name),                           &
     &               trim(light2%block_name)) .eqv. .FALSE.) return
!
      if(cmp_read_real_item(light1%ambient_coef_ctl,                    &
     &                      light2%ambient_coef_ctl)                    &
     &                                            .eqv. .FALSE.) return
      if(cmp_read_real_item(light1%diffuse_coef_ctl,                    &
     &                      light2%diffuse_coef_ctl)                    &
     &                                            .eqv. .FALSE.) return
      if(cmp_read_real_item(light1%specular_coef_ctl,                   &
     &                      light2%specular_coef_ctl)                   &
     &                                            .eqv. .FALSE.) return
!
      if(cmp_control_array_r3(light1%light_position_ctl,                &
     &                        light2%light_position_ctl)                &
     &                                            .eqv. .FALSE.) return
      if(cmp_control_array_r3(light1%light_sph_posi_ctl,                &
     &                        light2%light_sph_posi_ctl)                &
     &                                            .eqv. .FALSE.) return
      cmp_pvr_light_ctl = .TRUE.
!
      end function cmp_pvr_light_ctl
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_light_flags(light)
!
      type(pvr_light_ctl), intent(inout) :: light
!
!
      light%ambient_coef_ctl%iflag =  0
      light%diffuse_coef_ctl%iflag =  0
      light%specular_coef_ctl%iflag = 0
      light%i_pvr_lighting = 0
!
      end subroutine reset_pvr_light_flags
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_light_crl(light)
!
      type(pvr_light_ctl), intent(inout) :: light
!
      call dealloc_control_array_r3(light%light_sph_posi_ctl)
      call dealloc_control_array_r3(light%light_position_ctl)
      light%light_sph_posi_ctl%num = 0
      light%light_sph_posi_ctl%icou = 0
      light%light_position_ctl%num = 0
      light%light_position_ctl%icou = 0
!
      end subroutine dealloc_pvr_light_crl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_lighting_ctl(org_light, new_light)
!
      type(pvr_light_ctl), intent(in) :: org_light
      type(pvr_light_ctl), intent(inout) :: new_light
!
!
      new_light%block_name =     org_light%block_name
      new_light%i_pvr_lighting = org_light%i_pvr_lighting
!
      call dup_control_array_r3(org_light%light_position_ctl,           &
     &                          new_light%light_position_ctl)
      call dup_control_array_r3(org_light%light_sph_posi_ctl,           &
     &                          new_light%light_sph_posi_ctl)
!
      call copy_real_ctl(org_light%ambient_coef_ctl,                    &
     &                   new_light%ambient_coef_ctl)
      call copy_real_ctl(org_light%diffuse_coef_ctl,                    &
     &                   new_light%diffuse_coef_ctl )
      call copy_real_ctl(org_light%specular_coef_ctl,                   &
     &                   new_light%specular_coef_ctl)
!
      end subroutine dup_lighting_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_light
