!>@file   t_ctl_data_pvr_light.f90
!!@brief  module t_ctl_data_pvr_light
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief colormap control data for parallel volume rendering
!!
!!@verbatim
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
        type(read_real_item) :: ambient_coef_ctl
        type(read_real_item) :: diffuse_coef_ctl
        type(read_real_item) :: specular_coef_ctl
!
!>      Structure for light positions
!!@n      light_position_ctl%vec1:  X-component of light position
!!@n      light_position_ctl%vec2:  Y-component of light position
!!@n      light_position_ctl%vec3:  Z-component of light position
        type(ctl_array_r3) :: light_position_ctl
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
      character(len=kchara) :: hd_light_param =  'position_of_lights'
!
      integer(kind = kint), parameter :: n_label_pvr_light = 4
!
      private :: hd_ambient, hd_diffuse, hd_specular, hd_light_param
      private :: n_label_pvr_light
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
     &      hd_light_param, light%light_position_ctl, c_buf)
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
     &    hd_light_param, light%light_position_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_ambient, light%ambient_coef_ctl )
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_diffuse, light%diffuse_coef_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_specular, light%specular_coef_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_lighting_ctl
!
!  ---------------------------------------------------------------------
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
      call dealloc_control_array_r3(light%light_position_ctl)
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
      new_light%i_pvr_lighting = org_light%i_pvr_lighting
!
      call dup_control_array_r3(org_light%light_position_ctl,           &
     &                          new_light%light_position_ctl)
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
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_light()
      num_label_pvr_light = n_label_pvr_light
      return
      end function num_label_pvr_light
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_light(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_light)
!
!
      call set_control_labels(hd_light_param, names( 1))
      call set_control_labels(hd_ambient,     names( 2))
      call set_control_labels(hd_diffuse,     names( 3))
      call set_control_labels(hd_specular,    names( 4))
!
      end subroutine set_label_pvr_light
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_pvr_light
