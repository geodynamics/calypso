!>@file   t_ctl_data_valuable_diffuse.f90
!!@brief  module t_ctl_data_valuable_diffuse
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief Control data for valuable diffusivity
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_val_diffuse_ctl_label(hd_block, vdiffuse_ctl)
!!      subroutine read_val_diffuse_ctl_data                            &
!!     &         (id_control, hd_block, vdiffuse_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!  `     type(val_diffuse_ctl), intent(inout) :: vdiffuse_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_val_diffuse_ctl_data                           &
!!     &          (id_control, vdiffuse_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(val_diffuse_ctl), intent(in) :: vdiffuse_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_val_diffuse_ctl_data(vdiffuse_ctl)
!!        type(val_diffuse_ctl), intent(inout) :: vdiffuse_ctl
!!
!!      subroutine dup_val_diffuse_ctl_data(vdiffuse_c, new_vdiffuse_c)
!!        type(val_diffuse_ctl), intent(in) :: vdiffuse_c
!!        type(val_diffuse_ctl), intent(inout) :: new_vdiffuse_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin viscosity_ctl
!!      radial_variation_ctl      ON
!!      variation_file_name    'viscous_variation.dat'
!!!
!!      array diffusivity_list_ctl
!!        diffusivity_list_ctl      0.53846   1.2
!!        diffusivity_list_ctl      1.03846   1.05
!!        diffusivity_list_ctl      1.53846   1.0
!!      end array diffusivity_list_ctl
!!!
!!      ICB_reduction_radius    0.53846154
!!      ICB_reduction_ratio     0.1
!!      ICB_reduction_width     0.001
!!    end   viscosity_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_valuable_diffuse
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_real2
      use skip_comment_f
!
      implicit  none
!
!
!>      Block for diffusivity definision
      type val_diffuse_ctl
!>        Block name
        character(len=kchara) :: block_name = 'diffusion_control'
!
!>        Diffusivity variation flag
        type(read_character_item) :: r_variation_ctl
!>        Diffusivity variation data file name
        type(read_character_item) :: variation_file_name
!>        list of diffusivity
        type(ctl_array_r2) ::        diffusivity_list_ctl
!
!>        Latent heat point (Radius)
        type(read_real_item) :: ICB_reduction_radius
!>        Ratio of diffusivity reduction by the latent heat
        type(read_real_item) :: ICB_reduction_ratio
!>        Width of diffusivity reduction by the latent heat
        type(read_real_item) :: ICB_reduction_width
!
!>        loaded flag
        integer (kind=kint) :: i_val_diffuse = 0
      end type val_diffuse_ctl
!
!     4th level for masking
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_r_variation =     'radial_variation_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_val_file_name =   'variation_file_name'
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_diffusivity_list = 'diffusivity_list_ctl'
!
      character(len=kchara), parameter, private                         &
     &    :: hd_diff_reduce_radius = 'ICB_reduction_radius'
      character(len=kchara), parameter, private                         &
     &    :: hd_diff_reduce_ratio =  'ICB_reduction_ratio'
      character(len=kchara), parameter, private                         &
     &    :: hd_diff_reduce_width =  'ICB_reduction_width'
!
!    Deprecated label
      character(len=kchara), parameter, private                         &
     &    :: hd_diffusivity_reduction = 'ICB_diffusivity_reduction_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_val_diffuse_ctl_data                              &
     &         (id_control, hd_block, vdiffuse_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(val_diffuse_ctl), intent(inout) :: vdiffuse_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(vdiffuse_ctl%i_val_diffuse .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_r_variation,                 &
     &                           vdiffuse_ctl%r_variation_ctl)
        call read_chara_ctl_type(c_buf, hd_val_file_name,               &
     &                           vdiffuse_ctl%variation_file_name)
!
        call read_control_array_r2(id_control, hd_diffusivity_list,     &
     &      vdiffuse_ctl%diffusivity_list_ctl, c_buf)
!
        call read_real_ctl_type(c_buf, hd_diff_reduce_ratio,            &
     &                          vdiffuse_ctl%ICB_reduction_ratio)
        call read_real_ctl_type(c_buf, hd_diffusivity_reduction,        &
     &                          vdiffuse_ctl%ICB_reduction_ratio)
!
        call read_real_ctl_type(c_buf, hd_diff_reduce_width,            &
     &                          vdiffuse_ctl%ICB_reduction_width)
        call read_real_ctl_type(c_buf, hd_diff_reduce_radius,           &
     &                          vdiffuse_ctl%ICB_reduction_radius)
      end do
      vdiffuse_ctl%i_val_diffuse = 1
!
      end subroutine read_val_diffuse_ctl_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_val_diffuse_ctl_data                             &
     &         (id_control, vdiffuse_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(val_diffuse_ctl), intent(in) :: vdiffuse_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(vdiffuse_ctl%i_val_diffuse .le. 0) return
!
      maxlen = len_trim(hd_r_variation)
      maxlen = max(maxlen, len_trim(hd_val_file_name))
      maxlen = max(maxlen, len_trim(hd_diff_reduce_ratio))
      maxlen = max(maxlen, len_trim(hd_diff_reduce_width))
      maxlen = max(maxlen, len_trim(hd_diff_reduce_radius))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 vdiffuse_ctl%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          vdiffuse_ctl%r_variation_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          vdiffuse_ctl%variation_file_name)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
&                              vdiffuse_ctl%ICB_reduction_radius)
      call write_real_ctl_type(id_control, level, maxlen,               &
&                              vdiffuse_ctl%ICB_reduction_ratio)
      call write_real_ctl_type(id_control, level, maxlen,               &
&                              vdiffuse_ctl%ICB_reduction_width)
!
      call write_control_array_r2(id_control, level,                    &
     &                            vdiffuse_ctl%diffusivity_list_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                vdiffuse_ctl%block_name)
!
      end subroutine write_val_diffuse_ctl_data
!
!  ---------------------------------------------------------------------
!
      subroutine init_val_diffuse_ctl_label(hd_block, vdiffuse_ctl)
!
      character(len = kchara), intent(in) :: hd_block
      type(val_diffuse_ctl), intent(inout) :: vdiffuse_ctl
!
!
      vdiffuse_ctl%block_name = hd_block
!
        call init_chara_ctl_item_label                                  &
     &     (hd_r_variation, vdiffuse_ctl%r_variation_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_val_file_name, vdiffuse_ctl%variation_file_name)
!
        call init_r2_ctl_array_label                                    &
     &     (hd_diffusivity_list, vdiffuse_ctl%diffusivity_list_ctl)
!
        call init_real_ctl_item_label(hd_diff_reduce_radius,            &
     &      vdiffuse_ctl%ICB_reduction_radius)
        call init_real_ctl_item_label(hd_diff_reduce_ratio,             &
     &      vdiffuse_ctl%ICB_reduction_ratio)
        call init_real_ctl_item_label(hd_diff_reduce_width,             &
     &      vdiffuse_ctl%ICB_reduction_width)
!
      end subroutine init_val_diffuse_ctl_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_val_diffuse_ctl_data(vdiffuse_ctl)
!
      type(val_diffuse_ctl), intent(inout) :: vdiffuse_ctl
!
!
      vdiffuse_ctl%r_variation_ctl%iflag =  0
      vdiffuse_ctl%variation_file_name%iflag = 0
!
      call dealloc_control_array_r2                                     &
     &  (vdiffuse_ctl%diffusivity_list_ctl)
      vdiffuse_ctl%diffusivity_list_ctl%num =  0
      vdiffuse_ctl%diffusivity_list_ctl%icou = 0
!
      vdiffuse_ctl%ICB_reduction_radius%iflag = 0
      vdiffuse_ctl%ICB_reduction_ratio%iflag =  0
      vdiffuse_ctl%ICB_reduction_width%iflag =  0
!
      vdiffuse_ctl%i_val_diffuse =      0
!
      end subroutine dealloc_val_diffuse_ctl_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_val_diffuse_ctl_data(vdiffuse_c, new_vdiffuse_c)
!
      type(val_diffuse_ctl), intent(in) :: vdiffuse_c
      type(val_diffuse_ctl), intent(inout) :: new_vdiffuse_c
!
!
      new_vdiffuse_c%block_name =     vdiffuse_c%block_name
      new_vdiffuse_c%i_val_diffuse = vdiffuse_c%i_val_diffuse
!
      call copy_chara_ctl(vdiffuse_c%r_variation_ctl,                   &
     &                    new_vdiffuse_c%r_variation_ctl)
      call copy_chara_ctl(vdiffuse_c%variation_file_name,               &
     &                    new_vdiffuse_c%variation_file_name)
!
      call dup_control_array_r2(vdiffuse_c%diffusivity_list_ctl,        &
     &                          new_vdiffuse_c%diffusivity_list_ctl)
!
      end subroutine dup_val_diffuse_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_valuable_diffuse
