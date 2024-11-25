!>@file   t_ctl_data_valuable_density.f90
!!@brief  module t_ctl_data_valuable_density
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief Control data for valuable density
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine init_val_density_ctl_label(hd_block, polytrope_c)
!!      subroutine read_val_density_ctl_data                            &
!!     &         (id_control, hd_block, polytrope_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len = kchara), intent(in) :: hd_block
!!  `     type(val_density_ctl), intent(inout) :: polytrope_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_val_density_ctl_data                           &
!!     &          (id_control, polytrope_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        type(val_density_ctl), intent(in) :: polytrope_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_val_density_ctl_data(polytrope_c)
!!        type(val_density_ctl), intent(inout) :: polytrope_c
!!
!!      subroutine dup_val_density_ctl_data(polytrope_c,                &
!!     &                                    new_polytrope_c)
!!        type(val_density_ctl), intent(in) :: polytrope_c
!!        type(val_density_ctl), intent(inout) :: new_polytrope_c
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin polytrope_ctl
!!      density_variation_ctl    ON
!!      density_file_name     'density_variation'
!!
!!      bottom_density_ctl      0.53846   2.0
!!      top_density_ctl         1.53846   1.0
!!      polytrope_index_ctl     1.0
!!
!!      array density_list_ctl
!!        density_list_ctl      0.53846   1.2
!!        density_list_ctl      1.03846   1.05
!!        density_list_ctl      1.53846   1.0
!!      end array density_list_ctl
!!    end   polytrope_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_valuable_density
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
!>      Block for polytorope definision
      type val_density_ctl
!>        Block name
        character(len=kchara) :: block_name = 'polytrope_ctl'
!>        Density variation flag
        type(read_character_item) :: r_variation_ctl
!>        Density variation data file name
        type(read_character_item) :: variation_file_name
!
!>        Botton radius and density to define
        type(read_real2_item) :: bottom_density_ctl
!>        Top radius and density to define
        type(read_real2_item) :: top_density_ctl
!>        Polytrope index
        type(read_real_item) ::  polytrope_index_ctl
!
!>        list of density
        type(ctl_array_r2) ::   density_list_ctl
!
!>        loaded flag
        integer (kind=kint) :: i_val_density = 0
      end type val_density_ctl
!
!     4th level for masking
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_r_variation =      'density_variation_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_val_file_name =    'density_file_name'
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_bottom_density =  'bottom_density_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_top_density =     'top_density_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_polytrope_index = 'polytrope_index_ctl'
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_density_list = 'density_list_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_val_density_ctl_data                              &
     &         (id_control, hd_block, polytrope_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_block
!
      type(val_density_ctl), intent(inout) :: polytrope_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(polytrope_c%i_val_density .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_r_variation,                 &
     &                           polytrope_c%r_variation_ctl)
        call read_chara_ctl_type(c_buf, hd_val_file_name,               &
     &                           polytrope_c%variation_file_name)
!
        call read_real2_ctl_type(c_buf, hd_bottom_density,              &
     &                          polytrope_c%bottom_density_ctl)
        call read_real2_ctl_type(c_buf, hd_top_density,                 &
     &                          polytrope_c%top_density_ctl)
        call read_real_ctl_type(c_buf, hd_polytrope_index,              &
     &                          polytrope_c%polytrope_index_ctl)
!
        call read_control_array_r2(id_control, hd_density_list,         &
     &      polytrope_c%density_list_ctl, c_buf)
      end do
      polytrope_c%i_val_density = 1
!
      end subroutine read_val_density_ctl_data
!
!  ---------------------------------------------------------------------
!
      subroutine write_val_density_ctl_data                             &
     &         (id_control, polytrope_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      type(val_density_ctl), intent(in) :: polytrope_c
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(polytrope_c%i_val_density .le. 0) return
!
      maxlen = len_trim(hd_r_variation)
      maxlen = max(maxlen, len_trim(hd_val_file_name))
      maxlen = max(maxlen, len_trim(hd_bottom_density))
      maxlen = max(maxlen, len_trim(hd_top_density))
      maxlen = max(maxlen, len_trim(hd_polytrope_index))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 polytrope_c%block_name)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          polytrope_c%r_variation_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &                          polytrope_c%variation_file_name)
!
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &                          polytrope_c%bottom_density_ctl)
      call write_real2_ctl_type(id_control, level, maxlen,              &
     &                          polytrope_c%top_density_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &                          polytrope_c%polytrope_index_ctl)
!
      call write_control_array_r2(id_control, level,                    &
     &                            polytrope_c%density_list_ctl)
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                                polytrope_c%block_name)
!
      end subroutine write_val_density_ctl_data
!
!  ---------------------------------------------------------------------
!
      subroutine init_val_density_ctl_label(hd_block, polytrope_c)
!
      character(len = kchara), intent(in) :: hd_block
      type(val_density_ctl), intent(inout) :: polytrope_c
!
!
      polytrope_c%block_name = hd_block
!
        call init_chara_ctl_item_label                                  &
     &     (hd_r_variation, polytrope_c%r_variation_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_val_file_name, polytrope_c%variation_file_name)
!
        call init_real2_ctl_item_label                                  &
     &     (hd_bottom_density,  polytrope_c%bottom_density_ctl)
        call init_real2_ctl_item_label                                  &
     &     (hd_top_density,     polytrope_c%top_density_ctl)
        call init_real_ctl_item_label                                   &
     &     (hd_polytrope_index, polytrope_c%polytrope_index_ctl)
!
        call init_r2_ctl_array_label                                    &
     &     (hd_density_list, polytrope_c%density_list_ctl)
!
      end subroutine init_val_density_ctl_label
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_val_density_ctl_data(polytrope_c)
!
      type(val_density_ctl), intent(inout) :: polytrope_c
!
!
      polytrope_c%r_variation_ctl%iflag =  0
!
      polytrope_c%variation_file_name%iflag = 0
!
      polytrope_c%bottom_density_ctl%iflag =  0
      polytrope_c%top_density_ctl%iflag =     0
      polytrope_c%polytrope_index_ctl%iflag = 0
!
      call dealloc_control_array_r2                                     &
     &  (polytrope_c%density_list_ctl)
      polytrope_c%density_list_ctl%num =  0
      polytrope_c%density_list_ctl%icou = 0
!
      polytrope_c%i_val_density =      0
!
      end subroutine dealloc_val_density_ctl_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_val_density_ctl_data(polytrope_c,                  &
     &                                    new_polytrope_c)
!
      type(val_density_ctl), intent(in) :: polytrope_c
      type(val_density_ctl), intent(inout) :: new_polytrope_c
!
!
      new_polytrope_c%block_name =    polytrope_c%block_name
      new_polytrope_c%i_val_density = polytrope_c%i_val_density
!
      call copy_chara_ctl(polytrope_c%r_variation_ctl,                  &
     &                    new_polytrope_c%r_variation_ctl)
      call copy_chara_ctl(polytrope_c%variation_file_name,              &
     &                    new_polytrope_c%variation_file_name)
!
      call copy_real2_ctl(polytrope_c%bottom_density_ctl,               &
     &                    new_polytrope_c%bottom_density_ctl)
      call copy_real2_ctl(polytrope_c%top_density_ctl,                  &
     &                    new_polytrope_c%top_density_ctl)
      call copy_real_ctl(polytrope_c%polytrope_index_ctl,               &
     &                    new_polytrope_c%polytrope_index_ctl)
!
      call dup_control_array_r2(polytrope_c%density_list_ctl,           &
     &                          new_polytrope_c%density_list_ctl)
!
      end subroutine dup_val_density_ctl_data
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_valuable_density
