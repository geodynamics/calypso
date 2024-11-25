!>@file   t_control_data_4_fld_on_psf.f90
!!@brief  module t_control_data_4_fld_on_psf
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for field on isosurface
!!
!!@verbatim
!!      subroutine init_fld_on_psf_control(hd_block, fld_on_psf_c)
!!      subroutine dealloc_fld_on_psf_control(fld_on_psf_c)
!!        type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!!      subroutine dup_fld_on_psf_control                               &
!!     &         (org_fld_on_iso_c, new_fld_on_iso_c)
!!        type(field_on_psf_ctl), intent(in) :: org_fld_on_iso_c
!!        type(field_on_psf_ctl), intent(inout) :: new_fld_on_iso_c
!!
!!      subroutine read_fld_on_psf_control                              &
!!     &         (id_control, hd_block, fld_on_psf_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_fld_on_psf_control                             &
!!     &         (id_control, hd_block, fld_on_psf_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine add_fields_on_psf_to_fld_ctl(fld_on_psf_c, field_ctl)
!!        type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's surface rendering
!!
!!    begin field_on_isosurf
!!      result_type      constant
!!      result_value     0.7
!!      array output_field
!!        output_field    velocity         vector   end
!!        output_field    magnetic_field   radial   end
!!      end array output_field
!!    end field_on_isosurf
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    result_type:  (Original name: display_method)
!!                   specified_fields
!!                   constant
!!    output_field: (Original name: color_comp and color_subcomp)
!!         field and componenet name for output
!!           x, y, z, radial, elevation, azimuth, cylinder_r
!!           norm, vector, tensor, spherical_vector, cylindrical_vector
!!    result_value: (Original name: specified_color)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_4_fld_on_psf
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_character
      use t_control_array_character2
!
      implicit  none
!
!>      Structure of fields on isosurface control
      type field_on_psf_ctl
!>        Block name
        character(len=kchara) :: block_name = 'output_field_define'
!
!>      Structure for list of output field
!!@n      field_output_ctl%c1_tbl: Name of field
!!@n      field_output_ctl%c2_tbl: Name of component
        type(ctl_array_c2) :: field_output_ctl
!>        Structure for single number for isosurface
        type(read_real_item) :: output_value_ctl
!>        Structure for result type
        type(read_character_item) :: output_type_ctl
!
        integer (kind=kint) :: i_iso_result =    0
      end type field_on_psf_ctl
!
!     3rd level for field_on_isosurf
      character(len=kchara), parameter                                  &
     &             :: hd_result_type =       'result_type'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_result_field = 'output_field'
      character(len=kchara), parameter                                  &
     &             :: hd_result_value =      'result_value'
!
      private :: hd_result_type, hd_iso_result_field, hd_result_value
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_fld_on_psf_control(hd_block, fld_on_psf_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!
!
      fld_on_psf_c%field_output_ctl%num = 0
      fld_on_psf_c%output_value_ctl%realvalue = 0.0d0
!
      fld_on_psf_c%block_name = hd_block
        call init_chara2_ctl_array_label                                &
     &     (hd_iso_result_field, fld_on_psf_c%field_output_ctl)
!
        call init_chara_ctl_item_label(hd_result_type,                  &
     &      fld_on_psf_c%output_type_ctl)
        call init_real_ctl_item_label(hd_result_value,                  &
     &      fld_on_psf_c%output_value_ctl)
!
      end subroutine init_fld_on_psf_control
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fld_on_psf_control(fld_on_psf_c)
!
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
!
!
      fld_on_psf_c%output_value_ctl%iflag = 0
      fld_on_psf_c%output_type_ctl%iflag =  0
!
      call dealloc_control_array_c2(fld_on_psf_c%field_output_ctl)
      fld_on_psf_c%field_output_ctl%num =  0
      fld_on_psf_c%field_output_ctl%icou = 0
!
      fld_on_psf_c%i_iso_result =      0
!
      end subroutine dealloc_fld_on_psf_control
!
!  ---------------------------------------------------------------------
!
      subroutine dup_fld_on_psf_control                                 &
     &         (org_fld_on_iso_c, new_fld_on_iso_c)
!
      type(field_on_psf_ctl), intent(in) :: org_fld_on_iso_c
      type(field_on_psf_ctl), intent(inout) :: new_fld_on_iso_c
!
!
      call copy_real_ctl(org_fld_on_iso_c%output_value_ctl,             &
     &                   new_fld_on_iso_c%output_value_ctl)
      call copy_chara_ctl(org_fld_on_iso_c%output_type_ctl,             &
     &                    new_fld_on_iso_c%output_type_ctl)
      call dup_control_array_c2(org_fld_on_iso_c%field_output_ctl,      &
     &                          new_fld_on_iso_c%field_output_ctl)
!
      new_fld_on_iso_c%block_name =   org_fld_on_iso_c%block_name
      new_fld_on_iso_c%i_iso_result = org_fld_on_iso_c%i_iso_result
!
      end subroutine dup_fld_on_psf_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_fld_on_psf_control                                &
     &         (id_control, hd_block, fld_on_psf_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(field_on_psf_ctl), intent(inout) :: fld_on_psf_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(fld_on_psf_c%i_iso_result .gt. 0) return
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c2(id_control,                          &
     &      hd_iso_result_field, fld_on_psf_c%field_output_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_result_type,                 &
     &      fld_on_psf_c%output_type_ctl)
!
        call read_real_ctl_type(c_buf, hd_result_value,                 &
     &      fld_on_psf_c%output_value_ctl)
      end do
      fld_on_psf_c%i_iso_result = 1
!
      end subroutine read_fld_on_psf_control
!
!   --------------------------------------------------------------------
!
      subroutine write_fld_on_psf_control                               &
     &         (id_control, hd_block, fld_on_psf_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(fld_on_psf_c%i_iso_result .le. 0) return
!
      maxlen = len_trim(hd_result_type)
      maxlen = max(maxlen, len_trim(hd_result_value))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fld_on_psf_c%output_type_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    fld_on_psf_c%output_value_ctl)
!
      call write_control_array_c2(id_control, level,                    &
     &    fld_on_psf_c%field_output_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_fld_on_psf_control
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine add_fields_on_psf_to_fld_ctl(fld_on_psf_c, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(field_on_psf_ctl), intent(in) :: fld_on_psf_c
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i_fld
!
!
      do i_fld = 1, fld_on_psf_c%field_output_ctl%num
        call add_viz_name_ctl                                           &
     &     (fld_on_psf_c%field_output_ctl%c1_tbl(i_fld), field_ctl)
      end do
!
      end subroutine add_fields_on_psf_to_fld_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_fld_on_psf
