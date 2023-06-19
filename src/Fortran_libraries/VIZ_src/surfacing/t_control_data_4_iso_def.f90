!>@file   t_control_data_4_iso_def.f90
!!@brief  module t_control_data_4_iso_def
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each isosurface
!!
!!@verbatim
!!      subroutine init_iso_define_control(iso_def_c)
!!      subroutine dealloc_iso_define_control(iso_def_c)
!!        type(iso_define_ctl), intent(inout) :: iso_def_c
!!      subroutine dup_iso_define_control(org_iso_def_c, new_iso_def_c)
!!        type(iso_define_ctl), intent(in) :: org_iso_def_c
!!        type(iso_define_ctl), intent(inout) :: new_iso_def_c
!!
!!      subroutine read_iso_define_data                                 &
!!     &         (id_control, hd_block, iso_def_c, c_buf)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(iso_define_ctl), intent(inout) :: iso_def_c
!!         type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_iso_define_data                                &
!!     &         (id_control, hd_block, iso_def_c, level)
!!         integer(kind = kint), intent(in) :: id_control
!!         character(len=kchara), intent(in) :: hd_block
!!         type(iso_define_ctl), intent(in) :: iso_def_c
!!         integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_iso_define_control()
!!      subroutine set_label_iso_define_control(names)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's surface rendering
!!
!!    begin isosurf_define
!!      isosurf_field        pressure
!!      isosurf_component      scalar
!!      isosurf_value            4000.0
!!
!!      array isosurf_area_ctl   2
!!        isosurf_area_ctl   inner_core   end
!!        isosurf_area_ctl   outer_core   end
!!      end array isosurf_area_ctl
!!    end isosurf_define
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    iso_output_type:
!!           ucd, OpenDX
!!
!!    result_type:  (Original name: display_method)
!!                   specified_fields
!!                   constant
!!    num_result_comp: number of fields
!!    output_field: (Original name: color_comp and color_subcomp)
!!         field and componenet name for output
!!           x, y, z, radial, elevation, azimuth, cylinder_r
!!           norm, vector, tensor, spherical_vector, cylindrical_vector
!!    result_value: (Original name: specified_color)
!!
!!    
!!
!!    isosurf_data: field for isosurface
!!    isosurf_comp: component for isosurface
!!           x, y, z, radial, elevation, azimuth, cylinder_r, norm
!!    isosurf_value:  value for isosurface
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!@endverbatim
!
      module t_control_data_4_iso_def
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_real
      use t_control_array_character
      use t_control_data_4_fld_on_psf
!
      implicit  none
!
!>      Structure of isosurface define control
      type iso_define_ctl
!>        Structure for field name for isosurface
        type(read_character_item) :: isosurf_data_ctl
!>        Structure for component name for isosurface
        type(read_character_item) :: isosurf_comp_ctl
!
!>        Structure for isosurface value
        type(read_real_item) :: isosurf_value_ctl
!
!>      Structure for element group list for isosurfacing
!!@n      iso_area_ctl%c_tbl: Name of element group
        type(ctl_array_chara) :: iso_area_ctl
!
!         2nd level for isosurface_ctl
        integer (kind=kint) :: i_iso_define =    0
      end type iso_define_ctl
!
!     3nd level for isosurf_define
      character(len=kchara), parameter                                  &
     &             :: hd_iso_field =  'isosurf_field'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_comp =   'isosurf_component'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_value =  'isosurf_value'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_area =   'isosurf_area_ctl'
!
      integer(kind = kint), parameter :: n_label_iso_define_ctl = 4
!
      private :: hd_iso_area, hd_iso_value, hd_iso_comp, hd_iso_field
      private :: n_label_iso_define_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_iso_define_control(iso_def_c)
!
      type(iso_define_ctl), intent(inout) :: iso_def_c
!
!
      iso_def_c%isosurf_value_ctl%realvalue =    0.0d0
      iso_def_c%iso_area_ctl%num =      0
!
      end subroutine init_iso_define_control
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iso_define_control(iso_def_c)
!
      type(iso_define_ctl), intent(inout) :: iso_def_c
!
!
      call dealloc_control_array_chara(iso_def_c%iso_area_ctl)
      iso_def_c%iso_area_ctl%num =  0
      iso_def_c%iso_area_ctl%icou = 0
!
      iso_def_c%isosurf_value_ctl%iflag =    0
      iso_def_c%isosurf_comp_ctl%iflag =     0
      iso_def_c%isosurf_data_ctl%iflag =     0
!
      iso_def_c%i_iso_define =      0
!
      end subroutine dealloc_iso_define_control
!
!  ---------------------------------------------------------------------
!
      subroutine dup_iso_define_control(org_iso_def_c, new_iso_def_c)
!
      type(iso_define_ctl), intent(in) :: org_iso_def_c
      type(iso_define_ctl), intent(inout) :: new_iso_def_c
!
!
      call dup_control_array_c1(org_iso_def_c%iso_area_ctl,             &
     &                          new_iso_def_c%iso_area_ctl)
      call copy_real_ctl(org_iso_def_c%isosurf_value_ctl,               &
     &                   new_iso_def_c%isosurf_value_ctl)
      call copy_chara_ctl(org_iso_def_c%isosurf_comp_ctl,               &
     &                    new_iso_def_c%isosurf_comp_ctl)
      call copy_chara_ctl(org_iso_def_c%isosurf_data_ctl,               &
     &                    new_iso_def_c%isosurf_data_ctl)
!
      new_iso_def_c%i_iso_define =    org_iso_def_c%i_iso_define
!
      end subroutine dup_iso_define_control
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_iso_define_data                                   &
     &         (id_control, hd_block, iso_def_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_define_ctl), intent(inout) :: iso_def_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iso_def_c%i_iso_define .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iso_field, iso_def_c%isosurf_data_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iso_comp, iso_def_c%isosurf_comp_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_iso_value, iso_def_c%isosurf_value_ctl)
!
        call read_control_array_c1                                      &
     &     (id_control, hd_iso_area, iso_def_c%iso_area_ctl, c_buf)
      end do
      iso_def_c%i_iso_define = 1
!
      end subroutine read_iso_define_data
!
!   --------------------------------------------------------------------
!
      subroutine write_iso_define_data                                  &
     &         (id_control, hd_block, iso_def_c, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_define_ctl), intent(in) :: iso_def_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(iso_def_c%i_iso_define .le. 0) return
!
      maxlen = len_trim(hd_iso_field)
      maxlen = max(maxlen, len_trim(hd_iso_comp))
      maxlen = max(maxlen, len_trim(hd_iso_value))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iso_field, iso_def_c%isosurf_data_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iso_comp, iso_def_c%isosurf_comp_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_iso_value, iso_def_c%isosurf_value_ctl)
!
      call write_control_array_c1(id_control, level,                    &
     &    hd_iso_area, iso_def_c%iso_area_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_iso_define_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_label_iso_define_control()
      num_label_iso_define_control = n_label_iso_define_ctl
      return
      end function num_label_iso_define_control
!
! ----------------------------------------------------------------------
!
      subroutine set_label_iso_define_control(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_iso_define_ctl)
!
!
      call set_control_labels(hd_iso_field, names( 1))
      call set_control_labels(hd_iso_comp,  names( 2))
      call set_control_labels(hd_iso_value, names( 3))
      call set_control_labels(hd_iso_area,  names( 4))
!
      end subroutine set_label_iso_define_control
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_4_iso_def
