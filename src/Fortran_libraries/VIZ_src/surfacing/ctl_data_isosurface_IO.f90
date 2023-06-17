!>@file   ctl_data_isosurface_IO.f90
!!@brief  module ctl_data_isosurface_IO
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each isosurface
!!
!!@verbatim
!!      subroutine s_read_iso_control_data                              &
!!     &         (id_control, hd_block, iso_c, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(iso_ctl), intent(inout) :: iso_c
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_iso_control_data                               &
!!     &         (id_control, hd_block, iso_c, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(iso_ctl), intent(in) :: iso_c
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_iso_ctl()
!!      integer(kind = kint) function num_label_iso_ctl_w_dpl()
!!      subroutine set_label_iso_ctl(names)
!!      subroutine set_label_iso_ctl_w_dpl(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's surface rendering
!!
!!  begin isosurface_ctl
!!    isosurface_file_prefix    'psf'
!!    iso_output_type            ucd
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
!!    begin field_on_isosurf
!!      result_type      constant
!!      result_value     0.7
!!      array output_field   2
!!        output_field    velocity         vector   end
!!        output_field    magnetic_field   radial   end
!!      end array output_field
!!    end field_on_isosurf
!!  end isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    iso_output_type:
!!           ucd, ucd_gz, iso
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
      module ctl_data_isosurface_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_control_data_4_iso
      use t_read_control_elements
      use t_control_array_character
      use t_control_data_4_iso_def
      use t_control_data_4_fld_on_psf
      use calypso_mpi
!
      implicit  none
!
!     2nd level for isosurface_ctl
      character(len=kchara), parameter, private                         &
     &             :: hd_isosurf_prefix = 'isosurface_file_prefix'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_out_type =   'iso_output_type'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_define =     'isosurf_define'
      character(len=kchara), parameter, private                         &
     &             :: hd_field_on_iso =   'field_on_isosurf'
!
!      Deprecated labels
!
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_file_head = 'iso_file_head'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_result = 'isosurf_result_define'
!
      integer(kind = kint), parameter :: n_label_iso_ctl = 4
      integer(kind = kint), parameter :: n_label_iso_ctl_w_dpl = 6
!
      private :: n_label_iso_ctl, n_label_iso_ctl_w_dpl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_iso_control_data                                &
     &         (id_control, hd_block, iso_c, c_buf)
!
      use skip_comment_f
      use ctl_file_field_on_psf_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(inout) :: iso_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iso_c%i_iso_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call sel_read_ctl_field_on_psf_file                             &
     &     (id_control, hd_field_on_iso, iso_c%fname_fld_on_iso,        &
     &      iso_c%fld_on_iso_c, c_buf)
        call sel_read_ctl_field_on_psf_file                             &
     &     (id_control, hd_iso_result, iso_c%fname_fld_on_iso,          &
     &      iso_c%fld_on_iso_c, c_buf)
!
        call read_iso_define_data                                       &
     &     (id_control, hd_iso_define, iso_c%iso_def_c, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_isosurf_prefix,              &
     &      iso_c%iso_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_iso_file_head,               &
     &      iso_c%iso_file_head_ctl)
        call read_chara_ctl_type(c_buf, hd_iso_out_type,                &
     &      iso_c%iso_output_type_ctl)
      end do
      iso_c%i_iso_ctl = 1
!
      end subroutine s_read_iso_control_data
!
!   --------------------------------------------------------------------
!
      subroutine write_iso_control_data                                 &
     &         (id_control, hd_block, iso_c, level)
!
      use write_control_elements
      use ctl_file_field_on_psf_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(in) :: iso_c
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(iso_c%i_iso_ctl .le. 0) return
!
      maxlen = len_trim(hd_isosurf_prefix)
      maxlen = max(maxlen, len_trim(hd_iso_out_type))
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_isosurf_prefix, iso_c%iso_file_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_iso_out_type, iso_c%iso_output_type_ctl)
!
      call write_iso_define_data(id_control, hd_iso_define,             &
     &                           iso_c%iso_def_c, level)
      call sel_write_ctl_field_on_psf_file(id_control, hd_field_on_iso, &
     &    iso_c%fname_fld_on_iso, iso_c%fld_on_iso_c, level)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_iso_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function num_label_iso_ctl()
      num_label_iso_ctl = n_label_iso_ctl
      return
      end function num_label_iso_ctl
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_iso_ctl_w_dpl()
      num_label_iso_ctl_w_dpl = n_label_iso_ctl_w_dpl
      return
      end function num_label_iso_ctl_w_dpl
!
! ----------------------------------------------------------------------
!
      subroutine set_label_iso_ctl(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_iso_ctl)
!
!
      call set_control_labels(hd_isosurf_prefix, names( 1))
      call set_control_labels(hd_iso_out_type,   names( 2))
      call set_control_labels(hd_iso_define,     names( 3))
      call set_control_labels(hd_field_on_iso,   names( 4))
!
      end subroutine set_label_iso_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_iso_ctl_w_dpl(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_iso_ctl_w_dpl)
!
!
      call set_label_iso_ctl(names(1))
      call set_control_labels(hd_iso_file_head,  names( 5))
      call set_control_labels(hd_iso_result,     names( 6))
!
      end subroutine set_label_iso_ctl_w_dpl
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_isosurface_IO
