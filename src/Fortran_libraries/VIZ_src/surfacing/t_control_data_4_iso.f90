!>@file   t_control_data_4_iso.f90
!!@brief  module t_control_data_4_iso
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for each isosurface
!!
!!@verbatim
!!      subroutine init_iso_ctl_stract(iso_c)
!!      subroutine dealloc_cont_dat_4_iso(iso_c)
!!        type(iso_ctl), intent(inout) :: iso_c
!!      subroutine dup_control_4_iso(org_iso_c, new_iso_c)
!!        type(iso_ctl), intent(in) :: org_iso_c
!!        type(iso_ctl), intent(inout) :: new_iso_c
!!
!!      subroutine read_iso_control_data                                &
!!     &         (id_control, hd_block, iso_c, c_buf)
!!      subroutine bcast_iso_control_data(iso_c)
!!        type(iso_ctl), intent(inout) :: iso_c
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
      module t_control_data_4_iso
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_array_character
      use t_control_data_4_iso_def
      use t_control_data_4_fld_on_psf
!
      implicit  none
!
      type iso_ctl
!>        Structure of isosurface define control
        type(iso_define_ctl) :: iso_def_c
!>        Structure of fields on isosurface control
        type(field_on_psf_ctl) :: fld_on_iso_c
!
!>        Structure for file prefix
        type(read_character_item) :: iso_file_head_ctl
!>        Structure for data field format
        type(read_character_item) :: iso_output_type_ctl
!
!     Top level
        integer (kind=kint) :: i_iso_ctl = 0
      end type iso_ctl
!
!     2nd level for isosurface_ctl
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_prefix = 'isosurface_file_prefix'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_out_type =   'iso_output_type'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_define =     'isosurf_define'
      character(len=kchara), parameter                                  &
     &             :: hd_field_on_iso =   'field_on_isosurf'
!
!      Deprecated labels
!
      character(len=kchara), parameter                                  &
     &             :: hd_iso_file_head = 'iso_file_head'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_result = 'isosurf_result_define'
!
      integer(kind = kint), parameter :: n_label_iso_ctl = 4
      integer(kind = kint), parameter :: n_label_iso_ctl_w_dpl = 6
!
      private :: hd_iso_result, hd_field_on_iso
      private :: hd_iso_define, hd_iso_out_type
      private :: hd_isosurf_prefix, hd_iso_file_head
      private :: n_label_iso_ctl, n_label_iso_ctl_w_dpl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_iso_ctl_stract(iso_c)
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      call init_iso_define_control(iso_c%iso_def_c)
      call init_fld_on_psf_control(iso_c%fld_on_iso_c)
!
      end subroutine init_iso_ctl_stract
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_cont_dat_4_iso(iso_c)
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      iso_c%iso_file_head_ctl%iflag =    0
      iso_c%iso_output_type_ctl%iflag =  0
!
      iso_c%i_iso_ctl =         0
!
      call dealloc_iso_define_control(iso_c%iso_def_c)
      call dealloc_fld_on_psf_control(iso_c%fld_on_iso_c)
!
      end subroutine dealloc_cont_dat_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_iso(org_iso_c, new_iso_c)
!
      type(iso_ctl), intent(in) :: org_iso_c
      type(iso_ctl), intent(inout) :: new_iso_c
!
!
      call copy_chara_ctl(org_iso_c%iso_file_head_ctl,                  &
     &                    new_iso_c%iso_file_head_ctl)
      call copy_chara_ctl(org_iso_c%iso_output_type_ctl,                &
     &                    new_iso_c%iso_output_type_ctl)
!
      new_iso_c%i_iso_ctl =       org_iso_c%i_iso_ctl
!
      call dup_iso_define_control                                       &
     &   (org_iso_c%iso_def_c, new_iso_c%iso_def_c)
      call dup_fld_on_psf_control                                       &
     &   (org_iso_c%fld_on_iso_c, new_iso_c%fld_on_iso_c)
!
      end subroutine dup_control_4_iso
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_iso_control_data                                  &
     &         (id_control, hd_block, iso_c, c_buf)
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
        call read_fld_on_psf_control(id_control, hd_field_on_iso,       &
     &      iso_c%fld_on_iso_c, c_buf)
        call read_fld_on_psf_control(id_control, hd_iso_result,         &
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
      end subroutine read_iso_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_iso_control_data(iso_c)
!
      use calypso_mpi
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      call calypso_mpi_bcast_one_int(iso_c%i_iso_ctl, 0)
!
      call bcast_ctl_type_c1(iso_c%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso_c%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso_c%iso_output_type_ctl)
!
      call bcast_iso_define_control(iso_c%iso_def_c)
      call bcast_fld_on_psf_control(iso_c%fld_on_iso_c)
!
      end subroutine bcast_iso_control_data
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
      end module t_control_data_4_iso
