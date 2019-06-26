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
!
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
!!
!!  end isosurface_ctl
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
      module t_control_data_4_iso
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_control_elements
      use t_control_array_character
      use t_control_array_character2
!
      implicit  none
!
      type iso_ctl
!>        Structure for file prefix
        type(read_character_item) :: iso_file_head_ctl
!>        Structure for data field format
        type(read_character_item) :: iso_output_type_ctl
!
!>        Structure for field name for isosurface
        type(read_character_item) :: isosurf_data_ctl
!>        Structure for component name for isosurface
        type(read_character_item) :: isosurf_comp_ctl
!
!>        Structure for isosurface value
        type(read_real_item) :: isosurf_value_ctl
!
!>        Structure for single number for isosurface
        type(read_real_item) :: result_value_iso_ctl
!
!>        Structure for result type
        type(read_character_item) :: iso_result_type_ctl
!
!>      Structure for list of output field
!!@n      iso_out_field_ctl%c1_tbl: Name of field
!!@n      iso_out_field_ctl%c2_tbl: Name of component
        type(ctl_array_c2) :: iso_out_field_ctl
!
!>      Structure for element group list for isosurfacing
!!@n      iso_area_ctl%c_tbl: Name of element group
        type(ctl_array_chara) :: iso_area_ctl
!
!     Top level
        integer (kind=kint) :: i_iso_ctl = 0
!     2nd level for isosurface_ctl
        integer (kind=kint) :: i_iso_define =    0
        integer (kind=kint) :: i_iso_result =    0
!     3nd level for isosurf_define
        integer (kind=kint) :: i_iso_plot_area = 0
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
!     3nd level for isosurf_define
      character(len=kchara), parameter                                  &
     &             :: hd_iso_field =     'isosurf_field'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_comp =      'isosurf_component'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_value =     'isosurf_value'
!
      character(len=kchara), parameter                                  &
     &                  :: hd_iso_area = 'isosurf_area_ctl'
!
!     3rd level for field_on_isosurf
      character(len=kchara), parameter                                  &
     &             :: hd_result_type =       'result_type'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_result_field = 'output_field'
      character(len=kchara), parameter                                  &
     &             :: hd_result_value =      'result_value'
!
!
!      Deprecated labels
!
      character(len=kchara), parameter                                  &
     &             :: hd_iso_file_head = 'iso_file_head'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_result = 'isosurf_result_define'
!
      character(len=kchara), parameter                                  &
     &             :: hd_iso_plot_area = 'plot_area_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_plot_grp  = 'chosen_ele_grp_ctl'
!
      private :: hd_iso_plot_grp, hd_result_type, hd_iso_area
      private :: hd_result_value, hd_iso_plot_area, hd_iso_value
      private :: hd_iso_comp, hd_iso_field
      private :: hd_iso_result, hd_field_on_iso
      private :: hd_iso_define, hd_iso_out_type
      private :: hd_isosurf_prefix, hd_iso_file_head
      private :: hd_iso_result_field
!
      private :: read_iso_define_data
      private :: read_iso_result_control, read_iso_plot_area_ctl
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
      iso_c%isosurf_value_ctl%realvalue =    0.0d0
      iso_c%result_value_iso_ctl%realvalue = 0.0d0
      iso_c%iso_out_field_ctl%num = 0
      iso_c%iso_area_ctl%num =      0
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
      iso_c%isosurf_data_ctl%iflag =     0
      iso_c%isosurf_comp_ctl%iflag =     0
      iso_c%isosurf_value_ctl%iflag =    0
      iso_c%result_value_iso_ctl%iflag = 0
      iso_c%iso_result_type_ctl%iflag =  0
!
      iso_c%i_iso_ctl =         0
      iso_c%i_iso_define =      0
      iso_c%i_iso_result =      0
      iso_c%i_iso_plot_area =   0
!
      call dealloc_control_array_c2(iso_c%iso_out_field_ctl)
      iso_c%iso_out_field_ctl%num =  0
      iso_c%iso_out_field_ctl%icou = 0
!
      call dealloc_control_array_chara(iso_c%iso_area_ctl)
      iso_c%iso_area_ctl%num =  0
      iso_c%iso_area_ctl%icou = 0
!
      end subroutine dealloc_cont_dat_4_iso
!
!  ---------------------------------------------------------------------
!
      subroutine dup_control_4_iso(org_iso_c, new_iso_c)
!
      use copy_control_elements
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
      call copy_chara_ctl(org_iso_c%isosurf_data_ctl,                   &
     &                    new_iso_c%isosurf_data_ctl)
      call copy_chara_ctl(org_iso_c%isosurf_comp_ctl,                   &
     &                    new_iso_c%isosurf_comp_ctl)
      call copy_chara_ctl(org_iso_c%iso_result_type_ctl,                &
     &                    new_iso_c%iso_result_type_ctl)
!
      call copy_real_ctl(org_iso_c%isosurf_value_ctl,                   &
     &                   new_iso_c%isosurf_value_ctl)
      call copy_real_ctl(org_iso_c%result_value_iso_ctl,                &
     &                   new_iso_c%result_value_iso_ctl)
!
      call dup_control_array_c2(org_iso_c%iso_out_field_ctl,            &
     &                          new_iso_c%iso_out_field_ctl)
      call dup_control_array_c1(org_iso_c%iso_area_ctl,                 &
     &                          new_iso_c%iso_area_ctl)
!
      new_iso_c%i_iso_ctl =       org_iso_c%i_iso_ctl
      new_iso_c%i_iso_define =    org_iso_c%i_iso_define
      new_iso_c%i_iso_result =    org_iso_c%i_iso_result
      new_iso_c%i_iso_plot_area = org_iso_c%i_iso_plot_area
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
        call read_iso_result_control(id_control, hd_field_on_iso,       &
     &      iso_c, c_buf)
        call read_iso_result_control(id_control, hd_iso_result,         &
     &      iso_c, c_buf)
        call read_iso_define_data                                       &
     &     (id_control, hd_iso_define, iso_c, c_buf)
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
!
      subroutine read_iso_define_data                                   &
     &         (id_control, hd_block, iso_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(inout) :: iso_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iso_c%i_iso_define .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_iso_plot_area_ctl(id_control, hd_iso_plot_area,       &
     &      iso_c, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iso_field, iso_c%isosurf_data_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_iso_comp, iso_c%isosurf_comp_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_iso_value, iso_c%isosurf_value_ctl)
!
        call read_control_array_c1                                      &
     &     (id_control, hd_iso_area, iso_c%iso_area_ctl, c_buf)
      end do
      iso_c%i_iso_define = 1
!
      end subroutine read_iso_define_data
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_result_control                                &
     &         (id_control, hd_block, iso_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(inout) :: iso_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iso_c%i_iso_result .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c2(id_control,                          &
     &      hd_iso_result_field, iso_c%iso_out_field_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_result_type,                 &
     &      iso_c%iso_result_type_ctl)
!
        call read_real_ctl_type(c_buf, hd_result_value,                 &
     &      iso_c%result_value_iso_ctl)
      end do
      iso_c%i_iso_result = 1
!
      end subroutine read_iso_result_control
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_plot_area_ctl                                 &
     &         (id_control, hd_block, iso_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(iso_ctl), intent(inout) :: iso_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(iso_c%i_iso_plot_area.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control,                          &
     &      hd_iso_area, iso_c%iso_area_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_iso_plot_grp, iso_c%iso_area_ctl, c_buf)
      end do
      iso_c%i_iso_plot_area = 1
!
      end subroutine read_iso_plot_area_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_iso_control_data(iso_c)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(iso_ctl), intent(inout) :: iso_c
!
!
      call MPI_BCAST(iso_c%i_iso_ctl,  1,                               &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(iso_c%i_iso_define,  1,                            &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(iso_c%i_iso_result,  1,                            &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(iso_c%i_iso_plot_area,  1,                         &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_c1(iso_c%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso_c%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso_c%iso_output_type_ctl)
!
      call bcast_ctl_type_c1(iso_c%isosurf_data_ctl)
      call bcast_ctl_type_c1(iso_c%isosurf_comp_ctl)
!
      call bcast_ctl_type_r1(iso_c%isosurf_value_ctl)
!
      call bcast_ctl_array_c1(iso_c%iso_area_ctl)
      call bcast_ctl_array_c2(iso_c%iso_out_field_ctl)
!
      call bcast_ctl_type_c1(iso_c%iso_result_type_ctl)
!
      call bcast_ctl_type_r1(iso_c%result_value_iso_ctl)
!
      end subroutine bcast_iso_control_data
!
!   --------------------------------------------------------------------
!
      end module t_control_data_4_iso
