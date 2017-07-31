!
!      module t_control_data_4_iso
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine deallocate_cont_dat_4_iso(iso)
!!        type(iso_ctl), intent(inout) :: iso
!!
!!      subroutine read_iso_control_data(hd_block, iso)
!!      subroutine bcast_iso_control_data(iso)
!!        type(iso_ctl), intent(inout) :: iso
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      module t_control_data_4_iso
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_control_elements
      use t_read_control_arrays
!
      implicit  none
!
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
      subroutine deallocate_cont_dat_4_iso(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      call dealloc_control_array_c2(iso%iso_out_field_ctl)
      iso%iso_out_field_ctl%num =  0
      iso%iso_out_field_ctl%icou = 0
!
      call dealloc_control_array_chara(iso%iso_area_ctl)
      iso%iso_area_ctl%num =  0
      iso%iso_area_ctl%icou = 0
!
      end subroutine deallocate_cont_dat_4_iso
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_iso_control_data(hd_block, iso)
!
      character(len=kchara), intent(in) :: hd_block
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iso%i_iso_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iso%i_iso_ctl)
        if(iso%i_iso_ctl .gt. 0) exit
!
        call read_iso_result_control(iso)
        call read_iso_define_data(iso)
!
!
        call read_chara_ctl_type(hd_isosurf_prefix,                     &
     &      iso%iso_file_head_ctl)
        call read_chara_ctl_type(hd_iso_file_head,                      &
     &      iso%iso_file_head_ctl)
        call read_chara_ctl_type(hd_iso_out_type,                       &
     &      iso%iso_output_type_ctl)
      end do
!
      end subroutine read_iso_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_define_data(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_iso_define) .eq. 0) return
      if (iso%i_iso_define.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iso_define, iso%i_iso_define)
        if(iso%i_iso_define .gt. 0) exit
!
        call  read_iso_plot_area_ctl(iso)
!
!
        call read_chara_ctl_type(hd_iso_field, iso%isosurf_data_ctl)
        call read_chara_ctl_type(hd_iso_comp, iso%isosurf_comp_ctl)
!
        call read_real_ctl_type(hd_iso_value, iso%isosurf_value_ctl)
!
        call read_control_array_c1(hd_iso_area, iso%iso_area_ctl)
      end do
!
      end subroutine read_iso_define_data
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_result_control(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if      (right_begin_flag(hd_field_on_iso) .eq. 0                 &
     &   .and. right_begin_flag(hd_iso_result) .eq. 0) return
      if (iso%i_iso_result.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_field_on_iso, iso%i_iso_result)
        if(iso%i_iso_result .gt. 0) exit
        call find_control_end_flag(hd_iso_result, iso%i_iso_result)
        if(iso%i_iso_result .gt. 0) exit
!
        call read_control_array_c2                                      &
     &     (hd_iso_result_field, iso%iso_out_field_ctl)
!
        call read_chara_ctl_type(hd_result_type,                        &
     &      iso%iso_result_type_ctl)
!
        call read_real_ctl_type(hd_result_value,                        &
     &      iso%result_value_iso_ctl)
      end do
!
      end subroutine read_iso_result_control
!
!   --------------------------------------------------------------------
!
      subroutine read_iso_plot_area_ctl(iso)
!
      type(iso_ctl), intent(inout) :: iso
!
!
      if(right_begin_flag(hd_iso_plot_area) .eq. 0) return
      if (iso%i_iso_plot_area.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_iso_plot_area,                    &
     &      iso%i_iso_plot_area)
        if(iso%i_iso_plot_area .gt. 0) exit
!
        call read_control_array_c1(hd_iso_area, iso%iso_area_ctl)
        call read_control_array_c1(hd_iso_plot_grp, iso%iso_area_ctl)
      end do
!
      end subroutine read_iso_plot_area_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_iso_control_data(iso)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(iso_ctl), intent(inout) :: iso
!
!
      call MPI_BCAST(iso%i_iso_ctl,  ione,                              &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(iso%i_iso_define,  ione,                           &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(iso%i_iso_result,  ione,                           &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(iso%i_iso_plot_area,  ione,                        &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_c1(iso%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso%iso_file_head_ctl)
      call bcast_ctl_type_c1(iso%iso_output_type_ctl)
!
      call bcast_ctl_type_c1(iso%isosurf_data_ctl)
      call bcast_ctl_type_c1(iso%isosurf_comp_ctl)
!
      call bcast_ctl_type_r1(iso%isosurf_value_ctl)
!
      call bcast_ctl_array_c1(iso%iso_area_ctl)
      call bcast_ctl_array_c2(iso%iso_out_field_ctl)
!
      call bcast_ctl_type_c1(iso%iso_result_type_ctl)
!
      call bcast_ctl_type_r1(iso%result_value_iso_ctl)
!
      end subroutine bcast_iso_control_data
!
!   --------------------------------------------------------------------
!
      end module t_control_data_4_iso
