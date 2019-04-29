!t_control_param_assemble.f90
!      module t_control_param_assemble
!
!      Written by H. Matsui
!
!!      subroutine set_assemble_rst_file_param                          &
!!     &         (source_plt, assemble_plt, asbl_param)
!!      subroutine set_assemble_ucd_file_param                          &
!!     &         (source_plt, assemble_plt, asbl_param)
!!
!!      subroutine set_assemble_step_4_rst(t_mge_ctl, asbl_param)
!!      subroutine set_assemble_step_4_ucd(t_mge_ctl, asbl_param)
!!      subroutine set_control_new_step(t2_mge_ctl, asbl_param)
!!
!!      subroutine set_delete_flag_4_assemble(assemble_plt, asbl_param)
!!      subroutine set_magnetic_ratio_4_assemble                        &
!!     &         (magnetic_ratio_ctl, asbl_param)
!!        type(time_data_control), intent(in) :: t_mge_ctl
!!        type(time_data_control), intent(in) :: t2_mge_ctl
!!        type(platform_data_control), intent(in) :: source_plt
!!        type(platform_data_control), intent(in) :: assemble_plt
!!        type(read_real_item), intent(in)  :: magnetic_ratio_ctl
!!        type(control_param_assemble), intent(inout) :: asbl_param
!
      module t_control_param_assemble
!
      use m_precision
      use m_constants
      use t_file_IO_parameter
!
      implicit    none
!
      type control_param_assemble
!>        Structure of origianl mesh data file name
        type(field_IO_params) :: org_mesh_file
!>        Structure of assembled mesh data file name
        type(field_IO_params) :: new_mesh_file
!
!>        Structure of origianl field data file name
        type(field_IO_params) :: org_fld_file
!>        Structure of assembled field data file name
        type(field_IO_params) :: new_fld_file
!
!>        start step
        integer(kind = kint) :: istep_start
!>        end step
        integer(kind = kint) :: istep_end
!>        step increment
        integer(kind = kint) :: increment_step
!
!>        flag for replace time information
        integer(kind = kint) :: iflag_newtime = 0
!>        New time step
        integer(kind = kint) :: istep_new_rst
!>        New increment of time step
        integer(kind = kint) :: increment_new_step
!>        New time
        real(kind = kreal) :: time_new
!
!>        Re-scaling of magnetic field
        real(kind = kreal) :: b_ratio = 1.0d0
!
!>        Flag to delete original data
        integer(kind=kint ) :: iflag_delete_org =   0
      end type control_param_assemble
!
!>      File prefix for new restart data
      character(len=kchara), parameter, private                         &
     &                    :: def_org_sph_fst = "restart/rst"
!>      File prefix for new restart data
      character(len=kchara), parameter, private                         &
     &                    :: def_new_sph_fst = "rst_new/rst"
!
!>      File prefix for original field data
      character(len=kchara), parameter, private                         &
     &      :: def_org_udt_head = 'field/out'
!>      File prefix for new field data
      character(len=kchara), parameter, private                         &
     &      :: def_new_udt_head = 'field_new/out'
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_assemble_rst_file_param                            &
     &         (source_plt, assemble_plt, asbl_param)
!
      use t_ctl_data_4_platforms
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: source_plt
      type(platform_data_control), intent(in) :: assemble_plt
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      call set_parallel_file_ctl_params(def_org_sph_fst,                &
     &    source_plt%restart_file_prefix,                               &
     &    source_plt%restart_file_fmt_ctl, asbl_param%org_fld_file)
      call set_parallel_file_ctl_params(def_new_sph_fst,                &
     &    assemble_plt%restart_file_prefix,                             &
     &    assemble_plt%restart_file_fmt_ctl, asbl_param%new_fld_file)
!
      end subroutine set_assemble_rst_file_param
!
!------------------------------------------------------------------
!
      subroutine set_assemble_ucd_file_param                            &
     &         (source_plt, assemble_plt, asbl_param)
!
      use t_ctl_data_4_platforms
      use parallel_ucd_IO_select
!
      type(platform_data_control), intent(in) :: source_plt
      type(platform_data_control), intent(in) :: assemble_plt
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      call set_merged_ucd_file_ctl                                      &
     &   (def_org_udt_head, source_plt%field_file_prefix,               &
     &    source_plt%field_file_fmt_ctl, asbl_param%org_fld_file)
      call set_merged_ucd_file_ctl                                      &
     &   (def_new_udt_head, assemble_plt%field_file_prefix,             &
     &    assemble_plt%field_file_fmt_ctl, asbl_param%new_fld_file)
!
      end subroutine set_assemble_ucd_file_param
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_assemble_step_4_rst(t_mge_ctl, asbl_param)
!
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: t_mge_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      asbl_param%istep_start = 1
      if(t_mge_ctl%i_step_init_ctl%iflag .gt. 0) then
        asbl_param%istep_start = t_mge_ctl%i_step_init_ctl%intvalue
      end if
!
      asbl_param%istep_end =  1
      if(t_mge_ctl%i_step_number_ctl%iflag .gt. 0) then
        asbl_param%istep_end = t_mge_ctl%i_step_number_ctl%intvalue
      end if
!
      asbl_param%increment_step = 1
      if (t_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        asbl_param%increment_step = t_mge_ctl%i_step_rst_ctl%intvalue
      end if
!
      end subroutine set_assemble_step_4_rst
!
! -----------------------------------------------------------------------
!
      subroutine set_assemble_step_4_ucd(t_mge_ctl, asbl_param)
!
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: t_mge_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      asbl_param%istep_start = 1
      if(t_mge_ctl%i_step_init_ctl%iflag .gt. 0) then
        asbl_param%istep_start = t_mge_ctl%i_step_init_ctl%intvalue
      end if
!
      asbl_param%istep_end =  1
      if(t_mge_ctl%i_step_number_ctl%iflag .gt. 0) then
        asbl_param%istep_end = t_mge_ctl%i_step_number_ctl%intvalue
      end if
!
      asbl_param%increment_step = 1
      if (t_mge_ctl%i_step_ucd_ctl%iflag .gt. 0) then
        asbl_param%increment_step = t_mge_ctl%i_step_ucd_ctl%intvalue
      end if
!
      end subroutine set_assemble_step_4_ucd
!
! -----------------------------------------------------------------------
!
      subroutine set_control_new_step(t2_mge_ctl, asbl_param)
!
      use t_ctl_data_4_time_steps
!
      type(time_data_control), intent(in) :: t2_mge_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      if(t2_mge_ctl%i_step_init_ctl%iflag .gt. 0) then
        asbl_param%istep_new_rst = t2_mge_ctl%i_step_init_ctl%intvalue
      else
        asbl_param%istep_new_rst = asbl_param%istep_start
      end if
      if (t2_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        asbl_param%increment_new_step                                   &
     &       = t2_mge_ctl%i_step_rst_ctl%intvalue
      else
        asbl_param%increment_new_step = asbl_param%increment_step
      end if
      if(t2_mge_ctl%time_init_ctl%iflag .gt. 0) then
        asbl_param%time_new = t2_mge_ctl%time_init_ctl%realvalue
      end if
!
      if      (t2_mge_ctl%time_init_ctl%iflag .gt. 0                    &
     &   .and. t2_mge_ctl%i_step_init_ctl%iflag .gt. 0                  &
     &   .and. t2_mge_ctl%i_step_rst_ctl%iflag .gt. 0) then
        if(asbl_param%istep_start .ne. asbl_param%istep_end) then
          stop 'Choose one snapshot to change time step information'
        else
          asbl_param%iflag_newtime = 1
        end if
      else
        asbl_param%iflag_newtime = 0
      end if
!
      end subroutine set_control_new_step
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_delete_flag_4_assemble(assemble_plt, asbl_param)
!
      use t_ctl_data_4_platforms
      use t_control_elements
      use skip_comment_f
!
      type(platform_data_control), intent(in) :: assemble_plt
      type(control_param_assemble), intent(inout) :: asbl_param
!
      character(len = kchara) :: tmpchara
!
!
      if(assemble_plt%del_org_data_ctl%iflag .gt. 0) then
        tmpchara = assemble_plt%del_org_data_ctl%charavalue
        if(yes_flag(tmpchara)) asbl_param%iflag_delete_org = 1
      end if
!
      end subroutine set_delete_flag_4_assemble
!
! -----------------------------------------------------------------------
!
      subroutine set_magnetic_ratio_4_assemble                          &
     &         (magnetic_ratio_ctl, asbl_param)
!
      use t_control_elements
!
      type(read_real_item), intent(in)  :: magnetic_ratio_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
!
!
      asbl_param%b_ratio = 1.0d0
      if(magnetic_ratio_ctl%iflag .gt. 0) then
        asbl_param%b_ratio = magnetic_ratio_ctl%realvalue
      end if
!
      end subroutine set_magnetic_ratio_4_assemble
!
! -----------------------------------------------------------------------
!
      end module t_control_param_assemble
