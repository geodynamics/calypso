!>@file   t_control_data_4_merge.f90
!!@brief  module t_control_data_4_merge
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2007
!
!>@brief  Control data for merge program
!!
!!@verbatim
!!       subroutine read_control_4_merge(ctl_file_name, mgd_ctl)
!!       subroutine read_control_assemble_sph(mgd_ctl)
!!       subroutine reset_merge_control_data(mgd_ctl)
!!        type(control_data_4_merge), intent(inout) :: mgd_ctl
!!@endverbatim
!
!
      module t_control_data_4_merge
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_control_array_real
      use t_ctl_data_gen_sph_shell
      use skip_comment_f
!
      implicit none
!
      integer (kind = kint), parameter :: control_file_code = 13
!
!>        Structure for merged program control
      type control_data_4_merge
!>        Block name
        character(len=kchara) :: block_name = 'assemble_control'
!
!>        File name to read original spherical shell control file
        character (len = kchara) :: fname_src_psph_ctl
!>        Structure for file information for original data
        type(platform_data_control) :: source_plt
!
!>        File name to read assembled spherical shell control file
        character (len = kchara) :: fname_asbl_psph_ctl
!>        Structure for file information for assembled data
        type(platform_data_control) :: assemble_plt
!
!>        Control structure for original parallel spherical shell
        type(parallel_sph_shell_control) :: src_psph_ctl
!>        Control structure for assembled parallel spherical shell
        type(parallel_sph_shell_control) :: asbl_psph_ctl
!
!>        Structure for field information control
        type(field_control) :: fld_mge_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_mge_ctl
!>        Structure for new time stepping control
        type(time_data_control) :: t2_mge_ctl
!
!>        Re-normalization flag for magnetic field
        type(read_real_item) :: magnetic_ratio_ctl
!
        integer (kind=kint) :: i_assemble = 0
        integer (kind=kint) :: i_model =         0
        integer (kind=kint) :: i_control =       0
        integer (kind=kint) :: i_newrst_magne =  0
      end type control_data_4_merge
!
!   Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_assemble = 'assemble_control'
!
!   2nd level for assemble_control
!
      character(len=kchara), parameter                                  &
     &                :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                :: hd_new_data = 'new_data_files_def'
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
      character(len=kchara), parameter                                  &
     &                :: hd_newrst_magne = 'newrst_magne_ctl'
!
      character(len=kchara), parameter                                  &
     &                :: hd_orgsph_shell = 'spherical_shell_ctl'
      character(len=kchara), parameter                                  &
     &                :: hd_newsph_shell = 'new_spherical_shell_ctl'
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_new_time_step = 'new_time_step_ctl'
!
!   newrst_magne_ratio data
!
      character(len=kchara), parameter                                  &
     &      :: hd_magnetic_field_ratio =  'magnetic_field_ratio_ctl'
!
      private :: control_file_code
      private :: hd_assemble
      private :: hd_platform, hd_new_data, hd_model, hd_control
      private :: hd_phys_values, hd_time_step, hd_new_time_step
      private :: hd_newrst_magne, hd_magnetic_field_ratio
      private :: hd_orgsph_shell, hd_newsph_shell
!
      private :: read_merge_control_data
      private :: read_newrst_control
      private :: read_merge_field_data, read_merge_step_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_4_merge(file_name, mgd_ctl)
!
      character (len = kchara), intent(in) :: file_name
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open (control_file_code, file = file_name)
!
      do
        call load_one_line_from_control(control_file_code,              &
     &                                  hd_assemble, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
        call read_merge_control_data(control_file_code, hd_assemble,    &
     &     mgd_ctl, c_buf1)
        if(mgd_ctl%i_assemble .gt. 0) exit
      end do
      close(control_file_code)
!
      if(c_buf1%iend .gt. 0) mgd_ctl%i_assemble = c_buf1%iend
!
      end subroutine read_control_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine read_control_assemble_sph(file_name, mgd_ctl)
!
      character (len = kchara), intent(in) :: file_name
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open (control_file_code, file = file_name)
!
      do
        call load_one_line_from_control(control_file_code,              &
     &                                  hd_assemble, c_buf1)
        if(c_buf1%iend .gt. 0) exit
!
      call read_merge_control_data(control_file_code, hd_assemble,      &
     &                             mgd_ctl, c_buf1)
        if(mgd_ctl%i_assemble .gt. 0) exit
      end do
      close(control_file_code)
!
      if(c_buf1%iend .gt. 0) mgd_ctl%i_assemble = c_buf1%iend
!
      end subroutine read_control_assemble_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_control_data                               &
      &         (id_control, hd_block, mgd_ctl, c_buf)
!
      use ctl_data_platforms_IO
      use ctl_file_gen_sph_shell_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(mgd_ctl%i_assemble .gt. 0) return
      call init_platforms_labels(hd_platform, mgd_ctl%source_plt)
      call init_platforms_labels(hd_new_data, mgd_ctl%assemble_plt)
      call init_parallel_shell_ctl_label(hd_orgsph_shell,               &
     &                                   mgd_ctl%src_psph_ctl)
      call init_parallel_shell_ctl_label(hd_newsph_shell,               &
     &                                   mgd_ctl%asbl_psph_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, mgd_ctl%source_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_data, mgd_ctl%assemble_plt, c_buf)
!
        call read_merge_field_data                                      &
     &     (id_control, hd_model, mgd_ctl, c_buf)
        call read_merge_step_data                                       &
     &     (id_control, hd_control, mgd_ctl, c_buf)
        call read_newrst_control                                        &
     &     (id_control, hd_newrst_magne, mgd_ctl, c_buf)
!
        call sel_read_ctl_gen_shell_grids(id_control, hd_orgsph_shell,  &
     &      mgd_ctl%fname_src_psph_ctl,  mgd_ctl%src_psph_ctl, c_buf)
        call sel_read_ctl_gen_shell_grids(id_control, hd_newsph_shell,  &
     &      mgd_ctl%fname_asbl_psph_ctl, mgd_ctl%asbl_psph_ctl, c_buf)
      end do
      mgd_ctl%i_assemble = 1
!
      end subroutine read_merge_control_data
!
! -----------------------------------------------------------------------
!
       subroutine reset_merge_control_data(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      call dealloc_parallel_shell_ctl(mgd_ctl%src_psph_ctl)
      call dealloc_parallel_shell_ctl(mgd_ctl%asbl_psph_ctl)
      call reset_control_platforms(mgd_ctl%source_plt)
      call reset_control_platforms(mgd_ctl%assemble_plt)
!
      call reset_merge_field_data(mgd_ctl)
      call reset_merge_step_data(mgd_ctl)
      call reset_newrst_control(mgd_ctl)
!
      mgd_ctl%i_assemble = 0
!
      end subroutine reset_merge_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_merge_field_data                                 &
     &         (id_control, hd_block, mgd_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(mgd_ctl%i_model .gt. 0) return
      call init_phys_data_ctl_label(hd_phys_values,                     &
     &                              mgd_ctl%fld_mge_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, mgd_ctl%fld_mge_ctl, c_buf)
      end do
      mgd_ctl%i_model = 1
!
      end subroutine read_merge_field_data
!
! -----------------------------------------------------------------------
!
       subroutine reset_merge_field_data(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!/
      call dealloc_phys_control(mgd_ctl%fld_mge_ctl)
      mgd_ctl%i_model = 0
!
      end subroutine reset_merge_field_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_merge_step_data                                   &
     &         (id_control, hd_block, mgd_ctl, c_buf)
!
      use ctl_data_4_time_steps_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(mgd_ctl%i_control .gt. 0) return
      call init_ctl_time_step_label(hd_time_step, mgd_ctl%t_mge_ctl)
      call init_ctl_time_step_label(hd_new_time_step,                   &
     &                              mgd_ctl%t2_mge_ctl)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, mgd_ctl%t_mge_ctl, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_new_time_step, mgd_ctl%t2_mge_ctl, c_buf)
      end do
      mgd_ctl%i_control = 1
!
      end subroutine read_merge_step_data
!
! -----------------------------------------------------------------------
!
       subroutine reset_merge_step_data(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      mgd_ctl%i_control = 0
!
      end subroutine reset_merge_step_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_newrst_control                                    &
     &         (id_control, hd_block, mgd_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mgd_ctl%i_newrst_magne .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type(c_buf, hd_magnetic_field_ratio,         &
     &      mgd_ctl%magnetic_ratio_ctl)
      end do
      mgd_ctl%i_newrst_magne = 1
!
      end subroutine read_newrst_control
!
! -----------------------------------------------------------------------
!
      subroutine reset_newrst_control(mgd_ctl)
!
      type(control_data_4_merge), intent(inout) :: mgd_ctl
!
!
      mgd_ctl%magnetic_ratio_ctl%iflag = 0
      mgd_ctl%i_newrst_magne = 0
!
      end subroutine reset_newrst_control
!
! -----------------------------------------------------------------------
!
      end module t_control_data_4_merge
