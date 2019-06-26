!>@file   t_ctl_data_const_sph_mesh.f90
!!@brief  module t_ctl_data_const_sph_mesh
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_control_4_const_shell(file_name, gen_SPH_ctl)
!!        type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!!@endverbatim
!
      module t_ctl_data_const_sph_mesh
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use skip_comment_f
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_gen_sph_shell
!
      implicit none
!
!
      integer(kind=kint), parameter, private :: control_file_code = 11
!
      type sph_mesh_generation_ctl
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
!
        integer(kind=kint) :: i_sph_mesh_ctl = 0
      end type sph_mesh_generation_ctl
!
!
!   Top level of label
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_mhd_ctl = 'MHD_control'
!
!   2nd level for MHD
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
!
      private :: read_sph_shell_define_ctl, bcast_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_control_4_const_shell(file_name, gen_SPH_ctl)
!
      character(len=kchara), intent(in) :: file_name
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(control_file_code, file = file_name, status='old' )
!
        do
          call load_one_line_from_control(control_file_code, c_buf1)
          call read_sph_shell_define_ctl                                &
     &       (control_file_code, hd_mhd_ctl, gen_SPH_ctl, c_buf1)
          if(gen_SPH_ctl%i_sph_mesh_ctl .gt. 0) exit
        end do
!
        close(control_file_code)
      end if
!
      call bcast_sph_shell_define_ctl(gen_SPH_ctl)
!
      end subroutine read_control_4_const_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_shell_define_ctl                              &
     &         (id_control, hd_block, gen_SPH_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gen_SPH_ctl%i_sph_mesh_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, gen_SPH_ctl%plt, c_buf)
        call read_parallel_shell_in_MHD_ctl                             &
     &     (id_control, hd_sph_shell, gen_SPH_ctl%psph_ctl, c_buf)
      end do
      gen_SPH_ctl%i_sph_mesh_ctl = 1
!
      end subroutine read_sph_shell_define_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_shell_define_ctl(gen_SPH_ctl)
!
      use bcast_4_platform_ctl
!
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!
!
      call bcast_ctl_data_4_platform(gen_SPH_ctl%plt)
      call bcast_parallel_shell_ctl(gen_SPH_ctl%psph_ctl)
!
      call MPI_BCAST(gen_SPH_ctl%i_sph_mesh_ctl, 1,                     &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_const_sph_mesh
