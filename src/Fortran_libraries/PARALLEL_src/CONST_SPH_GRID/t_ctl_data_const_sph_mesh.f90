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
      use m_read_control_elements
      use calypso_mpi
      use skip_comment_f
!
      use t_ctl_data_4_platforms
      use t_ctl_data_gen_sph_shell
!
      implicit none
!
!
      type sph_mesh_generation_ctl
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Control structure for parallel spherical shell
        type(parallel_sph_shell_control) :: psph_ctl
      end type sph_mesh_generation_ctl
!
      integer(kind=kint), parameter :: control_file_code = 11
      private :: control_file_code
!
!   Top level of label
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_mhd_ctl = 'MHD_control'
      integer(kind=kint), private :: i_mhd_ctl = 0
!
!   2nd level for MHD
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_sph_shell = 'spherical_shell_ctl'
      integer(kind=kint), private  :: i_platform = 0
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
!
      if(my_rank .eq. 0) then
        ctl_file_code = control_file_code
        open ( ctl_file_code, file = file_name, status='old' )
!
        call load_ctl_label_and_line
        call read_sph_shell_define_ctl(gen_SPH_ctl)
!
        close(ctl_file_code)
      end if
!
      call bcast_sph_shell_define_ctl(gen_SPH_ctl)
!
      if(gen_SPH_ctl%psph_ctl%ifile_sph_shell .gt. 0) then
        call read_ctl_file_gen_shell_grids(gen_SPH_ctl%psph_ctl)
      end if
!
      end subroutine read_control_4_const_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine read_sph_shell_define_ctl(gen_SPH_ctl)
!
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!
!
      if(right_begin_flag(hd_mhd_ctl) .eq. 0) return
      if(i_mhd_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_mhd_ctl = find_control_end_flag(hd_mhd_ctl)
        if(i_mhd_ctl .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, gen_SPH_ctl%plt)
        call read_parallel_shell_in_MHD_ctl                             &
     &     (hd_sph_shell, gen_SPH_ctl%psph_ctl)
      end do
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
      end subroutine bcast_sph_shell_define_ctl
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_const_sph_mesh
