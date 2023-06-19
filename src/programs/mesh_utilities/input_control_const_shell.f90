!>@file   input_control_const_shell.f90
!!@brief  module input_control_const_shell
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
!!      subroutine s_input_control_const_shell(file_name, gen_SPH_ctl,  &
!!     &                                       sph_files, sph_maker)
!!        character(len=kchara), intent(in) :: file_name
!!        type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!!        type(gen_sph_file_IO_params), intent(inout)  ::  sph_files
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!@endverbatim
!
      module input_control_const_shell
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_ctl_data_const_sph_mesh
!
      implicit none
!
!
      private :: bcast_sph_shell_construct_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_const_shell(file_name, gen_SPH_ctl,    &
     &                                       sph_files, sph_maker)
!
      use t_ctl_params_gen_sph_shell
      use t_sph_grid_maker_in_sim
      use t_read_control_elements
      use m_error_IDs
!
      character(len=kchara), intent(in) :: file_name
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
      type(gen_sph_file_IO_params), intent(inout)  ::  sph_files
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
      integer(kind = kint) :: ierr = 0
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_control_4_const_shell(file_name, gen_SPH_ctl, c_buf1)
      end if
      call bcast_sph_shell_construct_ctl(gen_SPH_ctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(gen_SPH_ctl%i_sph_mesh_ctl,              &
     &                         trim(file_name))
      end if
!
!       set control data
      call set_control_4_gen_shell_grids                                &
     &   (my_rank, gen_SPH_ctl%plt, gen_SPH_ctl%psph_ctl,               &
     &    sph_files, sph_maker, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
      end subroutine s_input_control_const_shell
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine bcast_sph_shell_construct_ctl(gen_SPH_ctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_sphere_ctl
!
      type(sph_mesh_generation_ctl), intent(inout) :: gen_SPH_ctl
!
!
      call bcast_ctl_data_4_platform(gen_SPH_ctl%plt)
      call bcast_parallel_shell_ctl(gen_SPH_ctl%psph_ctl)
!
      call calypso_mpi_bcast_one_int(gen_SPH_ctl%i_sph_mesh_ctl, 0)
      call calypso_mpi_bcast_character                                  &
     &   (gen_SPH_ctl%fname_psph_ctl, cast_long(kchara), 0)
!
      end subroutine bcast_sph_shell_construct_ctl
!
! ----------------------------------------------------------------------
!
      end module input_control_const_shell
