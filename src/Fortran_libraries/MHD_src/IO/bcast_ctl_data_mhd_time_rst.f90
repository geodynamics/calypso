!>@file   bcast_ctl_data_mhd_time_rst.f90
!!@brief  module bcast_ctl_data_mhd_time_rst
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine bcast_time_loop_ctl(mevo_ctl)
!!        type(mhd_evo_scheme_control), intent(inout) :: mevo_ctl
!!      subroutine bcast_restart_ctl(mr_ctl)
!!        type(mhd_restart_control), intent(inout) :: mr_ctl
!!@endverbatim
!
      module bcast_ctl_data_mhd_time_rst
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine bcast_time_loop_ctl(mevo_ctl)
!
      use t_ctl_data_mhd_evo_scheme
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(mhd_evo_scheme_control), intent(inout) :: mevo_ctl
!
!
      call bcast_ctl_type_c1(mevo_ctl%scheme_ctl)
      call bcast_ctl_type_c1(mevo_ctl%diffuse_correct)
      call bcast_ctl_type_c1(mevo_ctl%method_4_CN)
      call bcast_ctl_type_c1(mevo_ctl%precond_4_CN)
      call bcast_ctl_type_c1(mevo_ctl%Legendre_trans_type)
      call bcast_ctl_type_c1(mevo_ctl%FFT_library)
      call bcast_ctl_type_c1(mevo_ctl%import_mode)
!
      call bcast_ctl_type_r1(mevo_ctl%eps_4_velo_ctl)
      call bcast_ctl_type_r1(mevo_ctl%eps_4_magne_ctl)
      call bcast_ctl_type_r1(mevo_ctl%coef_implicit_ctl)
      call bcast_ctl_type_r1(mevo_ctl%coef_imp_v_ctl)
      call bcast_ctl_type_r1(mevo_ctl%coef_imp_t_ctl)
      call bcast_ctl_type_r1(mevo_ctl%coef_imp_b_ctl)
      call bcast_ctl_type_r1(mevo_ctl%coef_imp_c_ctl)
      call bcast_ctl_type_r1(mevo_ctl%eps_crank_ctl)
      call bcast_ctl_type_r1(mevo_ctl%eps_B_crank_ctl)
!
      call bcast_ctl_type_c1(mevo_ctl%iflag_supg_ctl)
      call bcast_ctl_type_c1(mevo_ctl%iflag_supg_v_ctl)
      call bcast_ctl_type_c1(mevo_ctl%iflag_supg_t_ctl)
      call bcast_ctl_type_c1(mevo_ctl%iflag_supg_b_ctl)
      call bcast_ctl_type_c1(mevo_ctl%iflag_supg_c_ctl)
!
      call bcast_ctl_type_i1(mevo_ctl%num_multi_pass_ctl)
      call bcast_ctl_type_i1(mevo_ctl%maxiter_ctl)
      call bcast_ctl_type_i1(mevo_ctl%leg_vector_len)
!
      call calypso_mpi_bcast_character(mevo_ctl%block_name,             &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mevo_ctl%i_time_loop, 0)
!
      end subroutine bcast_time_loop_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_restart_ctl(mr_ctl)
!
      use t_ctl_data_mhd_restart
      use transfer_to_long_integers
      use calypso_mpi_char
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(mhd_restart_control), intent(inout) :: mr_ctl
!
!
      call bcast_ctl_type_c1(mr_ctl%restart_flag_ctl)
!
      call calypso_mpi_bcast_character(mr_ctl%block_name,               &
     &                                 cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(mr_ctl%i_restart_file, 0)
!
      end subroutine bcast_restart_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_mhd_time_rst
