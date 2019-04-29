!>@file   sph_mhd_rst_IO_control.f90
!!@brief  module sph_mhd_rst_IO_control
!!
!!@author H. Matsui
!!@date Programmed in 2009
!!@n    Modified in June, 2015
!
!>@brief  I/O routines for restart data
!!
!!@verbatim
!!      subroutine output_sph_restart_control(i_step, fst_file_IO,      &
!!     &         time_d, rj_fld, rst_step, sph_fst_IO)
!!        type(field_IO_params), intent(in) :: fst_file_IO
!!        type(phys_data), intent(in) :: rj_fld
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!
!!      subroutine read_alloc_sph_restart_data                          &
!!     &         (fst_file_IO, init_d, rj_fld, rst_step, sph_fst_IO)
!!        type(time_data), intent(inout) :: init_d
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(IO_step_param), intent(inout) :: rst_step
!!
!!      subroutine init_radial_sph_interpolation                        &
!!     &         (rj_file_param, sph_params, sph_rj)
!!      subroutine read_alloc_sph_rst_4_snap(i_step, rj_file_param,     &
!!     &          fst_file_IO, rst_step, sph, ipol, rj_fld, time_d)
!!        type(field_IO_params), intent(in) :: rj_file_param
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine output_spectr_4_snap(i_step, time_d,                 &
!!     &          sph_file_param, rj_fld, ucd_step)
!!      subroutine read_alloc_sph_spectr                                &
!!     &         (i_step, ucd_step, rj_file_param, sph_file_param,      &
!!     &          sph_rj, ipol, rj_fld, time_d)
!!        type(field_IO_params), intent(in) :: rj_file_param
!!        type(field_IO_params), intent(in) :: sph_file_param
!!        type(phys_data), intent(in) :: rj_fld
!!@endverbatim
!!
!!@n @param i_step  time step
!
      module sph_mhd_rst_IO_control
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_file_format_switch
!
      use t_time_data
      use t_IO_step_parameter
      use t_phys_address
      use t_phys_data
      use t_MHD_file_parameter
      use t_file_IO_parameter
      use t_field_data_IO
      use t_time_data
!
      use field_IO_select
!
      implicit  none
!
!
      type(time_data), save, private :: sph_time_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_sph_restart_control(i_step, fst_file_IO,        &
     &         time_d, rj_fld, rst_step, sph_fst_IO)
!
      use set_sph_restart_IO
!
      integer(kind = kint), intent(in) :: i_step
      type(field_IO_params), intent(in) :: fst_file_IO
      type(time_data), intent(in) :: time_d
      type(phys_data), intent(in) :: rj_fld
      type(IO_step_param), intent(in) :: rst_step
!
      type(field_IO), intent(inout) :: sph_fst_IO
!
      integer(kind = kint) :: istep_rst
!
!
      istep_rst = set_IO_step(i_step, rst_step)
!
      call copy_time_step_size_data(time_d, sph_time_IO)
      call set_sph_restart_data_to_IO(rj_fld, sph_fst_IO)
!
      call sel_write_step_SPH_field_file(nprocs, my_rank,               &
     &    istep_rst, fst_file_IO, sph_time_IO, sph_fst_IO)
!
      end subroutine output_sph_restart_control
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_restart_data                            &
     &         (fst_file_IO, init_d, rj_fld, rst_step, sph_fst_IO)
!
      use set_sph_restart_IO
!
      type(field_IO_params), intent(in) :: fst_file_IO
!
      type(time_data), intent(inout) :: init_d
      type(phys_data), intent(inout) :: rj_fld
      type(IO_step_param), intent(inout) :: rst_step
      type(field_IO), intent(inout) :: sph_fst_IO
!
      integer(kind = kint) :: istep_rst
!
!
      istep_rst = set_IO_step(init_d%i_time_step, rst_step)
!
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    istep_rst, fst_file_IO, sph_time_IO, sph_fst_IO)
!
      call set_sph_restart_from_IO(sph_fst_IO, rj_fld)
      call copy_time_step_data(sph_time_IO, init_d)
!
      call dealloc_phys_data_IO(sph_fst_IO)
      call dealloc_phys_name_IO(sph_fst_IO)
!
      end subroutine read_alloc_sph_restart_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_radial_sph_interpolation                          &
     &         (rj_file_param, sph_params, sph_rj)
!
      use t_spheric_parameter
      use r_interpolate_sph_data
!
      type(field_IO_params), intent(in) :: rj_file_param
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
!
      if(rj_file_param%iflag_IO .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*) 'input_old_rj_sph_trans'
        call input_old_rj_sph_trans                                     &
     &     (rj_file_param, sph_params%l_truncation, sph_rj)
      end if
!
      call copy_cmb_icb_radial_point                                    &
     &   (sph_params%nlayer_ICB, sph_params%nlayer_CMB)
!
      end subroutine init_radial_sph_interpolation
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_rst_4_snap(i_step, rj_file_param,       &
     &          fst_file_IO, rst_step, sph, ipol, rj_fld, time_d)
!
      use t_spheric_parameter
      use set_sph_restart_IO
      use r_interpolate_sph_data
!
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: rst_step
      type(field_IO_params), intent(in) :: rj_file_param
      type(field_IO_params), intent(in) :: fst_file_IO
      type(sph_grids), intent(in) ::  sph
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(time_data), intent(inout) :: time_d
!
      type(field_IO) :: sph_fst_IO
      integer(kind = kint) :: istep_rst
!
!
      istep_rst = set_IO_step(i_step, rst_step)
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    istep_rst, fst_file_IO, sph_time_IO, sph_fst_IO)
!
      call copy_time_step_data(sph_time_IO, time_d)
!
      if(rj_file_param%iflag_IO .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_sph_restart_from_IO'
        call set_sph_restart_from_IO(sph_fst_IO, rj_fld)
      else
        if (iflag_debug.gt.0)                                           &
     &            write(*,*) 'r_interpolate_sph_rst_from_IO'
        call r_interpolate_sph_rst_from_IO                              &
     &     (sph_fst_IO, sph%sph_rj, ipol, rj_fld)
      end if
!
      call dealloc_phys_data_IO(sph_fst_IO)
      call dealloc_phys_name_IO(sph_fst_IO)
!
      end subroutine read_alloc_sph_rst_4_snap
!
! -----------------------------------------------------------------------
!
      subroutine output_spectr_4_snap(i_step, time_d,                   &
     &          sph_file_param, rj_fld, ucd_step)
!
      use copy_rj_phys_data_4_IO
      use const_global_element_ids
!
      type(time_data), intent(in) :: time_d
      type(phys_data), intent(in) :: rj_fld
      type(field_IO_params), intent(in) :: sph_file_param
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
!
      type(field_IO) :: sph_out_IO
      integer(kind = kint) :: istep_udt
!
!
      if(sph_file_param%iflag_IO .eq. 0) return
      if(output_IO_flag(i_step,ucd_step) .ne. 0) return
      istep_udt = set_IO_step(i_step, ucd_step)
!
      call copy_time_step_size_data(time_d, sph_time_IO)
      call copy_rj_phys_name_to_IO                                      &
     &   (rj_fld%num_phys_viz, rj_fld, sph_out_IO)
      call alloc_phys_data_IO(sph_out_IO)
      call copy_rj_phys_data_to_IO                                      &
     &   (rj_fld%num_phys_viz, rj_fld, sph_out_IO)
!
      call alloc_merged_field_stack(nprocs, sph_out_IO)
      call count_number_of_node_stack                                   &
     &   (sph_out_IO%nnod_IO, sph_out_IO%istack_numnod_IO)
!
      call sel_write_step_SPH_field_file(nprocs, my_rank,               &
     &    istep_udt, sph_file_param, sph_time_IO, sph_out_IO)
!
      call dealloc_merged_field_stack(sph_out_IO)
      call dealloc_phys_data_IO(sph_out_IO)
      call dealloc_phys_name_IO(sph_out_IO)
!
      end subroutine output_spectr_4_snap
!
! -----------------------------------------------------------------------
!
      subroutine read_alloc_sph_spectr                                  &
     &         (i_step, ucd_step, rj_file_param, sph_file_param,        &
     &          sph_rj, ipol, rj_fld, time_d)
!
      use t_spheric_rj_data
      use copy_rj_phys_data_4_IO
      use r_interpolate_sph_data
!
      integer(kind = kint), intent(in) :: i_step
      type(IO_step_param), intent(in) :: ucd_step
      type(field_IO_params), intent(in) :: sph_file_param
      type(field_IO_params), intent(in) :: rj_file_param
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(time_data), intent(inout) :: time_d
      type(phys_data), intent(inout) :: rj_fld
!
      type(field_IO) :: sph_out_IO
      integer(kind = kint) :: istep_udt
!
!
      istep_udt = set_IO_step(i_step, ucd_step)
      call sel_read_alloc_step_SPH_file(nprocs, my_rank,                &
     &    istep_udt, sph_file_param, sph_time_IO, sph_out_IO)
!
      call copy_time_step_data(sph_time_IO, time_d)
!
      if(rj_file_param%iflag_IO .eq. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_rj_phys_data_from_IO'
        call set_rj_phys_data_from_IO(sph_out_IO, rj_fld)
      else
        if (iflag_debug.gt.0) write(*,*)                                &
     &                        'r_interpolate_sph_fld_from_IO'
        call r_interpolate_sph_fld_from_IO                              &
     &    (sph_out_IO, sph_rj, ipol, rj_fld)
      end if
!
!      call dealloc_merged_field_stack(sph_out_IO)
      call dealloc_phys_data_IO(sph_out_IO)
      call dealloc_phys_name_IO(sph_out_IO)
!
      end subroutine read_alloc_sph_spectr
!
! -----------------------------------------------------------------------
!
      end module sph_mhd_rst_IO_control
