!>@file   const_field_lines.f90
!!@brief  module const_field_lines
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine const_each_field_line                                &
!!     &         (elps_fline, mesh, para_surf, nod_fld, fln_prm,        &
!!     &          fln_tce, fln_SR, fln_bcast, fline_lc, m_SR)
!!        type(elapsed_lables), intent(in) :: elps_fline
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(local_fieldline), intent(inout) :: fline_lc(np_smp)
!!        type(trace_data_send_recv), intent(inout) :: fln_SR
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module const_field_lines
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
!
      use t_mesh_SR
      use t_mesh_data
      use t_control_params_4_fline
      use t_comm_table
      use t_phys_data
      use t_parallel_surface_indices
      use t_local_fline
      use t_next_node_ele_4_node
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_tracing_data
      use t_mesh_SR
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_each_field_line                                  &
     &         (elps_fline, mesh, para_surf, nod_fld, fln_prm,          &
     &          fln_tce, fln_SR, fln_bcast, fline_lc, m_SR)
!
      use calypso_SR
      use transfer_to_long_integers
      use extend_field_line
!
      type(elapsed_lables), intent(in) :: elps_fline
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc(np_smp)
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: nline, inum, ip, ist, ied
      integer(kind = kint) ::  jcou
!
!
      fln_tce%trace_length(1:fln_tce%num_current_fline) = 0.0d0
!      write(*,*) my_rank, 'reset_fline_start loop'
      do ip = 1, np_smp
        call reset_fline_start(fline_lc(ip))
      end do
!
      jcou = 0
      do
        jcou = jcou + 1
        if(elps_fline%flag_elapsed)                                     &
     &           call start_elapsed_time(elps_fline%ist_elapsed+2)
!$omp parallel do private(ip,ist,ied,inum)
        do ip = 1, np_smp
          ist = fln_tce%istack_smp_cur_fline(ip-1) + 1
          ied = fln_tce%istack_smp_cur_fline(ip  )
          do inum = ist, ied
            call s_extend_field_line                                    &
     &         (mesh%node, mesh%ele, mesh%surf, para_surf, nod_fld,     &
     &          fln_prm%max_line_stepping, fln_prm%max_trace_length,    &
     &          fln_prm%iflag_fline_used_ele,                           &
     &          fln_tce%iline_original(inum),                           &
     &          fln_tce%iflag_direction(inum), fln_prm%iphys_4_fline,   &
     &          fln_tce%isf_dbl_start(1,inum),                          &
     &          fln_tce%xx_fline_start(1,inum),                         &
     &          fln_tce%xi_fline_start(1,inum),                         &
     &          fln_tce%icount_fline(inum), fln_tce%trace_length(inum), &
     &          fln_tce%iflag_comm_start(inum), fline_lc(ip))
          end do
        end do
!$omp end parallel do
!
        call calypso_mpi_barrier()
        if(elps_fline%flag_elapsed)                                     &
     &           call end_elapsed_time(elps_fline%ist_elapsed+2)
!
        if(elps_fline%flag_elapsed)                                     &
     &           call start_elapsed_time(elps_fline%ist_elapsed+3)
        if(fln_prm%flag_use_broadcast) then
          call s_broadcast_trace_data(fln_tce, fln_bcast, nline)
        else
          call s_trace_data_send_recv(fln_tce, fln_SR,                  &
     &                                m_SR%SR_sig, nline)
        end if
        if(elps_fline%flag_elapsed)                                     &
     &           call end_elapsed_time(elps_fline%ist_elapsed+3)
!
       if(nline .le. 0) exit
      end do
!
      end subroutine const_each_field_line
!
!  ---------------------------------------------------------------------
!
      end module const_field_lines
