!>@file   trace_particle.f90
!!@brief  module trace_particle
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Routines to construct field lines
!!
!!@verbatim
!!      subroutine s_trace_particle(dt, elps_tracer, mesh, para_surf,   &
!!     &          nod_fld, fln_prm, fln_tce, fline_lc,                  &
!!     &          fln_SR, fln_bcast, v_prev, m_SR)
!!        real(kind = kreal), intent(in) :: dt
!!        type(elapsed_lables), intent(in) :: elps_tracer
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(paralell_surface_indices), intent(in) :: para_surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(local_fieldline), intent(inout) :: fline_lc(np_smp)
!!        type(broadcast_trace_data), intent(inout) :: fln_bcast
!!        real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
!!        type(mesh_SR), intent(inout) :: m_SR
!!endverbatim
!
      module trace_particle
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
!
      use t_time_data
      use t_mesh_data
      use t_phys_data
      use t_parallel_surface_indices
      use t_tracing_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_trace_data_send_recv
      use t_broadcast_trace_data
      use t_mesh_SR
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_trace_particle(dt, elps_tracer, mesh, para_surf,     &
     &          nod_fld, fln_prm, fln_tce, fline_lc,                    &
     &          fln_SR, fln_bcast, v_prev, m_SR)
!
      use transfer_to_long_integers
      use trace_particle_in_element
      use add_tracer_fieldline_list
      use copy_field_smp
!
      real(kind = kreal), intent(in) :: dt
      type(elapsed_lables), intent(in) :: elps_tracer
      type(mesh_geometry), intent(in) :: mesh
      type(paralell_surface_indices), intent(in) :: para_surf
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
      type(local_fieldline), intent(inout) :: fline_lc(np_smp)
      type(trace_data_send_recv), intent(inout) :: fln_SR
      type(broadcast_trace_data), intent(inout) :: fln_bcast
      real(kind = kreal), intent(inout) :: v_prev(nod_fld%n_point,3)
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: nline, inum, ip, ist, ied
!
!
      fln_tce%trace_length(1:fln_tce%num_current_fline) = 0.0d0
!
      do ip = 1, np_smp
        call reset_fline_start(fline_lc(ip))
      end do
!
      do
        if(elps_tracer%flag_elapsed)                                    &
     &         call start_elapsed_time(elps_tracer%ist_elapsed+1)
!
!$omp parallel do private(ip,ist,ied,inum)
        do ip = 1, np_smp
          ist = fln_tce%istack_smp_cur_fline(ip-1) + 1
          ied = fln_tce%istack_smp_cur_fline(ip  )
          do inum = ist, ied
            call s_trace_particle_in_element                            &
     &         (dt, mesh%node, mesh%ele, mesh%surf, para_surf, nod_fld, &
     &          v_prev, fln_prm%iphys_4_fline,                          &
     &          fln_prm%iflag_fline_used_ele,                           &
     &          fln_tce%isf_dbl_start(1,inum),                          &
     &          fln_tce%xx_fline_start(1,inum),                         &
     &          fln_tce%xi_fline_start(1,inum),                         &
     &          fln_tce%trace_length(inum),                             &
     &          fln_tce%iflag_comm_start(inum),                         &
     &          fline_lc(ip)%itp_ele_work_l, inum)
!
            if(fln_tce%iflag_comm_start(inum) .eq. -3) then
              fln_tce%iflag_comm_start(inum) = 0
            else if(fln_tce%iflag_comm_start(inum) .eq. 0) then
              call add_traced_list(fln_tce%iline_original(inum),        &
     &                             fln_tce%isf_dbl_start(1,inum),       &
     &                             fln_tce%xx_fline_start(1,inum),      &
     &                             fline_lc(ip))
!
              if(fln_tce%iline_original(inum) .eq. 13) then
               write(*,*) my_rank, inum, 'fln_tce%isf_dbl_start(1,inum)', &
     &              fln_tce%isf_dbl_start(:,inum),   &
     &              fln_tce%xi_fline_start(:,inum),  &
     &           sqrt(fln_tce%xx_fline_start(1,inum)**2  &
     &              + fln_tce%xx_fline_start(2,inum)**2  &
     &              + fln_tce%xx_fline_start(3,inum)**2)
              end if
            end if
          end do
!
        end do
        if(elps_tracer%flag_elapsed)                                    &
     &          call end_elapsed_time(elps_tracer%ist_elapsed+1)
!
        if(elps_tracer%flag_elapsed)                                    &
     &         call start_elapsed_time(elps_tracer%ist_elapsed+2)
        if(fln_prm%flag_use_broadcast) then
          call s_broadcast_trace_data(fln_tce, fln_bcast, nline)
        else
          call s_trace_data_send_recv(fln_tce, fln_SR,                  &
     &                                m_SR%SR_sig, nline)
        end if
        if(elps_tracer%flag_elapsed)                                    &
     &          call end_elapsed_time(elps_tracer%ist_elapsed+2)
!
        if(nline .le. 0) exit
      end do
!
      call copy_nod_vector_smp(nod_fld%n_point,                        &
     &    nod_fld%d_fld(1,fln_prm%iphys_4_fline), v_prev)
      call return_to_trace_list(my_rank, fline_lc(1), fln_tce)
!
      end subroutine s_trace_particle
!
!  ---------------------------------------------------------------------
!
      end module trace_particle
