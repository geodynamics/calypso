!>@file   analyzer_gen_sph_grids.f90
!!@brief  module analyzer_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine init_gen_sph_grids
!!      subroutine analyze_gen_sph_grids
!!@endverbatim
!
      module analyzer_gen_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_gen_SPH
!
      use t_mesh_data
      use t_SPH_mesh_field_data
      use t_sph_trans_comm_tbl
      use t_file_IO_parameter
      use t_ctl_data_const_sph_mesh
      use t_ctl_params_gen_sph_shell
!
      use para_const_kemoview_mesh
!
      implicit none
!
      character (len = kchara), private, parameter                      &
     &         :: control_file_name = 'control_sph_shell'
!
!
!>      Structure for file settings
      type(sph_mesh_generation_ctl), save, private :: SPH_MAKE_ctl
!
!>      Structure of spherical transform mesh information
      type(SPH_mesh_field_data), save :: SPH_GEN
!>      Structure of mesh file name and formats
      type(gen_sph_file_IO_params), save ::  sph_files1
!
!>      Structure of FEM mesh
      type(mesh_data), save, private :: geofem
!
      type(parallel_make_vierwer_mesh), save, private :: para_v1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gen_sph_grids
!
      use m_error_IDs
!
      integer(kind = kint) :: ierr = 0
!
! 
      call init_elapse_time_by_TOTAL
      call elpsed_label_gen_sph_grid
!
      call start_elapsed_time(ied_total_elapsed)
      call read_control_4_const_shell(control_file_name, SPH_MAKE_ctl)
      call set_control_4_gen_shell_grids                                &
     &   (my_rank, SPH_MAKE_ctl%plt, SPH_MAKE_ctl%psph_ctl,             &
     &    sph_files1, SPH_GEN%sph_maker, ierr)
      SPH_GEN%sph_maker%mesh_output_flag = .TRUE.
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
      if(SPH_GEN%sph_maker%gen_sph%s3d_ranks%ndomain_sph                &
     &     .ne. nprocs) then
        if(my_rank .eq. 0) write(*,*) 'The number of MPI processes ',   &
     &      'must be equal to the number of subdomains.', char(10),     &
     &      'Current subdomains: ',                                     &
     &      SPH_GEN%sph_maker%gen_sph%s3d_ranks%ndomain_sph
        write(e_message,'(a)') 'Parallellization error'
        call calypso_mpi_abort(ierr_P_MPI, e_message)
      end if
!
      end subroutine init_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine analyze_gen_sph_grids
!
      use parallel_FEM_mesh_init
      use mpi_gen_sph_grids_modes
      use output_gen_sph_grid_modes
      use parallel_load_data_4_sph
      use nod_phys_send_recv
!
!      integer :: i1, i2, i
!
!  ========= Generate spherical harmonics table ========================
!
      if(iflag_debug .gt. 0) write(*,*) 'mpi_gen_sph_grids'
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+2)
      call mpi_gen_sph_grids(SPH_GEN%sph_maker,                         &
     &    SPH_GEN%sph, SPH_GEN%comms, SPH_GEN%groups)
!
!
      call output_sph_mesh(sph_files1%sph_file_param,                   &
     &    SPH_GEN%sph, SPH_GEN%comms, SPH_GEN%groups)
      if(iflag_debug.gt.0) write(*,*) 'sph_index_flags_and_params'
      call sph_index_flags_and_params                                   &
     &   (SPH_GEN%groups, SPH_GEN%sph, SPH_GEN%comms)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+2)
!
!      if(my_rank .eq. 0) then
!        write(*,*) 'nidx_rtp', SPH_GEN%sph%sph_rtp%nidx_rtp(1:3)
!        write(*,*) 'istep_rtp', SPH_GEN%sph%sph_rtp%istep_rtp(1:3)
!
!        i2 = SPH_GEN%comms%comm_rtp%item_sr(2)
!        i1 = SPH_GEN%comms%comm_rtp%item_sr(1)
!        write(*,*) 'item_sr(1:2)', i1, i2
!        write(*,*) 'idx_global_rtp_SR1', SPH_GEN%sph%sph_rtp%idx_global_rtp(i1,1:3)
!        write(*,*) 'idx_global_rtp_SR2', SPH_GEN%sph%sph_rtp%idx_global_rtp(i2,1:3)
!        write(*,*) 'diff', SPH_GEN%sph%sph_rtp%idx_global_rtp(i2,1:3) - SPH_GEN%sph%sph_rtp%idx_global_rtp(i1,1:3)
!
!        do i = 1, SPH_GEN%sph%sph_rtp%istep_rtp(2)+1
!          i1 = SPH_GEN%comms%comm_rtp%item_sr(i)
!          write(*,*) i, 'idx_global_rtp', SPH_GEN%sph%sph_rtp%idx_global_rtp(i1,1:3)
!        end do
!
!        i2 = SPH_GEN%comms%comm_rtp%item_sr(1+SPH_GEN%sph%sph_rtp%istep_rtp(2))
!        i1 = SPH_GEN%comms%comm_rtp%item_sr(1)
!
!        write(*,*) 'item_sr(1:2)', i1, i2
!        write(*,*) 'idx_global_rtp_SR1', SPH_GEN%sph%sph_rtp%idx_global_rtp(i1,1:3)
!        write(*,*) 'idx_global_rtp_SR2', SPH_GEN%sph%sph_rtp%idx_global_rtp(i2,1:3)
!        write(*,*) 'diff', SPH_GEN%sph%sph_rtp%idx_global_rtp(i2,1:3) - SPH_GEN%sph%sph_rtp%idx_global_rtp(i1,1:3)
!      end if
!
      if(sph_files1%FEM_mesh_flags%iflag_access_FEM .eq. 0) goto 99
!
!  ========= Generate FEM mesh ===========================
!
      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+3)
      if(iflag_debug .gt. 0) write(*,*) 'const_FEM_mesh_4_SPH'
      call const_FEM_mesh_4_SPH                                         &
     &   (sph_files1%FEM_mesh_flags, sph_files1%sph_file_param,         &
     &    SPH_GEN, geofem)
      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+3)
      call calypso_MPI_barrier
!
!  ========= Generate viewer mesh ===========================
!
      if(sph_files1%FEM_mesh_flags%iflag_output_VMESH .gt. 0) then
        if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+5)
        if(iflag_debug .gt. 0) write(*,*) 'pickup_surface_mesh'
        call pickup_surface_mesh(sph_files1%sph_file_param, para_v1)
        if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+5)
      end if
!
!  ========= Generate FEM surface and edge mesh =======================
!
!      if(sph_files1%FEM_mesh_flags%iflag_output_SURF .eq. 0) goto 99
!
!      if(iflag_GSP_time) call start_elapsed_time(ist_elapsed_GSP+4)
!      if(iflag_debug .gt. 0) write(*,*) 'FEM_mesh_initialization'
!      call FEM_mesh_initialization(geofem%mesh, geofem%group)
!      if(iflag_GSP_time) call end_elapsed_time(ist_elapsed_GSP+4)
!
  99  continue
      call dealloc_sph_modes(SPH_GEN%sph, SPH_GEN%comms,                &
     &                       SPH_GEN%groups)
      call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      end module analyzer_gen_sph_grids
