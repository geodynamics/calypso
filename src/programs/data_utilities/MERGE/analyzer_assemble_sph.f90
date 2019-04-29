!>@file   analyzer_assemble_sph.f90
!!@brief  module analyzer_assemble_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_assemble_sph
!!      subroutine analyze_assemble_sph
!!@endverbatim
!
      module analyzer_assemble_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_spectr_data_4_assemble
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use assemble_sph_fields
      use set_control_newsph
      use field_IO_select
!
      implicit none
!
      type(control_data_4_merge), save :: mgd_ctl_s
      type(control_param_assemble), save :: asbl_param_s
      type(spectr_data_4_assemble), save :: sph_asbl_s
      type(time_data), save :: init_t
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_assemble_sph
!
      use m_error_IDs
!
      use bcast_4_assemble_sph_ctl
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use field_IO_select
      use share_spectr_index_data
      use count_nnod_4_asseble_sph
!
      integer(kind = kint) :: ip, jp
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      if(my_rank .eq. 0) call read_control_assemble_sph(mgd_ctl_s)
      call bcast_merge_control_data(mgd_ctl_s)
      call set_control_4_newsph(mgd_ctl_s, asbl_param_s, sph_asbl_s)
!
      call alloc_spectr_data_4_assemble(sph_asbl_s)
!
!  set original spectr data
!
      call set_local_rj_mesh_4_merge(asbl_param_s%org_mesh_file,        &
     &    sph_asbl_s%np_sph_org, sph_asbl_s%org_sph_mesh)
      call share_org_sph_rj_data                                        &
     &   (sph_asbl_s%np_sph_org, sph_asbl_s%org_sph_mesh)
!
!  set new spectr data
!
      call set_local_rj_mesh_4_merge(asbl_param_s%new_mesh_file,        &
     &    sph_asbl_s%np_sph_new, sph_asbl_s%new_sph_mesh)
      call load_new_spectr_rj_data                                      &
     &   (sph_asbl_s%np_sph_org, sph_asbl_s%np_sph_new,                 &
     &    sph_asbl_s%org_sph_mesh, sph_asbl_s%new_sph_mesh,             &
     &    sph_asbl_s%j_table)
!
!     Share number of nodes for new mesh
!
      call s_count_nnod_4_asseble_sph(sph_asbl_s%np_sph_new,            &
     &    sph_asbl_s%new_sph_mesh, sph_asbl_s%new_fst_IO)
!
!     construct radial interpolation table
!
      call const_r_interpolate_table                                    &
     &   (sph_asbl_s%org_sph_mesh(1), sph_asbl_s%new_sph_mesh(1),       &
     &    sph_asbl_s%r_itp)
!
!      Construct field list from spectr file
!
      call load_field_name_assemble_sph                                 &
     &   (asbl_param_s%istep_start, sph_asbl_s%np_sph_org,              &
     &    asbl_param_s%org_fld_file, sph_asbl_s%org_sph_phys(1),        &
     &    sph_asbl_s%new_sph_phys(1), sph_asbl_s%fst_time_IO)
!
      call share_org_spectr_field_names                                 &
     &   (sph_asbl_s%np_sph_org, sph_asbl_s%org_sph_phys)
      call share_new_spectr_field_names(sph_asbl_s%np_sph_new,          &
     &    sph_asbl_s%new_sph_mesh, sph_asbl_s%new_sph_phys)
!
!
!      do jp = 1, sph_asbl_s%np_sph_new
!        if(mod(jp-1,nprocs) .ne. my_rank) cycle
!        do ip = 1, sph_asbl_s%np_sph_org
!          do j = 1, sph_asbl_s%org_sph_mesh(1)%sph%sph_rj%nidx_rj(2)
!            if(sph_asbl_s%j_table(ip,jp)%j_org_to_new(j).gt. 0)        &
!     &          write(50+my_rank,*) jp, ip, j,                         &
!     &                  sph_asbl_s%j_table(ip,jp)%j_org_to_new(j)
!          end do
!        end do
!      end do
!      write(*,*) 'init_assemble_sph end'
!
      end subroutine init_assemble_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_assemble_sph
!
      use m_phys_labels
      use r_interpolate_marged_sph
      use set_field_file_names
      use share_field_data
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp
      integer(kind = kint) :: iloop
      integer(kind = kint) :: istep_out
      integer :: irank_new
!
!
!     ---------------------
!
      do istep = asbl_param_s%istep_start, asbl_param_s%istep_end,      &
     &          asbl_param_s%increment_step
!
!     Load original spectr data
        do iloop = 0, (sph_asbl_s%np_sph_org-1) / nprocs
          irank_new = int(my_rank + iloop * nprocs)
          ip = irank_new + 1
          call load_org_sph_data(irank_new, istep,                      &
     &        sph_asbl_s%np_sph_org, asbl_param_s%org_fld_file,         &
     &        sph_asbl_s%org_sph_mesh(ip)%sph, init_t,                  &
     &        sph_asbl_s%org_sph_phys(ip))
          call calypso_mpi_barrier
        end do
!
        istep_out = istep
        if(asbl_param_s%iflag_newtime .gt. 0) then
          istep_out =          asbl_param_s%istep_new_rst               &
     &                        / asbl_param_s%increment_new_step
          init_t%i_time_step = asbl_param_s%istep_new_rst
          init_t%time =        asbl_param_s%time_new
        end if
!
        call share_time_step_data(init_t)
!
!     Bloadcast original spectr data
        do ip = 1, sph_asbl_s%np_sph_org
          call share_each_field_data(ip, sph_asbl_s%org_sph_phys(ip))
!
!     Copy spectr data to temporal array
          do jp = 1, sph_asbl_s%np_sph_new
           if(mod(jp-1,nprocs) .ne. my_rank) cycle
            call set_assembled_sph_data                                 &
     &         (sph_asbl_s%org_sph_mesh(ip),                            &
     &          sph_asbl_s%new_sph_mesh(jp),                            &
     &          sph_asbl_s%j_table(ip,jp), sph_asbl_s%r_itp,            &
     &          sph_asbl_s%org_sph_phys(ip),                            &
     &          sph_asbl_s%new_sph_phys(jp))
          end do
          call dealloc_phys_data_type(sph_asbl_s%org_sph_phys(ip))
        end do
!
        jp = my_rank + 1
        call const_assembled_sph_data(asbl_param_s%b_ratio, init_t,     &
     &      sph_asbl_s%new_sph_mesh(jp)%sph, sph_asbl_s%r_itp,          &
     &      sph_asbl_s%new_sph_phys(jp),                                &
     &      sph_asbl_s%new_fst_IO, sph_asbl_s%fst_time_IO)
!
        call sel_write_step_SPH_field_file                              &
     &     (nprocs, my_rank, istep_out, asbl_param_s%new_fld_file,      &
     &      sph_asbl_s%fst_time_IO, sph_asbl_s%new_fst_IO)
!
        call dealloc_phys_data_IO(sph_asbl_s%new_fst_IO)
        call dealloc_phys_name_IO(sph_asbl_s%new_fst_IO)
        call calypso_mpi_barrier
      end do
!
      call dealloc_spectr_data_4_assemble                               &
     &   (my_rank, nprocs, sph_asbl_s)
!
      call calypso_MPI_barrier
!
      if(asbl_param_s%iflag_delete_org .gt. 0) then
        icou = 0
        do istep = asbl_param_s%istep_start, asbl_param_s%istep_end,    &
     &            asbl_param_s%increment_step
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_SPH_fld_file                                      &
     &        (asbl_param_s%org_fld_file, sph_asbl_s%np_sph_org, istep)
        end do
      end if
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_assemble_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_assemble_sph
