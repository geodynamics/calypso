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
      character(len = kchara), parameter, private                       &
     &               :: ctl_file_name = 'control_assemble_sph'
!
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
      use parallel_gen_sph_grids
!
      type(control_data_4_merge) :: mgd_ctl_s
      type(sph_grid_maker_in_sim) :: sph_org_maker_s
      integer(kind = kint) :: istep_in
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      if(my_rank .eq. 0) call read_control_assemble_sph                 &
     &                      (ctl_file_name, mgd_ctl_s)
      call bcast_merge_control_data(mgd_ctl_s)
!
      if(mgd_ctl_s%i_assemble .ne. 1) then
        call calypso_MPI_abort(mgd_ctl_s%i_assemble,                    &
     &                         trim(ctl_file_name))
      end if
!
      call set_control_4_newsph(mgd_ctl_s, asbl_param_s, sph_asbl_s,    &
     &    sph_org_maker_s, sph_asbl_s%new_sph_data)
!
      call alloc_spectr_data_4_assemble(sph_asbl_s)
!
!  set original spectr data
!
      call check_and_make_para_rj_mode(asbl_param_s%org_mesh_file,      &
     &    sph_org_maker_s, sph_asbl_s%org_sph_array)
      call share_org_sph_rj_data(sph_asbl_s%org_sph_array)
!
!  set new spectr data
!
      call check_and_make_SPH_rj_mode                                   &
     &   (asbl_param_s%new_mesh_file, sph_asbl_s%new_sph_data)
      call load_new_spectr_rj_data(sph_asbl_s%org_sph_array,            &
     &    sph_asbl_s%new_sph_data, sph_asbl_s%j_table)
!
!     Share number of nodes for new mesh
!
      call s_count_nnod_4_asseble_sph(sph_asbl_s%np_sph_new,            &
     &    sph_asbl_s%new_sph_data, sph_asbl_s%new_fst_IO)
!
!     construct radial interpolation table
!
      call const_r_interpolate_table                                    &
     &   (sph_asbl_s%org_sph_array%sph(1),                              &
     &    sph_asbl_s%new_sph_data%sph, sph_asbl_s%r_itp)
!
!      Construct field list from spectr file
!
      istep_in = asbl_param_s%istep_start                               &
     &          / asbl_param_s%increment_step
      call load_field_name_assemble_sph(istep_in,                       &
     &    asbl_param_s%org_fld_file, sph_asbl_s%org_sph_array,          &
     &    sph_asbl_s%new_sph_data, sph_asbl_s%fst_time_IO)
!
      call share_org_spectr_field_names(sph_asbl_s%org_sph_array)
!
      call share_new_spectr_field_names(sph_asbl_s%new_sph_data)
!
!      write(*,*) size(sph_asbl_s%new_sph_data%fld%phys_name),  &
!     &    'share_new_spectr_field_names', &
!     &    (trim(sph_asbl_s%new_sph_data%fld%phys_name(ip)) // ' ', &
!     &      ip=1,size(sph_asbl_s%new_sph_data%fld%phys_name))
!
!      do ip = 1, sph_asbl_s%org_sph_array%num_pe
!        do j = 1, sph_asbl_s%org_sph_array%sph(1)%sph_rj%nidx_rj(2)
!          if(sph_asbl_s%j_table(ip)%j_org_to_new(j).gt. 0)             &
!     &          write(50+my_rank,*) my_rank+1, ip, j,                  &
!     &               sph_asbl_s%j_table(ip)%j_org_to_new(j)
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
      use r_interpolate_marged_sph
      use set_field_file_names
      use share_field_data
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: istep_in, istep_out
!
!     ---------------------
!
      do istep = asbl_param_s%istep_start, asbl_param_s%istep_end
        if(mod(istep, asbl_param_s%increment_step) .ne. 0) cycle
        istep_in = istep / asbl_param_s%increment_step
!
!     Load original spectr data
        call load_org_sph_data(istep_in, asbl_param_s%org_fld_file,     &
     &                         init_t, sph_asbl_s%org_sph_array)
!
        istep_out = istep_in
        if(asbl_param_s%iflag_newtime .gt. 0) then
          istep_out =          asbl_param_s%istep_new_rst               &
     &                        / asbl_param_s%increment_new_step
          init_t%i_time_step = asbl_param_s%istep_new_rst
          init_t%time =        asbl_param_s%time_new
        end if
!
        call share_time_step_data(init_t)
!
!     Copy spectr data to temporal array
        call set_assembled_sph_data(sph_asbl_s%org_sph_array%num_pe,    &
     &      sph_asbl_s%org_sph_array%sph, sph_asbl_s%j_table,           &
     &      sph_asbl_s%r_itp, sph_asbl_s%org_sph_array%fld,             &
     &      sph_asbl_s%new_sph_data)
!
        call const_assembled_sph_data(asbl_param_s%b_ratio, init_t,     &
     &      sph_asbl_s%r_itp, sph_asbl_s%new_sph_data,                  &
     &      sph_asbl_s%new_fst_IO, sph_asbl_s%fst_time_IO)
!
        call sel_write_step_SPH_field_file                              &
     &     (istep_out, asbl_param_s%new_fld_file,                       &
     &      sph_asbl_s%fst_time_IO, sph_asbl_s%new_fst_IO)
!
        call dealloc_phys_data_IO(sph_asbl_s%new_fst_IO)
        call dealloc_phys_name_IO(sph_asbl_s%new_fst_IO)
        call calypso_mpi_barrier
      end do
!
      call dealloc_spectr_data_4_assemble(sph_asbl_s)
!
      call calypso_MPI_barrier
!
      if(asbl_param_s%iflag_delete_org .gt. 0) then
        icou = 0
        do istep = asbl_param_s%istep_start, asbl_param_s%istep_end
          if(mod(istep, asbl_param_s%increment_step) .ne. 0) cycle
          istep_in = istep / asbl_param_s%increment_step
!
          icou = icou + 1
          if(mod(icou,nprocs) .ne. my_rank) cycle
          call delete_SPH_fld_file(asbl_param_s%org_fld_file,           &
     &        sph_asbl_s%org_sph_array%num_pe, istep_in)
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
