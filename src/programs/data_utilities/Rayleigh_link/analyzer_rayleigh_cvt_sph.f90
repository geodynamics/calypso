!>@file   analyzer_rayleigh_cvt_sph.f90
!!@brief  module analyzer_rayleigh_cvt_sph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2018
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_cvt_rayleigh_sph
!!      subroutine analyze_cvt_rayleigh_sph
!!@endverbatim
!
      module analyzer_rayleigh_cvt_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_file_format_switch
!
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_spectr_data_4_assemble
      use t_convert_from_rayleigh
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use assemble_sph_fields
      use set_control_newsph
      use rayleigh_restart_IO
      use field_IO_select
      use convert_from_rayleigh_rst
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
      type(rayleigh_restart), save :: ra_rst_s
      type(field_IO), save :: fld_IO_r
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_cvt_rayleigh_sph
!
      use m_error_IDs
!
      use bcast_4_assemble_sph_ctl
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use field_IO_select
      use share_spectr_index_data
      use count_nnod_4_asseble_sph
      use rayleigh_restart_IO
!
      use share_field_data
!
      type(control_data_4_merge) :: mgd_ctl_s
      type(sph_grid_maker_in_sim) :: sph_org_maker_s
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
      sph_asbl_s%np_sph_org = 1
      call alloc_spectr_data_4_assemble(sph_asbl_s)
!
!  set original Rayleigh spectr data
!
      ra_rst_s%i_version = 1
      if(asbl_param_s%org_fld_file%iflag_format .eq. id_Rayleigh99)     &
     & ra_rst_s%i_version = 0
!
      call init_rayleigh_restart_params(asbl_param_s%istep_start,       &
     &    asbl_param_s%org_fld_file, ra_rst_s)
        if(my_rank .eq. 0) call check_rayleigh_rst_params(6, ra_rst_s)
!
      call copy_rayleigh_radial_data                                    &
     &   (ra_rst_s, sph_asbl_s%org_sph_array%sph(1))
!
!  set new spectr data
      call check_and_make_SPH_rj_mode                                   &
     &   (asbl_param_s%new_mesh_file, sph_asbl_s%new_sph_data)
!
!     Share number of nodes for new mesh
!
      call s_count_nnod_4_asseble_sph(sph_asbl_s%np_sph_new,            &
     &   sph_asbl_s%new_sph_data, sph_asbl_s%new_fst_IO)
!
!     construct radial interpolation table
!
      call const_r_interpolate_table                                    &
     &   (sph_asbl_s%org_sph_array%sph(1),                              &
     &    sph_asbl_s%new_sph_data%sph, sph_asbl_s%r_itp)
!
!      call chebyshev_fwd_mat_4_rayleigh                                &
!     &   (sph_asbl_s%new_sph_data%sph, sph_asbl_s%r_itp, ra_rst_s)
!
      call dealloc_rayleigh_radial_grid(ra_rst_s)
!
!      Construct field list from spectr file
!
      call init_rayleigh_restart_input(ra_rst_s%i_version,              &
     &    asbl_param_s%org_fld_file%file_prefix,                        &
     &    asbl_param_s%istep_start, fld_IO_r)
!
!      call check_field_name_4_IO(50+my_rank, fld_IO_r)
!
      if(my_rank .eq. 0) then
        call copy_rj_phys_name_from_IO                                  &
     &     (fld_IO_r, sph_asbl_s%new_sph_data%fld)
      end if
      call share_new_spectr_field_names(sph_asbl_s%new_sph_data)
!
!      call check_nodal_field_name                                      &
!     &   (50+my_rank, sph_asbl_s%new_sph_data%fld(1))
!
      end subroutine init_cvt_rayleigh_sph
!
! ----------------------------------------------------------------------
!
      subroutine analyze_cvt_rayleigh_sph
!
      use r_interpolate_marged_sph
      use set_field_file_names
      use share_field_data
      use matmul_for_legendre_trans
!
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      integer(kind = kint) :: istep
      integer(kind = kint) :: istep_out
!
!     ---------------------
!
      do istep = asbl_param_s%istep_start, asbl_param_s%istep_end,      &
     &          asbl_param_s%increment_step
!
        call init_rayleigh_restart_params(istep,                        &
     &      asbl_param_s%org_fld_file, ra_rst_s)
        call dealloc_rayleigh_radial_grid(ra_rst_s)
!
        istep_out =          istep / asbl_param_s%increment_step
        init_t%i_time_step = istep
        init_t%time =        ra_rst_s%time_org
!
        call calypso_mpi_barrier
        if(my_rank .eq. 0) write(*,*) 'share_time_step_data'
        call share_time_step_data(init_t)
!
        call calypso_mpi_barrier
        if(my_rank .eq. 0) write(*,*) 'convert_fields_from_rayleigh'
        call convert_fields_from_rayleigh                               &
     &     (istep, asbl_param_s%org_fld_file,                           &
     &      sph_asbl_s%new_sph_data%sph, sph_asbl_s%r_itp,              &
     &      ra_rst_s, sph_asbl_s%new_sph_data%fld)
!
        call calypso_mpi_barrier
        if(my_rank .eq. 0) write(*,*) 'const_assembled_sph_data'
        call const_assembled_sph_data(asbl_param_s%b_ratio, init_t,     &
     &      sph_asbl_s%r_itp, sph_asbl_s%new_sph_data,                  &
     &      sph_asbl_s%new_fst_IO, sph_asbl_s%fst_time_IO)
!
        call calypso_mpi_barrier
        if(my_rank .eq. 0) write(*,*) 'sel_write_step_SPH_field_file'
        call sel_write_step_SPH_field_file                              &
     &     (istep_out, asbl_param_s%new_fld_file,                       &
     &      sph_asbl_s%fst_time_IO, sph_asbl_s%new_fst_IO)
!
        call calypso_mpi_barrier
        if(my_rank .eq. 0) write(*,*) 'dealloc_phys_data_IO'
        call dealloc_phys_data_IO(sph_asbl_s%new_fst_IO)
        call dealloc_phys_name_IO(sph_asbl_s%new_fst_IO)
      end do
        close(50+my_rank)
      call calypso_MPI_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_cvt_rayleigh_sph
!
! ----------------------------------------------------------------------
!
      end module analyzer_rayleigh_cvt_sph
