!>@file   set_control_newsph.f90
!!@brief  module set_control_newsph
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine bcast_ctl_param_newsph(asbl_param, sph_asbl)
!!      subroutine set_control_4_newsph(mgd_ctl, asbl_param,            &
!!     &          sph_asbl, org_sph_maker, new_sph_data)
!!        type(control_data_4_merge), intent(in) :: mgd_ctl
!!        type(control_param_assemble), intent(inout) :: asbl_param
!!        type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!!        type(sph_grid_maker_in_sim), intent(inout) :: org_sph_maker
!!      type(SPH_mesh_field_data), intent(inout) :: new_sph_data
!!@endverbatim
!
      module set_control_newsph
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_file_IO_parameter
      use t_control_param_assemble
      use t_spectr_data_4_assemble
!
      implicit    none
!
      character(len=kchara), parameter, private                         &
     &         :: def_org_sph_head = 'mesh_org/in_rj'
      character(len=kchara), parameter, private                         &
     &         :: def_new_sph_head = 'mesh_new/in_rj'
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine bcast_ctl_param_newsph(asbl_param, sph_asbl)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use bcast_file_IO_parameter
!
      type(control_param_assemble), intent(inout) :: asbl_param
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
!
!
      call calypso_mpi_bcast_one_int(sph_asbl%np_sph_org, 0)
      call calypso_mpi_bcast_one_int(sph_asbl%np_sph_new, 0)
!
      call calypso_mpi_bcast_one_int(asbl_param%iflag_newtime, 0)
      call calypso_mpi_bcast_one_int(asbl_param%istep_new_rst, 0)
      call calypso_mpi_bcast_one_int(asbl_param%increment_new_step, 0)
      call calypso_mpi_bcast_one_real(asbl_param%time_new, 0)
!
      call calypso_mpi_bcast_one_int(asbl_param%istep_start, 0)
      call calypso_mpi_bcast_one_int(asbl_param%istep_end, 0)
      call calypso_mpi_bcast_one_int(asbl_param%increment_step, 0)
!
      call bcast_field_IO_parameter(asbl_param%org_mesh_file)
      call bcast_field_IO_parameter(asbl_param%new_mesh_file)
!
      call bcast_field_IO_parameter(asbl_param%org_fld_file)
      call bcast_field_IO_parameter(asbl_param%new_fld_file)
!
      call calypso_mpi_bcast_one_int(asbl_param%iflag_delete_org, 0)
!
      call calypso_mpi_bcast_one_real(asbl_param%b_ratio, 0)
!
      end subroutine bcast_ctl_param_newsph
!
!------------------------------------------------------------------
!
      subroutine set_control_4_newsph(mgd_ctl, asbl_param,              &
     &          sph_asbl, org_sph_maker, new_sph_data)
!
      use m_error_IDs
      use t_control_data_4_merge
      use t_SPH_mesh_field_data
      use t_ctl_params_gen_sph_shell
      use m_file_format_switch
      use set_control_platform_item
      use set_ctl_4_shell_grids
      use new_SPH_restart
      use skip_comment_f
!
      type(control_data_4_merge), intent(in) :: mgd_ctl
      type(control_param_assemble), intent(inout) :: asbl_param
      type(spectr_data_4_assemble), intent(inout) :: sph_asbl
      type(sph_grid_maker_in_sim), intent(inout) :: org_sph_maker
      type(SPH_mesh_field_data), intent(inout) :: new_sph_data
!
      integer(kind = kint) :: ierr
!
!
      if (mgd_ctl%source_plt%ndomain_ctl%iflag .gt. 0) then
        sph_asbl%np_sph_org                                             &
     &      = int(mgd_ctl%source_plt%ndomain_ctl%intvalue)
      else
        write(e_message,'(a)') 'Set number of subdomains'
        call calypso_mpi_abort(ierr_file, e_message)
      end if
!
      if (mgd_ctl%assemble_plt%ndomain_ctl%iflag .gt. 0) then
        sph_asbl%np_sph_new                                             &
     &      = int(mgd_ctl%assemble_plt%ndomain_ctl%intvalue)
      else
        write(e_message,'(a)') 'Set number of subdomains for new grid'
        call calypso_mpi_abort(ierr_file, e_message)
      end if
!
      if(sph_asbl%np_sph_new .ne. nprocs) then
        write(e_message,'(a,a)') 'Number of MPI prosesses should be ',  &
     &                           'the number of target subdomains.'
        call calypso_mpi_abort(ierr_P_MPI, e_message)
      end if
!
      call set_parallel_file_ctl_params(def_org_sph_head,               &
     &    mgd_ctl%source_plt%sph_file_prefix,                           &
     &    mgd_ctl%source_plt%sph_file_fmt_ctl,                          &
     &    asbl_param%org_mesh_file)
!
      call set_parallel_file_ctl_params(def_new_sph_head,               &
     &    mgd_ctl%assemble_plt%sph_file_prefix,                         &
     &    mgd_ctl%assemble_plt%sph_file_fmt_ctl,                        &
     &    asbl_param%new_mesh_file)
!
!
      call set_assemble_rst_file_param                                  &
     &   (mgd_ctl%source_plt, mgd_ctl%assemble_plt, asbl_param)
!
!
!      if((asbl_param%new_fld_file%iflag_format/iflag_single) .gt. 0    &
!     &     .and. sph_asbl%np_sph_new .ne. nprocs) then
!        asbl_param%new_fld_file%iflag_format                           &
!     &           = asbl_param%new_fld_file%iflag_format - iflag_single
!        write(*,*) 'Turn off Merged data IO ',                         &
!     &             'when number of MPI prosesses is not ',             &
!     &             'the number of target subdomains.'
!      end if
!
      call set_delete_flag_4_assemble(mgd_ctl%assemble_plt, asbl_param)
!
      call set_magnetic_ratio_4_assemble                                &
     &   (mgd_ctl%magnetic_ratio_ctl, asbl_param)
!
      call set_assemble_step_4_rst(mgd_ctl%t_mge_ctl, asbl_param)
      call set_control_new_step(mgd_ctl%t2_mge_ctl, asbl_param)
!
!   set spherical shell parameters
!
      if(mgd_ctl%src_psph_ctl%iflag_sph_shell .gt. 0) then
        org_sph_maker%make_SPH_flag =    .TRUE.
        org_sph_maker%mesh_output_flag = .FALSE.
!
        if(iflag_debug.gt.0) write(*,*) 'set_control_4_shell_grids org'
        call set_control_4_shell_grids                                  &
     &     (sph_asbl%np_sph_org, mgd_ctl%src_psph_ctl%Fmesh_ctl,        &
     &      mgd_ctl%src_psph_ctl%spctl, mgd_ctl%src_psph_ctl%sdctl,     &
     &      org_sph_maker%sph_tmp, org_sph_maker%gen_sph, ierr)
        if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
      end if
!
      call set_ctl_4_sph_grid_maker(nprocs, mgd_ctl%asbl_psph_ctl,      &
     &    mgd_ctl%assemble_plt%sph_file_prefix,                         &
     &    asbl_param%new_mesh_file, new_sph_data%sph_maker, ierr)
      if(ierr .gt. 0) call calypso_mpi_abort(ierr, e_message)
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &          'istep_start, istep_end, increment_step',               &
     &           asbl_param%istep_start, asbl_param%istep_end,          &
     &           asbl_param%increment_step
!
      end subroutine set_control_4_newsph
!
! -----------------------------------------------------------------------
!
      end module set_control_newsph
