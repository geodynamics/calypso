!>@file   r_interpolate_sph_data.f90
!!@brief  module r_interpolate_sph_data
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Interpolate spectr data
!!
!!@verbatim
!!      subroutine copy_cmb_icb_radial_point(nlayer_ICB, nlayer_CMB,    &
!!     &                                     r_itp)
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!      subroutine set_cmb_icb_radial_point                             &
!!     &         (cmb_r_grp, icb_r_grp, radial_rj_grp, r_itp)
!!        character(len = kchara), intent(in) :: cmb_r_grp, icb_r_grp
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!      subroutine set_sph_magne_address(rj_fld, ipol)
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(inout) :: ipol
!!      subroutine input_old_rj_sph_trans                               &
!!     &         (rj_file_param, l_truncation, sph_rj, r_itp)
!!        type(field_IO_params), intent(in) :: rj_file_param
!!        integer(kind = kint), intent(inout) :: l_truncation
!!        type(sph_rj_grid), intent(inout) ::  sph_rj
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!
!!      subroutine r_interpolate_sph_rst_from_IO                        &
!!     &         (fld_IO, sph_rj, ipol, rj_fld, r_itp)
!!      subroutine r_interpolate_sph_fld_from_IO                        &
!!     &         (fld_IO, sph_rj, ipol, rj_fld, r_itp)
!!        type(field_IO), intent(in) :: fld_IO
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!@endverbatim
!
      module r_interpolate_sph_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_spheric_rj_data
      use t_phys_address
      use t_phys_data
      use t_sph_radial_interpolate
!
      implicit  none
!
      private :: copy_original_sph_rj_from_IO
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine copy_cmb_icb_radial_point(nlayer_ICB, nlayer_CMB,      &
     &                                     r_itp)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      r_itp%kr_target_inside =  nlayer_ICB
      r_itp%kr_target_outside = nlayer_CMB
!
      end subroutine copy_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!
      subroutine set_cmb_icb_radial_point                               &
     &         (cmb_r_grp, icb_r_grp, radial_rj_grp, r_itp)
!
      use t_group_data
!
      character(len = kchara), intent(in) :: cmb_r_grp, icb_r_grp
      type(group_data), intent(in) :: radial_rj_grp
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      integer(kind = kint) :: igrp, inum
!
!
      r_itp%kr_target_outside = 0
      do igrp = 1, radial_rj_grp%num_grp
        if(radial_rj_grp%grp_name(igrp) .eq. cmb_r_grp) then
          inum = radial_rj_grp%istack_grp(igrp-1) + 1
          r_itp%kr_target_outside = radial_rj_grp%item_grp(inum)
          exit
        end if
      end do
!
      r_itp%kr_target_inside = 0
      do igrp = 1, radial_rj_grp%num_grp
        if(radial_rj_grp%grp_name(igrp) .eq. icb_r_grp) then
          inum = radial_rj_grp%istack_grp(igrp-1) + 1
          r_itp%kr_target_inside = radial_rj_grp%item_grp(inum)
          exit
        end if
      end do
!
      end subroutine set_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_magne_address(rj_fld, ipol)
!
      use m_base_field_labels
!
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(inout) :: ipol
!
      integer(kind = kint) :: i
!
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          ipol%base%i_magne = rj_fld%istack_component(i-1) + 1
          exit
        end if
      end do
!
      end subroutine set_sph_magne_address
!
!  -------------------------------------------------------------------
!
      subroutine input_old_rj_sph_trans                                 &
     &         (rj_file_param, l_truncation, sph_rj, r_itp)
!
      use t_file_IO_parameter
      use t_spheric_data_IO
      use sph_file_MPI_IO_select
      use check_sph_file_access
      use radial_interpolation
!
      type(field_IO_params), intent(in) :: rj_file_param
!
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid), intent(inout) ::  sph_rj
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      type(field_IO_params) :: sph_file_param
      type(sph_file_data_type) :: sph_file
!
!
      call set_sph_mesh_file_fmt_prefix                                 &
     &   (rj_file_param%iflag_format, rj_file_param%file_prefix,        &
     &    sph_file_param)
      call sel_mpi_read_spectr_rj_file                                  &
     &   (nprocs, my_rank, sph_file_param, sph_file)
      call copy_original_sph_rj_from_IO                                 &
     &   (l_truncation, sph_rj, sph_file%sph_IO, r_itp)
      call dealloc_rj_mode_IO(sph_file)
!
      call cal_radial_interpolation_coef                                &
     &   (r_itp%nri_source, r_itp%source_radius,                        &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    r_itp%kr_inner_source, r_itp%kr_outer_source,                 &
     &    r_itp%k_old2new_in, r_itp%k_old2new_out,                      &
     &    r_itp%coef_old2new_in)
!
      end subroutine input_old_rj_sph_trans
!
!  -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_rst_from_IO                          &
     &         (fld_IO, sph_rj, ipol, rj_fld, r_itp)
!
      use m_base_field_labels
      use m_explicit_term_labels
!
      use t_field_data_IO
      use extend_potential_field
      use radial_interpolation
!
      type(field_IO), intent(in) :: fld_IO
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      integer(kind = kint) :: i_fld, j_fld, ist, ncomp
!
!
      do i_fld = 1, rj_fld%num_phys
        do j_fld = 1, fld_IO%num_field_IO
          if(rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_fld)) then
            if   (rj_fld%phys_name(i_fld) .eq. velocity%name            &
     &       .or. rj_fld%phys_name(i_fld) .eq. vorticity%name           &
     &       .or. rj_fld%phys_name(i_fld) .eq. pressure%name            &
     &       .or. rj_fld%phys_name(i_fld) .eq. temperature%name         &
     &       .or. rj_fld%phys_name(i_fld) .eq. composition%name         &
     &       .or. rj_fld%phys_name(i_fld) .eq. magnetic_field%name      &
     &       .or. rj_fld%phys_name(i_fld) .eq. magnetic_potential%name  &
     &       .or. rj_fld%phys_name(i_fld) .eq. entropy%name             &
     &       .or. rj_fld%phys_name(i_fld) .eq. previous_momentum%name   &
     &       .or. rj_fld%phys_name(i_fld) .eq. previous_induction%name  &
     &       .or. rj_fld%phys_name(i_fld) .eq. previous_heat%name       &
     &       .or. rj_fld%phys_name(i_fld) .eq.previous_composition%name &
     &       .or. rj_fld%phys_name(i_fld) .eq. heat_source%name         &
     &       .or. rj_fld%phys_name(i_fld) .eq. composition_source%name  &
     &       .or. rj_fld%phys_name(i_fld) .eq. entropy_source%name      &
     &         ) then
              ist = rj_fld%istack_component(i_fld-1) + 1
              ncomp = rj_fld%istack_component(i_fld)                    &
     &               - rj_fld%istack_component(i_fld-1)
              call set_org_rj_phys_data_from_IO                         &
     &           (j_fld, fld_IO, r_itp%n_rj_org, r_itp%d_rj_org)
              call r_interpolate_sph_vector(sph_rj%nidx_rj,             &
     &            r_itp%kr_target_inside, r_itp%kr_target_outside,      &
     &            r_itp%nri_target, r_itp%k_old2new_in,                 &
     &            r_itp%k_old2new_out, r_itp%coef_old2new_in,           &
     &            r_itp%n_rj_org, r_itp%d_rj_org,                       &
     &            ncomp, rj_fld%n_point, rj_fld%d_fld(1,ist))
              exit
            end if
          end if
        end do
      end do
!
      if (ipol%base%i_magne .gt. 0) then
        call ext_outside_potential(r_itp%kr_target_outside,             &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol%base%i_magne))
        call ext_inside_potential(r_itp%kr_target_inside,               &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol%base%i_magne))
      end if
!
      end subroutine r_interpolate_sph_rst_from_IO
!
! -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_fld_from_IO                          &
     &         (fld_IO, sph_rj, ipol, rj_fld, r_itp)
!
      use t_field_data_IO
      use extend_potential_field
      use radial_interpolation
!
      type(field_IO), intent(in) :: fld_IO
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      integer(kind = kint) ::  i_fld, j_fld, ist, ncomp
!
!
      do i_fld = 1, rj_fld%num_phys
        do j_fld = 1, fld_IO%num_field_IO
          if(rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_fld)) then
            ist = rj_fld%istack_component(i_fld-1) + 1
            ncomp = rj_fld%istack_component(i_fld)                      &
     &             - rj_fld%istack_component(i_fld-1)
            call set_org_rj_phys_data_from_IO                           &
     &         (j_fld, fld_IO, r_itp%n_rj_org, r_itp%d_rj_org)
            call r_interpolate_sph_vector(sph_rj%nidx_rj,               &
     &          r_itp%kr_target_inside, r_itp%kr_target_outside,        &
     &          r_itp%nri_target, r_itp%k_old2new_in,                   &
     &          r_itp%k_old2new_out, r_itp%coef_old2new_in,             &
     &          r_itp%n_rj_org, r_itp%d_rj_org,                         &
     &          ncomp, rj_fld%n_point, rj_fld%d_fld(1,ist))
            exit
          end if
        end do
      end do
!
      if (ipol%base%i_magne .gt. 0) then
        call ext_outside_potential(r_itp%kr_target_outside,             &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol%base%i_magne))
        call ext_inside_potential(r_itp%kr_target_inside,               &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%d_fld(1,ipol%base%i_magne))
      end if
!
      end subroutine r_interpolate_sph_fld_from_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_original_sph_rj_from_IO                           &
     &         (l_truncation, sph_rj, sph_IO, r_itp)
!
      use m_error_IDs
      use t_node_id_spherical_IO
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_IO_data), intent(in) :: sph_IO
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      if(sph_rj%irank_sph_rj(1).ne.sph_IO%sph_rank(1)                   &
     &       .or. sph_rj%irank_sph_rj(2).ne.sph_IO%sph_rank(2)) then
        call calypso_MPI_abort(ierr_sph,'rj rank ID is wrong')
      end if
!
      if(sph_rj%nidx_global_rj(2) .ne. sph_IO%nidx_gl_sph(2)) then
        call calypso_MPI_abort                                          &
     &     (ierr_sph,'number of local mode is wrong')
      end if
      if(l_truncation .ne. sph_IO%ltr_gl) then
        call calypso_MPI_abort(ierr_sph,'truncation is wrong')
      end if
!
      if(sph_rj%ist_rj(2).ne.sph_IO%ist_sph(2)) then
        call calypso_MPI_abort                                          &
     &      (ierr_sph,'start point of harminics is wrong')
      end if
      if(sph_rj%ied_rj(2) .ne. sph_IO%ied_sph(2)) then
        call calypso_MPI_abort                                          &
     &     (ierr_sph,'end point of harminics is wrong')
      end if
!
      call alloc_org_radius_interpolate(sph_IO%nidx_sph(1), r_itp)
      call alloc_radial_interpolate(sph_rj%nidx_rj(1), r_itp)
      call alloc_original_sph_data(sph_IO%numnod_sph, r_itp)
!
      r_itp%source_radius(1:r_itp%n_rj_org)                             &
     &                    = sph_IO%r_gl_1(1:r_itp%n_rj_org)
!
      end subroutine copy_original_sph_rj_from_IO
!
! ----------------------------------------------------------------------
!
      end module r_interpolate_sph_data
