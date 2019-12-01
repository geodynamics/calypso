!r_interpolate_sph_data.f90
!     module r_interpolate_sph_data
!
!      Written by H. Matsui on Sep., 2011
!
!      subroutine deallocate_original_sph_data
!
!!      subroutine copy_cmb_icb_radial_point(nlayer_ICB, nlayer_CMB)
!!      subroutine set_cmb_icb_radial_point                             &
!!     &         (cmb_r_grp, icb_r_grp, radial_rj_grp)
!!        type(group_data), intent(in) :: radial_rj_grp
!!      subroutine set_sph_magne_address(rj_fld, ipol)
!!        type(phys_data), intent(in) :: rj_fld
!!        type(phys_address), intent(inout) :: ipol
!!      subroutine input_old_rj_sph_trans                               &
!!     &         (rj_file_param, l_truncation, sph_rj)
!!
!!      subroutine r_interpolate_sph_rst_from_IO                        &
!!     &         (fld_IO, sph_rj, ipol, rj_fld)
!!      subroutine r_interpolate_sph_fld_from_IO                        &
!!     &         (fld_IO, sph_rj, ipol, rj_fld)
!!      subroutine set_poloidal_b_by_gauss_coefs                        &
!!     &         (sph_rj, ipol, d_gauss, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_address), intent(in) :: ipol
!!        type(global_gauss_points), intent(in) :: d_gauss
!!        type(phys_data), intent(inout) :: rj_fld
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
!
      implicit  none
!
!
      integer(kind = kint) :: kr_inside, kr_outside
!
      integer(kind = kint) :: nri_org, n_rj_org
      integer(kind = kint), allocatable :: k_inter(:,:)
      real(kind = kreal), allocatable :: rcoef_inter(:,:)
!
      real(kind = kreal), allocatable :: r_org(:)
!
      integer(kind = kint) :: ntot_phys_rj_itp
      real(kind = kreal), allocatable :: d_rj_org(:,:)
!
      private :: allocate_original_sph_data
      private :: copy_original_sph_rj_from_IO
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine copy_cmb_icb_radial_point(nlayer_ICB, nlayer_CMB)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!
!
      kr_outside = nlayer_CMB
      kr_inside =  nlayer_ICB
!
      end subroutine copy_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!
      subroutine set_cmb_icb_radial_point                               &
     &         (cmb_r_grp, icb_r_grp, radial_rj_grp)
!
      use t_group_data
!
      character(len = kchara), intent(in) :: cmb_r_grp, icb_r_grp
      type(group_data), intent(in) :: radial_rj_grp
!
      integer(kind = kint) :: igrp, inum
!
!
      kr_outside = 0
      do igrp = 1, radial_rj_grp%num_grp
        if(radial_rj_grp%grp_name(igrp) .eq. cmb_r_grp) then
          inum = radial_rj_grp%istack_grp(igrp-1) + 1
          kr_outside = radial_rj_grp%item_grp(inum)
          exit
        end if
      end do
!
      kr_inside = 0
      do igrp = 1, radial_rj_grp%num_grp
        if(radial_rj_grp%grp_name(igrp) .eq. icb_r_grp) then
          inum = radial_rj_grp%istack_grp(igrp-1) + 1
          kr_inside = radial_rj_grp%item_grp(inum)
          exit
        end if
      end do
!
      end subroutine set_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine allocate_original_sph_data
!
!
      allocate(r_org(nri_org))
      allocate(k_inter(nri_org,2))
      allocate(rcoef_inter(nri_org,2))
!
      allocate(d_rj_org(n_rj_org,6))
!
      k_inter = izero
      r_org = zero
      rcoef_inter = zero
      d_rj_org =    zero
!
      end subroutine allocate_original_sph_data
!
!  -------------------------------------------------------------------
!
      subroutine deallocate_original_sph_data
!
      deallocate(r_org, d_rj_org)
      deallocate(k_inter, rcoef_inter)
!
      end subroutine deallocate_original_sph_data
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_magne_address(rj_fld, ipol)
!
      use m_phys_labels
!
      type(phys_data), intent(in) :: rj_fld
      type(phys_address), intent(inout) :: ipol
!
      integer(kind = kint) :: i
!
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. fhd_magne) then
          ipol%i_magne = rj_fld%istack_component(i-1) + 1
          exit
        end if
      end do
!
      end subroutine set_sph_magne_address
!
!  -------------------------------------------------------------------
!
      subroutine input_old_rj_sph_trans                                 &
     &         (rj_file_param, l_truncation, sph_rj)
!
      use t_file_IO_parameter
      use t_spheric_data_IO
      use sph_file_MPI_IO_select
      use radial_interpolation
!
      type(field_IO_params), intent(in) :: rj_file_param
      integer(kind = kint), intent(inout) :: l_truncation
      type(sph_rj_grid), intent(inout) ::  sph_rj
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
     &   (l_truncation, sph_rj, sph_file%sph_IO)
      call dealloc_rj_mode_IO(sph_file)
!
      call const_radial_itp_table(nri_org, r_org,                       &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    kr_inside, kr_outside, k_inter, rcoef_inter)
!
      end subroutine input_old_rj_sph_trans
!
!  -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_rst_from_IO                          &
     &         (fld_IO, sph_rj, ipol, rj_fld)
!
      use m_phys_labels
      use t_field_data_IO
      use extend_potential_field
      use radial_interpolation
!
      type(field_IO), intent(in) :: fld_IO
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      do i_fld = 1, rj_fld%ntot_phys
        do j_fld = 1, fld_IO%num_field_IO
          if(rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_fld)) then
            if     (rj_fld%phys_name(i_fld) .eq. fhd_velo               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_vort               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_press              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_temp               &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_magne              &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_mag_potential      &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_mom            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_uxb            &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_heat           &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_pre_composit       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_heat_source        &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_light_source       &
     &         .or. rj_fld%phys_name(i_fld) .eq. fhd_entropy_source     &
     &         ) then
              call set_org_rj_phys_data_from_IO                         &
     &           (j_fld, fld_IO, n_rj_org, d_rj_org)
              call r_interpolate_sph_vector(i_fld, sph_rj%nidx_rj,      &
     &            rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,    &
     &            rj_fld%istack_component, kr_inside, kr_outside,       &
     &            nri_org, k_inter, rcoef_inter, n_rj_org, d_rj_org,    &
     &            rj_fld%d_fld)
              exit
            end if
          end if
        end do
      end do
!
      if (ipol%i_magne .gt. 0) then
        call ext_outside_potential(kr_outside, ipol%i_magne,            &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call ext_inside_potential(kr_inside, ipol%i_magne,              &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine r_interpolate_sph_rst_from_IO
!
! -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_fld_from_IO                          &
     &         (fld_IO, sph_rj, ipol, rj_fld)
!
      use t_field_data_IO
      use extend_potential_field
      use radial_interpolation
!
      type(field_IO), intent(in) :: fld_IO
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) ::  i_fld, j_fld
!
!
      do i_fld = 1, rj_fld%ntot_phys
        do j_fld = 1, fld_IO%num_field_IO
          if(rj_fld%phys_name(i_fld) .eq. fld_IO%fld_name(j_fld)) then
            call set_org_rj_phys_data_from_IO                           &
     &         (j_fld, fld_IO, n_rj_org, d_rj_org)
            call r_interpolate_sph_vector(i_fld, sph_rj%nidx_rj,        &
     &          rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,      &
     &          rj_fld%istack_component,  kr_inside, kr_outside,        &
     &          nri_org, k_inter, rcoef_inter, n_rj_org, d_rj_org,      &
     &          rj_fld%d_fld)
            exit
          end if
        end do
      end do
!
      if (ipol%i_magne .gt. 0) then
        call ext_outside_potential(kr_outside, ipol%i_magne,            &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call ext_inside_potential(kr_inside, ipol%i_magne,              &
     &      sph_rj%nidx_rj, sph_rj%idx_gl_1d_rj_j,                      &
     &      sph_rj%radius_1d_rj_r, sph_rj%a_r_1d_rj_r,                  &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine r_interpolate_sph_fld_from_IO
!
! -----------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_poloidal_b_by_gauss_coefs                          &
     &         (sph_rj, ipol, d_gauss, rj_fld)
!
      use t_global_gauss_coefs
      use extend_potential_field
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_address), intent(in) :: ipol
      type(global_gauss_points), intent(in) :: d_gauss
      type(phys_data), intent(inout) :: rj_fld
!
!
      write(*,*) ' ipol%i_magne', ipol%i_magne, kr_outside, kr_inside
      if (ipol%i_magne .gt. 0) then
        call gauss_to_poloidal_out                                      &
     &     (kr_outside, d_gauss%ltr_w, d_gauss%r_gauss,                 &
     &      d_gauss%w_gauss, d_gauss%index_w, ipol%i_magne, sph_rj,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        call gauss_to_poloidal_in                                       &
     &     (kr_inside, d_gauss%ltr_w, d_gauss%r_gauss,                  &
     &      d_gauss%w_gauss, d_gauss%index_w, ipol%i_magne, sph_rj,     &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine set_poloidal_b_by_gauss_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_original_sph_rj_from_IO                           &
     &         (l_truncation, sph_rj, sph_IO)
!
      use m_error_IDs
      use t_node_id_spherical_IO
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(sph_IO_data), intent(in) :: sph_IO
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
      n_rj_org = sph_IO%numnod_sph
      nri_org =  sph_IO%nidx_sph(1)
!
      call allocate_original_sph_data
!
      r_org(1:n_rj_org) = sph_IO%r_gl_1(1:n_rj_org)
!
      end subroutine copy_original_sph_rj_from_IO
!
! ----------------------------------------------------------------------
!
      end module r_interpolate_sph_data
