!>@file   set_bc_sph_scalars.f90
!!@brief  module set_bc_sph_scalars
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for scalar fields
!!
!!@verbatim
!!      subroutine set_sph_bc_temp_sph
!!      subroutine set_sph_bc_composition_sph
!!      subroutine adjust_sph_temp_bc_by_reftemp
!!@endverbatim
!
      module set_bc_sph_scalars
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use m_control_parameter
      use m_boundary_condition_IDs
!
      implicit none
!
      private :: set_sph_bc_scalar_sph
      private :: inner_sph_bc_scalar_sph, outer_sph_bc_scalar_sph
      private :: set_homogenious_scalar_bc, set_homogenious_grad_bc
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_temp_sph
!
      use m_phys_labels
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_boundary_params_sph_MHD
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
      use m_sph_boundary_input_data
!
      integer(kind = kint) :: i
!
!
      call set_sph_bc_scalar_sph(fhd_temp, fhd_h_flux,                  &
     &    temp_nod, h_flux_surf, sph_bc_T)
!
      sph_bc_T%ICB_flux(1:nidx_rj(2))                                   &
     &      = -sph_bc_T%ICB_flux(1:nidx_rj(2))
!
      if(iflag_debug .gt. 0) then
        do i = 1, nidx_rj(2)
          if( sph_bc_T%ICB_fld(i) .ne. 0.0d0) write(*,*)                &
     &      ' sph_bc_T%ICB_fld', i,  idx_gl_1d_rj_j(i,2:3),             &
     &       sph_bc_T%ICB_fld(i)
          if( sph_bc_T%ICB_flux(i) .ne. 0.0d0) write(*,*)               &
     &      ' sph_bc_T%ICB_flux', i,  idx_gl_1d_rj_j(i,2:3),            &
     &       sph_bc_T%ICB_flux(i)
        end do
        do i = 1, nidx_rj(2)
          if( sph_bc_T%CMB_fld(i) .ne. 0.0d0) write(*,*)                &
     &       ' sph_bc_T%CMB_fld', i, idx_gl_1d_rj_j(i,2:3),             &
     &        sph_bc_T%CMB_fld(i)
          if( sph_bc_T%CMB_flux(i) .ne. 0.0d0) write(*,*)               &
     &       ' sph_bc_T%CMB_flux', i,  idx_gl_1d_rj_j(i,2:3),           &
     &        sph_bc_T%CMB_flux(i)
        end do
      end if
      
!
      if(sph_bc_T%iflag_icb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(a)')                               &
     &   'Thermal boundary condition for ICB is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_sph_file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if
!
      if(sph_bc_T%iflag_cmb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(a)')                               &
     &   'Thermal boundary condition for CMB is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_sph_file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if
!
      end subroutine set_sph_bc_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_composition_sph
!
      use m_phys_labels
      use m_spheric_parameter
      use m_boundary_params_sph_MHD
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
      use m_sph_boundary_input_data
!
      integer(kind = kint) :: i
!
!
      call set_sph_bc_scalar_sph(fhd_light, fhd_c_flux,                 &
     &    light_nod, light_surf, sph_bc_C)
!
      if(iflag_debug .gt. 0) then
        do i = 1, nidx_rj(2)
          if( sph_bc_C%ICB_fld(i) .ne. 0.0d0) write(*,*)                &
     &      ' sph_bc_C%ICB_fld', i,  idx_gl_1d_rj_j(i,2:3),             &
     &       sph_bc_C%ICB_fld(i)
          if( sph_bc_C%ICB_flux(i) .ne. 0.0d0) write(*,*)               &
     &      ' sph_bc_C%ICB_flux', i,  idx_gl_1d_rj_j(i,2:3),            &
     &       sph_bc_C%ICB_flux(i)
        end do
        do i = 1, nidx_rj(2)
          if( sph_bc_C%CMB_fld(i) .ne. 0.0d0) write(*,*)                &
     &       ' sph_bc_C%CMB_fld', i, idx_gl_1d_rj_j(i,2:3),             &
     &        sph_bc_C%CMB_fld(i)
          if( sph_bc_C%CMB_flux(i) .ne. 0.0d0) write(*,*)               &
     &       ' sph_bc_C%CMB_flux', i,  idx_gl_1d_rj_j(i,2:3),           &
     &        sph_bc_C%CMB_flux(i)
        end do
      end if
!
      if(sph_bc_C%iflag_icb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Compositional  boundary condition for ICB ',                  &
     &   'is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_sph_file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if
!
      if(sph_bc_C%iflag_cmb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Compositional  boundary condition for CMB ',                  &
     &   'is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_sph_file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if

      end subroutine set_sph_bc_composition_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_scalar_sph(fhd_field, fhd_flux,             &
     &          nod_bc_list, surf_bc_list, sph_bc)
!
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
      use m_sph_boundary_input_data
      use t_boundary_params_sph_MHD
!
      character(len=kchara), intent(in) :: fhd_field
      character(len=kchara), intent(in) :: fhd_flux
      type(nod_bc_list_type), intent(in) :: nod_bc_list
      type(surface_bc_list_type), intent(in) :: surf_bc_list
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      integer(kind = kint) :: igrp_icb, igrp_cmb
!
!
      call find_both_sides_of_boundaries(nod_bc_list, surf_bc_list,     &
     &    sph_bc, igrp_icb, igrp_cmb)
!
      call alloc_fixed_bc_array(nidx_rj(2), sph_bc)
!
!      Boundary setting for inner boundary
      call inner_sph_bc_scalar_sph(fhd_field, fhd_flux,                 &
     &    nod_bc_list, surf_bc_list, igrp_icb, sph_bc)
!
!      Boundary setting for outer boundary
      call outer_sph_bc_scalar_sph(fhd_field, fhd_flux,                 &
     &    nod_bc_list, surf_bc_list, igrp_cmb, sph_bc)
!
      end subroutine set_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine inner_sph_bc_scalar_sph(fhd_field, fhd_flux,           &
     &          nod_bc_list, surf_bc_list, igrp_icb, sph_bc)
!
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
      use m_sph_boundary_input_data
      use t_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: igrp_icb
      character(len=kchara), intent(in) :: fhd_field
      character(len=kchara), intent(in) :: fhd_flux
      type(nod_bc_list_type), intent(in) :: nod_bc_list
      type(surface_bc_list_type), intent(in) :: surf_bc_list
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      integer(kind = kint) :: i
!
!      Boundary setting for inner boundary
      i = abs(igrp_icb)
      if(igrp_icb .lt. 0) then
        if( surf_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (surf_bc_list%bc_name(i), surf_bc_list%bc_magnitude(i),    &
     &        nidx_rj(2), sph_bc%icb_grp_name, sph_bc%ICB_flux,         &
     &        sph_bc%iflag_icb)
        else if(surf_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_flux, nidx_rj(2), sph_bc%icb_grp_name,                &
     &        sph_bc%ICB_flux, sph_bc%iflag_icb)
!
        else if(surf_bc_list%ibc_type(i) .eq. iflag_sph_2_center        &
     &    .and. surf_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
         sph_bc%iflag_icb = iflag_sph_fill_center
        else if ( surf_bc_list%ibc_type(i) .eq. iflag_sph_clip_center   &
     &    .and. surf_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
         sph_bc%iflag_icb = iflag_sph_fix_center
         sph_bc%CTR_fld =   surf_bc_list%bc_magnitude(i)
        end if
!
      else if(igrp_icb .gt. 0) then
        if ( nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (nod_bc_list%bc_name(i), nod_bc_list%bc_magnitude(i),      &
     &        nidx_rj(2), sph_bc%icb_grp_name, sph_bc%ICB_flux,         &
     &        sph_bc%iflag_icb)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_flux) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_flux,  nidx_rj(2), sph_bc%icb_grp_name,               &
     &       sph_bc%ICB_flux, sph_bc%iflag_icb)
!
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc(nod_bc_list%bc_name(i),        &
     &        nod_bc_list%bc_magnitude(i), nidx_rj(2),                  &
     &        sph_bc%icb_grp_name, sph_bc%ICB_fld, sph_bc%iflag_icb)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_scalar_bc_by_file(fhd_field, nidx_rj(2),       &
     &        sph_bc%icb_grp_name, sph_bc%ICB_fld,                      &
     &        sph_bc%iflag_icb)
!
        else if(nod_bc_list%ibc_type(i) .eq. iflag_sph_2_center         &
     &    .and. nod_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_fill_center
        else if(nod_bc_list%ibc_type(i) .eq. iflag_sph_clip_center      &
     &    .and. nod_bc_list%bc_name(i) .eq. sph_bc%icb_grp_name) then
          sph_bc%iflag_icb = iflag_sph_fix_center
          sph_bc%CTR_fld =   nod_bc_list%bc_magnitude(i)
        end if
      end if
!
      sph_bc%ICB_flux(1:nidx_rj(2))  = -sph_bc%ICB_flux(1:nidx_rj(2))
!
      end subroutine inner_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine outer_sph_bc_scalar_sph(fhd_field, fhd_flux,           &
     &          nod_bc_list, surf_bc_list, igrp_cmb, sph_bc)
!
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_bc_data_list
      use m_surf_data_list
      use m_sph_spectr_data
      use m_sph_boundary_input_data
      use t_boundary_params_sph_MHD
!
      integer(kind = kint), intent(in) :: igrp_cmb
      character(len=kchara), intent(in) :: fhd_field
      character(len=kchara), intent(in) :: fhd_flux
      type(nod_bc_list_type), intent(in) :: nod_bc_list
      type(surface_bc_list_type), intent(in) :: surf_bc_list
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      integer(kind = kint) :: i
!
!      Boundary setting for outer boundary
      i = abs(igrp_cmb)
      if(igrp_cmb .lt. 0) then
        if ( surf_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_grad_bc                                  &
     &       (surf_bc_list%bc_name(i), surf_bc_list%bc_magnitude(i),    &
     &        nidx_rj(2), sph_bc%cmb_grp_name, sph_bc%CMB_flux,         &
     &        sph_bc%iflag_cmb)
        else if (surf_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_flux, nidx_rj(2), sph_bc%cmb_grp_name,                &
     &        sph_bc%CMB_flux, sph_bc%iflag_cmb)
        end if
!
      else if(igrp_cmb .gt. 0) then
        if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_flux) then
          call set_homogenious_grad_bc                                  &
     &       (nod_bc_list%bc_name(i), nod_bc_list%bc_magnitude(i),      &
     &        nidx_rj(2), sph_bc%cmb_grp_name, sph_bc%CMB_flux,         &
     &        sph_bc%iflag_cmb)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_flux) then
          call set_fixed_gradient_bc_by_file                            &
     &       (fhd_flux, nidx_rj(2), sph_bc%cmb_grp_name,                &
     &        sph_bc%CMB_flux, sph_bc%iflag_cmb)
!
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_fix_s) then
          call set_homogenious_scalar_bc(nod_bc_list%bc_name(i),        &
     &        nod_bc_list%bc_magnitude(i), nidx_rj(2),                  &
     &        sph_bc%cmb_grp_name, sph_bc%CMB_fld, sph_bc%iflag_cmb)
        else if(nod_bc_list%ibc_type(i)  .eq. iflag_bc_file_s) then
          call set_fixed_scalar_bc_by_file(fhd_field, nidx_rj(2),       &
     &        sph_bc%cmb_grp_name, sph_bc%CMB_fld,                      &
     &        sph_bc%iflag_cmb)
        end if
      end if
!
      end subroutine outer_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_homogenious_scalar_bc(bc_name, bc_magnitude,       &
     &          jmax, ref_grp, bc_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use m_spheric_parameter
!
      character(len=kchara), intent(in) :: ref_grp
      character(len=kchara), intent(in) :: bc_name
      real(kind = kreal), intent(in) :: bc_magnitude
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
!
      if     (bc_name .eq. ref_grp) then
        iflag_bc_scalar =  iflag_fixed_field
        if(idx_rj_degree_zero .gt. 0) then
          bc_data(idx_rj_degree_zero) = bc_magnitude
        end if
      end if
!
      end subroutine set_homogenious_scalar_bc
!
! -----------------------------------------------------------------------
!
      subroutine set_homogenious_grad_bc(bc_name, bc_magnitude,         &
     &          jmax, ref_grp, bc_data, iflag_bc_scalar)
!
      use t_boundary_params_sph_MHD
      use m_spheric_parameter
!
      character(len=kchara), intent(in) :: ref_grp
      character(len=kchara), intent(in) :: bc_name
      real(kind = kreal), intent(in) :: bc_magnitude
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(inout) :: bc_data(jmax)
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
!
      if     (bc_name .eq. ref_grp) then
        iflag_bc_scalar =  iflag_fixed_flux
        if(idx_rj_degree_zero .gt. 0) then
          bc_data(idx_rj_degree_zero) = bc_magnitude
        end if
      end if
!
      end subroutine set_homogenious_grad_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine find_both_sides_of_boundaries(bc_nod, bc_surf,         &
     &          sph_bc, igrp_icb, igrp_cmb)
!
      use m_spheric_parameter
      use m_group_data_sph_specr
      use m_bc_data_list
      use m_surf_data_list
      use t_boundary_params_sph_MHD
!
      type(nod_bc_list_type), intent(in) :: bc_nod
      type(surface_bc_list_type), intent(in) :: bc_surf
!
      integer(kind = kint), intent(inout) :: igrp_icb, igrp_cmb
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      integer(kind = kint) :: icou, i, j, inum, num, ibc_in, ibc_out
      integer(kind = kint) :: igrp_bc(2), kr_bc(2)
      character(len=kchara) :: r_bc_grp_name(2)
      character(len=kchara) :: sf_grp_name
!
!
      icou = 0
      do i = 1, bc_nod%num_bc
        if(icou .ge. 2) exit
        do j = 1, num_radial_grp_rj
          if(bc_nod%bc_name(i) .eq. name_radial_grp_rj(j)) then
            num = istack_radial_grp_rj(j) - istack_radial_grp_rj(j-1)
            if(num .ne. 1) go to 10
!
            icou = icou + 1
            inum = istack_radial_grp_rj(j)
            kr_bc(icou) = item_radial_grp_rj(inum)
            r_bc_grp_name(icou) = bc_nod%bc_name(i)
            igrp_bc(icou) = i
            exit
          end if
        end do
      end do
!
      do i = 1, bc_surf%num_bc
        if(icou .ge. 2) exit
        do j = 1, num_radial_grp_rj
          if(bc_surf%bc_name(i) .eq. name_radial_grp_rj(j)) then
            num = istack_radial_grp_rj(j) - istack_radial_grp_rj(j-1)
            if(num .ne. 1) go to 10
!
            icou = icou + 1
            inum = istack_radial_grp_rj(j)
            kr_bc(icou) = item_radial_grp_rj(inum)
            r_bc_grp_name(icou) = bc_surf%bc_name(i)
            igrp_bc(icou) = -i
            exit
          end if
        end do
      end do
!
      if(kr_bc(1).le.0 .or. kr_bc(2).le.0 .or. kr_bc(1).eq.kr_bc(2))    &
     &     then
        write(*,*) 'Inner and outer boundary: ', kr_bc(1:2)
        go to 10
      else if(kr_bc(1) .gt. kr_bc(2)) then
        ibc_in =  2
        ibc_out = 1
      else
        ibc_in =  1
        ibc_out = 2
      end if
!
      igrp_icb = igrp_bc(ibc_in)
      sph_bc%kr_in =         kr_bc(ibc_in)
      sph_bc%icb_grp_name =  r_bc_grp_name(ibc_in)
!
      igrp_cmb = igrp_bc(ibc_out)
      sph_bc%kr_out =       kr_bc(ibc_out)
      sph_bc%cmb_grp_name = r_bc_grp_name(ibc_out)
!
      sph_bc%r_ICB(0) = radius_1d_rj_r(sph_bc%kr_in)
      sph_bc%r_ICB(1) = ar_1d_rj(sph_bc%kr_in,1)
      sph_bc%r_ICB(2) = ar_1d_rj(sph_bc%kr_in,2)
      sph_bc%r_CMB(0) = radius_1d_rj_r(sph_bc%kr_out)
      sph_bc%r_CMB(1) = ar_1d_rj(sph_bc%kr_out,1)
      sph_bc%r_CMB(2) = ar_1d_rj(sph_bc%kr_out,2)
!
      return
!
  10  continue
      call calypso_MPI_abort(ierr_BC, 'Set correct boundary condition')
!
      end subroutine find_both_sides_of_boundaries
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_scalars
