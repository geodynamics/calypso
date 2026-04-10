!>@file   set_bc_sph_scalars.f90
!!@brief  module set_bc_sph_scalars
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for scalar fields
!!
!!@verbatim
!!      subroutine set_sph_bc_temp_sph(bc_IO, sph_params, sph_rj,       &
!!     &          radial_rj_grp, temp_nod, h_flux_surf, sph_bc_T, bcs_T)
!!      subroutine set_sph_bc_composition_sph(bc_IO, sph_params, sph_rj,&
!!     &          radial_rj_grp, light_nod, light_surf, sph_bc_C, bcs_C)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(sph_boundary_type), intent(inout) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(inout) :: bcs_T
!!        type(sph_boundary_type), intent(inout) :: sph_bc_C
!!        type(sph_scalar_boundary_data), intent(inout) :: bcs_C
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
      use m_boundary_condition_IDs
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_group_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_field_labels
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_bc_scalar_sph                                &
     &         (sph_rj, sph_bc, ICB_Sspec, CMB_Sspec)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_coef), intent(in) :: ICB_Sspec, CMB_Sspec
!
      integer(kind = kint) :: i
!
!
      if(my_rank .gt. 0) return
      if(i_debug .gt. 0) then
        write(*,*) 'sph_bc%iflag_icb', sph_bc%iflag_icb
        do i = 1, sph_rj%nidx_rj(2)
          if( ICB_Sspec%S_BC(i) .ne. 0.0d0) write(*,*)                  &
     &      ' ICB_Sspec%S_BC', i,  sph_rj%idx_gl_1d_rj_j(i,2:3),        &
     &       ICB_Sspec%S_BC(i)
        end do
        write(*,*) 'sph_bc%iflag_cmb', sph_bc%iflag_cmb
        do i = 1, sph_rj%nidx_rj(2)
          if( CMB_Sspec%S_BC(i) .ne. 0.0d0) write(*,*)                  &
     &       ' CMB_Sspec%S_BC', i, sph_rj%idx_gl_1d_rj_j(i,2:3),        &
     &        CMB_Sspec%S_BC(i)
        end do
      end if
!
      end subroutine check_sph_bc_scalar_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_temp_sph(bc_IO, sph_params, sph_rj,         &
     &          radial_rj_grp, temp_nod, h_flux_surf, sph_bc_T, bcs_T)
!
      use m_base_field_labels
      use m_base_force_labels
      use set_bc_sph_scalar
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(boundary_condition_list), intent(in) :: temp_nod
      type(boundary_condition_list), intent(in) :: h_flux_surf
!
      type(sph_boundary_type), intent(inout) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(inout) :: bcs_T
!
!
      call s_set_sph_bc_scalar(temperature, heat_flux, bc_IO,           &
     &    sph_params, sph_rj, radial_rj_grp, temp_nod, h_flux_surf,     &
     &    sph_bc_T, bcs_T)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Fixed boundary condition spectr for heat'
        call check_sph_bc_scalar_sph                                    &
     &    (sph_rj, sph_bc_T, bcs_T%ICB_Sspec, bcs_T%CMB_Sspec)
        write(*,*) 'sph_bc_T%iflag_icb', sph_bc_T%iflag_icb
        write(*,*) 'sph_bc_T%iflag_cmb', sph_bc_T%iflag_cmb
      end if
!
      if(sph_bc_T%iflag_icb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(a)')                               &
     &   'Thermal boundary condition for ICB is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_IO%file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if
!
      if(sph_bc_T%iflag_cmb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(a)')                               &
     &   'Thermal boundary condition for CMB is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_IO%file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if
!
      end subroutine set_sph_bc_temp_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_bc_composition_sph(bc_IO, sph_params, sph_rj,  &
     &          radial_rj_grp, light_nod, light_surf, sph_bc_C, bcs_C)
!
      use m_base_field_labels
      use m_base_force_labels
      use set_bc_sph_scalar
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(boundary_condition_list), intent(in) :: light_nod
      type(boundary_condition_list), intent(in) :: light_surf
!
      type(sph_boundary_type), intent(inout) :: sph_bc_C
      type(sph_scalar_boundary_data), intent(inout) :: bcs_C
!
!
      call s_set_sph_bc_scalar(composition, composite_flux, bc_IO,      &
     &    sph_params, sph_rj, radial_rj_grp, light_nod, light_surf,     &
     &    sph_bc_C, bcs_C)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Fixed boundary condition spectr for composition'
        call check_sph_bc_scalar_sph                                    &
     &    (sph_rj, sph_bc_C, bcs_C%ICB_Sspec, bcs_C%CMB_Sspec)
        write(*,*) 'sph_bc_C%iflag_icb', sph_bc_C%iflag_icb
        write(*,*) 'sph_bc_C%iflag_cmb', sph_bc_C%iflag_cmb
      end if
!
      if(sph_bc_C%iflag_icb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Compositional  boundary condition for ICB ',                  &
     &   'is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_IO%file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if
!
      if(sph_bc_C%iflag_cmb .eq. iflag_undefined_bc) then
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Compositional  boundary condition for CMB ',                  &
     &   'is not defined correctly.'
        if(my_rank .eq. 0) write(*,'(2a)')                              &
     &   'Check control_MHD and ', trim(bc_IO%file_name)
        call calypso_MPI_abort(ierr_BC, 'Check boundary conditions')
      end if

      end subroutine set_sph_bc_composition_sph
!
! -----------------------------------------------------------------------
!
      end module set_bc_sph_scalars
