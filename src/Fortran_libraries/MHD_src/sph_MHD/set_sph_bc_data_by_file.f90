!> @file  set_sph_bc_data_by_file.f90
!!      module set_sph_bc_data_by_file
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine set_fixed_vector_bc_by_file(field, sph_rj,           &
!!     &          bc_IO, ref_grp, iflag_bc_vector, bc_Vspec)
!!      subroutine set_evolved_vector_bc_by_file(field, sph_rj, bc_IO,  &
!!     &          ref_grp, iflag_bc_vector, bc_Vevo)
!!              type(boundary_spectra), intent(in) :: bc_IO
!!              type(sph_rj_grid), intent(in) :: sph_rj
!!              type(field_def), intent(in) :: field
!!
!!      subroutine set_fixed_scalar_bc_by_file(field, sph_rj, bc_IO,    &
!!     &          ref_grp, iflag_bc_scalar, bc_Sspec)
!!      subroutine set_evolved_scalar_bc_by_file(field, sph_rj, bc_IO,  &
!!     &          ref_grp, iflag_bc_scalar, bc_Sevo)
!!              type(boundary_spectra), intent(in) :: bc_IO
!!              type(sph_rj_grid), intent(in) :: sph_rj
!!              type(field_def), intent(in) :: field
!!
!!      subroutine set_fixed_grad_bc_by_file(flux, sph_rj, bc_IO,       &
!!     &          ref_grp, iflag_bc_scalar, bc_Sspec)
!!      subroutine set_evolved_grad_bc_by_file(flux, sph_rj, bc_IO,     &
!!     &          ref_grp, iflag_bc_scalar, bc_Sevo)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(field_def), intent(in) :: flux
!!        type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!!        type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!!        type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!!        type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!!@endverbatim
!
      module set_sph_bc_data_by_file
!
      use m_precision
      use t_spheric_rj_data
      use t_each_sph_boundary_IO_data
      use t_sph_boundary_input_data
      use t_field_labels
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_fixed_vector_bc_by_file(field, sph_rj,             &
     &          bc_IO, ref_grp, iflag_bc_vector, bc_Vspec)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(field_def), intent(in) :: field
!
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_vector
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_vector .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field%name)) then
            iflag_bc_vector =  iflag_fixed_field
            call set_bc_4_sph_vector_by_file(sph_rj, bc_IO%ctls(igrp),  &
     &          bc_Vspec%Vp_BC, bc_Vspec%Dp_BC, bc_Vspec%Vt_BC)
          end if
        end if
      end do
!
      end subroutine set_fixed_vector_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_evolved_vector_bc_by_file(field, sph_rj, bc_IO,    &
     &          ref_grp, iflag_bc_vector, bc_Vevo)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(field_def), intent(in) :: field
!
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_vector
      type(sph_vector_BC_evo), intent(inout) :: bc_Vevo
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_vector .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field%name)         &
     &       .or. find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          mag_label)) then
            iflag_bc_vector =  iflag_evolve_field
            call bc_4_evo_vector_sph_by_file(sph_rj, bc_IO%ctls(igrp),  &
     &         bc_Vevo%Vp_BC_mag, bc_Vevo%Dp_BC_mag, bc_Vevo%Vt_BC_mag)
          else if(find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          freq1_label)                            &
     &       .or. find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          freq2_label)) then
            iflag_bc_vector =  iflag_evolve_field
            call bc_4_evo_vect2_sph_by_file(sph_rj, bc_IO%ctls(igrp),   &
     &          bc_Vevo%Vp_BC_freq, bc_Vevo%Vt_BC_freq)
          else if(find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          phase_label)) then
            iflag_bc_vector =  iflag_evolve_field
            call bc_4_evo_vector_sph_by_file(sph_rj, bc_IO%ctls(igrp),  &
     &          bc_Vevo%Vp_BC_phase, bc_Vevo%Dp_BC_phase,               &
     &          bc_Vevo%Vt_BC_phase)
          end if
        end if
      end do
!
      end subroutine set_evolved_vector_bc_by_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fixed_scalar_bc_by_file(field, sph_rj, bc_IO,      &
     &          ref_grp, iflag_bc_scalar, bc_Sspec)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(field_def), intent(in) :: field
!
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field%name)) then
            iflag_bc_scalar =  iflag_fixed_field
            call set_bc_4_sph_scalar_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sspec%S_BC)
          end if
        end if
      end do
!
      end subroutine set_fixed_scalar_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_evolved_scalar_bc_by_file(field, sph_rj, bc_IO,    &
     &          ref_grp, iflag_bc_scalar, bc_Sevo)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(field_def), intent(in) :: field
!
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, field%name)         &
     &       .or. find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          mag_label)) then
            iflag_bc_scalar =  iflag_evolve_field
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_mag)
          else if(find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          freq1_label)                            &
     &       .or. find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          freq2_label)) then
            iflag_bc_scalar =  iflag_evolve_field
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_freq)
          else if(find_bc_label(field, bc_IO%ctls(igrp)%bc_field,       &
     &                          phase_label)) then
            iflag_bc_scalar =  iflag_evolve_field
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_phase)
          end if
        end if
      end do
!
      end subroutine set_evolved_scalar_bc_by_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fixed_grad_bc_by_file(flux, sph_rj, bc_IO,         &
     &          ref_grp, iflag_bc_scalar, bc_Sspec)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(field_def), intent(in) :: flux
!
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, flux%name)) then
            iflag_bc_scalar =  iflag_fixed_flux
            call set_bc_4_sph_scalar_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sspec%S_BC)
          end if
        end if
      end do
!
      end subroutine set_fixed_grad_bc_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_evolved_grad_bc_by_file(flux, sph_rj, bc_IO,       &
     &          ref_grp, iflag_bc_scalar, bc_Sevo)
!
      use skip_comment_f
      use set_sph_boundary_from_file
!
      type(boundary_spectra), intent(in) :: bc_IO
      type(sph_rj_grid), intent(in) :: sph_rj
      type(field_def), intent(in) :: flux
!
      character(len=kchara), intent(in) :: ref_grp
!
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
      type(sph_scalar_BC_evo), intent(inout) :: bc_Sevo
!
      integer(kind = kint) :: igrp
!
!
      if(iflag_bc_scalar .ne. iflag_undefined_bc) return
      do igrp = 1, bc_IO%num_bc_fld
        if(cmp_no_case(bc_IO%ctls(igrp)%bc_group, ref_grp)) then
          if(cmp_no_case(bc_IO%ctls(igrp)%bc_field, flux%name)          &
     &       .or. find_bc_label(flux, bc_IO%ctls(igrp)%bc_field,        &
     &                          mag_label)) then
            iflag_bc_scalar =  iflag_evolve_flux
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_mag)
          else if(find_bc_label(flux, bc_IO%ctls(igrp)%bc_field,        &
     &                          freq1_label)                            &
     &       .or. find_bc_label(flux, bc_IO%ctls(igrp)%bc_field,        &
     &                          freq2_label)) then
            iflag_bc_scalar =  iflag_evolve_flux
            call bc_4_evo_scalar_sph_by_file                            &
     &         (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_freq)
          else if(find_bc_label(flux, bc_IO%ctls(igrp)%bc_field,        &
     &                          phase_label)) then
            iflag_bc_scalar =  iflag_evolve_flux
            call bc_4_evo_scalar_sph_by_file                            &
     &           (sph_rj, bc_IO%ctls(igrp), BC_Sevo%S_BC_phase)
          end if
        end if
      end do
!
      end subroutine set_evolved_grad_bc_by_file
!
! -----------------------------------------------------------------------
!
      end module set_sph_bc_data_by_file
