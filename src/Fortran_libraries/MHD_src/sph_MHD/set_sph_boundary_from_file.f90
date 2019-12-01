!> @file  set_sph_boundary_from_file.f90
!!      module set_sph_boundary_from_file
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      integer(kind = kint) function num_comp_bc_data(label)
!!      logical function find_bc_label(label, field_name, postfix)
!!@endverbatim
!
      module set_sph_boundary_from_file
!
      use m_precision
      use m_phys_labels
      use t_spheric_rj_data
!
      implicit  none
!
!>      Postfix for magnetide
      character(len=kchara), parameter :: mag_label =   '_magnitude'
!>      Postfix for frequency
      character(len=kchara), parameter :: freq1_label = '_freq'
!>      Postfix for frequency
      character(len=kchara), parameter :: freq2_label = '_freqency'
!>      Postfix for initial phase
      character(len=kchara), parameter :: phase_label = '_phase'
!
!
      private :: find_scalar_bc_label, find_vector_bc_label
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function num_comp_bc_data(label)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: num_comp = 0
!
      if(      cmp_no_case(label, fhd_velo)                             &
     &    .or. cmp_no_case(label, fhd_vort)                             &
     &    .or. cmp_no_case(label, fhd_magne)) num_comp = 3
!
      if(      cmp_no_case(label, fhd_temp)                             &
     &    .or. cmp_no_case(label, fhd_light)                            &
     &    .or. cmp_no_case(label, fhd_entropy)                          &
     &    .or. cmp_no_case(label, fhd_h_flux)                           &
     &    .or. cmp_no_case(label, fhd_c_flux)) num_comp = 1
!
      if(find_vector_bc_label(label, mag_label))   num_comp = 3
      if(find_vector_bc_label(label, phase_label)) num_comp = 3
      if(find_vector_bc_label(label, freq1_label)) num_comp = 2
      if(find_vector_bc_label(label, freq2_label)) num_comp = 2
!
      if(find_scalar_bc_label(label, mag_label))   num_comp = 1
      if(find_scalar_bc_label(label, phase_label)) num_comp = 1
      if(find_scalar_bc_label(label, freq1_label)) num_comp = 1
      if(find_scalar_bc_label(label, freq2_label)) num_comp = 1
!
       num_comp_bc_data = num_comp
!
      end function num_comp_bc_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function find_scalar_bc_label(label, postfix)
!
      character(len = kchara), intent(in) :: label, postfix
!
      find_scalar_bc_label = find_bc_label(label, fhd_temp, postfix)    &
     &                  .or. find_bc_label(label, fhd_light, postfix)   &
     &                  .or. find_bc_label(label, fhd_entropy, postfix) &
     &                  .or. find_bc_label(label, fhd_h_flux, postfix)  &
     &                  .or. find_bc_label(label, fhd_c_flux, postfix)
!
      end function find_scalar_bc_label
!
! -----------------------------------------------------------------------
!
      logical function find_vector_bc_label(label, postfix)
!
      character(len = kchara), intent(in) :: label, postfix
!
      find_vector_bc_label  = find_bc_label(label, fhd_velo, postfix)   &
     &                   .or. find_bc_label(label, fhd_vort, postfix)   &
     &                   .or. find_bc_label(label, fhd_magne, postfix)
!
      end function find_vector_bc_label
!
! -----------------------------------------------------------------------
!
      logical function find_bc_label(label, field_name, postfix)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: label, field_name, postfix
!
      character(len = kchara) :: tmpchara
!
      write(tmpchara,'(a,a)') trim(field_name), trim(postfix)
      find_bc_label = cmp_no_case(label, tmpchara)
!
      end function find_bc_label
!
! -----------------------------------------------------------------------
!
      end module set_sph_boundary_from_file
