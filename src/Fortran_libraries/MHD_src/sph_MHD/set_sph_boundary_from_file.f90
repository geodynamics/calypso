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
!!      logical function find_bc_label(field, label, postfix)
!!        type(field_def), intent(in) :: field
!!@endverbatim
!
      module set_sph_boundary_from_file
!
      use m_precision
      use t_spheric_rj_data
      use t_field_labels
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
      use m_base_field_labels
      use m_base_force_labels
      use skip_comment_f
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: num_comp = 0
!
      if(      cmp_no_case(label, velocity%name)                        &
     &    .or. cmp_no_case(label, vorticity%name)                       &
     &    .or. cmp_no_case(label, magnetic_field%name)) num_comp = 3
!
      if(      cmp_no_case(label, temperature%name)                     &
     &    .or. cmp_no_case(label, composition%name)                     &
     &    .or. cmp_no_case(label, entropy%name)                         &
     &    .or. cmp_no_case(label, heat_flux%name)                       &
     &    .or. cmp_no_case(label, composite_flux%name)) num_comp = 1
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
      use m_base_field_labels
      use m_base_force_labels
!
      character(len = kchara), intent(in) :: label, postfix
!
      find_scalar_bc_label                                              &
     &     =    find_bc_label(temperature,    label, postfix)           &
     &     .or. find_bc_label(composition,    label, postfix)           &
     &     .or. find_bc_label(entropy,        label, postfix)           &
     &     .or. find_bc_label(heat_flux,      label, postfix)           &
     &     .or. find_bc_label(composite_flux, label, postfix)
!
      end function find_scalar_bc_label
!
! -----------------------------------------------------------------------
!
      logical function find_vector_bc_label(label, postfix)
!
      use m_base_field_labels
!
      character(len = kchara), intent(in) :: label, postfix
!
      find_vector_bc_label                                              &
     &     =    find_bc_label(velocity,       label, postfix)           &
     &     .or. find_bc_label(vorticity,      label, postfix)           &
     &     .or. find_bc_label(magnetic_field, label, postfix)
!
      end function find_vector_bc_label
!
! -----------------------------------------------------------------------
!
      logical function find_bc_label(field, label, postfix)
!
      use skip_comment_f
!
      type(field_def), intent(in) :: field
      character(len = kchara), intent(in) :: label, postfix
!
      character(len = kchara) :: tmpchara
!
      write(tmpchara,'(a,a)') trim(field%name), trim(postfix)
      find_bc_label = cmp_no_case(label, tmpchara)
!
      end function find_bc_label
!
! -----------------------------------------------------------------------
!
      end module set_sph_boundary_from_file
