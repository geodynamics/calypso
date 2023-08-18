!>@file   m_field_product_labels.f90
!!        module m_field_product_labels
!!
!!@author H. Matsui (UC Davis)
!!@n      and T. Kera (Tohoku University)
!!
!!@date   Programmed in Jan., 2020
!!@n      Modified in July, 2021
!!
!> @brief Labels and addresses for products of forces
!!
!!@verbatim
!!      logical function check_field_product_vectors(field_name)
!!      logical function check_field_product_scalars(field_name)
!!
!!      subroutine set_field_product_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  product of fields names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   rest_of_geostrophic      [i_geostrophic]:
!!   truncated_magnetic_field [i_truncated_B]:
!!   electric_field           [i_electric]:
!!   poynting_flux            [i_poynting]:
!!
!!   kinetic_helicity         [i_k_heli]:
!!   magnetic_helicity        [i_m_heli]:
!!   current_helicity         [i_c_heli]:
!!   cross_helicity           [i_x_heli]:
!!
!!   square_velocity          [i_square_v]:
!!   square_vorticity         [i_square_w]:
!!   square_magne             [i_square_b]:
!!   square_vector_potential  [i_square_a]:
!!   square_current           [i_square_j]:
!!   square_temperature       [i_square_t]:
!!   square_composition       [i_square_c]:
!!
!!   velocity_scale           [i_velo_scale]:
!!   magnetic_scale           [i_magne_scale]:
!!   temperature_scale        [i_temp_scale]:
!!   composition_scale        [i_comp_scale]:
!!
!!   stream_pol_velocity      [i_stream_pol_u]:
!!   stream_pol_magne         [i_stream_pol_b]:
!!
!!   magnetic_intensity       [i_magnetic_intensity]:
!!   inclination              [i_inclination]:
!!   declination              [i_declination]:
!!   vgp_latitude             [i_vgp_latitude]:
!!   vgp_longigude            [i_vgp_llongigude]:
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_field_product_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!>        Field label for ageostrophic balance
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} - \partial_{i} p @f$
      type(field_def), parameter :: rest_of_geostrophic                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rest_of_geostrophic',                     &
     &                math = '$ -2 e_{ijk} \Omega_{j} u_{k}'            &
     &                     // ' - \partial_{i} p $')
!
!>        Field label for truncated magnetic field
!!         @f$ B_{i} @f$
      type(field_def), parameter :: truncated_magnetic_field            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'truncated_magnetic_field',                &
     &                math = '$ B_{i} $')
!>        Field label for electric field
!!         @f$ E_{i} @f$
      type(field_def), parameter :: electric_field                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'electric_field',                          &
     &                math = '$ E_{i} $')
!>        Field label for poynting flux
!!         @f$  e_{ijk} E_{j} B_{k} @f$
      type(field_def), parameter :: poynting_flux                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'poynting_flux',                           &
     &                math = '$ e_{ijk} E_{j} B_{k} $')
!
!
!>        Field label for kinetic helicity
!!         @f$ H_{u} = u_{i} \omega_{i} @f$
      type(field_def), parameter :: kinetic_helicity                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'kinetic_helicity',                        &
     &                math = '$ u_{i} \omega_{i} $')
!>        Field label for magnetic helicity
!!         @f$ H_{B} = B_{i} A_{i} @f$
      type(field_def), parameter :: magnetic_helicity                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_helicity',                       &
     &                math = '$ B_{i} A_{i} $')
!>        Field label for current helicity
!!         @f$ H_{J} = J_{i} B_{i} @f$
      type(field_def), parameter :: current_helicity                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'current_helicity',                        &
     &                math = '$ J_{i} B_{i} $')
!>        Field label for cross helicity
!!         @f$ H_{x} = u_{i} B{i} @f$
      type(field_def), parameter :: cross_helicity                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'cross_helicity',                          &
     &                math = '$ u_{i} B_{i} $')
!
!  Square of each component of fields
!
!>        Square of velocity @f$ u_{i}^{2} @f$
      type(field_def), parameter :: square_velocity                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'square_velocity',                         &
     &                math = '$ u_{i}^{2} $')
!>        Square of vorticity @f$ \omega_{i}^{2} @f$
      type(field_def), parameter :: square_vorticity                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'square_vorticity',                        &
     &                math = '$ \omega_{i}^{2} $')
!>        Square of magnetic field @f$ B_{i}^{2} @f$
      type(field_def), parameter :: square_magne                        &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'square_magne',                            &
     &                math = '$ B_{i}^{2} $')
!>        Square of magnetic vector potential @f$ A_{i}^{2} @f$
      type(field_def), parameter :: square_vector_potential             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'square_vector_potential',                 &
     &                math = '$ A_{i}^{2} $')
!>        Square of current density @f$ J_{i}^{2} @f$
      type(field_def), parameter :: square_current                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'square_current',                          &
     &                math = '$ J_{i}^{2} $')
!
!>        Square of temperature @f$ T^{2} @f$
      type(field_def), parameter :: square_temperature                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'square_temperature',                      &
     &                math = '$ T^{2} $')
!>        Square of composition @f$ C^{2} @f$
      type(field_def), parameter :: square_composition                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'square_composition',                      &
     &                math = '$ C^{2} $')
!
!
!>        Field label for velocity length scale  @f$ L_{u} @f$
      type(field_def), parameter :: velocity_scale                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'velocity_scale',                          &
     &                math = '$ L_{u} $')
!>        Field label for magnetic field length scale  @f$ L_{B} @f$
      type(field_def), parameter :: magnetic_scale                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_scale',                          &
     &                math = '$ L_{B} $')
!>        Field label for temperature length scale  @f$ L_{T} @f$
      type(field_def), parameter :: temperature_scale                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temperature_scale',                       &
     &                math = '$ L_{T} $')
!>        Field label for composition length scale @f$ L_{C} @f$
      type(field_def), parameter :: composition_scale                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'composition_scale',                       &
     &                math = '$ L_{C} $')
!
!
!>        Stream functin for the poloidal velocity @f$ \Psi_{Us} @f$
      type(field_def), parameter :: stream_pol_velocity                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'stream_pol_velocity',                     &
     &                math = '$ \Psi_{Us} $')
!
!>        Stream functin for the poloidal magnetic field @f$ \Phi_{Bs} @f$
      type(field_def), parameter :: stream_pol_magne                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'stream_pol_magne',                        &
     &                math = '$ \Phi_{Bs} $')
!
!
!>      Total magnetic field intensity  @f$ B @f$
      type(field_def), parameter :: magnetic_intensity                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magnetic_intensity',                      &
     &                math = '$ B $')
!>      Magnetic field declination  @f$ B @f$
      type(field_def), parameter :: declination                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'declination',                             &
     &                math = '$ B_{Dec} $')
!>      Magnetic field inclination  @f$ B @f$
      type(field_def), parameter :: inclination                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'inclination',                             &
     &                math = '$ B_{Inc} $')
!
!>      VGP latitude  @f$ \pi/2 - \theta_{VGP} @f$
      type(field_def), parameter :: vgp_latitude                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'vgp_latitude',                            &
     &                math = '$ VGP_{lat} $')
!>      VGP longitude  @f$ \phi_{VGP} @f$
      type(field_def), parameter :: vgp_longigude                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'vgp_longigude',                           &
     &                math = '$ VGP_{\phi} $')
!
!    ----------   Ole definision  ------------------
!>        Field label for ageostrophic balance
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} - \partial_{i} p @f$
      type(field_def), parameter :: geostrophic_balance                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'geostrophic_balance',                     &
     &                math = '$ -2 e_{ijk} \Omega_{j} u_{k}'            &
     &                     // ' - \partial_{i} p $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_field_product_vectors(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_field_product_vectors = .FALSE.
      if (    (field_name .eq. rest_of_geostrophic%name)                &
     &   .or. (field_name .eq. truncated_magnetic_field%name)           &
     &   .or. (field_name .eq. electric_field%name)                     &
     &   .or. (field_name .eq. poynting_flux%name)                      &
!
     &   .or. (field_name .eq. square_velocity%name)                    &
     &   .or. (field_name .eq. square_vorticity%name)                   &
     &   .or. (field_name .eq. square_magne%name)                       &
     &   .or. (field_name .eq. square_vector_potential%name)            &
     &   .or. (field_name .eq. square_current%name)                     &
!
     &   .or. (field_name .eq. stream_pol_velocity%name)                &
     &   .or. (field_name .eq. stream_pol_magne%name)                   &
     &      )   check_field_product_vectors = .TRUE.
!
      end function check_field_product_vectors
!
! ----------------------------------------------------------------------
!
      logical function check_field_product_scalars(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_field_product_scalars = .FALSE.
      if (    (field_name .eq. kinetic_helicity%name)                   &
     &   .or. (field_name .eq. magnetic_helicity%name)                  &
     &   .or. (field_name .eq. current_helicity%name)                   &
     &   .or. (field_name .eq. cross_helicity%name)                     &
!
     &   .or. (field_name .eq. square_temperature%name)                 &
     &   .or. (field_name .eq. square_composition%name)                 &
!
     &   .or. (field_name .eq. velocity_scale%name)                     &
     &   .or. (field_name .eq. magnetic_scale%name)                     &
     &   .or. (field_name .eq. temperature_scale%name)                  &
     &   .or. (field_name .eq. composition_scale%name)                  &
!
     &   .or. (field_name .eq. magnetic_intensity%name)                 &
     &   .or. (field_name .eq. declination%name)                        &
     &   .or. (field_name .eq. inclination%name)                        &
     &   .or. (field_name .eq. vgp_latitude%name)                       &
     &   .or. (field_name .eq. vgp_longigude%name)                      &
     &      )   check_field_product_scalars = .TRUE.
!
      end function check_field_product_scalars
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_field_product_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(rest_of_geostrophic,      array_c2i)
      call set_field_label_to_ctl(truncated_magnetic_field, array_c2i)
      call set_field_label_to_ctl(electric_field,           array_c2i)
      call set_field_label_to_ctl(poynting_flux,            array_c2i)
      call set_field_label_to_ctl(kinetic_helicity,         array_c2i)
      call set_field_label_to_ctl(magnetic_helicity,        array_c2i)
      call set_field_label_to_ctl(current_helicity,         array_c2i)
      call set_field_label_to_ctl(cross_helicity,           array_c2i)
      call set_field_label_to_ctl(square_velocity,          array_c2i)
      call set_field_label_to_ctl(square_vorticity,         array_c2i)
      call set_field_label_to_ctl(square_magne,             array_c2i)
      call set_field_label_to_ctl(square_vector_potential,  array_c2i)
      call set_field_label_to_ctl(square_current,           array_c2i)
      call set_field_label_to_ctl(square_temperature,       array_c2i)
      call set_field_label_to_ctl(square_composition,       array_c2i)
      call set_field_label_to_ctl(velocity_scale,           array_c2i)
      call set_field_label_to_ctl(magnetic_scale,           array_c2i)
      call set_field_label_to_ctl(temperature_scale,        array_c2i)
      call set_field_label_to_ctl(composition_scale,        array_c2i)
      call set_field_label_to_ctl(stream_pol_velocity,      array_c2i)
      call set_field_label_to_ctl(stream_pol_magne,         array_c2i)
      call set_field_label_to_ctl(magnetic_intensity,       array_c2i)
      call set_field_label_to_ctl(declination,              array_c2i)
      call set_field_label_to_ctl(inclination,              array_c2i)
      call set_field_label_to_ctl(vgp_latitude,             array_c2i)
      call set_field_label_to_ctl(vgp_longigude,            array_c2i)
!
      end subroutine set_field_product_names
!
! ----------------------------------------------------------------------
!
      end module m_field_product_labels
