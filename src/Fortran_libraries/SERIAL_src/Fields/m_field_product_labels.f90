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
!!      integer(kind = kint) function num_field_products()
!!      subroutine set_field_product_labels(n_comps, names, maths)
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
      integer(kind = kint), parameter, private :: nfid_product = 26
!
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
      integer(kind = kint) function num_field_products()
      num_field_products = nfid_product
      return
      end function num_field_products
!
! ----------------------------------------------------------------------
!
      subroutine set_field_product_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nfid_product)
      character(len = kchara), intent(inout) :: names(nfid_product)
      character(len = kchara), intent(inout) :: maths(nfid_product)
!
!
      call set_field_labels(rest_of_geostrophic,                        &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(truncated_magnetic_field,                   &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(electric_field,                             &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(poynting_flux,                              &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(kinetic_helicity,                           &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(magnetic_helicity,                          &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(current_helicity,                           &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(cross_helicity,                             &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(square_velocity,                            &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(square_vorticity,                           &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(square_magne,                               &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(square_vector_potential,                    &
     &    n_comps(12), names(12), maths(12))
      call set_field_labels(square_current,                             &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(square_temperature,                         &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(square_composition,                         &
     &    n_comps(15), names(15), maths(15))
!
      call set_field_labels(velocity_scale,                             &
     &    n_comps(16), names(16), maths(16))
      call set_field_labels(magnetic_scale,                             &
     &    n_comps(17), names(17), maths(17))
      call set_field_labels(temperature_scale,                          &
     &    n_comps(18), names(18), maths(18))
      call set_field_labels(composition_scale,                          &
     &    n_comps(19), names(19), maths(19))
!
      call set_field_labels(stream_pol_velocity,                        &
     &    n_comps(20), names(20), maths(20))
      call set_field_labels(stream_pol_magne,                           &
     &    n_comps(21), names(21), maths(21))
!
      call set_field_labels(magnetic_intensity,                         &
     &    n_comps(22), names(22), maths(22))
      call set_field_labels(declination,                                &
     &    n_comps(23), names(23), maths(23))
      call set_field_labels(inclination,                                &
     &    n_comps(24), names(24), maths(24))
      call set_field_labels(vgp_latitude,                               &
     &    n_comps(25), names(25), maths(25))
      call set_field_labels(vgp_longigude,                              &
     &    n_comps(26), names(26), maths(26))
!
      end subroutine set_field_product_labels
!
! ----------------------------------------------------------------------
!
      end module m_field_product_labels
