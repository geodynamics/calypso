!>@file   t_field_product_labels.f90
!!        module t_field_product_labels
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
!!      subroutine set_field_product_addresses                          &
!!     &         (i_phys, field_name, prod_fld, flag)
!!        type(phys_products_address), intent(inout) :: prod_fld
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
!!   declination              [i_declination]:
!!   inclination              [i_inclination]:
!!   vgp_latitude             [i_vgp_latitude]:
!!   vgp_longigude            [i_vgp_longigude]:
!!
!!   magnetic_dipole          [i_dipole_B]
!!   current_for_dipole       [i_dipole_J]
!!
!!   Lorentz_force_dipole     [i_dipole_Lorentz]:
!!   Lorentz_work_dipole      [i_dipole_ujb]:
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_field_product_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!!
!>       Structure for start address for fields
      type phys_products_address
!>        Field address for ageostrophic balance
!!         @f$ -2 e_{ijk} \Omega_{j} u_{k} - \partial_{i} p @f$
        integer (kind=kint) :: i_geostrophic = izero
!>        Start address for truncated magnetic field
!!         @f$ \bar{B}_{i} @f$
        integer (kind=kint) :: i_truncated_B =    izero
!
!>        start address for electric field
!!         @f$ E_{i} @f$
        integer (kind=kint) :: i_electric =        izero
!>        start address for magnetic potential
!>        start address for poynting flux
!!         @f$  e_{ijk} E_{j} B_{k} @f$
        integer (kind=kint) :: i_poynting =        izero
!
!  Helicities
!>        start address for kinetic helicity
!!         @f$ H_{u} = u_{i} \omega_{i} @f$
        integer (kind=kint) :: i_k_heli =          izero
!>        start address for magnetic helicity
!!         @f$ H_{B} = B_{i} A_{i} @f$
        integer (kind=kint) :: i_m_heli =          izero
!>        start address for current helicity
!!         @f$ H_{J} = J_{i} B_{i} @f$
        integer (kind=kint) :: i_c_heli =          izero
!>        start address for cross helicity
!!         @f$ H_{x} = u_{i} B{i} @f$
        integer (kind=kint) :: i_x_heli =          izero
!
!  Square of each component of fields
!
!>        Square of velocity @f$ u_{i}^{2} @f$
        integer (kind=kint) :: i_square_v = izero
!>        Square of vorticity @f$ \omega_{i}^{2} @f$
        integer (kind=kint) :: i_square_w = izero
!>        Square of magnetic field @f$ B_{i}^{2} @f$
        integer (kind=kint) :: i_square_b = izero
!>        Square of magnetic vector potential @f$ A_{i}^{2} @f$
        integer (kind=kint) :: i_square_a = izero
!>        Square of current density @f$ J_{i}^{2} @f$
        integer (kind=kint) :: i_square_j = izero
!>        Square of temperature @f$ T^{2} @f$
        integer (kind=kint) :: i_square_t = izero
!>        Square of composition @f$ C^{2} @f$
        integer (kind=kint) :: i_square_c = izero
!
!>        start address for velocity length scale
        integer (kind=kint) :: i_velo_scale =       izero
!>        start address for magnetic field length scale
        integer (kind=kint) :: i_magne_scale =      izero
!>        start address for temperature length scale
        integer (kind=kint) :: i_temp_scale =       izero
!>        start address for composition length scale
        integer (kind=kint) :: i_comp_scale =       izero
!
!>        start address for stream function for poloidal velocity
        integer (kind=kint) :: i_stream_pol_u =     izero
!>        start address for stream function for poloidal magnetic field
        integer (kind=kint) :: i_stream_pol_b =     izero
!
!>        start address for Total magnetic field intensity  @f$ B @f$
        integer (kind=kint) :: i_magnetic_intensity = izero
!>        start address for Magnetic field declination  @f$ B @f$
        integer (kind=kint) :: i_declination =        izero
!>        start address for Magnetic field inclination  @f$ B @f$
        integer (kind=kint) :: i_inclination =        izero
!
!>        start address for VGP latitude  @f$ \pi/2 - \theta_{VGP} @f$
        integer (kind=kint) :: i_vgp_latitude =       izero
!>        start address for VGP longitude  @f$ \phi_{VGP} @f$
        integer (kind=kint) :: i_vgp_longigude =      izero
!
!
!>        start address for dipole magnetic field @f$ B_{S1}^{0} @f$
        integer (kind=kint) :: i_dipole_B =            izero
!>        start address for toroidal dipole current @f$ J_{T1}^{0} @f$
        integer (kind=kint) :: i_dipole_J =            izero
!
!>         start address force by dipole component
!!             @f$ J_{1}^{0} \times B @f$
        integer (kind=kint) :: i_dipole_Lorentz =      izero
!>          start address of Work of Lorentz force by dipole component
!!             @f$ u \cdot (J_{1}^{0} \times B) @f$
        integer (kind=kint) :: i_dipole_ujb =          izero
      end type phys_products_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_field_product_addresses                            &
     &         (i_phys, field_name, prod_fld, flag)
!
      use m_field_product_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(phys_products_address), intent(inout) :: prod_fld
      logical, intent(inout) :: flag
!
!
      flag =     check_field_product_vectors(field_name)                &
     &      .or. check_field_product_scalars(field_name)
      if(flag) then
        if (field_name .eq. rest_of_geostrophic%name) then
          prod_fld%i_geostrophic = i_phys
        else if (field_name .eq. truncated_magnetic_field%name) then
          prod_fld%i_truncated_B = i_phys
        else if (field_name .eq. electric_field%name) then
          prod_fld%i_electric =    i_phys
        else if (field_name .eq. poynting_flux%name) then
          prod_fld%i_poynting =    i_phys
!
        else if (field_name .eq. kinetic_helicity%name) then
          prod_fld%i_k_heli =     i_phys
        else if (field_name .eq. magnetic_helicity%name) then
          prod_fld%i_m_heli =     i_phys
        else if (field_name .eq. current_helicity%name) then
          prod_fld%i_c_heli =     i_phys
        else if (field_name .eq. cross_helicity%name) then
          prod_fld%i_x_heli =     i_phys
!
        else if (field_name .eq. square_velocity%name) then
          prod_fld%i_square_v =  i_phys
        else if (field_name .eq. square_vorticity%name) then
          prod_fld%i_square_w =  i_phys
        else if (field_name .eq. square_magne%name) then
          prod_fld%i_square_b =  i_phys
        else if (field_name .eq. square_vector_potential%name) then
          prod_fld%i_square_a =  i_phys
        else if (field_name .eq. square_current%name) then
          prod_fld%i_square_j =  i_phys
        else if (field_name .eq. square_temperature%name) then
          prod_fld%i_square_t =  i_phys
        else if (field_name .eq. square_composition%name) then
          prod_fld%i_square_c =  i_phys
!
        else if (field_name .eq. velocity_scale%name) then
          prod_fld%i_velo_scale = i_phys
        else if (field_name .eq. magnetic_scale%name) then
          prod_fld%i_magne_scale = i_phys
        else if (field_name .eq. temperature_scale%name) then
          prod_fld%i_temp_scale = i_phys
        else if (field_name .eq. composition_scale%name) then
          prod_fld%i_comp_scale = i_phys
!
        else if (field_name .eq. stream_pol_velocity%name) then
          prod_fld%i_stream_pol_u = i_phys
        else if (field_name .eq. stream_pol_magne%name) then
          prod_fld%i_stream_pol_b = i_phys
!
        else if (field_name .eq. magnetic_intensity%name) then
          prod_fld%i_magnetic_intensity = i_phys
        else if (field_name .eq. declination%name) then
          prod_fld%i_declination = i_phys
        else if (field_name .eq. inclination%name) then
          prod_fld%i_inclination = i_phys
!
        else if (field_name .eq. vgp_latitude%name) then
          prod_fld%i_vgp_latitude = i_phys
        else if (field_name .eq. vgp_longigude%name) then
          prod_fld%i_vgp_longigude = i_phys
!
!
        else if (field_name .eq. magnetic_dipole%name) then
          prod_fld%i_dipole_B =    i_phys
        else if (field_name .eq. current_for_dipole%name) then
          prod_fld%i_dipole_J =    i_phys
!
        else if (field_name .eq. Lorentz_force_dipole%name) then
          prod_fld%i_dipole_Lorentz = i_phys
        else if (field_name .eq. Lorentz_work_dipole%name) then
          prod_fld%i_dipole_ujb =     i_phys
        end if
      end if
!
      end subroutine set_field_product_addresses
!
! ----------------------------------------------------------------------
!
      end module t_field_product_labels
