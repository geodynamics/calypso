!>@file   t_field_component_labels.f90
!!        module t_field_component_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Addresses for basic fields
!!
!!@verbatim
!!      subroutine set_field_component_addresses                        &
!!     &         (i_phys, field_name, fld_cmp, flag)
!!        type(field_component_address), intent(inout) :: fld_cmp
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   r_velocity                [i_velo_r]:
!!   theta_velocity            [i_velo_t]:
!!   phi_velocity              [i_velo_p]:
!!   cyl_r_velocity            [i_velo_s]:
!!   x_velocity                [i_velo_x]:
!!   y_velocity                [i_velo_y]:
!!   z_velocity                [i_velo_z]:
!!
!!   r_magnetic_f              [i_magne_r]:
!!   theta_magnetic_f          [i_magne_t]:
!!   phi_magnetic_f            [i_magne_p]:
!!   cyl_r_magnetic_f          [i_magne_s]:
!!   x_magnetic_f              [i_magne_x]:
!!   y_magnetic_f              [i_magne_y]:
!!   z_magnetic_f              [i_magne_z]:
!!
!!   temperature_from_CMB     [i_temp_from_CMB]
!!   composition_from_CMB     [i_light_from_CMB]
!!   entropy_from_CMB         [i_entropy_from_CMB]
!!   density_from_CMB         [i_density_from_CMB]
!!   aspherical_pressure      [i_asph_pressure]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_field_component_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!
!>       Structure for start address for field components
      type field_component_address
!>        Start address for radial velocity @f$ u_{r} @f$
        integer (kind=kint) :: i_velo_r  =           izero
!>        Start address for theta component of velocity
!!         @f$ u_{\theta} @f$
        integer (kind=kint) :: i_velo_t  =           izero
!>        Start address for phi-component of  velocity @f$ u_{\phi} @f$
        integer (kind=kint) :: i_velo_p  =           izero
!>        Start address for cylindrical radial velocity @f$ u_{s} @f$
        integer (kind=kint) :: i_velo_s  =           izero
!>        Start address for x-componennt of velocity @f$ u_{z} @f$
        integer (kind=kint) :: i_velo_x  =           izero
!>        Start address for y-componennt of velocity @f$ u_{z} @f$
        integer (kind=kint) :: i_velo_y  =           izero
!>        Start address for z-componennt of velocity @f$ u_{z} @f$
        integer (kind=kint) :: i_velo_z  =           izero
!
!>        Start address for radial magnetic field @f$ B_{r} @f$
        integer (kind=kint) :: i_magne_r  =          izero
!>        Start address for theta component of magnetic field
!!         @f$ B_{\theta} @f$
        integer (kind=kint) :: i_magne_t  =          izero
!>        Start address for phi-component of  magnetic field 
!!         @f$ B_{\phi} @f$
        integer (kind=kint) :: i_magne_p  =          izero
!>        Start address for cylindrical radial magnetic field
!!          @f$ B_{s} @f$
        integer (kind=kint) :: i_magne_s  =          izero
!>        Start address for x-component of magnetic field
!!          @f$ B_{x} @f$
        integer (kind=kint) :: i_magne_x  =          izero
!>        Start address for x-component of magnetic field
!!          @f$ B_{y} @f$
        integer (kind=kint) :: i_magne_y  =          izero
!>        Start address for x-component of magnetic field
!!          @f$ B_{z} @f$
        integer (kind=kint) :: i_magne_z  =          izero
!
!>        start address for temperature from CMB average
!!         @f$ T-|T(r_{o})| @f$
        integer (kind=kint) :: i_temp_from_CMB =      izero
!>        start address for light element from CMB average
!!         @f$ C-|C(r_{o})| @f$
        integer (kind=kint) :: i_light_from_CMB =      izero
!>        start address for entropy from CMB average
!!         @f$ S-|S(r_{o})| @f$
        integer (kind=kint) :: i_entropy_from_CMB =    izero
!>        start address for density from CMB average
!!         @f$ \rho-|\rho(r_{o})| @f$
        integer (kind=kint) :: i_density_from_CMB =    izero
!>        start address for aspherical component of pressure
!!         @f$ P-|\int P dS| @f$
        integer (kind=kint) :: i_asph_pressure =       izero
      end type field_component_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_field_component_addresses                          &
     &         (i_phys, field_name, fld_cmp, flag)
!
      use m_field_component_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(field_component_address), intent(inout) :: fld_cmp
      logical, intent(inout) :: flag
!
!
      flag = check_field_comp_list(field_name)
      if(flag) then
        if (field_name .eq. r_velocity%name) then
          fld_cmp%i_velo_r = i_phys
        else if (field_name .eq. theta_velocity%name) then
          fld_cmp%i_velo_t =    i_phys
        else if (field_name .eq. phi_velocity%name) then
          fld_cmp%i_velo_p = i_phys
        else if (field_name .eq. cyl_r_velocity%name) then
          fld_cmp%i_velo_s = i_phys
        else if (field_name .eq. x_velocity%name) then
          fld_cmp%i_velo_x = i_phys
        else if (field_name .eq. y_velocity%name) then
          fld_cmp%i_velo_y = i_phys
        else if (field_name .eq. z_velocity%name) then
          fld_cmp%i_velo_z = i_phys
!
        else if (field_name .eq. r_magnetic_f%name) then
          fld_cmp%i_magne_r =      i_phys
        else if (field_name .eq. theta_magnetic_f%name) then
          fld_cmp%i_magne_t =      i_phys
        else if (field_name .eq. phi_magnetic_f%name) then
          fld_cmp%i_magne_p =      i_phys
        else if (field_name .eq. cyl_r_magnetic_f%name) then
          fld_cmp%i_magne_s =      i_phys
        else if (field_name .eq. x_magnetic_f%name) then
          fld_cmp%i_magne_x =      i_phys
        else if (field_name .eq. y_magnetic_f%name) then
          fld_cmp%i_magne_y =      i_phys
        else if (field_name .eq. z_magnetic_f%name) then
          fld_cmp%i_magne_z =      i_phys
!
        else if (field_name .eq. temperature_from_CMB%name) then
          fld_cmp%i_temp_from_CMB =    i_phys
        else if (field_name .eq. composition_from_CMB%name) then
          fld_cmp%i_light_from_CMB =   i_phys
        else if (field_name .eq. entropy_from_CMB%name) then
          fld_cmp%i_entropy_from_CMB = i_phys
        else if (field_name .eq. density_from_CMB%name) then
          fld_cmp%i_density_from_CMB = i_phys
!
        else if (field_name .eq. aspherical_pressure%name) then
          fld_cmp%i_asph_pressure =    i_phys
        end if
      end if  
!
      end subroutine set_field_component_addresses
!
! ----------------------------------------------------------------------
!
      end module t_field_component_labels
