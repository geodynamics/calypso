!>@file   t_explicit_term_labels.f90
!!        module t_explicit_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      subroutine set_work_field_addresses                             &
!!     &         (i_phys, field_name, exp_work, flag)
!!        type(explicit_term_address), intent(inout) :: exp_work
!!      subroutine set_check_field_addresses                            &
!!     &         (i_phys, field_name, check_fld1, check_fld2, flag)
!!        type(explicit_term_address), intent(inout) :: check_fld1
!!        type(explicit_term_address), intent(inout) :: check_fld2
!!
!! !!!!!  force include SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!      Explicit terms at previous time step
!!    sum_forces       [exp_work%i_forces]:     Total force
!!    rot_sum_forces   [exp_work%i_rot_forces]: Rotation of toatl force
!!    div_sum_forces   [exp_work%i_div_forces]: Divergence of total force
!!
!!      Explicit terms at previous time step
!!    previous_momentum    [exp_work%i_pre_mom]
!!    previous_induction   [exp_work%i_pre_uxb]
!!    previous_heat        [exp_work%i_pre_heat]
!!    previous_composition [exp_work%i_pre_composit]
!!    previous_pressure    [exp_work%i_pre_press]
!!    previous_potential   [exp_work%i_pre_phi]
!!
!!
!!    pressure_work     [i_p_phi]: work for pressure Poisson equation
!!    m_potential_work  [i_m_phi]: work for potential Poisson equation
!!
!!      Check of explicit terms
!!    check_momentum       [check_fld1%i_pre_mom]
!!    check_induction      [check_fld1%i_pre_uxb]
!!    check_heat           [check_fld1%i_pre_heat]
!!    check_composition    [check_fld1%i_pre_composit]
!!    check_pressure       [check_fld1%i_pre_press]
!!    check_potential      [check_fld1%i_pre_phi]
!!
!!      Second check of explicit terms
!!    check_momentum_2     [check_fld2%i_pre_mom]
!!    check_induction_2    [check_fld2%i_pre_uxb]
!!    check_heat_2         [check_fld2%i_pre_heat]
!!    check_composition_2  [check_fld2%i_pre_composit]
!!    check_pressure_2     [check_fld2%i_pre_press]
!!    check_potential_2    [check_fld2%i_pre_phi]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_explicit_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!  arrays for current forces
!>      Work area for explicit terms
      type explicit_term_address
!>        start address for total forces
        integer (kind=kint) :: i_forces =       izero
!>        start address for curl of total forces
        integer (kind=kint) :: i_rot_forces =   izero
!>        start address for divergence of total forces
        integer (kind=kint) :: i_div_forces =   izero
!
!>        start address for explicit term for momentum at previous step
        integer (kind=kint) :: i_pre_mom =      izero
!>        start address for explicit term for induction at previous step
        integer (kind=kint) :: i_pre_uxb =      izero
!>        start address for explicit term for heat at previous step
        integer (kind=kint) :: i_pre_heat =     izero
!>        start address for explicit term for composition
!!        at previous step
        integer (kind=kint) :: i_pre_composit = izero
!>        start address for explicit term for pressure at previous step
        integer (kind=kint) :: i_pre_press =    izero
!>        start address for explicit term for potential at previous step
        integer (kind=kint) :: i_pre_phi =    izero
!
!>        Field label of energy flux by potential in momentum euqaion
!!         @f$ \varphi @f$
        integer (kind=kint) :: i_p_phi =           izero
!>        Field address of energy flux by potential in induction euqaion
!!         @f$ \varphi @f$
        integer (kind=kint) :: i_m_phi =           izero
      end type explicit_term_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_work_field_addresses                               &
     &         (i_phys, field_name, exp_work, flag)
!
      use m_explicit_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(explicit_term_address), intent(inout) :: exp_work
      logical, intent(inout) :: flag
!
!
      flag =   check_vector_work_field(field_name)                      &
     &    .or. check_scalar_work_field(field_name)                      &
     &    .or. check_work_4_poisson(field_name)
      if(flag) then
        if (field_name .eq. sum_forces%name) then
          exp_work%i_forces =       i_phys
        else if (field_name .eq. rot_sum_forces%name) then
          exp_work%i_rot_forces =   i_phys
        else if (field_name .eq. div_sum_forces%name) then
          exp_work%i_div_forces =   i_phys
!
        else if (field_name .eq. previous_momentum%name) then
          exp_work%i_pre_mom =      i_phys
        else if (field_name .eq. previous_induction%name) then
          exp_work%i_pre_uxb =      i_phys
!
        else if (field_name .eq. previous_heat%name) then
          exp_work%i_pre_heat =     i_phys
        else if (field_name .eq. previous_composition%name) then
          exp_work%i_pre_composit = i_phys
        else if (field_name .eq. previous_pressure%name) then
          exp_work%i_pre_press =    i_phys
        else if (field_name .eq. previous_potential%name) then
          exp_work%i_pre_phi =      i_phys
!
        else if (field_name .eq. pressure_work%name) then
          exp_work%i_p_phi = i_phys
        else if (field_name .eq. m_potential_work%name) then
          exp_work%i_m_phi = i_phys
        end if
      end if  
!
      end subroutine set_work_field_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_check_field_addresses                              &
     &         (i_phys, field_name, check_fld1, check_fld2, flag)
!
      use m_explicit_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(explicit_term_address), intent(inout) :: check_fld1
      type(explicit_term_address), intent(inout) :: check_fld2
      logical, intent(inout) :: flag
!
!
      flag =   check_vector_check_field(field_name)                     &
     &    .or. check_scalar_check_field(field_name)
      if(flag) then
        if (field_name .eq. check_momentum%name) then
          check_fld1%i_pre_mom =      i_phys
        else if (field_name .eq. check_induction%name) then
          check_fld1%i_pre_uxb =      i_phys
!
        else if (field_name .eq. check_heat%name) then
          check_fld1%i_pre_heat =     i_phys
        else if (field_name .eq. check_composition%name) then
          check_fld1%i_pre_composit = i_phys
        else if (field_name .eq. check_pressure%name) then
          check_fld1%i_pre_press =    i_phys
        else if (field_name .eq. check_potential%name) then
          check_fld1%i_pre_phi =      i_phys
!
        else if (field_name .eq. check_momentum_2%name) then
          check_fld2%i_pre_mom =      i_phys
        else if (field_name .eq. check_induction_2%name) then
          check_fld2%i_pre_uxb =      i_phys
!
        else if (field_name .eq. check_heat_2%name) then
          check_fld2%i_pre_heat =     i_phys
        else if (field_name .eq. check_composition_2%name) then
          check_fld2%i_pre_composit = i_phys
        else if (field_name .eq. check_pressure_2%name) then
          check_fld2%i_pre_press =    i_phys
        else if (field_name .eq. check_potential_2%name) then
          check_fld2%i_pre_phi =      i_phys
        end if
      end if  
!
      end subroutine set_check_field_addresses
!
! ----------------------------------------------------------------------
!
      end module t_explicit_term_labels
