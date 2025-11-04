!>@file   t_scalar_property.f90
!!@brief  module t_scalar_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients for a scalar field
!!
!!@verbatim
!!      subroutine set_filtered_advection_ctl(filterd_advect_ctl,       &
!!     &                                      scl_prop)
!!        type(read_character_item), intent(in) :: filterd_advect_ctl
!!        type(scalar_property), intent(inout) :: scl_prop
!!@endverbatim
!
      module t_scalar_property
!
      use m_precision
      use m_constants
!
      use m_property_flags
!
      implicit  none
!
!>      Structure for scalar property
      type scalar_property
!>        Time evolution flag for velocity
        integer (kind=kint) :: iflag_scheme = id_no_evolution
!>        Coefficient of implicit term
        real(kind = kreal) :: coef_imp = half
!>        Coefficient of explicit term
        real(kind = kreal) :: coef_exp = half
!
!>       coefficient for time evolution of temperature and heat flux
        real(kind = kreal) :: coef_advect
!>       coefficient for heat flux (-coef_advect)
        real(kind = kreal) :: coef_nega_adv
!
!>       coefficient for thermal diffusion
        real(kind = kreal) :: coef_diffuse
!>       coefficient for heat source term
        real(kind = kreal) :: coef_source = zero
!
!>       radial field index for diffusivity variation
        integer(kind = kint) :: ir_kappa =        izero
!>       radial field index for diffusivity variation
        integer(kind = kint) :: ir_dkappa_norm =  izero
!
!>       coefficient for diffusion reduction for ICB
        real(kind = kreal) :: diffuse_reduction_ratio_ICB = one
!>       coefficient for diffusion reduction for ICB
        real(kind = kreal) :: diffuse_reduction_width_ICB = zero
!
!>        Force flag for advection
        logical :: iflag_4_advection = .FALSE.
!>        Force flag for Filtered advection
        logical :: iflag_4_filter_advection = .FALSE.
      end type scalar_property
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_filtered_advection_ctl(filterd_advect_ctl,         &
     &                                      scl_prop)
!
      use t_control_array_character
      use skip_comment_f
!
      type(read_character_item), intent(in) :: filterd_advect_ctl
      type(scalar_property), intent(inout) :: scl_prop
!
!
      if(scl_prop%iflag_scheme .eq. id_no_evolution) return
      scl_prop%iflag_4_advection = .TRUE.
!
      if(filterd_advect_ctl%iflag .gt. 0                                &
     &   .and. yes_flag(filterd_advect_ctl%charavalue)) then
        scl_prop%iflag_4_advection = .FALSE.
        scl_prop%iflag_4_filter_advection = .TRUE.
      end if
!
      end subroutine set_filtered_advection_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_diffusion_reduction_ctl(ref_scl_ctl, scl_prop)
!
      use t_ctl_data_temp_model
!
      type(reference_temperature_ctl), intent(in) :: ref_scl_ctl
      type(scalar_property), intent(inout) :: scl_prop
!
!
      scl_prop%diffuse_reduction_ratio_ICB = 1.0d0
      scl_prop%diffuse_reduction_width_ICB = 0.0d0
      if(ref_scl_ctl%ICB_diffuse_reduction_ratio%iflag .gt. 0) then
        scl_prop%diffuse_reduction_ratio_ICB                            &
     &        = ref_scl_ctl%ICB_diffuse_reduction_ratio%realvalue
      end if
      if(ref_scl_ctl%ICB_diffuse_reduction_width%iflag .gt. 0) then
        scl_prop%diffuse_reduction_width_ICB                            &
     &        = ref_scl_ctl%ICB_diffuse_reduction_width%realvalue
      end if
!
      end subroutine set_diffusion_reduction_ctl
!
! -----------------------------------------------------------------------
!
      end module t_scalar_property
