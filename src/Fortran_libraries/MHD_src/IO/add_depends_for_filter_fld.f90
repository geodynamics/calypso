!>@file   add_depends_for_filter_fld.f90
!!@brief  module add_depends_for_filter_fld
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui Sep., 2006
!
!> @brief Add missing field for MHD dynamo to field list
!!
!!@verbatim
!!      subroutine add_dependent_filterd_field(SGS_param, field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_depends_for_filter_fld
!
      use m_precision
!
      use m_machine_parameter
      use t_control_parameter
      use t_physical_property
      use add_nodal_fields_ctl
      use calypso_mpi
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_dependent_filterd_field(field_ctl)
!
      use check_filtered_field
      use check_filtered_forces
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      call add_field_ctl_4_fil_ene_flux(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_fil_ene_flux end'
!
      call add_field_ctl_4_filter_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_filter_forces end'
!
      call add_field_ctl_4_filterd_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_filterd_field end'
!
      end subroutine add_dependent_filterd_field
!
! -----------------------------------------------------------------------
!
      end module add_depends_for_filter_fld
