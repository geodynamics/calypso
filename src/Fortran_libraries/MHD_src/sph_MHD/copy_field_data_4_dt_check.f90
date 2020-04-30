!copy_field_data_4_dt_check.f90
!     module copy_field_data_4_dt_check
!
!      Written by H. Matsui on Nov., 2009
!
!!      subroutine s_copy_field_data_for_dt_check                       &
!!     &         (cd_prop, iphys, nod_fld)
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!
      module copy_field_data_4_dt_check
!
      use m_constants
!
      use t_physical_property
      use t_phys_data
      use t_phys_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_field_data_for_dt_check                         &
     &         (cd_prop, iphys, nod_fld)
!
      use copy_nodal_fields
!
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iphys%check_fld2%i_pre_mom .gt. izero) then
        call copy_vector_component(nod_fld,                             &
     &      iphys%check_fld1%i_pre_mom, iphys%check_fld2%i_pre_mom)
      end if
!
      if(iphys%check_fld2%i_pre_press .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%check_fld1%i_pre_press, iphys%check_fld2%i_pre_press)
      end if
!
!
      if(iphys%check_fld2%i_pre_uxb .gt. izero) then
        call copy_vector_component(nod_fld,                             &
     &      iphys%check_fld1%i_pre_uxb, iphys%check_fld2%i_pre_uxb)
      end if
!
      if(iphys%check_fld2%i_pre_phi .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%check_fld1%i_pre_phi, iphys%check_fld2%i_pre_phi)
      end if
!
!
      if(iphys%check_fld2%i_pre_heat .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%check_fld1%i_pre_heat, iphys%check_fld2%i_pre_heat)
      end if
!
      if(iphys%check_fld2%i_pre_composit .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%check_fld1%i_pre_composit,                            &
     &      iphys%check_fld2%i_pre_composit)
      end if
!
!
!
      if(iphys%check_fld1%i_pre_mom .gt. izero) then
        call copy_vector_component(nod_fld,                             &
     &      iphys%base%i_velo, iphys%check_fld1%i_pre_mom)
      end if
!
      if(iphys%check_fld1%i_pre_press .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_press, iphys%check_fld1%i_pre_press)
      end if
!
!
      if(iphys%check_fld1%i_pre_uxb .gt. izero) then
        if(cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
          call copy_vector_component(nod_fld,                           &
     &        iphys%base%i_vecp, iphys%check_fld1%i_pre_uxb)
        else
          call copy_vector_component(nod_fld,                           &
     &        iphys%base%i_magne, iphys%check_fld1%i_pre_uxb)
        end if
      end if
!
      if(iphys%check_fld1%i_pre_phi .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_mag_p, iphys%check_fld1%i_pre_phi)
      end if
!
!
      if(iphys%check_fld1%i_pre_heat .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_temp, iphys%check_fld1%i_pre_heat)
      end if
!
      if(iphys%check_fld1%i_pre_composit .gt. izero) then
        call copy_scalar_component(nod_fld,                             &
     &      iphys%base%i_light, iphys%check_fld1%i_pre_composit)
      end if
!
      end subroutine s_copy_field_data_for_dt_check
!
! ----------------------------------------------------------------------
!
      end module copy_field_data_4_dt_check
