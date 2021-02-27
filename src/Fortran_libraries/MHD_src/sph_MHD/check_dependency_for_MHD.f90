!>@file   check_dependency_for_MHD.f90
!!@brief  module check_dependency_for_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine set_sph_MHD_sprctr_data(MHD_prop, SPH_MHD)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!      subroutine init_sph_MHD_field_data(sph, rj_fld, ipol)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_address), intent(inout) :: ipol
!!
!!      subroutine check_field_dependencies                             &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          iphys_base, iphys_fil, fld)
!!      subroutine check_dependence_SPH_evo(fl_prop, iphys, fld)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(phys_data), intent(in) :: fld
!!@endverbatim
!
      module check_dependency_for_MHD
!
      use m_precision
      use m_error_IDs
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_physical_property
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_MHD_sprctr_data(MHD_prop, SPH_MHD)
!
      use t_SPH_mesh_field_data
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      call init_sph_MHD_field_data                                      &
     &   (SPH_MHD%sph, SPH_MHD%fld, SPH_MHD%ipol)
!
      call check_field_dependencies                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    SPH_MHD%ipol%base, SPH_MHD%fld)
      call check_dependence_SPH_evo                                     &
     &   (MHD_prop%fl_prop, SPH_MHD%ipol, SPH_MHD%fld)
!
      end subroutine set_sph_MHD_sprctr_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_sph_MHD_field_data(sph, rj_fld, ipol)
!
      use set_control_field_data
!
      type(sph_grids), intent(in) :: sph
!
      type(phys_address), intent(inout) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call init_field_data(sph%sph_rj%nnod_rj, rj_fld, ipol)
!
      end subroutine init_sph_MHD_field_data
!
! -----------------------------------------------------------------------
!
      subroutine check_field_dependencies                               &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, iphys_base, fld)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(base_field_address), intent(in) :: iphys_base
      type(phys_data), intent(in) :: fld
!
      character(len=kchara) :: msg
!
!
!   check dependencies for time evolution
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution                &
     &     .and. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
         call calypso_MPI_abort(ierr_fld,                               &
     &        'You should choose vector potential OR magnetic field for &
     & time evolution')
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        msg = 'time integration for momentum equation needs'
        call check_missing_field_w_msg(fld, msg, iphys_base%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys_base%i_press)
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        msg = 'time integration for vorticity equation needs'
        call check_missing_field_w_msg(fld, msg, iphys_base%i_vort)
      end if
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        msg = 'Time integration for heat equation needs'
        call check_missing_field_w_msg(fld, msg, iphys_base%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys_base%i_temp)
      end if
!
      if (cp_prop%iflag_scheme .ne. id_no_evolution) then
        msg =  'Time integration for composition equation needs'
        call check_missing_field_w_msg(fld, msg, iphys_base%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys_base%i_light)
      end if
!
      if (cd_prop%iflag_Bevo_scheme .ne. id_no_evolution) then
        msg = 'Time integration for magnetic induction equation needs'
        call check_missing_field_w_msg(fld, msg, iphys_base%i_magne)
        call check_missing_field_w_msg(fld, msg, iphys_base%i_velo)
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        msg = 'Time integration for vector potential induction needs'
        call check_missing_field_w_msg(fld, msg, iphys_base%i_vecp)
        call check_missing_field_w_msg(fld, msg, iphys_base%i_velo)
      end if
!
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(fl_prop%iflag_4_gravity) then
          msg = 'Buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys_base%i_temp)
        end if
!
        if(fl_prop%iflag_4_composit_buo) then
          msg = 'Compositional buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys_base%i_light)
        end if
!
        if(fl_prop%iflag_4_lorentz) then
          msg = 'Lorentz force needs'
          call check_missing_field_w_msg(fld, msg, iphys_base%i_magne)
        end if
      end if
!
      end subroutine check_field_dependencies
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_dependence_SPH_evo(fl_prop, iphys, fld)
!
      type(fluid_property), intent(in) :: fl_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: fld
!
      character(len=kchara) :: msg
!
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        msg = 'time integration for velocity needs'
        call check_missing_field_w_msg(fld, msg, iphys%base%i_vort)
      end if
!
      end subroutine check_dependence_SPH_evo
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_missing_field_w_msg(fld, target_msg, iphys_ref)
!
      type(phys_data), intent(in) :: fld
      character(len=kchara), intent(in) :: target_msg
      integer(kind = kint), intent(in) :: iphys_ref
!
      if(iphys_ref .gt. 0) return
      write(*,*) target_msg, ': ',                                      &
     &    trim(field_name_by_address(fld, iphys_ref))
      call calypso_MPI_abort(ierr_fld,'Stop program.')
!
      end subroutine check_missing_field_w_msg
!
! -----------------------------------------------------------------------
!
      end module check_dependency_for_MHD
