!>@file   check_dependency_for_MHD.f90
!!@brief  module check_dependency_for_MHD
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Check dependecy of field list fro MHD dynamo
!!
!!@verbatim
!!      subroutine set_FEM_MHD_field_data                               &
!!     &         (node, MHD_prop, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(phys_address), intent(inout) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine set_sph_MHD_sprctr_data                              &
!!     &         (sph_rj, MHD_prop, ipol, idpdr, itor, rj_fld)
!!@endverbatim
!
      module check_dependency_for_MHD
!
      use m_precision
      use m_error_IDs
      use m_machine_parameter
!
      use calypso_mpi
      use m_phys_labels
!
      use t_control_parameter
      use t_phys_data
      use t_phys_address
      use t_physical_property
!
      implicit none
!
      private :: check_field_dependencies
      private :: check_dependence_FEM_evo, check_dependence_SPH_evo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_FEM_MHD_field_data                                 &
     &         (node, MHD_prop, iphys, nod_fld)
!
      use t_geometry_data
      use t_FEM_phys_data
      use check_MHD_dependency_by_id
!
      type(node_data), intent(in) :: node
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(phys_address), intent(inout) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.ge.1)  write(*,*) 'init_field_address'
      call init_field_address(node%numnod, nod_fld, iphys)
!
      call check_field_dependencies                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, iphys, nod_fld)
      call check_dependencies_by_id(MHD_prop%cd_prop, iphys, nod_fld)
      call check_dependence_FEM_MHD_by_id(iphys, nod_fld)
      call check_dependence_FEM_evo(MHD_prop%fl_prop, iphys, nod_fld)
!
      end subroutine set_FEM_MHD_field_data
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_MHD_sprctr_data                                &
     &         (sph_rj, MHD_prop, ipol, idpdr, itor, rj_fld)
!
      use t_spheric_rj_data
!
      use set_sph_phys_address
      use check_MHD_dependency_by_id
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(phys_address), intent(inout) :: ipol, idpdr, itor
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_sph_sprctr_data_address                                  &
     &   (sph_rj, ipol, idpdr, itor, rj_fld)
!
      call check_field_dependencies                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, ipol, rj_fld)
      call check_dependencies_by_id(MHD_prop%cd_prop, ipol, rj_fld)
      call check_dependence_SPH_MHD_by_id(ipol, rj_fld)
      call check_dependence_SPH_evo(MHD_prop%fl_prop, ipol, rj_fld)
!
      end subroutine set_sph_MHD_sprctr_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_field_dependencies                               &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, iphys, fld)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: iphys
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
        msg = 'time integration for velocity needs'
        call check_missing_field_w_msg(fld, msg, iphys%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys%i_press)
      end if
!
      if (fl_prop%iflag_scheme .gt. id_no_evolution) then
        msg = 'time integration for velocity needs'
        call check_missing_field_w_msg(fld, msg, iphys%i_vort)
      end if
!
      if (ht_prop%iflag_scheme .gt. id_no_evolution) then
        msg = 'Time integration for temperature needs'
        call check_missing_field_w_msg(fld, msg, iphys%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys%i_temp)
      end if
!
      if (cp_prop%iflag_scheme .ne. id_no_evolution) then
        msg =  'Time integration for composition needs'
        call check_missing_field_w_msg(fld, msg, iphys%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys%i_light)
      end if
!
      if (cd_prop%iflag_Bevo_scheme .ne. id_no_evolution) then
        msg = 'Time integration for magnetic field needs'
        call check_missing_field_w_msg(fld, msg, iphys%i_magne)
        call check_missing_field_w_msg(fld, msg, iphys%i_velo)
      end if
!
      if (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        msg = 'Time integration for vector potential needs'
        call check_missing_field_w_msg(fld, msg, iphys%i_vecp)
        call check_missing_field_w_msg(fld, msg, iphys%i_velo)
      end if
!
!
      if ( fl_prop%iflag_scheme .gt. id_no_evolution) then
        if (fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          msg = 'Buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_temp)
        end if
!
        if (fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          msg = 'Compositional buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_light)
        end if
!
        if (fl_prop%iflag_4_filter_gravity .gt. id_turn_OFF) then
          msg = 'Filtered buoyancy needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_filter_temp)
        end if
!
        if (fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          msg = 'Lorentz force needs'
          call check_missing_field_w_msg(fld, msg, iphys%i_magne)
        end if
      end if
!
      end subroutine check_field_dependencies
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_dependence_FEM_evo(fl_prop, iphys, fld)
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
        call check_missing_field_w_msg(fld, msg, iphys%i_velo)
        call check_missing_field_w_msg(fld, msg, iphys%i_press)
      end if
!
      end subroutine check_dependence_FEM_evo
!
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
        call check_missing_field_w_msg(fld, msg, iphys%i_vort)
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
