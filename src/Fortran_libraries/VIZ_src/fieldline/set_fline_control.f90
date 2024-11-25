!>@file   set_fline_control.f90
!!@brief  module set_fline_control
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for field lines
!!
!!@verbatim
!!      subroutine s_set_fline_control                                  &
!!     &         (mesh, group, nod_fld, num_tracer, tracer_prm,         &
!!     &          fline_ctl_struct, fln_prm)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in) :: num_tracer
!!      type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fline_ctl), intent(inout)  :: fline_ctl_struct
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!      subroutine s_set_tracer_control(mesh, group, nod_fld,           &
!!     &          fline_ctl_struct, fln_prm)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(tracer_module), intent(in) :: tracer
!!        type(fline_ctl), intent(inout)  :: fline_ctl_struct
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!@endverbatim
!
      module set_fline_control
!
      use m_precision
!
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data
      use t_group_data
      use t_phys_data
      use t_control_params_4_fline
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fline_control                                    &
     &         (mesh, group, nod_fld, num_tracer, tracer_prm,           &
     &          fline_ctl_struct, fln_prm)
!
      use t_control_data_flines
      use set_control_each_fline
      use set_iflag_for_used_ele
      use set_control_fline_seeds
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
      integer(kind = kint), intent(in) :: num_tracer
      type(fieldline_paramter), intent(in) :: tracer_prm(num_tracer)
!
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: i, ierr
!
!
      call count_control_4_fline(fline_ctl_struct,                      &
     &     group%ele_grp, group%surf_grp, fln_prm, ierr)
      call count_control_fline_seeds(fline_ctl_struct%seeds_ctl,        &
     &                               fln_prm)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr,                                    &
     &                         'Check Directory for Fieldline output')
      end if
!
      call alloc_iflag_fline_used_ele(mesh%ele, fln_prm)
      call alloc_fline_starts_ctl(fln_prm)
!
      call set_control_4_fline(fline_ctl_struct,                        &
     &                          group%ele_grp, nod_fld, fln_prm)
      call s_set_control_fline_seeds(fline_ctl_struct%seeds_ctl,        &
     &                               fln_prm)
      call set_fline_ctl_4_tracer_seed(num_tracer, tracer_prm,          &
     &                                 fline_ctl_struct, fln_prm)
!      call s_set_iflag_for_used_ele(mesh%ele, group%ele_grp,           &
!     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,  &
!     &    fln_prm%iflag_fline_used_ele)
      call set_iflag_used_ele_w_overlap(mesh%ele, group%ele_grp,        &
     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,   &
     &    fln_prm%iflag_fline_used_ele)
      call deallocate_cont_dat_fline(fline_ctl_struct)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'field line parameters for No.', i
        call check_control_params_fline(fln_prm)
      end if
!
      end subroutine s_set_fline_control
!
!   --------------------------------------------------------------------
!
      subroutine s_set_tracer_control(mesh, group, nod_fld,             &
     &                                fline_ctl_struct, fln_prm)
!
      use t_control_data_flines
      use set_control_each_fline
      use set_iflag_for_used_ele
      use set_control_fline_seeds
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: i, ierr
!
!
      call count_control_4_fline(fline_ctl_struct,                      &
     &    group%ele_grp, group%surf_grp, fln_prm, ierr)
      call count_control_fline_seeds(fline_ctl_struct%seeds_ctl,        &
     &                               fln_prm)
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr,                                    &
     &                         'Check Directory for tracer output')
      end if
!
      call alloc_iflag_fline_used_ele(mesh%ele, fln_prm)
      call alloc_fline_starts_ctl(fln_prm)
!
      call set_control_4_fline(fline_ctl_struct,                        &
     &                         group%ele_grp, nod_fld, fln_prm)
      call s_set_control_fline_seeds(fline_ctl_struct%seeds_ctl,        &
     &                               fln_prm)
!      call s_set_iflag_for_used_ele(mesh%ele, group%ele_grp,           &
!     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,  &
!     &    fln_prm%iflag_fline_used_ele)
      call set_iflag_used_ele_w_overlap(mesh%ele, group%ele_grp,        &
     &    fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,   &
     &    fln_prm%iflag_fline_used_ele)
      call deallocate_cont_dat_fline(fline_ctl_struct)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'field line parameters for No.', i
        call check_control_params_fline(fln_prm)
      end if
!
      end subroutine s_set_tracer_control
!
!   --------------------------------------------------------------------
!
      end module set_fline_control
