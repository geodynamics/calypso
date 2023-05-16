!>@file   set_fline_control.f90
!!@brief  module set_fline_control
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control data for field lines
!!
!!@verbatim
!!      subroutine s_set_fline_control(mesh, group, nod_fld,            &
!!     &          fline_ctl_struct, fln_prm, fln_src)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_controls), intent(inout) :: fline_ctls
!!        type(fline_ctl), intent(inout)  :: fline_ctl_struct
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
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
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_fline_control(mesh, group, nod_fld,              &
     &          fline_ctl_struct, fln_prm, fln_src)
!
      use t_control_data_flines
      use t_control_params_4_fline
      use t_source_of_filed_line
      use set_control_each_fline
      use set_iflag_for_used_ele
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(phys_data), intent(in) :: nod_fld
!
      type(fline_ctl), intent(inout)  :: fline_ctl_struct
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint) :: i
!
!
      call count_control_4_fline(fline_ctl_struct,                      &
     &    mesh%ele, group%ele_grp, group%surf_grp, fln_prm, fln_src)
!
      call alloc_iflag_fline_used_ele(mesh%ele, fln_prm)
      call alloc_fline_starts_ctl(fln_prm)
      call alloc_local_start_grp_item(fln_src)
!
      call set_control_4_fline(fline_ctl_struct,                        &
     &    mesh%ele, group%ele_grp, group%surf_grp, nod_fld,             &
     &    fln_prm, fln_src)
      call s_set_iflag_for_used_ele(mesh%ele, group%ele_grp,            &
&         fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,   &
&         fln_prm%iflag_fline_used_ele)
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
      end module set_fline_control
