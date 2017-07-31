!>@file   t_bc_data_list.f90
!!@brief  module t_bc_data_list
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>@brief  Boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine alloc_bc_type_ctl(bc_list)
!!      subroutine dealloc_bc_type_ctl(bc_list)
!!        type(boundary_condition_list), intent(inout) :: bc_list
!!
!!      subroutine alloc_nod_bc_list(fld_BC)
!!      subroutine alloc_surf_bc_list(fld_BC)
!!      subroutine dealloc_nod_bc_list(fld_BC)
!!      subroutine dealloc_surf_bc_list(fld_BC)
!!        type(boundary_condition_lists), intent(inout) :: fld_BC
!!
!!      subroutine deallocate_surf_bc_lists(MHD_prop, MHD_BC)
!!@endverbatim
!
      module t_bc_data_list
!
      use m_precision
!
      implicit  none
!
!
!>       Structure for boundary condition list
      type boundary_condition_list
!>       number of boundary condition list
        integer (kind=kint) :: num_bc
!>       Value for the boundary condition
        real (kind=kreal), allocatable :: bc_magnitude(:)
!>       Type of the boundary condition
        integer (kind=kint), allocatable :: ibc_type(:)
!>       Name of group to apply the boundary condition
        character (len=kchara), allocatable :: bc_name(:)
      end type boundary_condition_list
!
!
!>       Structure for boundary group data list
      type boundary_condition_lists
!>         Node group data list
        type(boundary_condition_list) :: nod_BC
!>         Surfaqce group data list
        type(boundary_condition_list) :: surf_BC
      end type boundary_condition_lists
!
!
!>     Structure for boundary condition lists for MHD
      type MHD_BC_lists
!>         Node group data list for velocity
        type(boundary_condition_lists) :: velo_BC
!>         Node group data list for pressure
        type(boundary_condition_lists) :: press_BC
!
!>         Node group data list for temperarure
        type(boundary_condition_lists) :: temp_BC
!>         Node group data list for composition
        type(boundary_condition_lists) :: light_BC
!
!>         Node group data list for magnetic field
        type(boundary_condition_lists) :: magne_BC
!>         Node group data list for magnetic vector potential
        type(boundary_condition_lists) :: a_potential_BC
!>         Node group data list for electric scalar potential
        type(boundary_condition_lists) :: e_potential_BC
!>         Node group data list for current density
        type(boundary_condition_lists) :: current_BC
      end type MHD_BC_lists
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
      subroutine alloc_bc_type_ctl(bc_list)
!
      type(boundary_condition_list), intent(inout) :: bc_list
!
!
      allocate(bc_list%bc_magnitude(bc_list%num_bc))
      allocate(bc_list%ibc_type(bc_list%num_bc))
      allocate(bc_list%bc_name(bc_list%num_bc))
!
      if(bc_list%num_bc .gt. 0) then
        bc_list%ibc_type =     0
        bc_list%bc_magnitude = 0.0d0
      end if
!
      end subroutine alloc_bc_type_ctl
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_bc_type_ctl(bc_list)
!
      type(boundary_condition_list), intent(inout) :: bc_list
!
!
      deallocate(bc_list%bc_magnitude)
      deallocate(bc_list%ibc_type)
      deallocate(bc_list%bc_name)
!
      end subroutine dealloc_bc_type_ctl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_surf_bc_lists(MHD_prop, MHD_BC)
!
      use t_control_parameter
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
!
!
      if (MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_BC%temp_BC%surf_BC%num_bc .gt. 0)                        &
     &      call dealloc_surf_bc_list(MHD_BC%temp_BC)
      end if
!
      if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_BC%velo_BC%surf_BC%num_bc.gt.0)                          &
     &      call dealloc_surf_bc_list(MHD_BC%velo_BC)
        if(MHD_BC%press_BC%surf_BC%num_bc.gt.0)                         &
     &      call dealloc_surf_bc_list(MHD_BC%press_BC)
      end if
!
      if    (MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution    &
     &  .or. MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution)   &
     & then
        if(MHD_BC%magne_BC%surf_BC%num_bc .gt. 0)                       &
     &        call dealloc_surf_bc_list(MHD_BC%magne_BC)
        if(MHD_BC%current_BC%surf_BC%num_bc .gt. 0)                     &
     &        call dealloc_surf_bc_list(MHD_BC%current_BC)
        if(MHD_BC%e_potential_BC%surf_BC%num_bc.gt.0)                   &
     &        call dealloc_surf_bc_list(MHD_BC%e_potential_BC)
      end if
!
      if (MHD_prop%cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        if(MHD_BC%a_potential_BC%surf_BC%num_bc.gt.0)                   &
     &        call dealloc_surf_bc_list(MHD_BC%a_potential_BC)
      end if
! 
      if (MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        if(MHD_BC%light_BC%surf_BC%num_bc.gt.0)                         &
     &     call dealloc_surf_bc_list(MHD_BC%light_BC)
      end if
!
      end subroutine deallocate_surf_bc_lists
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_nod_bc_list(fld_BC)
!
      type(boundary_condition_lists), intent(inout) :: fld_BC
!
      call alloc_bc_type_ctl(fld_BC%nod_BC)
!
      end subroutine alloc_nod_bc_list
!
! -----------------------------------------------------------------------
!
      subroutine alloc_surf_bc_list(fld_BC)
!
      type(boundary_condition_lists), intent(inout) :: fld_BC
!
      call alloc_bc_type_ctl(fld_BC%surf_BC)
!
      end subroutine alloc_surf_bc_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_bc_list(fld_BC)
!
      type(boundary_condition_lists), intent(inout) :: fld_BC
!
      call dealloc_bc_type_ctl(fld_BC%nod_BC)
!
      end subroutine dealloc_nod_bc_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_surf_bc_list(fld_BC)
!
      type(boundary_condition_lists), intent(inout) :: fld_BC
!
      call dealloc_bc_type_ctl(fld_BC%surf_BC)
!
      end subroutine dealloc_surf_bc_list
!
! -----------------------------------------------------------------------
!
      end module t_bc_data_list
