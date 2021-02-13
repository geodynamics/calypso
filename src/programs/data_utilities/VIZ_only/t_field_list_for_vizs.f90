!>@file   t_field_list_for_vizs.f90
!!@brief  module t_field_list_for_vizs
!!
!!@author H. Matsui
!!@date Programmed in July, 2020
!
!>@brief Field list for visualization
!!
!!@verbatim
!!      subroutine init_viz_field_list_control(viz_field_ctl,           &
!!     &                                       viz_fld_list)
!!      subroutine dealloc_field_lists_for_vizs(viz_fld_list)
!!        type(ctl_array_c3), intent(in) :: viz_field_ctl
!!        type(visulize_field_list), intent(inout) :: viz_fld_list
!!
!!      subroutine add_field_in_viz_controls(viz_fld_list, phys_nod)
!!        type(visulize_field_list), intent(in) :: viz_fld_list
!!        type(phys_data), intent(inout) :: phys_nod
!!@endverbatim
!
      module t_field_list_for_vizs
!
      use m_precision
      use m_machine_parameter
!
!>      structure of field list for visualization
      type visulize_field_list
!>        number of field
        integer(kind = kint) :: num_field
!>        name of field
        character(len = kchara), allocatable :: field_name(:)
      end type visulize_field_list
!
      private :: alloc_field_lists_for_vizs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_viz_field_list_control(viz_field_ctl,             &
     &                                       viz_fld_list)
!
      use t_control_array_character3
!
      type(ctl_array_c3), intent(in) :: viz_field_ctl
      type(visulize_field_list), intent(inout) :: viz_fld_list
!
!
      call alloc_field_lists_for_vizs(viz_field_ctl%num, viz_fld_list)
!
      if(viz_field_ctl%num .le. 0) return
      viz_fld_list%field_name(1:viz_fld_list%num_field)                 &
     &    = viz_field_ctl%c1_tbl(1:viz_fld_list%num_field)
!
      end subroutine init_viz_field_list_control
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_field_lists_for_vizs(viz_fld_list)
!
      type(visulize_field_list), intent(inout) :: viz_fld_list
!
      deallocate(viz_fld_list%field_name)
!
      end subroutine dealloc_field_lists_for_vizs
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_field_in_viz_controls(viz_fld_list, phys_nod)
!
      use t_phys_data
      use set_each_field_name
!
      type(visulize_field_list), intent(in) :: viz_fld_list
      type(phys_data), intent(inout) :: phys_nod
!
      integer(kind = kint) :: i_fld, j_fld
      logical :: flag
!
!
      do i_fld = 1, viz_fld_list%num_field
        flag = .FALSE.
        do j_fld = 1, phys_nod%num_phys
          if(viz_fld_list%field_name(i_fld)                             &
     &       .eq. phys_nod%phys_name(j_fld)) then
            flag = .TRUE.
            exit
          end if
        end do
!
        if(flag) cycle
        call set_vector_field_name(viz_fld_list%field_name(i_fld),      &
     &      (.TRUE.), (.TRUE.), phys_nod, flag)
        if(flag) cycle
        call set_scalar_field_name(viz_fld_list%field_name(i_fld),      &
     &      (.TRUE.), (.TRUE.), phys_nod, flag)
        if(flag) cycle
        call set_tensor_field_name(viz_fld_list%field_name(i_fld),      &
     &      (.TRUE.), (.TRUE.), phys_nod, flag)
      end do
!
      end subroutine add_field_in_viz_controls
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_field_lists_for_vizs(n_field, viz_fld_list)
!
      integer(kind = kint), intent(in) :: n_field
      type(visulize_field_list), intent(inout) :: viz_fld_list
!
!
      viz_fld_list%num_field = n_field
      allocate(viz_fld_list%field_name(viz_fld_list%num_field))
!
      end subroutine alloc_field_lists_for_vizs
!
! ----------------------------------------------------------------------
!
      end module t_field_list_for_vizs
 
