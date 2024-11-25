!>@file   t_ctl_params_viz_fields.f90
!!@brief  module t_ctl_params_viz_fields
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief control parameters for each field line
!!
!!@verbatim
!!      subroutine dealloc_ctl_params_viz_fields(viz_fields)
!!        type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!      subroutine set_ctl_params_viz_fields(field_output_ctl, nod_fld, &
!!     &                                     viz_fields)
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_array_c2), intent(in) :: field_output_ctl
!!       type(ctl_params_viz_fields), intent(inout) :: viz_fields
!!
!!      subroutine copy_ctl_params_viz_fields(num_field, viz_fields,    &
!!     &                                      new_viz_fields)
!!      subroutine append_ctl_params_viz_fields                         &
!!     &         (field_name, iphys_field, icomp_flag,                  &
!!     &          num_comp, ncomp_org, viz_fields)
!!@endverbatim
!
      module t_ctl_params_viz_fields
!
      use m_precision
!
      implicit  none
!
!
!>        control parameter for vizulization field output
      type ctl_params_viz_fields
!>        number of field for coloring
        integer(kind = kint) :: num_color_fields = 0
!>        number of field for coloring
        integer(kind = kint) :: ntot_color_comp =  0
!>        Field name for fieldline color
        character(len = kchara), allocatable :: color_field_name(:)
!>        Field address for fieldline color
        integer(kind = kint), allocatable :: ifleld_color_field(:)
!>        component address for fieldline color
        integer(kind = kint), allocatable :: icomp_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: istack_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: ncomp_color_field(:)
!
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: ncomp_org_color_field(:)
!>        number of component for fieldline color
        integer(kind = kint), allocatable :: istack_org_ncomp(:)
!>        number of field for coloring
        integer(kind = kint) :: ntot_org_comp =  0
      end type ctl_params_viz_fields
!
      private :: alloc_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ctl_params_viz_fields(num_field, viz_fields)
!
      integer(kind = kint), intent(in) :: num_field
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
!
      viz_fields%num_color_fields = num_field
      allocate(viz_fields%color_field_name(num_field))
      allocate(viz_fields%ifleld_color_field(num_field))
      allocate(viz_fields%icomp_color_field(num_field))
      allocate(viz_fields%istack_color_field(0:num_field))
      allocate(viz_fields%ncomp_color_field(num_field))
      allocate(viz_fields%ncomp_org_color_field(num_field))
      allocate(viz_fields%istack_org_ncomp(0:num_field))
!
      end subroutine alloc_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_ctl_params_viz_fields(viz_fields)
!
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
!
      deallocate(viz_fields%color_field_name)
      deallocate(viz_fields%ifleld_color_field)
      deallocate(viz_fields%icomp_color_field)
      deallocate(viz_fields%istack_color_field)
      deallocate(viz_fields%ncomp_color_field)
      deallocate(viz_fields%ncomp_org_color_field)
      deallocate(viz_fields%istack_org_ncomp)
!
      end subroutine dealloc_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_ctl_params_viz_fields                           &
     &         (field_name, iphys_field, icomp_flag,                    &
     &          num_comp, ncomp_org, viz_fields)
!
      character(len = kchara), intent(in) :: field_name
      integer(kind = kint), intent(in) :: iphys_field, icomp_flag
      integer(kind = kint), intent(in) :: num_comp, ncomp_org
!
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
      type(ctl_params_viz_fields) :: tmp_fields
      integer(kind = kint) :: new_num_fields
!
      call alloc_ctl_params_viz_fields(viz_fields%num_color_fields,     &
     &                                 tmp_fields)
      call copy_ctl_params_viz_fields(viz_fields%num_color_fields,      &
     &                                viz_fields, tmp_fields)
      call dealloc_ctl_params_viz_fields(viz_fields)
!
      new_num_fields = viz_fields%num_color_fields + 1
      call alloc_ctl_params_viz_fields(new_num_fields, viz_fields)
!
      call copy_ctl_params_viz_fields(tmp_fields%num_color_fields,      &
     &                                tmp_fields, viz_fields)
      call dealloc_ctl_params_viz_fields(tmp_fields)
!
      viz_fields%ntot_color_comp                                        &
     &    = viz_fields%ntot_color_comp + num_comp 
      viz_fields%color_field_name(new_num_fields) =   field_name
      viz_fields%ifleld_color_field(new_num_fields) = iphys_field
      viz_fields%icomp_color_field(new_num_fields) =  icomp_flag
      viz_fields%istack_color_field(new_num_fields)                     &
     &    = viz_fields%istack_color_field(new_num_fields-1) + num_comp
      viz_fields%ncomp_color_field(new_num_fields) =      num_comp

      viz_fields%ntot_org_comp                                          &
     &    = viz_fields%ntot_org_comp + ncomp_org
      viz_fields%ncomp_org_color_field(new_num_fields) = ncomp_org
      viz_fields%istack_org_ncomp(new_num_fields)                       &
     &    = viz_fields%istack_org_ncomp(new_num_fields-1) + ncomp_org
!
      end subroutine append_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      subroutine copy_ctl_params_viz_fields(num_field, viz_fields,      &
     &                                      new_viz_fields)
!
      integer(kind = kint), intent(in) :: num_field
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(ctl_params_viz_fields), intent(inout) :: new_viz_fields
!
      new_viz_fields%ntot_color_comp = viz_fields%ntot_color_comp
      new_viz_fields%ntot_org_comp =   viz_fields%ntot_org_comp
      new_viz_fields%istack_color_field(0)                              &
     &       = viz_fields%istack_color_field(0)
      new_viz_fields%istack_org_ncomp(0)                                &
     &       = viz_fields%istack_org_ncomp(0)
!$omp parallel workshare
      new_viz_fields%color_field_name(1:num_field)                      &
     &       = viz_fields%color_field_name(1:num_field)
      new_viz_fields%ifleld_color_field(1:num_field)                    &
     &       = viz_fields%ifleld_color_field(1:num_field)
      new_viz_fields%icomp_color_field(1:num_field)                     &
     &       = viz_fields%icomp_color_field(1:num_field)
      new_viz_fields%istack_color_field(1:num_field)                    &
     &       = viz_fields%istack_color_field(1:num_field)
      new_viz_fields%ncomp_color_field(1:num_field)                     &
     &       = viz_fields%ncomp_color_field(1:num_field)
      new_viz_fields%ncomp_org_color_field(1:num_field)                 &
     &       = viz_fields%ncomp_org_color_field(1:num_field)
      new_viz_fields%istack_org_ncomp(1:num_field)                      &
     &       = viz_fields%istack_org_ncomp(1:num_field)
!$omp end parallel workshare
!
      end subroutine copy_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      subroutine set_ctl_params_viz_fields(field_output_ctl, nod_fld,   &
     &                                     viz_fields)
!
      use t_phys_data
      use t_control_array_character2
      use set_components_flags
      use set_field_comp_for_viz
!
      type(phys_data), intent(in) :: nod_fld
      type(ctl_array_c2), intent(in) :: field_output_ctl
      type(ctl_params_viz_fields), intent(inout) :: viz_fields
!
      integer(kind = kint) :: i
!
!
      if(field_output_ctl%num .le. izero) then
        call alloc_ctl_params_viz_fields(ione, viz_fields)
      else
        call alloc_ctl_params_viz_fields                              &
     &     (field_output_ctl%num, viz_fields)
      end if
!
      if(field_output_ctl%num .le. izero) then
        viz_fields%color_field_name(1) = 'data'
        viz_fields%ifleld_color_field = -1
        viz_fields%icomp_color_field =  -1
        viz_fields%ncomp_color_field =   1
        viz_fields%ncomp_org_color_field = 0
      else
        call set_components_4_viz                                       &
     &     (nod_fld%num_phys, nod_fld%phys_name, field_output_ctl%num,  &
     &      field_output_ctl%c1_tbl, field_output_ctl%c2_tbl,           &
     &      viz_fields%num_color_fields, viz_fields%ifleld_color_field, &
     &      viz_fields%icomp_color_field, viz_fields%ncomp_color_field, &
     &      viz_fields%ncomp_org_color_field,                           &
     &      viz_fields%color_field_name)
      end if
      viz_fields%istack_color_field(0) = 0
      viz_fields%istack_org_ncomp(0) = 0
      do i = 1, viz_fields%num_color_fields
        viz_fields%istack_color_field(i)                                &
     &        = viz_fields%istack_color_field(i-1)                      &
     &         + viz_fields%ncomp_color_field(i)
        viz_fields%istack_org_ncomp(i)                                  &
     &        = viz_fields%istack_org_ncomp(i-1)                        &
     &         + viz_fields%ncomp_org_color_field(i)
      end do
      viz_fields%ntot_color_comp                                        &
     &     = viz_fields%istack_color_field(viz_fields%num_color_fields)
      viz_fields%ntot_org_comp                                          &
     &     = viz_fields%istack_org_ncomp(viz_fields%num_color_fields)
!
      end subroutine set_ctl_params_viz_fields
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_params_viz_fields
