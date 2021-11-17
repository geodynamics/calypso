!>@file  t_layering_ele_list.f90
!!       module t_layering_ele_list
!!
!!@author H. Matsui
!!@date   Programmed in Nov., 2009
!
!> @brief Structure of grouping of elements
!!
!!@verbatim
!!      subroutine alloc_layering_ele_list_type(layer_tbl)
!!      subroutine alloc_layer_items_type(layer_tbl)
!!      subroutine alloc_layering_volumes_type(layer_tbl)
!!
!!      subroutine dealloc_layering_ele_list_type(layer_tbl)
!!      subroutine dealloc_layering_volumes_type(layer_tbl)
!!
!!      subroutine check_layer_stack_type(id_rank, layer_tbl)
!!@endverbatim
!
      module t_layering_ele_list
!
      use m_precision
      use t_group_data
!
      implicit none
!
!   layering element table
!
!> Structure of grouping of elements
      type layering_tbl
!>  element group structure for layering
        type(group_data) :: e_grp
!
!>      minimum number of each layer with SMP
        integer(kind = kint) :: minlayer_4_smp
!
!>      starting address for each layer for SMP
        integer (kind = kint), allocatable                              &
     &                 :: istack_item_layer_d_smp(:)
!>      minimum number of each layer for SMP
        integer(kind = kint) :: min_item_layer_d_smp
!>      maximum number of each layer for SMP
        integer(kind = kint) :: max_item_layer_d_smp
!
!   volumes of layering area
!
!>      volumes for each layer
        real(kind = kreal), allocatable :: volumes_layer(:)
!>       1 / volumes_layer
        real(kind = kreal), allocatable :: a_vol_layer(:)
!
!>      volumes for all layer
        real(kind = kreal) :: vol_total_layer(1)
!>       1 / vol_total_layer
        real(kind = kreal) :: a_vol_total_layer(1)
      end type layering_tbl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_layering_ele_list_type(layer_tbl)
!
      use m_machine_parameter
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      layer_tbl%e_grp%num_grp_smp = layer_tbl%e_grp%num_grp * np_smp
      call alloc_group_num(layer_tbl%e_grp)
      call alloc_group_smp(layer_tbl%e_grp)
!
      allocate (layer_tbl%istack_item_layer_d_smp(0:np_smp) )
!
      if (layer_tbl%e_grp%num_grp .gt. 0) then
        layer_tbl%istack_item_layer_d_smp = 0
      end if
!
      end subroutine alloc_layering_ele_list_type
!
! ----------------------------------------------------------------------
!
      subroutine alloc_layer_items_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call alloc_group_item(layer_tbl%e_grp)
!
      end subroutine alloc_layer_items_type
!
! ----------------------------------------------------------------------
!
      subroutine alloc_layering_volumes_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
       allocate( layer_tbl%volumes_layer(layer_tbl%e_grp%num_grp) )
       allocate( layer_tbl%a_vol_layer(layer_tbl%e_grp%num_grp) )
!
       if(layer_tbl%e_grp%num_grp .gt. 0) then
         layer_tbl%volumes_layer = 0.0d0
         layer_tbl%a_vol_layer  =  0.0d0
       end if
!
      end subroutine alloc_layering_volumes_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_layering_ele_list_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
      call dealloc_group_smp(layer_tbl%e_grp)
      call dealloc_group_item(layer_tbl%e_grp)
      call dealloc_group_num(layer_tbl%e_grp)
!
      deallocate (layer_tbl%istack_item_layer_d_smp )
!
      end subroutine dealloc_layering_ele_list_type
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_layering_volumes_type(layer_tbl)
!
      type(layering_tbl), intent(inout) :: layer_tbl
!
!
       deallocate( layer_tbl%volumes_layer, layer_tbl%a_vol_layer)
!
      end subroutine dealloc_layering_volumes_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_layer_stack_type(id_rank, layer_tbl)
!
      integer, intent(in) :: id_rank
      type(layering_tbl), intent(in) :: layer_tbl
!
!
      call check_group_type_data(id_rank, layer_tbl%e_grp)
!
      end subroutine check_layer_stack_type
!
! ----------------------------------------------------------------------
!
      end module t_layering_ele_list
