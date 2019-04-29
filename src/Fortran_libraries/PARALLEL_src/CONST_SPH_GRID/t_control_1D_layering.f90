!>@file   t_control_1D_layering.f90
!!@brief  module t_control_1D_layering
!!
!!@author H. Matsui
!!@date Programmed on Aug., 2016
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine alloc_layering_group(num_layer, layering)
!!      subroutine dealloc_layering_group(layering)
!!
!!      subroutine set_group_by_layering_list                         &
!!     &         (name_prefix, layer_list_ctl, layering)
!!      subroutine set_group_by_equidivide                            &
!!     &         (name_prefix, istart, iend, num_layer_ctl, layering)
!!        type(read_integer_item), intent(in) :: istart, iend
!!        type(read_integer_item), intent(in) :: num_layer_ctl
!!        type(ctl_array_i2), intent(in) :: layer_list_ctl
!!        type(layering_group_list), intent(inout) :: layering
!!@endverbatim
!
      module t_control_1D_layering
!
      use m_precision
!
!>      Structure for layering information
      type layering_group_list
!>        Number of groups
        integer(kind = kint) :: nlayer
!>        global start address
        integer(kind = kint), allocatable :: istart(:)
!>        global end address
        integer(kind = kint), allocatable :: iend(:)
!>        name of group
        character(len = kchara), allocatable :: name(:)
      end type layering_group_list
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_layering_group(num_layer, layering)
!
      integer(kind = kint), intent(in) :: num_layer
      type(layering_group_list), intent(inout) :: layering
!
!
      layering%nlayer = num_layer
      allocate(layering%istart(layering%nlayer))
      allocate(layering%iend(layering%nlayer))
      allocate(layering%name(layering%nlayer))
!
      if(layering%nlayer .le. 0) return
      layering%istart = 0
      layering%iend =   0
!
      end subroutine alloc_layering_group
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_layering_group(layering)
!
      type(layering_group_list), intent(inout) :: layering
!
!
      deallocate(layering%istart, layering%iend)
      deallocate(layering%name)
!
      end subroutine dealloc_layering_group
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_group_by_layering_list                             &
     &         (name_prefix, layer_list_ctl, layering)
!
      use t_read_control_arrays
      use set_parallel_file_name
!
      character(len=kchara), intent(in) :: name_prefix
      type(ctl_array_i2), intent(in) :: layer_list_ctl
      type(layering_group_list), intent(inout) :: layering
!
      integer(kind = kint) :: i
!
!
      call alloc_layering_group(layer_list_ctl%num, layering)
!
      do i = 1, layering%nlayer
        call add_index_after_name(i, name_prefix, layering%name(i))
        layering%istart(i) =  layer_list_ctl%int1(i)
        layering%iend(i) =    layer_list_ctl%int2(i)
      end do
!
      end subroutine set_group_by_layering_list
!
!   --------------------------------------------------------------------
!
      subroutine set_group_by_equidivide                                &
     &         (name_prefix, istart, iend, num_layer_ctl, layering)
!
      use t_control_elements
      use set_parallel_file_name
      use cal_minmax_and_stacks
!
      character(len=kchara), intent(in) :: name_prefix
      integer(kind = kint), intent(in) :: istart, iend
      type(read_integer_item), intent(in) :: num_layer_ctl
      type(layering_group_list), intent(inout) :: layering
!
      integer(kind = kint) :: i, max_tmp
      integer(kind = kint), allocatable :: istack_tmp(:)
!
!
      call alloc_layering_group(num_layer_ctl%intvalue, layering)
!
      allocate(istack_tmp(0:layering%nlayer))
      call count_number_4_smp(layering%nlayer, istart, iend,            &
     &   istack_tmp, max_tmp)
!
      do i = 1, layering%nlayer
        call add_index_after_name(i, name_prefix, layering%name(i))
        layering%istart(i) =  istack_tmp(i-1) + 1
        layering%iend(i) =    istack_tmp(i)
      end do
      deallocate(istack_tmp)
!
      end subroutine set_group_by_equidivide
!
!   --------------------------------------------------------------------
!
      end module t_control_1D_layering
