!>@file   set_smp_4_group_types.f90
!!@brief  module set_smp_4_group_types
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!> @brief Set SMP stacks for group data
!!
!!@verbatim
!!      subroutine count_num_groups_smp                                 &
!!     &         (id_rank, nod_grp, ele_grp, surf_grp)
!!        type(group_data), intent(inout) :: nod_grp
!!        type(group_data), intent(inout) :: ele_grp
!!        type(surface_group_data), intent(inout) :: surf_grp
!!@endverbatim
!
      module set_smp_4_group_types
!
      use m_precision
      use m_machine_parameter
!
      use t_group_data
!
      implicit none
!
      private :: count_grp_type_smp, count_surf_grp_type_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine count_num_groups_smp                                   &
     &         (id_rank, nod_grp, ele_grp, surf_grp)
!
      integer, intent(in) :: id_rank
      type(group_data), intent(inout) :: nod_grp
      type(group_data), intent(inout) :: ele_grp
      type(surface_group_data), intent(inout) :: surf_grp
!
!
      call count_grp_type_smp(nod_grp)
      call count_grp_type_smp(ele_grp)
      call count_surf_grp_type_smp(surf_grp)
!
!       if (iflag_debug.gt.0) then
!         call check_grp_4_sheard_para(id_rank, nod_grp)
!         call check_grp_4_sheard_para(id_rank, ele_grp)
!         call check_surf_grp_4_sheard_para(id_rank, surf_grp)
!       end if
!
      end subroutine count_num_groups_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_grp_type_smp(grp)
!
      use cal_minmax_and_stacks
!
      type(group_data), intent(inout) :: grp
!
!
      grp%num_grp_smp = np_smp * grp%num_grp
!
      call alloc_group_smp(grp)
!
      call set_group_size_4_smp(np_smp, grp%num_grp, grp%istack_grp,    &
     &    grp%istack_grp_smp, grp%max_grp_smp)
!
      end subroutine count_grp_type_smp
!
!-----------------------------------------------------------------------
!
      subroutine count_surf_grp_type_smp(sf_grp)
!
      use cal_minmax_and_stacks
!
      type(surface_group_data), intent(inout) :: sf_grp
!
!
      sf_grp%num_grp_smp = np_smp * sf_grp%num_grp
!
      call alloc_sf_group_smp(sf_grp)
!
      call set_group_size_4_smp(np_smp,                                 &
     &    sf_grp%num_grp, sf_grp%istack_grp,                            &
     &    sf_grp%istack_grp_smp, sf_grp%max_grp_smp)
!
      end subroutine count_surf_grp_type_smp
!
!-----------------------------------------------------------------------
!
      end module set_smp_4_group_types
