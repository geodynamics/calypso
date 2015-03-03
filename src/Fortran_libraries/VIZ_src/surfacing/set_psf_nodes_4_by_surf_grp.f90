!
!      module set_psf_nodes_4_by_surf_grp
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine count_node_at_node_on_grp(num_item, istack_n_on_n_smp)
!!      subroutine set_node_at_node_on_grp                              &
!!     &          (nitem_surf, inod_surf_grp, psf_list)
!!
!!      subroutine count_node_on_edge_on_grp(istack_n_on_e_smp)
!
      module set_psf_nodes_4_by_surf_grp
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_node_at_node_on_grp(num_item, istack_n_on_n_smp)
!
      use cal_minmax_and_stacks
!
      integer(kind=kint), intent(in) :: num_item
      integer(kind=kint), intent(inout) :: istack_n_on_n_smp(0:np_smp)

      integer(kind=kint) :: ist, ied, max_4_smp
!
!
      ist = istack_n_on_n_smp(0)
      ied = istack_n_on_n_smp(0) + num_item
      call count_number_4_smp(np_smp, ist, ied,                         &
     &    istack_n_on_n_smp, max_4_smp)
!
      end subroutine count_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!
      subroutine set_node_at_node_on_grp                                &
     &          (nitem_surf, inod_surf_grp, psf_list)
!
      use m_constants
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nitem_surf
      integer(kind = kint), intent(in) :: inod_surf_grp(nitem_surf)
!
      type(sectiong_list), intent(inout) :: psf_list
!
      integer(kind = kint) :: i, icou, inod
!
!
      do i = 1, nitem_surf
        icou = psf_list%istack_n_on_n_smp(0) + i
        inod = inod_surf_grp(i)
        psf_list%inod_4_nod(icou) = inod
        psf_list%id_n_on_n(inod) = icou
      end do
!
      end subroutine set_node_at_node_on_grp
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_node_on_edge_on_grp(istack_n_on_e_smp)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_n_on_e_smp(0:np_smp)
!
      integer(kind = kint) :: i
!
!
      do i = 1, np_smp
        istack_n_on_e_smp(i) = istack_n_on_e_smp(0)
      end do
!
      end subroutine count_node_on_edge_on_grp
!
!  ---------------------------------------------------------------------
!
      end module set_psf_nodes_4_by_surf_grp
