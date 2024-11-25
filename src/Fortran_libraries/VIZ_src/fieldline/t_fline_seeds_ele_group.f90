!>@file   t_fline_seeds_ele_group.f90
!!@brief  module t_fline_seeds_ele_group
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Element group list to set seed points
!!
!!@verbatim
!!      subroutine init_density_on_seed_ele(node, ele, ele_grp, nod_fld,&
!!     &                                    fln_prm, seed_ele_grp)
!!      subroutine dealloc_density_on_seed_ele(seed_ele_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!        type(phys_data), intent(in) :: nod_fld
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(fieldline_seeds_ele_group), intent(inout) :: seed_ele_grp
!!@endverbatim
!
      module t_fline_seeds_ele_group
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_tracing_data
!
      implicit  none
!
      type fieldline_seeds_ele_group
        integer(kind = kint) :: nele_seed = 0
        integer(kind = kint), allocatable :: iele_grp_seed_item(:)
        real(kind = kreal),   allocatable :: density_seed(:)
!
        real(kind = kreal),   allocatable :: seed_field(:)
      end type fieldline_seeds_ele_group
!
      type(fieldline_seeds_ele_group), save :: seed_ele_grp
!
      private :: alloc_local_start_grp_item
      private :: count_nele_for_seeds, set_iele_for_seeds
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_density_on_seed_ele(node, ele, ele_grp, nod_fld,  &
     &                                    fln_prm, seed_ele_grp)
!
      use convert_components_4_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(fieldline_seeds_ele_group), intent(inout) :: seed_ele_grp
!
      integer(kind = kint) :: num_ele, i_field, ist_fld, num_comp
!
!
      num_ele = count_nele_for_seeds(ele, ele_grp,                      &
     &                               fln_prm%igrp_start_fline_ele_grp)
      call alloc_local_start_grp_item(num_ele, seed_ele_grp)
!
      call set_iele_for_seeds                                           &
     &   (ele, ele_grp, fln_prm%igrp_start_fline_ele_grp,               &
     &    seed_ele_grp%nele_seed, seed_ele_grp%iele_grp_seed_item)
!
      if(     fln_prm%id_seed_distribution .eq. iflag_random_by_area    &
     &   .or. fln_prm%id_seed_distribution .eq. iflag_no_random) then
        if(iflag_debug .gt. 0) write(*,*) 'cal_volume_for_1egrp'
        call cal_volume_for_1egrp(ele,                                  &
     &      seed_ele_grp%nele_seed, seed_ele_grp%iele_grp_seed_item,    &
     &      seed_ele_grp%density_seed)
      else
        i_field =  fln_prm%ifield_4_density
        ist_fld =  nod_fld%istack_component(i_field-1)
        num_comp = nod_fld%istack_component(i_field) - ist_fld
        call alloc_density_for_seed(node%numnod, seed_ele_grp)
        call convert_comps_4_viz                                        &
     &     (node%numnod, node%istack_nod_smp, node%xx, node%rr,         &
     &      node%a_r, node%ss, node%a_s, ione, num_comp,                &
     &      fln_prm%icomp_4_density, nod_fld%d_fld(1,ist_fld+1),        &
     &      seed_ele_grp%seed_field)
!
        if(iflag_debug .gt. 0) write(*,*) 'cal_density_for_1egrp'
        call cal_density_for_1egrp(ele,                                 &
     &      seed_ele_grp%nele_seed, seed_ele_grp%iele_grp_seed_item,    &
     &      nod_fld%n_point, seed_ele_grp%seed_field,                   &
     &      seed_ele_grp%density_seed)
        call dealloc_density_for_seed(seed_ele_grp)
      end if
!
      end subroutine init_density_on_seed_ele
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_density_on_seed_ele(seed_ele_grp)
!
      type(fieldline_seeds_ele_group), intent(inout) :: seed_ele_grp
!
!
      deallocate(seed_ele_grp%iele_grp_seed_item)
      deallocate(seed_ele_grp%density_seed)
!
      end subroutine dealloc_density_on_seed_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_local_start_grp_item(num, seed_ele_grp)
!
      integer(kind = kint), intent(in) :: num
      type(fieldline_seeds_ele_group), intent(inout) :: seed_ele_grp
!
!
      seed_ele_grp%nele_seed = num
      allocate(seed_ele_grp%iele_grp_seed_item(num))
      allocate(seed_ele_grp%density_seed(num))
      if(num .gt. 0) seed_ele_grp%iele_grp_seed_item = 0
      if(num .gt. 0) seed_ele_grp%density_seed = 0.0d0
!
      end subroutine alloc_local_start_grp_item
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_density_for_seed(numnod, seed_ele_grp)
!
      integer(kind = kint), intent(in) :: numnod
      type(fieldline_seeds_ele_group), intent(inout) :: seed_ele_grp
!
      allocate(seed_ele_grp%seed_field(numnod))
      if(numnod .gt. 0) seed_ele_grp%seed_field = 0
!
      end subroutine alloc_density_for_seed
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_density_for_seed(seed_ele_grp)
      type(fieldline_seeds_ele_group), intent(inout) :: seed_ele_grp
!
      deallocate(seed_ele_grp%seed_field)
!
      end subroutine dealloc_density_for_seed
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_nele_for_seeds(ele, ele_grp,  &
     &                                                   igrp_seed)
!
      integer(kind = kint), intent(in) :: igrp_seed
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint) :: inum, iele, icou, ist, ied
!
!
      icou = 0
      ist = ele_grp%istack_grp(igrp_seed-1) + 1
      ied = ele_grp%istack_grp(igrp_seed)
      do inum = ist, ied
        iele = ele_grp%item_grp(inum)
        if(ele%interior_ele(iele) .ne. izero) icou = icou + 1
      end do
!
      count_nele_for_seeds = icou
!
      end function count_nele_for_seeds
!
!  ---------------------------------------------------------------------
!
      subroutine set_iele_for_seeds(ele, ele_grp, igrp_seed,            &
     &                              nele_seed, iele_grp_seed_item)
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
      integer(kind = kint), intent(in) :: igrp_seed
!
      integer(kind = kint), intent(in) :: nele_seed
      integer(kind = kint), intent(inout)                               &
     &                     :: iele_grp_seed_item(nele_seed)
!
      integer(kind = kint) :: icou, inum, iele, ist, ied
!
!
      icou = 0
      ist = ele_grp%istack_grp(igrp_seed-1) + 1
      ied = ele_grp%istack_grp(igrp_seed)
      do inum = ist, ied
        iele = ele_grp%item_grp(inum)
        if(ele%interior_ele(iele) .ne. izero) then
          icou = icou + 1
          iele_grp_seed_item(inum) = ele_grp%item_grp(iele)
        end if
      end do
!
      end subroutine set_iele_for_seeds
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine cal_density_for_1egrp(ele, num_egrp, iele_grp,         &
     &                                 n_point, d_nod, density)
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: num_egrp
      integer(kind = kint), intent(in) :: iele_grp(num_egrp)
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(in) :: d_nod(n_point)
!
      real(kind = kreal), intent(inout) :: density(num_egrp)
!
      integer (kind = kint) :: iele, inum
      integer (kind = kint) :: i1, i2, i3, i4, i5, i6, i7, i8
      real(kind = kreal) :: d_ele
!
!
!$omp parallel workshare
      density(1:num_egrp) = 0.0d0
!$omp end parallel workshare
!
!$omp  parallel do                                                      &
!$omp& private(inum,iele,i1,i2,i3,i4,i5,i6,i7,i8,d_ele)
!$cdir nodep
      do inum = 1, num_egrp
        iele = iele_grp(inum)
!
        i1 =  ele%ie(iele, 1)
        i2 =  ele%ie(iele, 2)
        i3 =  ele%ie(iele, 3)
        i4 =  ele%ie(iele, 4)
        i5 =  ele%ie(iele, 5)
        i6 =  ele%ie(iele, 6)
        i7 =  ele%ie(iele, 7)
        i8 =  ele%ie(iele, 8)
!
        d_ele = r125 * (d_nod(i1) + d_nod(i2) + d_nod(i3) + d_nod(i4)   &
     &                + d_nod(i5) + d_nod(i7) + d_nod(i7) + d_nod(i8))
!
        density(inum) = density(inum)                                   &
     &                 + abs(d_ele) * ele%volume_ele(iele)              &
     &                  * dble(ele%interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_density_for_1egrp
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_for_1egrp(ele, num_egrp, iele_grp, density)
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: num_egrp
      integer(kind = kint), intent(in) :: iele_grp(num_egrp)
!
      real(kind = kreal), intent(inout) :: density(num_egrp)
!
      integer (kind = kint) :: inum, iele
!
!
!$omp parallel workshare
      density(1:num_egrp) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(inum,iele)
      do inum = 1, num_egrp
        iele = iele_grp(inum)
        density(inum) = density(inum) + ele%volume_ele(iele)            &
     &                                 * dble(ele%interior_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_volume_for_1egrp
!
!  ---------------------------------------------------------------------
!
      end module t_fline_seeds_ele_group
