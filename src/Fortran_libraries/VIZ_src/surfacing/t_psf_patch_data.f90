!>@file   t_psf_patch_data.f90
!!@brief  module t_psf_patch_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2014
!
!>@brief Structure for parallel sectioned data
!!
!!@verbatim
!!      subroutine alloc_output_comps_psf(num_phys, params)
!!      subroutine alloc_area_group_psf(params)
!!      subroutine dealloc_output_comps_psf(params)
!!      subroutine dealloc_area_group_psf(params)
!!
!!      subroutine alloc_dat_on_patch_psf(ncomp, psf_mesh)
!!      subroutine dealloc_dat_on_patch_psf(psf_mesh)
!!@endverbatim
!
      module t_psf_patch_data
!
      use m_precision
      use t_geometry_data
      use t_phys_data
!
      implicit none
!
!
      type psf_parameters
        integer(kind = kint) :: nele_grp_area
        integer(kind = kint), allocatable :: id_ele_grp_area(:)
!
        integer(kind = kint), allocatable :: id_output(:)
        integer(kind = kint), allocatable :: icomp_output(:)
        integer(kind = kint), allocatable :: ncomp_org(:)
      end type psf_parameters
!
!>
      type psf_local_data
!>        structure for node position
        type(node_data) ::    node
!>        structure for patch connectivity
        type(element_data) :: patch
!>        Structure for sectioned field
        type(phys_data) ::    field
!
        integer(kind = kint) :: ntot_comp
!>        local temporal field data for psf
        real(kind = kreal), allocatable :: tmp_psf(:,:)
      end type psf_local_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_output_comps_psf(num_phys, params)
!
      integer(kind= kint), intent(in) :: num_phys
      type(psf_parameters), intent(inout) :: params
!
!
      allocate(params%id_output(num_phys)    )
      allocate(params%icomp_output(num_phys) )
      allocate(params%ncomp_org(num_phys)    )
!
      if(num_phys .le. 0) return
      params%id_output =     0
      params%icomp_output =  0
      params%ncomp_org =  0
!
      end subroutine alloc_output_comps_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_area_group_psf(params)
!
      type(psf_parameters), intent(inout) :: params
!
!
      allocate(params%id_ele_grp_area(params%nele_grp_area))
      if(params%nele_grp_area .le. 0)   params%id_ele_grp_area = 0
!
      end subroutine alloc_area_group_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_output_comps_psf(params)
!
      type(psf_parameters), intent(inout) :: params
!
      deallocate(params%id_output, params%icomp_output)
      deallocate(params%ncomp_org)
!
      end subroutine dealloc_output_comps_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_area_group_psf(params)
!
      type(psf_parameters), intent(inout) :: params
!
!
      deallocate(params%id_ele_grp_area)
!
      end subroutine dealloc_area_group_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_dat_on_patch_psf(psf_mesh)
!
      type(psf_local_data), intent(inout) :: psf_mesh
!
      allocate(psf_mesh%tmp_psf(psf_mesh%node%numnod,6))
      if(psf_mesh%node%numnod .gt. 0) psf_mesh%tmp_psf = 0.0d0
!
      end subroutine alloc_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dat_on_patch_psf(psf_mesh)
!
      type(psf_local_data), intent(inout) :: psf_mesh
!
      deallocate(psf_mesh%tmp_psf)
!
      end subroutine dealloc_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      end module t_psf_patch_data
