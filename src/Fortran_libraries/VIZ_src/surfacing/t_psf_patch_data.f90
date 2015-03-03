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
!!      subroutine alloc_num_patch_psf(np_smp, num_psf, pat)
!!      subroutine alloc_position_psf(pat)
!!      subroutine alloc_dat_on_patch_psf(pat)
!!      subroutine alloc_patch_data_psf(pat)
!!
!!      subroutine dealloc_num_patch_psf(pat)
!!      subroutine dealloc_position_psf(pat)
!!      subroutine dealloc_dat_on_patch_psf(pat)
!!      subroutine dealloc_patch_data_psf(pat)
!!@endverbatim
!
      module t_psf_patch_data
!
      use m_precision
!
      implicit none
!
!
      type psf_parameters
        integer(kind = kint) :: nele_grp_area
        integer(kind = kint), pointer :: id_ele_grp_area(:)
!
        integer(kind = kint), pointer :: id_output(:)
        integer(kind = kint), pointer :: icomp_output(:)
        integer(kind = kint), pointer :: ncomp_org(:)
      end type psf_parameters
!
      type psf_patch_data
        integer(kind = kint) :: nnod_psf_tot
        integer(kind = kint), pointer :: istack_nod_psf(:)
        integer(kind = kint), pointer :: istack_nod_psf_smp(:)
!
        integer(kind = kint), pointer :: inod_hash_psf(:)
        real(kind = kreal), pointer :: xyz_psf(:,:)
!
        real(kind = kreal), pointer :: rr(:)
        real(kind = kreal), pointer :: theta(:)
        real(kind = kreal), pointer :: phi(:)
        real(kind = kreal), pointer :: ar(:)
        real(kind = kreal), pointer :: ss(:)
        real(kind = kreal), pointer :: as(:)
!
!
        integer(kind = kint) :: npatch_tot
        integer(kind = kint), pointer :: istack_patch_psf(:)
        integer(kind = kint), pointer :: istack_patch_psf_smp(:)
        integer(kind = kint), pointer :: ie_tri(:,:)
!
        integer(kind = kint) :: max_ncomp_psf
        real(kind = kreal), pointer :: dat_psf(:,:)
        real(kind = kreal), pointer :: tmp_psf(:,:)
      end type psf_patch_data
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
      subroutine alloc_num_patch_psf(np_smp, num_psf, pat)
!
      integer(kind= kint), intent(in) :: np_smp, num_psf
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%istack_nod_psf(0:num_psf))
      allocate(pat%istack_patch_psf(0:num_psf))
      allocate(pat%istack_nod_psf_smp(0:np_smp*num_psf))
      allocate(pat%istack_patch_psf_smp(0:np_smp*num_psf))
!
      pat%istack_nod_psf = 0
      pat%istack_patch_psf = 0
      pat%istack_nod_psf_smp = 0
      pat%istack_patch_psf_smp = 0
!
      end subroutine alloc_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_position_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%inod_hash_psf(pat%nnod_psf_tot))
      allocate(pat%xyz_psf(pat%nnod_psf_tot,3))
      allocate(pat%rr(pat%nnod_psf_tot))
      allocate(pat%theta(pat%nnod_psf_tot))
      allocate(pat%phi(pat%nnod_psf_tot))
      allocate(pat%ar(pat%nnod_psf_tot))
      allocate(pat%ss(pat%nnod_psf_tot))
      allocate(pat%as(pat%nnod_psf_tot))
!
      if(pat%nnod_psf_tot .gt. 0) then
        pat%inod_hash_psf = 0
        pat%xyz_psf = 0.0d0
        pat%rr =    0.0d0
        pat%theta = 0.0d0
        pat%phi =   0.0d0
        pat%ar =    0.0d0
        pat%ss =    0.0d0
        pat%as =    0.0d0
      end if
!
      end subroutine alloc_position_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dat_on_patch_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%dat_psf(pat%nnod_psf_tot,pat%max_ncomp_psf))
      allocate(pat%tmp_psf(pat%nnod_psf_tot,6))
!
      if(pat%nnod_psf_tot .gt. 0) then
        pat%dat_psf = 0.0d0
        pat%tmp_psf = 0.0d0
      end if
!
      end subroutine alloc_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_patch_data_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      allocate(pat%ie_tri(pat%npatch_tot,3))
      if(pat%npatch_tot .gt. 0) pat%ie_tri = 0
!
      end subroutine alloc_patch_data_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_num_patch_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%istack_nod_psf, pat%istack_nod_psf_smp)
      deallocate(pat%istack_patch_psf, pat%istack_patch_psf_smp)
!
      end subroutine dealloc_num_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_position_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%xyz_psf, pat%inod_hash_psf)
      deallocate(pat%rr, pat%theta, pat%phi)
      deallocate(pat%ar, pat%ss, pat%as)
!
      end subroutine dealloc_position_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_dat_on_patch_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%dat_psf, pat%tmp_psf)
!
      end subroutine dealloc_dat_on_patch_psf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_patch_data_psf(pat)
!
      type(psf_patch_data), intent(inout) :: pat
!
      deallocate(pat%ie_tri)
!
      end subroutine dealloc_patch_data_psf
!
!  ---------------------------------------------------------------------
!
      end module t_psf_patch_data
