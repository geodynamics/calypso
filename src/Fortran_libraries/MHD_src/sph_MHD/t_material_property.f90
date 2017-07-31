!t_material_property.f90
!     module t_material_property
!
!> @brief addresses for SGS coefficients
!
!     Written by H. Matsui
!
!!       subroutine alloc_velo_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_temp_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_magne_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_dscalar_diff_MHD_AMG(numele, ak_MHD)
!!       subroutine alloc_buoyancy_coef_ele(numele, ak_MHD)
!!       subroutine alloc_comp_buo_coef_ele(numele, ak_MHD)
!!
!!       subroutine dealloc_velo_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_temp_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_magne_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_dscalar_diff_MHD_AMG(ak_MHD)
!!       subroutine dealloc_buoyancy_coef_ele(ak_MHD)
!!       subroutine dealloc_comp_buo_coef_ele(ak_MHD)
!
      module t_material_property
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
      type coefs_4_MHD_type
!>       coeffeicient for viscous diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_velo(:)
!>       coeffeicient for thermal diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_temp(:)
!>       coeffeicient for magnetic diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_magne(:)
!>       coeffeicient for chemical diffusion for each element
        real  (kind=kreal), allocatable :: ak_d_composit(:)
!
!>       coeffeicient for thermal buoyancy for each element
        real  (kind=kreal), allocatable :: ak_buo(:)
!>       coeffeicient for compositional buoyancy for each element
        real  (kind=kreal), allocatable :: ak_comp_buo(:)
      end type coefs_4_MHD_type
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
       subroutine alloc_velo_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_velo(numele) )
       if (numele.gt.0) ak_MHD%ak_d_velo = 0.0d0
!
       end subroutine alloc_velo_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_temp_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_temp(numele) )
       if (numele.gt.0) ak_MHD%ak_d_temp = 0.0d0
!
       end subroutine alloc_temp_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_magne_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_magne(numele) )
       if (numele.gt.0) ak_MHD%ak_d_magne = 0.0d0
!
       end subroutine alloc_magne_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_dscalar_diff_MHD_AMG(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_d_composit(numele) )
       if (numele.gt.0) ak_MHD%ak_d_composit = 0.0d0
!
       end subroutine alloc_dscalar_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine alloc_buoyancy_coef_ele(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_buo(numele) )
       if (numele.gt.0) ak_MHD%ak_buo = 0.0d0
!
       end subroutine alloc_buoyancy_coef_ele
!
! ----------------------------------------------------------------------
!
       subroutine alloc_comp_buo_coef_ele(numele, ak_MHD)
!
       integer(kind = kint), intent(in) :: numele
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       allocate( ak_MHD%ak_comp_buo(numele) )
       if (numele.gt.0) ak_MHD%ak_comp_buo = 0.0d0
!
       end subroutine alloc_comp_buo_coef_ele
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
       subroutine dealloc_velo_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_velo )
!
       end subroutine dealloc_velo_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_temp_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_temp )
!
       end subroutine dealloc_temp_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_magne_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_magne )
!
       end subroutine dealloc_magne_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_dscalar_diff_MHD_AMG(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_d_composit )
!
       end subroutine dealloc_dscalar_diff_MHD_AMG
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_buoyancy_coef_ele(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_buo )
!
       end subroutine dealloc_buoyancy_coef_ele
!
! ----------------------------------------------------------------------
!
       subroutine dealloc_comp_buo_coef_ele(ak_MHD)
!
       type(coefs_4_MHD_type), intent(inout) :: ak_MHD
!
       deallocate( ak_MHD%ak_comp_buo )
!
       end subroutine dealloc_comp_buo_coef_ele
!
! ----------------------------------------------------------------------
!
      end module t_material_property
