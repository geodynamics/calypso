!>@file   t_shape_functions.f90
!!@brief  module t_shape_functions
!!
!!@author H. Matsui
!!@date Programmed on 2001
!!
!>@brief  arrays for shape functions in element coordinate
!!
!!@verbatim
!!      subroutine alloc_3d_gauss_point_id(g_FEM, spf_3d)
!!      subroutine alloc_2d_gauss_point_id(g_FEM, spf_2d)
!!      subroutine alloc_1d_gauss_point_id(g_FEM, spf_1d)
!!      subroutine alloc_integrate_parameters
!!
!!      subroutine dealloc_gauss_point_id
!!      subroutine dealloc_3d_gauss_point_id(spf_3d)
!!      subroutine dealloc_2d_gauss_point_id(spf_2d)
!!      subroutine dealloc_1d_gauss_point_id(spf_1d)
!!
!!      subroutine alloc_vol_shape_func(nnod_4_ele, g_FEM, spf_3d)
!!      subroutine alloc_shape_func_infty                               &
!!     &         (nnod_4_ele, nsurf_4_ele, g_FEM, spf_inf)
!!      subroutine alloc_surf_shape_func(nnod_4_sf, g_FEM, spf_2d)
!!      subroutine alloc_edge_shape_func(nnod_4_ed, g_FEM, spf_1d)
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(volume_shape_function), intent(inout) :: spf_3d
!!        type(infty_shape_function), intent(inout) :: spf_inf
!!        type(surface_shape_function), intent(inout) :: spf_2d
!!        type(edge_shape_function), intent(inout) :: spf_1d
!!@endverbatim
!
      module t_shape_functions
!
      use m_precision
      use t_fem_gauss_int_coefs
!
      implicit  none
!
!
      type volume_shape_function
        integer(kind = kint), allocatable :: l_int(:,:,:)
!
        real(kind=kreal), allocatable :: xi(:)
        real(kind=kreal), allocatable :: ei(:)
        real(kind=kreal), allocatable :: zi(:)
!
        real(kind=kreal), allocatable :: dnxi(:,:)
        real(kind=kreal), allocatable :: dnei(:,:)
        real(kind=kreal), allocatable :: dnzi(:,:)
      end type volume_shape_function
!
      type infty_shape_function
        real(kind=kreal), allocatable :: dnxi_inf(:,:,:)
        real(kind=kreal), allocatable :: dnei_inf(:,:,:)
        real(kind=kreal), allocatable :: dnzi_inf(:,:,:)
      end type infty_shape_function
!
      type surface_shape_function
        integer (kind=kint), allocatable :: l_int(:,:,:)
!
        real(kind=kreal), allocatable :: xi(:)
        real(kind=kreal), allocatable :: ei(:)
!
        real(kind=kreal), allocatable :: dnxi_sf(:,:)
        real(kind=kreal), allocatable :: dnei_sf(:,:)
      end type surface_shape_function
!
      type edge_shape_function
        integer (kind=kint), allocatable :: l_int(:,:,:)
        real (kind=kreal), allocatable :: xi(:)
!
        real (kind=kreal), allocatable :: dnxi_ed(:,:)
      end type edge_shape_function
!
!
      type shape_finctions_at_points
        type(volume_shape_function)  :: spf_3d
        type(surface_shape_function)  :: spf_2d
        type(edge_shape_function)  :: spf_1d
      end type shape_finctions_at_points
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_3d_gauss_point_id(g_FEM, spf_3d)
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      allocate(spf_3d%l_int(3,g_FEM%maxtot_int_3d,g_FEM%max_int_point))
      allocate(spf_3d%xi(g_FEM%maxtot_int_3d) )
      allocate(spf_3d%ei(g_FEM%maxtot_int_3d) )
      allocate(spf_3d%zi(g_FEM%maxtot_int_3d) )
!
      spf_3d%l_int = 0
      spf_3d%xi = 0.0d0
      spf_3d%ei = 0.0d0
      spf_3d%zi = 0.0d0
!
      end subroutine alloc_3d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine alloc_2d_gauss_point_id(g_FEM, spf_2d)
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      allocate(spf_2d%l_int(2,g_FEM%maxtot_int_2d,g_FEM%max_int_point))
      allocate(spf_2d%xi(g_FEM%maxtot_int_2d) )
      allocate(spf_2d%ei(g_FEM%maxtot_int_2d) )
      spf_2d%l_int = 0
      spf_2d%xi = 0.0d0
      spf_2d%ei = 0.0d0
!
      end subroutine alloc_2d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine alloc_1d_gauss_point_id(g_FEM, spf_1d)
!
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      allocate(spf_1d%l_int(1,g_FEM%maxtot_int_1d,g_FEM%max_int_point))
      allocate(spf_1d%xi(g_FEM%maxtot_int_1d))
      spf_1d%l_int = 0
      spf_1d%xi = 0.0d0
!
      end subroutine alloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_gauss_point_id(spf_3d, spf_2d, spf_1d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
      type(surface_shape_function), intent(inout) :: spf_2d
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      call dealloc_3d_gauss_point_id(spf_3d)
      call dealloc_2d_gauss_point_id(spf_2d)
      call dealloc_1d_gauss_point_id(spf_1d)
!
      end subroutine dealloc_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_3d_gauss_point_id(spf_3d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      if(allocated(spf_3d%l_int) .eqv. .FALSE.) return
      deallocate(spf_3d%l_int)
      deallocate(spf_3d%xi, spf_3d%ei, spf_3d%zi)
!
      end subroutine dealloc_3d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_2d_gauss_point_id(spf_2d)
!
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      if(allocated(spf_2d%l_int) .eqv. .FALSE.) return
      deallocate(spf_2d%l_int)
      deallocate(spf_2d%xi, spf_2d%ei)
!
      end subroutine dealloc_2d_gauss_point_id
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_1d_gauss_point_id(spf_1d)
!
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      if(allocated(spf_1d%l_int) .eqv. .FALSE.) return
      deallocate(spf_1d%l_int)
      deallocate(spf_1d%xi)
!
      end subroutine dealloc_1d_gauss_point_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_vol_shape_func(nnod_4_ele, g_FEM, spf_3d)
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      allocate(spf_3d%dnxi(nnod_4_ele,g_FEM%maxtot_int_3d) )
      allocate(spf_3d%dnei(nnod_4_ele,g_FEM%maxtot_int_3d) )
      allocate(spf_3d%dnzi(nnod_4_ele,g_FEM%maxtot_int_3d) )
!
       spf_3d%dnxi = 0.0d0
       spf_3d%dnei = 0.0d0
       spf_3d%dnzi = 0.0d0
!
      end subroutine alloc_vol_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine alloc_shape_func_infty                                 &
     &         (nnod_4_ele, nsurf_4_ele, g_FEM, spf_inf)
!
      integer(kind = kint), intent(in) :: nnod_4_ele, nsurf_4_ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(infty_shape_function), intent(inout) :: spf_inf
!
      integer(kind = kint) :: ntot_int
!
!
      ntot_int = g_FEM%maxtot_int_3d
      allocate(spf_inf%dnxi_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
      allocate(spf_inf%dnei_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
      allocate(spf_inf%dnzi_inf(nnod_4_ele,nsurf_4_ele,ntot_int) )
! 
      spf_inf%dnxi_inf = 0.0d0
      spf_inf%dnei_inf = 0.0d0
      spf_inf%dnzi_inf = 0.0d0
!
      end subroutine alloc_shape_func_infty
!
! ----------------------------------------------------------------------
!
      subroutine alloc_surf_shape_func(nnod_4_sf, g_FEM, spf_2d)
!
      integer(kind = kint), intent(in) :: nnod_4_sf
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      allocate(spf_2d%dnxi_sf(nnod_4_sf,g_FEM%maxtot_int_2d) )
      allocate(spf_2d%dnei_sf(nnod_4_sf,g_FEM%maxtot_int_2d) )
!
      spf_2d%dnxi_sf = 0.0d0
      spf_2d%dnei_sf = 0.0d0
!
      end subroutine alloc_surf_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine alloc_edge_shape_func(nnod_4_ed, g_FEM, spf_1d)
!
      integer(kind = kint), intent(in) :: nnod_4_ed
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      allocate(spf_1d%dnxi_ed(nnod_4_ed,g_FEM%maxtot_int_1d) )
!
      spf_1d%dnxi_ed = 0.0d0
!
      end subroutine alloc_edge_shape_func
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_vol_shape_func(spf_3d)
!
      type(volume_shape_function), intent(inout) :: spf_3d
!
!
      if(allocated(spf_3d%dnxi) .eqv. .FALSE.) return
      deallocate(spf_3d%dnxi)
      deallocate(spf_3d%dnei)
      deallocate(spf_3d%dnzi)
! 
      end subroutine dealloc_vol_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_shape_func_infty(spf_inf)
!
      type(infty_shape_function), intent(inout) :: spf_inf
!
!
      if(allocated(spf_inf%dnxi_inf) .eqv. .FALSE.) return
      deallocate(spf_inf%dnxi_inf)
      deallocate(spf_inf%dnei_inf)
      deallocate(spf_inf%dnzi_inf)
! 
      end subroutine dealloc_shape_func_infty
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_surf_shape_func(spf_2d)
!
      type(surface_shape_function), intent(inout) :: spf_2d
!
!
      if(allocated(spf_2d%dnxi_sf) .eqv. .FALSE.) return
      deallocate(spf_2d%dnxi_sf)
      deallocate(spf_2d%dnei_sf)
! 
      end subroutine dealloc_surf_shape_func
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_edge_shape_func(spf_1d)
!
      type(edge_shape_function), intent(inout) :: spf_1d
!
!
      if(allocated(spf_1d%dnxi_ed) .eqv. .FALSE.) return
      deallocate(spf_1d%dnxi_ed)
! 
      end subroutine dealloc_edge_shape_func
!
! ----------------------------------------------------------------------
!
      end module   t_shape_functions
