!>@file   t_surface_data.f90
!!@brief  module t_surface_data
!!
!!@author  H. Matsui
!!@date Programmed in 2008
!
!> @brief structure of surface data (geometry and connectivity)
!!
!!@verbatim
!!      subroutine allocate_inod_in_surf(surf)
!!      subroutine alloc_surface_connect(surf, nele)
!!      subroutine alloc_global_surf_id(surf)
!!      subroutine alloc_interior_surf(surf)
!!      subroutine alloc_ext_surface(surf)
!!      subroutine alloc_iso_surface(surf)
!!      subroutine alloc_surface_geometory(surf)
!!      subroutine alloc_normal_vector(surf)
!!      subroutine alloc_surf_param_smp(surf)
!!      subroutine alloc_element_4_surface(surf)
!!
!!      subroutine dealloc_inod_in_surf(surf)
!!      subroutine dealloc_surface_connect(surf)
!!      
!!      subroutine dealloc_global_surf_id(surf)
!!      subroutine dealloc_interior_surf(nele)
!!      subroutine deallocate_ext_surface_type(surf)
!!      subroutine deallocate_iso_surface_type(surf)
!!      subroutine dealloc_surface_geometory(surf)
!!      subroutine dealloc_normal_vector(surf)
!!      subroutine dealloc_surf_param_smp(surf)
!!      subroutine dealloc_element_4_surface(surf)
!!        integer(kind = kint), intent(in) :: nele
!!        type(surface_data), intent(inout) :: surf
!!@endverbatim
!
      module t_surface_data
!
      use m_precision
!
      implicit  none
!
!>      structure of surface data (geometry and connectivity)
      type surface_data
!>       number of surface on local PE
        integer( kind=kint )  ::  numsurf
!>       number of nodes in each surface
        integer(kind=kint) :: nnod_4_surf
!>       number of external sueface
        integer(kind=kint) ::  numsurf_ext
!>       number of isolated sueface
        integer(kind=kint) ::  numsurf_iso
!
!>   local index for surface on each element
        integer (kind=kint), allocatable :: node_on_sf(:,:)
!>   local index for opposite surface on each element
        integer (kind=kint), allocatable :: node_on_sf_n(:,:)
!
!>     smp stack for surface on  local PE
        integer( kind=kint ), allocatable :: istack_surf_smp(:)
!>     maximum number of smp surface on local PE
        integer( kind=kint )  ::  max_surf_smp
!
!>       global surface id (where i:surface id)
        integer(kind=kint_gl), allocatable  ::  isurf_global(:)
!>       integer flag for interior surface 1...interior, 0...exterior
        integer(kind = kint), allocatable :: interior_surf(:)
!
!>        surface connectivity ie_surf(i:surface ID,j:surface index)
        integer(kind=kint), allocatable  :: ie_surf(:,:)
!
!>   surface ID for element surface isf_4_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          Positive: normal direction negative: reverse direction
        integer(kind=kint), allocatable  :: isf_4_ele(:,:)
!>   rotation ID for element surface isf_rot_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          0: normal direction  1-4: rotation flag for reverse surface
        integer(kind=kint), allocatable  :: isf_rot_ele(:,:)
!
!>     external surface list
        integer(kind=kint), allocatable  ::  isf_external(:)
!>     isolated surface list
        integer(kind=kint), allocatable  ::  isf_isolate(:)
!
!>   belonged element for surface(surface#,face#,
!>                                1:element or 2:local surface)
        integer(kind=kint), allocatable  :: iele_4_surf(:,:,:)
!
!>       position of center of surface
        real(kind=kreal)  , allocatable  :: x_surf(:,:)
! 
!>       area of each surface
        real (kind=kreal), allocatable :: area_surf(:)
!>       1 / area_surf
        real (kind=kreal), allocatable :: a_area_surf(:)
!
!>       normal vector for sach surface
        real (kind=kreal), allocatable :: vnorm_surf(:,:)
      end type surface_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_inod_in_surf(surf)
!
      use m_geometry_constants
!
      type(surface_data), intent(inout) :: surf
!
!
       allocate ( surf%node_on_sf  (surf%nnod_4_surf,nsurf_4_ele) )
       allocate ( surf%node_on_sf_n(surf%nnod_4_surf,nsurf_4_ele) )
!
       surf%node_on_sf =   0
       surf%node_on_sf_n = 0
!
       end subroutine allocate_inod_in_surf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surface_connect(surf, nele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_4_ele(nele,nsurf_4_ele) )
      allocate( surf%isf_rot_ele(nele,nsurf_4_ele) )
      allocate( surf%ie_surf(surf%numsurf,surf%nnod_4_surf) )
!
      if (surf%numsurf.gt. 0) then
!$omp parallel workshare
        surf%isf_4_ele =     0
        surf%isf_rot_ele =  -1
!$omp end parallel workshare
!$omp parallel workshare
        surf%ie_surf =       0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_surface_connect
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_global_surf_id(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate(surf%isurf_global(surf%numsurf))
      if (surf%numsurf .le. 0) return
!$omp parallel workshare
      surf%isurf_global(1:surf%numsurf) =  0
!$omp end parallel workshare
!
      end subroutine alloc_global_surf_id
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_interior_surf(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate(surf%interior_surf(surf%numsurf))
      if (surf%numsurf .le. 0) return
!$omp parallel workshare
      surf%interior_surf = 0
!$omp end parallel workshare
!
      end subroutine alloc_interior_surf
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_ext_surface(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_external(surf%numsurf_ext) )
      if (surf%numsurf_ext .gt. 0) surf%isf_external = 0
!
      end subroutine alloc_ext_surface
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iso_surface(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_isolate(surf%numsurf_iso) )
      if (surf%numsurf_iso .gt. 0) surf%isf_isolate = 0
!
      end subroutine alloc_iso_surface
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_surface_geometory(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%x_surf(surf%numsurf,3) )
!
      if(surf%numsurf .gt. 0) then
!$omp parallel workshare
        surf%x_surf(1:surf%numsurf,1:3) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_surface_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_normal_vector(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%area_surf(surf%numsurf) )
      allocate( surf%a_area_surf(surf%numsurf) )
      allocate( surf%vnorm_surf(surf%numsurf,3) )
!
      if ( surf%numsurf .gt. 0 ) then
!$omp parallel workshare
        surf%area_surf(1:surf%numsurf) =     0.0d0
        surf%a_area_surf(1:surf%numsurf) =   0.0d0
        surf%vnorm_surf(1:surf%numsurf,1) =  0.0d0
        surf%vnorm_surf(1:surf%numsurf,2) =  0.0d0
        surf%vnorm_surf(1:surf%numsurf,3) =  0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_normal_vector
!
!-----------------------------------------------------------------------
!
      subroutine alloc_surf_param_smp(surf)
!
      use m_machine_parameter
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%istack_surf_smp(0:np_smp))
      surf%istack_surf_smp = 0
!
      end subroutine alloc_surf_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine alloc_element_4_surface(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      allocate(surf%iele_4_surf(surf%numsurf,2,2) )
      if ( surf%numsurf .gt. 0 ) surf%iele_4_surf = 0
!
      end subroutine alloc_element_4_surface
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_inod_in_surf(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      if(allocated(surf%node_on_sf) .eqv. .FALSE.) return
      deallocate (surf%node_on_sf, surf%node_on_sf_n)
!
      end subroutine dealloc_inod_in_surf
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surface_connect(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isf_4_ele) .eqv. .FALSE.) return
      deallocate( surf%isf_4_ele     )
      deallocate( surf%isf_rot_ele   )
      deallocate( surf%ie_surf       )
!
      end subroutine dealloc_surface_connect
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_global_surf_id(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isurf_global) .eqv. .FALSE.) return
      deallocate(surf%isurf_global)
!
      end subroutine dealloc_global_surf_id
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_interior_surf(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%interior_surf) .eqv. .FALSE.) return
      deallocate(surf%interior_surf)
!
      end subroutine dealloc_interior_surf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ext_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isf_external) .eqv. .FALSE.) return
      deallocate( surf%isf_external )
!
      end subroutine deallocate_ext_surface_type
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%isf_isolate) .eqv. .FALSE.) return
      deallocate( surf%isf_isolate )
!
      end subroutine deallocate_iso_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surface_geometory(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%x_surf) .eqv. .FALSE.) return
      deallocate( surf%x_surf )
!
      end subroutine dealloc_surface_geometory
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_normal_vector(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%area_surf) .eqv. .FALSE.) return
      deallocate( surf%area_surf )
      deallocate( surf%a_area_surf )
      deallocate( surf%vnorm_surf )
!
      end subroutine dealloc_normal_vector
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_surf_param_smp(surf)
!
      type(surface_data), intent(inout) :: surf
!
      if(allocated(surf%istack_surf_smp) .eqv. .FALSE.) return
      deallocate( surf%istack_surf_smp )
!
      end subroutine dealloc_surf_param_smp
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_element_4_surface(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      if(allocated(surf%iele_4_surf) .eqv. .FALSE.) return
      deallocate(surf%iele_4_surf)
!
      end subroutine dealloc_element_4_surface
!
!-----------------------------------------------------------------------
!
      end module t_surface_data
