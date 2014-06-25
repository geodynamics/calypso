!t_surface_data.f90
!      module t_surface_data
!
!> @brief structure of surface data (geometry and connectivity)
!
!> Substitution of
!> @n      (module m_geometry_parameter)
!> @n      (module m_geometry_data)
!> @n      (module m_surface_geometry_data)
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_inod_in_surf_type(surf)
!      subroutine allocate_surface_connect_type(surf, nele)
!      subroutine allocate_ext_surface_type(surf)
!      subroutine allocate_iso_surface_type(surf)
!      subroutine allocate_surface_geom_type(surf)
!      subroutine allocate_normal_vect_type(surf)
!      subroutine allocate_normal_vect_sph_type(surf)
!      subroutine allocate_normal_vect_cyl_type(surf)
!      subroutine allocate_surf_param_smp_type(surf)
!      subroutine alloc_ele_4_surf_type(surf)
!
!      subroutine deallocate_inod_in_surf_type(surf)
!      subroutine deallocate_surface_connect_type(surf)
!      subroutine deallocate_ext_surface_type(surf)
!      subroutine deallocate_iso_surface_type(surf)
!      subroutine deallocate_surface_geom_type(surf)
!      subroutine deallocate_normal_vect_type(surf)
!      subroutine deallocate_normal_vect_sph_type(surf)
!      subroutine deallocate_normal_vect_cyl_type(surf)
!      subroutine deallocate_surf_param_smp_type(surf)
!      subroutine dealloc_ele_4_surf_type(surf)
!        integer(kind = kint), intent(in) :: nele
!        type(surface_data), intent(inout) :: surf
!
!      subroutine link_new_surf_connect_type(surf_org, surf)
!        type(surface_data), intent(in) :: surf_org
!        type(surface_data), intent(inout) :: surf
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
!>       number of internal surface on local PE
        integer( kind=kint )  ::  internal_surf
!>       number of nodes in each surface
        integer(kind=kint) :: nnod_4_surf
!>       number of external sueface
        integer(kind=kint) ::  numsurf_ext
!>       number of isolated sueface
        integer(kind=kint) ::  numsurf_iso
!
!>   local index for surface on each element
        integer (kind=kint), pointer :: node_on_sf(:,:)
!>   local index for opposite surface on each element
        integer (kind=kint), pointer :: node_on_sf_n(:,:)
!
!>     smp stack for surface on  local PE
        integer( kind=kint ), pointer :: istack_surf_smp(:)
!>     maximum number of smp surface on local PE
        integer( kind=kint )  ::  max_surf_smp
!>     maximum number of smp internal surface on local PE
        integer( kind=kint )  ::  max_internal_surf_smp
!
!>       global surface id (where i:surface id)
        integer(kind=kint), pointer  ::  isurf_global(:)
!
!>   surface connectivity ie_surf(i:surface ID,j:surface index)
        integer(kind=kint), pointer  :: ie_surf(:,:)
!
!>   surface ID for element surface isf_4_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          Positive: normal direction negative: reverse direction
        integer(kind=kint), pointer  :: isf_4_ele(:,:)
!>   rotation ID for element surface isf_rot_ele(:,:)
!>          ...i:element ID, j:surface ID
!>@n          0: normal direction  1-4: rotation flag for reverse surface
        integer(kind=kint), pointer  :: isf_rot_ele(:,:)
!
!>     external surface list
        integer(kind=kint), pointer  ::  isf_external(:)
!>     isolated surface list
        integer(kind=kint), pointer  ::  isf_isolate(:)
!
!>   belonged element for surface(surface#,face#,
!>                                1:element or 2:local surface)
        integer(kind=kint), pointer  :: iele_4_surf(:,:,:)
!
!>  integer flag for interior surface 1...interior, 0...exterior
        integer(kind = kint), pointer :: interior_surf(:)
!
!>       position of center of surface
        real(kind=kreal)  , pointer  :: x_surf(:,:)
!>       distance from the center of surface
        real(kind=kreal)  , pointer  :: r_surf(:)
!>       1/r_surf
        real(kind=kreal)  , pointer  :: ar_surf(:)
!>       longitude of center of surface
        real(kind=kreal)  , pointer  :: phi_surf(:)
!>       colatitude of center of surface
        real(kind=kreal)  , pointer  :: theta_surf(:)
!>       cylindorical radius of center of surface
        real(kind=kreal)  , pointer  :: s_surf(:)
!>       1 / s_surf
        real(kind=kreal)  , pointer  :: as_surf(:)
! 
!>       area of each surface
        real (kind=kreal), pointer :: area_surf(:)
!>       1 / area_surf
        real (kind=kreal), pointer :: a_area_surf(:)
!
!>       normal vector for sach surface
        real (kind=kreal), pointer :: vnorm_surf(:,:)
!
!>       normal vector for sach surface (spherical coordinate)
        real (kind=kreal), pointer :: vnorm_surf_sph(:,:)
!>       normal vector for sach surface (cylindrical coordinate)
        real (kind=kreal), pointer :: vnorm_surf_cyl(:,:)
      end type surface_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_inod_in_surf_type(surf)
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
       end subroutine allocate_inod_in_surf_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surface_connect_type(surf, nele)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nele
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_4_ele(nele,nsurf_4_ele) )
      allocate( surf%isf_rot_ele(nele,nsurf_4_ele) )
      allocate( surf%ie_surf(surf%numsurf,surf%nnod_4_surf) )
      allocate( surf%isurf_global(surf%numsurf) )
      allocate( surf%interior_surf(surf%numsurf) )
!
      if (surf%numsurf.gt. 0) then
        surf%isf_4_ele =     0
        surf%isf_rot_ele =  -1
        surf%ie_surf =       0
        surf%isurf_global =  0
        surf%interior_surf = 0
      end if
!
      end subroutine allocate_surface_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ext_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_external(surf%numsurf_ext) )
      if (surf%numsurf_ext .gt. 0) surf%isf_external = 0
!
      end subroutine allocate_ext_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_iso_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%isf_isolate(surf%numsurf_iso) )
      if (surf%numsurf_iso .gt. 0) surf%isf_isolate = 0
!
      end subroutine allocate_iso_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surface_geom_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%x_surf(surf%numsurf,3) )
!
      allocate( surf%r_surf(surf%numsurf) )
      allocate( surf%ar_surf(surf%numsurf) )
      allocate( surf%phi_surf(surf%numsurf) )
      allocate( surf%theta_surf(surf%numsurf) )
!
      allocate( surf%s_surf(surf%numsurf) )
      allocate( surf%as_surf(surf%numsurf) )
!
      if ( surf%numsurf .gt. 0 ) then
        surf%x_surf =      0.0d0
!
        surf%r_surf =      0.0d0
        surf%ar_surf =     0.0d0
        surf%phi_surf =    0.0d0
        surf%theta_surf =  0.0d0
!
        surf%s_surf =      0.0d0
        surf%as_surf =     0.0d0
      end if
!
      end subroutine allocate_surface_geom_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_normal_vect_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%area_surf(surf%numsurf) )
      allocate( surf%a_area_surf(surf%numsurf) )
      allocate( surf%vnorm_surf(surf%numsurf,3) )
!
      if ( surf%numsurf .gt. 0 ) then
        surf%area_surf =   0.0d0
        surf%a_area_surf = 0.0d0
        surf%vnorm_surf =  0.0d0
      end if
!
      end subroutine allocate_normal_vect_type
!
! ------------------------------------------------------
!
      subroutine allocate_normal_vect_sph_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%vnorm_surf_sph(surf%numsurf,3) )
      if ( surf%numsurf .gt. 0 ) surf%vnorm_surf_sph =  0.0d0
!
      end subroutine allocate_normal_vect_sph_type
!
! ------------------------------------------------------
!
      subroutine allocate_normal_vect_cyl_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%vnorm_surf_cyl(surf%numsurf,3) )
      if ( surf%numsurf .gt. 0 ) surf%vnorm_surf_cyl =  0.0d0
!
      end subroutine allocate_normal_vect_cyl_type
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_surf_param_smp_type(surf)
!
      use m_machine_parameter
!
      type(surface_data), intent(inout) :: surf
!
      allocate( surf%istack_surf_smp(0:np_smp))
      surf%istack_surf_smp = 0
!
      end subroutine allocate_surf_param_smp_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_ele_4_surf_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      allocate(surf%iele_4_surf(surf%numsurf,2,2) )
      if ( surf%numsurf .gt. 0 ) surf%iele_4_surf = 0
!
      end subroutine alloc_ele_4_surf_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_inod_in_surf_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
       deallocate (surf%node_on_sf, surf%node_on_sf_n)
!
       end subroutine deallocate_inod_in_surf_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surface_connect_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      deallocate( surf%isf_4_ele     )
      deallocate( surf%isf_rot_ele   )
      deallocate( surf%ie_surf       )
      deallocate( surf%isurf_global  )
      deallocate( surf%interior_surf )
!
      end subroutine deallocate_surface_connect_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ext_surface_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
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
      deallocate( surf%isf_isolate )
!
      end subroutine deallocate_iso_surface_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surface_geom_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      deallocate( surf%x_surf )
!
      deallocate( surf%r_surf )
      deallocate( surf%ar_surf )
      deallocate( surf%phi_surf )
      deallocate( surf%theta_surf )
!
      deallocate( surf%s_surf )
      deallocate( surf%as_surf )
!
      end subroutine deallocate_surface_geom_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_normal_vect_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      deallocate( surf%area_surf )
      deallocate( surf%a_area_surf )
      deallocate( surf%vnorm_surf )
!
      end subroutine deallocate_normal_vect_type
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vect_sph_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      deallocate( surf%vnorm_surf_sph )
!
      end subroutine deallocate_normal_vect_sph_type
!
! ------------------------------------------------------
!
      subroutine deallocate_normal_vect_cyl_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      deallocate( surf%vnorm_surf_cyl )
!
      end subroutine deallocate_normal_vect_cyl_type
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_surf_param_smp_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
      deallocate( surf%istack_surf_smp )
!
      end subroutine deallocate_surf_param_smp_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_4_surf_type(surf)
!
      type(surface_data), intent(inout) :: surf
!
!
      deallocate(surf%iele_4_surf)
!
      end subroutine dealloc_ele_4_surf_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine link_new_surf_connect_type(surf_org, surf)
!
      type(surface_data), intent(in) :: surf_org
      type(surface_data), intent(inout) :: surf
!
!
      surf%node_on_sf =>   surf_org%node_on_sf
      surf%node_on_sf_n => surf_org%node_on_sf_n
!
      surf%numsurf =     surf_org%numsurf
      surf%nnod_4_surf = surf_org%nnod_4_surf
!
      surf%ie_surf =>       surf_org%ie_surf
      surf%isf_4_ele =>     surf_org%isf_4_ele
      surf%interior_surf => surf_org%interior_surf
!
      surf%istack_surf_smp => surf_org%istack_surf_smp
      surf%max_surf_smp =     surf_org%max_surf_smp
!
      end subroutine link_new_surf_connect_type
!
! ----------------------------------------------------------------------
!
      end module t_surface_data
