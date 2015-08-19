!>@file  t_surface_group_geometry.f90
!!       module t_surface_group_geometry
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!>   @brief Structure of geometry data for surface group
!>@n   substitution of 
!>@n        (module m_surface_group_geometry)
!>@n        (module m_int_surface_data)
!!
!!@verbatim
!!      subroutine alloc_surf_grp_type_geom(num_item, sf_grp_v)
!!      subroutine alloc_vectors_surf_grp_type                          &
!!     &         (num_grp, num_item, sf_grp_v)
!!      subroutine alloc_normal_sf_grp_type_sph(num_item, sf_grp_v)
!!      subroutine alloc_normal_sf_grp_type_cyl(num_item, sf_grp_v)
!!         type(surface_group_data), intent(in) :: sf_grp
!!         type(surface_group_geometry), intent(inout) :: sf_grp_v
!!
!!      subroutine dealloc_surf_grp_type_geom(sf_grp_v)
!!      subroutine dealloc_vectors_surf_grp_type(sf_grp_v)
!!      subroutine dealloc_normal_sf_grp_type_sph(sf_grp_v)
!!      subroutine dealloc_normal_sf_grp_type_cyl(sf_grp_v)
!!         type(surface_group_geometry), intent(inout) :: sf_grp_v
!!
!!
!!      subroutine alloc_type_int_surf_data(nnod_4_surf,                &
!!     &          sf_grp, sf_grp_int)
!!      subroutine dealloc_type_int_surf_data(sf_grp_int)
!!@endverbatim
!
      module t_surface_group_geometry
!
      use m_precision
!
      implicit  none
!
!>   Structure of geometry data for surface group
      type surface_group_geometry
!>   position of surface group items
        real(kind=kreal),   pointer :: x_sf_grp(:,:)
!
!>   radius of surface group items
        real(kind=kreal),   pointer :: r_sf_grp(:)
!>   colatitude of surface group items
        real(kind=kreal),   pointer :: theta_sf_grp(:)
!>   longitude of surface group items
        real(kind=kreal),   pointer :: phi_sf_grp(:)
!>   cylindrical radius of surface group items
        real(kind=kreal),   pointer :: s_sf_grp(:)
!>   1 / r_sf_grp
        real(kind=kreal),   pointer :: ar_sf_grp(:)
!>   1 / s_sf_grp
        real(kind=kreal),   pointer :: as_sf_grp(:)
!
!
!>   normal vector of surface group items
        real(kind=kreal),   pointer :: vnorm_sf_grp(:,:)
!>   normal vector of surface group items (spherical coordinate)
        real(kind=kreal),   pointer :: vnorm_sf_grp_sph(:,:)
!>   normal vector of surface group items (cylindrical coordinate)
        real(kind=kreal),   pointer :: vnorm_sf_grp_cyl(:,:)
!>   area of surface group items
        real(kind=kreal),   pointer :: area_sf_grp(:)
!>   1 / area_sf_grp
        real(kind=kreal),   pointer :: a_area_sf_grp(:)
!
!>   total area of each surface group
        real(kind=kreal),   pointer :: tot_area_sf_grp(:)
      end type surface_group_geometry
!
!
!>   Structure of work area for integration over surface group
      type surf_grp_geom_4_fem_int
!>   vector data on surface group item
        real (kind=kreal), pointer :: vect_sf(:,:)
!
!>  position on surface group item
        real (kind=kreal), pointer :: xe_sf(:,:,:)
!>  element width on surface group
        real (kind=kreal), pointer :: dxe_sf(:,:,:)
      end type surf_grp_geom_4_fem_int
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_surf_grp_type_geom(num_item, sf_grp_v)
!
      integer(kind = kint), intent(in) :: num_item
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      allocate(sf_grp_v%x_sf_grp    (num_item,3) )
      allocate(sf_grp_v%r_sf_grp    (num_item)   )
      allocate(sf_grp_v%theta_sf_grp(num_item)   )
      allocate(sf_grp_v%phi_sf_grp  (num_item)   )
      allocate(sf_grp_v%s_sf_grp    (num_item)   )
      allocate(sf_grp_v%ar_sf_grp   (num_item)   )
      allocate(sf_grp_v%as_sf_grp   (num_item)   )
!
      if( num_item .gt. 0) then
        sf_grp_v%x_sf_grp =     0.0d0
!
        sf_grp_v%r_sf_grp =     0.0d0
        sf_grp_v%theta_sf_grp = 0.0d0
        sf_grp_v%phi_sf_grp =   0.0d0
        sf_grp_v%s_sf_grp =     0.0d0
        sf_grp_v%ar_sf_grp =    0.0d0
        sf_grp_v%as_sf_grp =    0.0d0
      end if
!
      end subroutine alloc_surf_grp_type_geom
!
! -----------------------------------------------------------------------
!
      subroutine alloc_vectors_surf_grp_type                            &
     &         (num_grp, num_item, sf_grp_v)
!
      integer(kind = kint), intent(in) :: num_grp, num_item
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      allocate ( sf_grp_v%vnorm_sf_grp(num_item,3)   )
!
      allocate ( sf_grp_v%tot_area_sf_grp(num_grp) )
      allocate ( sf_grp_v%area_sf_grp  (num_item)   )
      allocate ( sf_grp_v%a_area_sf_grp(num_item)   )
!
      if(num_grp .gt. 0) sf_grp_v%tot_area_sf_grp = 0.0d0
      if(num_item .gt. 0) then
        sf_grp_v%vnorm_sf_grp =  0.0d0
        sf_grp_v%area_sf_grp =   0.0d0
        sf_grp_v%a_area_sf_grp = 0.0d0
      end if
!
      end subroutine alloc_vectors_surf_grp_type
!
! -----------------------------------------------------------------------
!
      subroutine alloc_normal_sf_grp_type_sph(num_item, sf_grp_v)
!
      integer(kind = kint), intent(in) :: num_item
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      allocate( sf_grp_v%vnorm_sf_grp_sph(num_item,3) )
      if(num_item .gt. 0) sf_grp_v%vnorm_sf_grp_sph = 0.0d0
!
      end subroutine alloc_normal_sf_grp_type_sph
!
!-----------------------------------------------------------------------
!
      subroutine alloc_normal_sf_grp_type_cyl(num_item, sf_grp_v)
!
      integer(kind = kint), intent(in) :: num_item
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      allocate( sf_grp_v%vnorm_sf_grp_cyl(num_item,3) )
      if(num_item .gt. 0) sf_grp_v%vnorm_sf_grp_cyl = 0.0d0
!
      end subroutine alloc_normal_sf_grp_type_cyl
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_surf_grp_type_geom(sf_grp_v)
!
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      deallocate(sf_grp_v%x_sf_grp)
      deallocate(sf_grp_v%r_sf_grp    , sf_grp_v%ar_sf_grp )
      deallocate(sf_grp_v%theta_sf_grp, sf_grp_v%phi_sf_grp)
      deallocate(sf_grp_v%s_sf_grp    , sf_grp_v%as_sf_grp )
!
      end subroutine dealloc_surf_grp_type_geom
!
! -----------------------------------------------------------------------
!
       subroutine dealloc_vectors_surf_grp_type(sf_grp_v)
!
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
       deallocate ( sf_grp_v%vnorm_sf_grp )
!
       deallocate ( sf_grp_v%tot_area_sf_grp  )
       deallocate ( sf_grp_v%area_sf_grp,   sf_grp_v%a_area_sf_grp)
!
      end subroutine dealloc_vectors_surf_grp_type
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_normal_sf_grp_type_sph(sf_grp_v)
!
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      deallocate( sf_grp_v%vnorm_sf_grp_sph )
!
      end subroutine dealloc_normal_sf_grp_type_sph
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_normal_sf_grp_type_cyl(sf_grp_v)
!
      type(surface_group_geometry), intent(inout) :: sf_grp_v
!
!
      deallocate( sf_grp_v%vnorm_sf_grp_cyl )
!
      end subroutine dealloc_normal_sf_grp_type_cyl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_type_int_surf_data(nnod_4_surf,                  &
     &          sf_grp, sf_grp_int)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surf_grp_geom_4_fem_int), intent(inout) :: sf_grp_int
!
      allocate( sf_grp_int%vect_sf(sf_grp%num_item,3) )
      allocate( sf_grp_int%xe_sf(sf_grp%num_item,4,nnod_4_surf) )
      allocate( sf_grp_int%dxe_sf(sf_grp%num_item,4,nnod_4_surf) )
!
      if (sf_grp%num_item .gt. 0) then
        sf_grp_int%vect_sf = 0.0d0
        sf_grp_int%xe_sf =   0.0d0
        sf_grp_int%dxe_sf = 0.0d0
      end if
!
      end subroutine alloc_type_int_surf_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_type_int_surf_data(sf_grp_int)
!
      type(surf_grp_geom_4_fem_int), intent(inout) :: sf_grp_int
!
      deallocate( sf_grp_int%vect_sf )
      deallocate( sf_grp_int%xe_sf )
      deallocate( sf_grp_int%dxe_sf )
!
      end subroutine dealloc_type_int_surf_data
!
! -----------------------------------------------------------------------
!
      end module t_surface_group_geometry
