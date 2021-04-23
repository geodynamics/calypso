!>@file  t_surface_group_normals.f90
!!       module t_surface_group_normals
!!
!!@author H. Matsui
!!@date   Programmed in Dec., 2008
!
!>@brief Structure of geometry data for surface group
!!
!!@verbatim
!!      subroutine alloc_vectors_surf_group                             &
!!     &         (num_grp, num_item, sf_grp_v)
!!      subroutine dealloc_vectors_surf_group(sf_grp_v)
!!         type(surface_group_data), intent(in) :: sf_grp
!!         type(surface_group_normals), intent(inout) :: sf_grp_v
!!
!!      subroutine pick_normal_of_surf_group                            &
!!     &         (ele, surf, edge, sf_grp, sf_grp_v)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data),    intent(in) :: edge
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_group_normals), intent(inout) :: sf_grp_v
!!@endverbatim
!
      module t_surface_group_normals
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
!>   Structure of geometry data for surface group
      type surface_group_normals
!>   normal vector of surface group items
        real(kind=kreal),   allocatable :: vnorm_sf_grp(:,:)
!>   area of surface group items
        real(kind=kreal),   allocatable :: area_sf_grp(:)
!>   1 / area_sf_grp
        real(kind=kreal),   allocatable :: a_area_sf_grp(:)
!
!>   total area of each surface group
        real(kind=kreal),   allocatable :: tot_area_sf_grp(:)
      end type surface_group_normals
!
      private :: pick_vect_by_surf_grp_w_side, pick_scalar_by_surf_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_vectors_surf_group                               &
     &         (num_grp, num_item, sf_grp_v)
!
      integer(kind = kint), intent(in) :: num_grp, num_item
      type(surface_group_normals), intent(inout) :: sf_grp_v
!
!
      allocate(sf_grp_v%vnorm_sf_grp(num_item,3))
!
      allocate(sf_grp_v%tot_area_sf_grp(num_grp))
      allocate(sf_grp_v%area_sf_grp(num_item)   )
      allocate(sf_grp_v%a_area_sf_grp(num_item) )
!
      if(num_grp .gt. 0) sf_grp_v%tot_area_sf_grp = 0.0d0
      if(num_item .gt. 0) then
        sf_grp_v%vnorm_sf_grp =  0.0d0
        sf_grp_v%area_sf_grp =   0.0d0
        sf_grp_v%a_area_sf_grp = 0.0d0
      end if
!
      end subroutine alloc_vectors_surf_group
!
! -----------------------------------------------------------------------
!
       subroutine dealloc_vectors_surf_group(sf_grp_v)
!
      type(surface_group_normals), intent(inout) :: sf_grp_v
!
!
       deallocate ( sf_grp_v%vnorm_sf_grp )
!
       deallocate ( sf_grp_v%tot_area_sf_grp  )
       deallocate ( sf_grp_v%area_sf_grp,   sf_grp_v%a_area_sf_grp)
!
      end subroutine dealloc_vectors_surf_group
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine pick_normal_of_surf_group                              &
     &         (ele, surf, edge, sf_grp, sf_grp_v)
!
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_group_connects
      use t_surface_group_table
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data),    intent(in) :: edge
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_normals), intent(inout) :: sf_grp_v
!
      type(surface_group_table) :: sf_grp_tbl
!
!
      call alloc_vectors_surf_group                                     &
     &   (sf_grp%num_grp, sf_grp%num_item, sf_grp_v)
!
      if (sf_grp%num_grp .le. 0) return
!
      call const_surface_group_table(ele, surf, edge,                   &
     &                               sf_grp, sf_grp_tbl)
!
      call pick_vect_by_surf_grp_w_side                                 &
     &   (sf_grp%num_grp, sf_grp%num_item, sf_grp%num_grp_smp,          &
     &    sf_grp%istack_grp_smp, sf_grp_tbl%isurf_grp,                  &
     &    surf%numsurf, surf%vnorm_surf, sf_grp_v%vnorm_sf_grp)
!
      call pick_scalar_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_tbl%isurf_grp, surf%numsurf, surf%area_surf,           &
     &    sf_grp_v%area_sf_grp)
      call pick_scalar_by_surf_grp(sf_grp%num_grp, sf_grp%num_item,     &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_tbl%isurf_grp, surf%numsurf, surf%a_area_surf,         &
     &    sf_grp_v%a_area_sf_grp)
      call dealloc_surf_item_sf_grp(sf_grp_tbl)
!
      end subroutine pick_normal_of_surf_group
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_vect_by_surf_grp_w_side(num_surf, num_surf_bc,    &
     &          num_surf_smp, isurf_grp_smp_stack, isurf_grp,           &
     &          numsurf, x_surf, x_sf_grp)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
      integer (kind = kint), intent(in) :: isurf_grp(num_surf_bc)
!
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf,3)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(num_surf_bc,3)
!
      integer (kind = kint) :: i_grp, ip, i, ist, ied, inum, isurf
      real(kind = kreal) :: side
!
!
      do i_grp = 1, num_surf
!
!$omp parallel do private(i,ist,ied,inum,isurf)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = isurf_grp_smp_stack(i-1) + 1
          ied = isurf_grp_smp_stack(i)
!
!poption parallel
          do inum = ist, ied
            isurf = abs( isurf_grp(inum) )
            side =  dble(isurf_grp(inum) / isurf)
            x_sf_grp(inum,1) = x_surf(isurf,1) * side
            x_sf_grp(inum,2) = x_surf(isurf,2) * side
            x_sf_grp(inum,3) = x_surf(isurf,3) * side
          end do
        end do
!poption parallel
      end do
!
      end subroutine pick_vect_by_surf_grp_w_side
!
!-----------------------------------------------------------------------
!
      subroutine pick_scalar_by_surf_grp(num_surf, num_surf_bc,         &
     &          num_surf_smp, isurf_grp_smp_stack, isurf_grp,           &
     &          numsurf, x_surf, x_sf_grp)
!
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &                 :: isurf_grp_smp_stack(0:num_surf_smp)
      integer(kind = kint), intent(in) :: isurf_grp(num_surf_bc)
!
      integer(kind = kint), intent(in) :: numsurf
      real(kind=kreal), intent(in) :: x_surf(numsurf)
!
      real(kind=kreal), intent(inout) :: x_sf_grp(num_surf_bc)
!
      integer (kind = kint) :: i_grp, ip, i, ist, ied, inum, isurf
!
!
      do i_grp = 1, num_surf
!
!$omp parallel do private(i,ist,ied,inum,isurf)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = isurf_grp_smp_stack(i-1) + 1
          ied = isurf_grp_smp_stack(i)
!
!poption parallel
          do inum = ist, ied
            isurf = abs( isurf_grp(inum) )
            x_sf_grp(inum) = x_surf(isurf)
          end do
        end do
!poption parallel
      end do
!
      end subroutine pick_scalar_by_surf_grp
!
!-----------------------------------------------------------------------
!
      end module t_surface_group_normals
