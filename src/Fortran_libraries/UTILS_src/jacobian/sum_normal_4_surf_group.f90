!>@file  sum_normal_4_surf_group.f90
!!       module sum_normal_4_surf_group
!!
!!@author H. Matsui
!!@date   Programmed in Aug, 2006
!!        modified in June, 2007
!!        modified in Jan., 2009
!>@brief  Get size of area for each surface group
!!
!!@verbatim
!!      subroutine s_sum_normal_4_surf_group(ele, sf_grp, sf_grp_v)
!!        type(element_data), intent(in) :: ele
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_group_normals), intent(inout) :: sf_grp_v
!!@endverbatim
!
      module sum_normal_4_surf_group
!
      use m_precision
!
      implicit none
!
      private :: sum_norm_of_surf_group
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_sum_normal_4_surf_group(ele, sf_grp, sf_grp_v)
!
      use calypso_mpi
      use calypso_mpi_real
!
      use m_machine_parameter
      use t_geometry_data
      use t_group_data
      use t_surface_group_normals
      use transfer_to_long_integers
!
      type(element_data), intent(in) :: ele
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_group_normals), intent(inout) :: sf_grp_v
!
      integer(kind = kint) :: i
      real(kind= kreal), allocatable :: sum_sf_grp_l(:)
!
!
      allocate(sum_sf_grp_l(sf_grp%num_grp))
      sum_sf_grp_l(1:sf_grp%num_grp) = 0.0d0
!
      call sum_norm_of_surf_group                                       &
     &   (np_smp, ele%numele, ele%interior_ele,                         &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%item_sf_grp,          &
     &    sf_grp%num_grp_smp, sf_grp%istack_grp_smp,                    &
     &    sf_grp_v%area_sf_grp, sum_sf_grp_l)
!
      call calypso_mpi_allreduce_real                                   &
     &   (sum_sf_grp_l, sf_grp_v%tot_area_sf_grp,                       &
     &    cast_long(sf_grp%num_grp), MPI_SUM)
      deallocate(sum_sf_grp_l)
!
      if (my_rank.eq.0) then
        do i = 1, sf_grp%num_grp
           write(*,*) i, trim(sf_grp%grp_name(i)),                      &
     &                   sf_grp_v%tot_area_sf_grp(i)
        end do
      end if 
!
      end subroutine s_sum_normal_4_surf_group
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sum_norm_of_surf_group(np_smp, numele, interior_ele,   &
     &          num_surf, num_surf_bc, surf_item, num_surf_smp,         &
     &          isurf_grp_smp_stack, area_sf_grp, sum_sf_grp)
!
      use calypso_mpi
!
      integer(kind = kint) , intent(in) :: numele
      integer (kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: np_smp, num_surf_smp
      integer(kind = kint), intent(in)                                  &
     &            :: isurf_grp_smp_stack(0:num_surf_smp)
      integer(kind = kint), intent(in) :: num_surf, num_surf_bc
      integer(kind = kint), intent(in) :: surf_item(2,num_surf_bc)
      real(kind = kreal), intent(in) :: area_sf_grp(num_surf_bc)
!
      real(kind= kreal), intent(inout) :: sum_sf_grp(num_surf)
!
      integer(kind = kint) :: i_grp, ip, i, ist, ied, isurf, iele
      real(kind= kreal), allocatable :: area_grp_smp(:)
!
!
!
!$omp parallel workshare
      sum_sf_grp(1:num_surf) = 0.0d0
!$omp end parallel workshare
!
      allocate(area_grp_smp(np_smp))
!
      do i_grp = 1, num_surf
!$omp parallel do private(ist,ied,isurf,iele)
        do ip = 1, np_smp
          i = (i_grp-1)*np_smp + ip
          ist = isurf_grp_smp_stack(i-1) + 1
          ied = isurf_grp_smp_stack(i)
!
          area_grp_smp(ip) = 0.0d0
          do isurf = ist, ied
            iele = surf_item(1,isurf)
            area_grp_smp(ip) = area_grp_smp(ip) + area_sf_grp(isurf)    &
     &                        * dble(interior_ele(iele))
          end do
        end do
!$omp end parallel do
!
        sum_sf_grp(i_grp) = sum(area_grp_smp)
      end do
      deallocate(area_grp_smp)
!
      end subroutine sum_norm_of_surf_group
!
! ----------------------------------------------------------------------
!
      end module sum_normal_4_surf_group
