!>@file  mesh_outline_4_pvr.f90
!!       module mesh_outline_4_pvr
!!
!!@author H. Matsui
!!@date   Programmed in May, 2006
!
!> @brief Set mesh outline for PVR
!!
!!@verbatim
!!      subroutine pvr_mesh_outline(node, outline)
!!        type(node_data), intent(in) :: node
!!        type(pvr_domain_outline), intent(inout) :: outline
!!@endverbatim
!
      module mesh_outline_4_pvr
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_surf_grp_4_pvr_domain
!
      implicit  none
!
!
      real(kind = kreal), allocatable :: xx_minmax_l(:,:,:)
      real(kind = kreal), allocatable :: xx_minmax_tbl(:,:,:)
      private :: xx_minmax_l, xx_minmax_tbl
      private :: cal_mesh_outline_pvr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine pvr_mesh_outline(node, outline)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      type(pvr_domain_outline), intent(inout) :: outline
!
      allocate( xx_minmax_l(2,3,nprocs) )
      allocate( xx_minmax_tbl(2,3,nprocs) )
      xx_minmax_l =     0.0d0
      xx_minmax_tbl =   0.0d0
!
      call cal_mesh_outline_pvr(node%numnod, node%xx, outline)
!
      if (iflag_debug .gt. 0) then
        write(*,*) 'xx_min_g', outline%xx_minmax_g(1,1:3)
        write(*,*) 'xx_max_g', outline%xx_minmax_g(2,1:3)
        write(*,*) 'center_g', outline%center_g(1:3)
        write(*,*) 'rmax_g', outline%rmax_g
      end if
!
      deallocate(xx_minmax_l, xx_minmax_tbl)
!
      end subroutine pvr_mesh_outline
!
! -----------------------------------------------------------------------
!
      subroutine cal_mesh_outline_pvr(numnod, xx, outline)
!
      use calypso_mpi_real
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      type(pvr_domain_outline), intent(inout) :: outline
!
      integer(kind = kint_gl) :: np6
      integer(kind = kint) :: inod, ip
      real(kind = kreal) :: rmax_l, r_from_ct
!
!
      ip = my_rank + 1
!
      xx_minmax_l(1,1:3,ip) = xx(1,1:3)
      xx_minmax_l(2,1:3,ip) = xx(1,1:3)
      do inod = 2, numnod
        xx_minmax_l(1,1,ip) = min(xx_minmax_l(1,1,ip), xx(inod,1))
        xx_minmax_l(1,2,ip) = min(xx_minmax_l(1,2,ip), xx(inod,2))
        xx_minmax_l(1,3,ip) = min(xx_minmax_l(1,3,ip), xx(inod,3))
        xx_minmax_l(2,1,ip) = max(xx_minmax_l(2,1,ip), xx(inod,1))
        xx_minmax_l(2,2,ip) = max(xx_minmax_l(2,2,ip), xx(inod,2))
        xx_minmax_l(2,3,ip) = max(xx_minmax_l(2,3,ip), xx(inod,3))
      end do
!
      np6 = isix * nprocs
      xx_minmax_tbl = 0.0d0
      call calypso_mpi_allreduce_real(xx_minmax_l(1,1,1),               &
     &    xx_minmax_tbl(1,1,1), np6, MPI_SUM)
!
      outline%xx_minmax_g(1,1:3) = xx_minmax_tbl(1,1:3,1)
      outline%xx_minmax_g(2,1:3) = xx_minmax_tbl(2,1:3,1)
      do ip = 2, nprocs
        outline%xx_minmax_g(1,1:3) = min(outline%xx_minmax_g(1,1:3),    &
     &                                 xx_minmax_tbl(1,1:3,ip) )
        outline%xx_minmax_g(2,1:3) = max(outline%xx_minmax_g(1,1:3),    &
     &                                 xx_minmax_tbl(2,1:3,ip) )
      end do
!
      outline%center_g(1:3) = (outline%xx_minmax_g(1,1:3)               &
     &                        + outline%xx_minmax_g(2,1:3)) / two
!
!
      inod = 1
      rmax_l = sqrt( (xx(inod,1) - outline%center_g(1))                 &
     &              *(xx(inod,1) - outline%center_g(1))                 &
     &             + (xx(inod,2) - outline%center_g(2))                 &
     &              *(xx(inod,2) - outline%center_g(2))                 &
     &             + (xx(inod,3) - outline%center_g(3))                 &
     &              *(xx(inod,3) - outline%center_g(3)) )
!
!
!
      do inod = 2, numnod
        r_from_ct = sqrt( (xx(inod,1) - outline%center_g(1))            &
     &                   *(xx(inod,1) - outline%center_g(1))            &
     &                  + (xx(inod,2) - outline%center_g(2))            &
     &                   *(xx(inod,2) - outline%center_g(2))            &
     &                  + (xx(inod,3) - outline%center_g(3))            &
     &                   *(xx(inod,3) - outline%center_g(3)) )
        rmax_l = max(rmax_l, r_from_ct)
      end do
!
      call calypso_mpi_allreduce_one_real(rmax_l, outline%rmax_g,       &
     &                                    MPI_MAX)
!
      end subroutine cal_mesh_outline_pvr
!
! ----------------------------------------------------------------------
!
      end module mesh_outline_4_pvr
