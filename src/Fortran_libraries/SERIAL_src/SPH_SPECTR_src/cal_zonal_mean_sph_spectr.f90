!>@file   cal_zonal_mean_sph_spectr.f90
!!@brief  module cal_zonal_mean_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Subroutines to choose spewcific modes of spectrum data
!!
!!@verbatim
!!      subroutine zonal_mean_all_sph_spectr(sph_rj, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine take_zonal_mean_rj_field                             &
!!     &         (numdir, is_fld, sph_rj, rj_fld)
!!      subroutine delete_zonal_mean_rj_field                           &
!!     &         (numdir, is_fld, sph_rj, rj_fld)
!!
!!      subroutine pick_order_sph_spectr(num_order, ipick_order,        &
!!     &          numdir, is_fld, sph_rj, rj_fld)
!!      subroutine delete_order_sph_spectr(num_order, ipick_order,      &
!!     &          numdir, is_fld, sph_rj, rj_fld)
!!
!!      subroutine pick_degree_sph_spectr(num_degree, ipick_degree,     &
!!     &          numdir, is_fld, sph_rj, rj_fld)
!!      subroutine delete_degree_sph_spectr(num_degree, ipick_degree,   &
!!     &          numdir, is_fld, sph_rj, rj_fld)
!!@endverbatim
!!
!!@n @param numdir  number of component of field
!!@n @param is_fld  field data address
!!@n @param num_order     number of spherical harmonics order
!!                        to keep (or delete)
!!@n @param id_order(num_order)        spherical harmonics order
!!                                     to keep (or delete)
!!@n @param num_degree     number of spherical harmonics degree
!!                        to keep (or delete)
!!@n @param ipick_degree(num_degree)   spherical harmonics degree
!!                                     to keep (or delete)
!!
      module cal_zonal_mean_sph_spectr
!
      use m_precision
      use m_constants
!
      use t_spheric_rj_data
      use t_phys_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: iflag_pick =   izero
      integer(kind = kint), parameter, private :: iflag_delete = ione
!
      private :: pick_del_order_sph_spectr, pick_del_degree_sph_spectr
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine zonal_mean_all_sph_spectr(sph_rj, rj_fld)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call pick_order_sph_spectr                                        &
     &   (ione, m_zero, rj_fld%ntot_phys, ione, sph_rj, rj_fld)
!
      end subroutine zonal_mean_all_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine take_zonal_mean_rj_field                               &
     &         (numdir, is_fld, sph_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call pick_order_sph_spectr                                        &
     &   (ione, m_zero, numdir, is_fld, sph_rj, rj_fld)
!
      end subroutine take_zonal_mean_rj_field
!
!-----------------------------------------------------------------------
!
      subroutine delete_zonal_mean_rj_field                             &
     &         (numdir, is_fld, sph_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call delete_order_sph_spectr                                      &
     &   (ione, m_zero, numdir, is_fld, sph_rj, rj_fld)
!
      end subroutine delete_zonal_mean_rj_field
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_order_sph_spectr(num_order, ipick_order,          &
     &          numdir, is_fld, sph_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call pick_del_order_sph_spectr(iflag_pick,                        &
     &    num_order, ipick_order, numdir, is_fld, sph_rj,               &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine pick_order_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine delete_order_sph_spectr(num_order, ipick_order,        &
     &          numdir, is_fld, sph_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call pick_del_order_sph_spectr(iflag_delete,                      &
     &    num_order, ipick_order, numdir, is_fld, sph_rj,               &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine delete_order_sph_spectr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_degree_sph_spectr(num_degree, ipick_degree,       &
     &          numdir, is_fld, sph_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call pick_del_degree_sph_spectr(iflag_pick,                       &
     &    num_degree, ipick_degree, numdir, is_fld, sph_rj,             &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine pick_degree_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine delete_degree_sph_spectr(num_degree, ipick_degree,     &
     &          numdir, is_fld, sph_rj, rj_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call pick_del_degree_sph_spectr(iflag_delete,                     &
     &    num_degree, ipick_degree, numdir, is_fld, sph_rj,             &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      end subroutine delete_degree_sph_spectr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_del_order_sph_spectr(iswitch,                     &
     &          num_order, ipick_order, numdir, is_fld, sph_rj,         &
     &          n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: iswitch
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: nd, j, kr, inod, inum, iflag
!
!
!$omp parallel do private(iflag,nd,kr,inod,inum)
      do j = 1, sph_rj%nidx_rj(2)
        iflag = izero
        do inum = 1, num_order
          if(ipick_order(inum) .eq. sph_rj%idx_gl_1d_rj_j(j,3)) then
            iflag = ione
            exit
          end if
        end do
!
        if (iflag .eq. iswitch) then
          do nd = is_fld, is_fld+numdir-1
            do kr = 1, sph_rj%nidx_rj(1)
              inod = j + (kr-1) * sph_rj%nidx_rj(2)
              d_rj(inod,nd) = zero
            end do
          end do
        end if
!
      end do
!$omp end parallel do
!
      end subroutine pick_del_order_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine pick_del_degree_sph_spectr(iswitch,                    &
     &          num_degree, ipick_degree, numdir, is_fld,               &
     &          sph_rj, n_point, ntot_phys_rj, d_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: iswitch
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: nd, j, kr, inod, inum, iflag
!
!
!$omp parallel do private(iflag,nd,kr,inod,inum)
      do j = 1, sph_rj%nidx_rj(2)
        iflag = izero
        do inum = 1, num_degree
          if(ipick_degree(inum) .eq. sph_rj%idx_gl_1d_rj_j(j,2)) then
            iflag = ione
            exit
          end if
        end do
!
        if (iflag .eq. iswitch) then
          do nd = is_fld, is_fld+numdir-1
            do kr = 1, sph_rj%nidx_rj(1)
              inod = j + (kr-1) * sph_rj%nidx_rj(2)
              d_rj(inod,nd) = zero
            end do
          end do
        end if
!
      end do
!$omp end parallel do
!
      end subroutine pick_del_degree_sph_spectr
!
!-----------------------------------------------------------------------
!
      end module cal_zonal_mean_sph_spectr
