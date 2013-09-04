!>@file   cal_zonal_mean_sph_spectr.f90
!!@brief  module cal_zonal_mean_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2008
!
!>@brief Subroutines to choose spewcific modes of spectrum data
!!
!!@verbatim
!!      subroutine delete_rj_phys_data(numdir, is_fld)
!!
!!      subroutine zonal_mean_all_sph_spectr
!!      subroutine take_zonal_mean_rj_field(numdir, is_fld)
!!      subroutine delete_zonal_mean_rj_field(numdir, is_fld)
!!
!!      subroutine pick_order_sph_spectr(num_order, ipick_order,        &
!!     &          numdir, is_fld)
!!      subroutine delete_order_sph_spectr(num_order, ipick_order,      &
!!     &          numdir, is_fld)
!!
!!      subroutine pick_degree_sph_spectr(num_degree, ipick_degree,     &
!!     &          numdir, is_fld)
!!      subroutine delete_degree_sph_spectr(num_degree, ipick_degree,   &
!!     &          numdir, is_fld)
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
      subroutine delete_rj_phys_data(numdir, is_fld)
!
      use m_machine_parameter
      use m_spheric_parameter
      use m_spheric_param_smp
      use m_sph_spectr_data
      use delete_field_smp
!
      integer(kind = kint), intent(in) :: numdir, is_fld
!
!
!$omp parallel
      call delete_phys_data_smp(np_smp, nnod_rtp, inod_rj_smp_stack,   &
     &    ntot_phys_rj, numdir, is_fld, d_rj)
!$omp end parallel
!
      end subroutine delete_rj_phys_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine zonal_mean_all_sph_spectr
!
      use m_sph_spectr_data
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call pick_order_sph_spectr(ione, m_zero, ntot_phys_rj, ione)
!
      end subroutine zonal_mean_all_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine take_zonal_mean_rj_field(numdir, is_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call pick_order_sph_spectr(ione, m_zero, numdir, is_fld)
!
      end subroutine take_zonal_mean_rj_field
!
!-----------------------------------------------------------------------
!
      subroutine delete_zonal_mean_rj_field(numdir, is_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
!
      integer(kind = kint), parameter :: m_zero(1) = (/izero/)
!
!
      call delete_order_sph_spectr(ione, m_zero, numdir, is_fld)
!
      end subroutine delete_zonal_mean_rj_field
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_order_sph_spectr(num_order, ipick_order,          &
     &          numdir, is_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
!
!
      call pick_del_order_sph_spectr(iflag_pick,                        &
     &    num_order, ipick_order, numdir, is_fld)
!
      end subroutine pick_order_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine delete_order_sph_spectr(num_order, ipick_order,        &
     &          numdir, is_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
!
!
      call pick_del_order_sph_spectr(iflag_delete,                      &
     &    num_order, ipick_order, numdir, is_fld)
!
      end subroutine delete_order_sph_spectr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_degree_sph_spectr(num_degree, ipick_degree,       &
     &          numdir, is_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
!
!
      call pick_del_degree_sph_spectr(iflag_pick,                       &
     &    num_degree, ipick_degree, numdir, is_fld)
!
      end subroutine pick_degree_sph_spectr
!
!-----------------------------------------------------------------------
!
      subroutine delete_degree_sph_spectr(num_degree, ipick_degree,     &
     &          numdir, is_fld)
!
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
!
!
      call pick_del_degree_sph_spectr(iflag_delete,                     &
     &    num_degree, ipick_degree, numdir, is_fld)
!
      end subroutine delete_degree_sph_spectr
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine pick_del_order_sph_spectr(iswitch,                     &
     &          num_order, ipick_order, numdir, is_fld)
!
      use m_sph_spectr_data
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: iswitch
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_order
      integer(kind = kint), intent(in) :: ipick_order(num_order)
!
      integer(kind = kint) :: nd, j, kr, inod, inum, iflag
!
!
!$omp parallel do private(iflag,nd,kr,inod,inum)
      do j = 1, nidx_rj(2)
        iflag = izero
        do inum = 1, num_order
          if(ipick_order(inum) .eq. idx_gl_1d_rj_j(j,3)) then
            iflag = ione
            exit
          end if
        end do
!
        if (iflag .eq. iswitch) then
          do nd = is_fld, is_fld+numdir-1
            do kr = 1, nidx_rj(1)
              inod = j + (kr-1)*nidx_rj(2)
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
     &          num_degree, ipick_degree, numdir, is_fld)
!
      use m_sph_spectr_data
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: iswitch
      integer(kind = kint), intent(in) :: numdir, is_fld
      integer(kind = kint), intent(in) :: num_degree
      integer(kind = kint), intent(in) :: ipick_degree(num_degree)
!
      integer(kind = kint) :: nd, j, kr, inod, inum, iflag
!
!
!$omp parallel do private(iflag,nd,kr,inod,inum)
      do j = 1, nidx_rj(2)
        iflag = izero
        do inum = 1, num_degree
          if(ipick_degree(inum) .eq. idx_gl_1d_rj_j(j,2)) then
            iflag = ione
            exit
          end if
        end do
!
        if (iflag .eq. iswitch) then
          do nd = is_fld, is_fld+numdir-1
            do kr = 1, nidx_rj(1)
              inod = j + (kr-1)*nidx_rj(2)
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
