!>@file   m_group_data_sph_specr.f90
!!@brief  module m_group_data_sph_specr
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Grouping information for spherical hermonics data
!!
!!@verbatim
!!      subroutine allocate_rtp_nod_grp_stack
!!      subroutine allocate_rtp_r_grp_stack
!!      subroutine allocate_rtp_theta_grp_stack
!!      subroutine allocate_rtp_zonal_grp_stack
!!      subroutine allocate_rj_r_grp_stack
!!      subroutine allocate_rj_sphere_grp_stack
!!
!!      subroutine allocate_rtp_nod_grp_item
!!      subroutine allocate_rtp_r_grp_item
!!      subroutine allocate_rtp_theta_grp_item
!!      subroutine allocate_rtp_zonal_grp_item
!!      subroutine allocate_rj_r_grp_item
!!      subroutine allocate_rj_sphere_grp_item
!!
!!      subroutine deallocate_rtp_nod_grp_item
!!      subroutine deallocate_rtp_r_grp_item
!!      subroutine deallocate_rtp_theta_grp_item
!!      subroutine deallocate_rtp_zonal_grp_item
!!      subroutine deallocate_rj_r_grp_item
!!      subroutine deallocate_rj_sphere_grp_item
!!
!!      subroutine check_rtp_nod_grp_stack(my_rank)
!!      subroutine check_rtp_r_grp_stack(my_rank)
!!      subroutine check_rtp_theta_grp_stack(my_rank)
!!      subroutine check_rtp_zonal_grp_stack(my_rank)
!!      subroutine check_rj_r_grp_stack(my_rank)
!!      subroutine check_rj_sph_grp_stack(my_rank)
!!@endverbatim
!!
!!@n @param  my_rank  Proccess ID
!
      module m_group_data_sph_specr
!
      use m_precision
!
      implicit none
!
!      groups for grid space
!
      integer(kind = kint) :: num_bc_grp_rtp
      integer(kind = kint) :: ntot_bc_grp_rtp
      integer(kind = kint), allocatable :: istack_bc_grp_rtp(:)
      integer(kind = kint), allocatable :: item_bc_grp_rtp(:)
      character(len = kchara), allocatable :: name_bc_grp_rtp(:)
!
      integer(kind = kint) :: num_radial_grp_rtp
      integer(kind = kint) :: ntot_radial_grp_rtp
      integer(kind = kint), allocatable :: istack_radial_grp_rtp(:)
      integer(kind = kint), allocatable :: item_radial_grp_rtp(:)
      character(len = kchara), allocatable :: name_radial_grp_rtp(:)
!
      integer(kind = kint) :: num_theta_grp_rtp
      integer(kind = kint) :: ntot_theta_grp_rtp
      integer(kind = kint), allocatable :: istack_theta_grp_rtp(:)
      integer(kind = kint), allocatable :: item_theta_grp_rtp(:)
      character(len = kchara), allocatable :: name_theta_grp_rtp(:)
!
      integer(kind = kint) :: num_zonal_grp_rtp
      integer(kind = kint) :: ntot_zonal_grp_rtp
      integer(kind = kint), allocatable :: istack_zonal_grp_rtp(:)
      integer(kind = kint), allocatable :: item_zonal_grp_rtp(:)
      character(len = kchara), allocatable :: name_zonal_grp_rtp(:)
!
!      groups for sphectral space
!
      integer(kind = kint) :: num_radial_grp_rj
      integer(kind = kint) :: ntot_radial_grp_rj
      integer(kind = kint), allocatable :: istack_radial_grp_rj(:)
      integer(kind = kint), allocatable :: item_radial_grp_rj(:)
      character(len = kchara), allocatable :: name_radial_grp_rj(:)
!
      integer(kind = kint) :: num_sphere_grp_rj
      integer(kind = kint) :: ntot_sphere_grp_rj
      integer(kind = kint), allocatable :: istack_sphere_grp_rj(:)
      integer(kind = kint), allocatable :: item_sphere_grp_rj(:)
      character(len = kchara), allocatable :: name_sphere_grp_rj(:)
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_nod_grp_stack
!
      allocate(istack_bc_grp_rtp(0:num_bc_grp_rtp))
      allocate(name_bc_grp_rtp(num_bc_grp_rtp))
      istack_bc_grp_rtp(0:num_bc_grp_rtp) = 0
!
      end subroutine allocate_rtp_nod_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_r_grp_stack
!
      allocate(istack_radial_grp_rtp(0:num_radial_grp_rtp))
      allocate(name_radial_grp_rtp(num_radial_grp_rtp))
      istack_radial_grp_rtp(0:num_radial_grp_rtp) = 0
!
      end subroutine allocate_rtp_r_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_theta_grp_stack
!
      allocate(istack_theta_grp_rtp(0:num_theta_grp_rtp))
      allocate(name_theta_grp_rtp(num_theta_grp_rtp))
      istack_theta_grp_rtp(0:num_theta_grp_rtp) = 0
!
      end subroutine allocate_rtp_theta_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_zonal_grp_stack
!
      allocate(istack_zonal_grp_rtp(0:num_zonal_grp_rtp))
      allocate(name_zonal_grp_rtp(num_zonal_grp_rtp))
      istack_zonal_grp_rtp(0:num_zonal_grp_rtp) = 0
!
      end subroutine allocate_rtp_zonal_grp_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_r_grp_stack
!
      allocate(istack_radial_grp_rj(0:num_radial_grp_rj))
      allocate(name_radial_grp_rj(num_radial_grp_rj))
      istack_radial_grp_rj(0:num_radial_grp_rj) = 0
!
      end subroutine allocate_rj_r_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_sphere_grp_stack
!
      allocate(istack_sphere_grp_rj(0:num_sphere_grp_rj))
      allocate(name_sphere_grp_rj(num_sphere_grp_rj))
      istack_sphere_grp_rj(0:num_sphere_grp_rj) = 0
!
      end subroutine allocate_rj_sphere_grp_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_nod_grp_item
!
      allocate(item_bc_grp_rtp(ntot_bc_grp_rtp))
      if(ntot_bc_grp_rtp .gt. 0) item_bc_grp_rtp = 0
!
      end subroutine allocate_rtp_nod_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_r_grp_item
!
      allocate(item_radial_grp_rtp(ntot_radial_grp_rtp))
      if(ntot_radial_grp_rtp .gt. 0) item_radial_grp_rtp = 0
!
      end subroutine allocate_rtp_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_theta_grp_item
!
      allocate(item_theta_grp_rtp(ntot_theta_grp_rtp))
      if(ntot_theta_grp_rtp .gt. 0) item_theta_grp_rtp = 0
!
      end subroutine allocate_rtp_theta_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_zonal_grp_item
!
      allocate(item_zonal_grp_rtp(ntot_zonal_grp_rtp))
      if(ntot_zonal_grp_rtp .gt. 0) item_zonal_grp_rtp = 0
!
      end subroutine allocate_rtp_zonal_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_r_grp_item
!
      allocate(item_radial_grp_rj(ntot_radial_grp_rj))
      if(ntot_radial_grp_rj .gt. 0) item_radial_grp_rj = 0
!
      end subroutine allocate_rj_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_sphere_grp_item
!
      allocate(item_sphere_grp_rj(ntot_sphere_grp_rj))
      if(ntot_sphere_grp_rj .gt. 0) item_sphere_grp_rj = 0
!
      end subroutine allocate_rj_sphere_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_nod_grp_item
!
      deallocate(item_bc_grp_rtp)
      deallocate(istack_bc_grp_rtp)
      deallocate(name_bc_grp_rtp)
!
      end subroutine deallocate_rtp_nod_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_r_grp_item
!
      deallocate(item_radial_grp_rtp)
      deallocate(istack_radial_grp_rtp)
      deallocate(name_radial_grp_rtp)
!
      end subroutine deallocate_rtp_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_theta_grp_item
!
      deallocate(item_theta_grp_rtp)
      deallocate(istack_theta_grp_rtp)
      deallocate(name_theta_grp_rtp)
!
      end subroutine deallocate_rtp_theta_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_zonal_grp_item
!
      deallocate(item_zonal_grp_rtp)
      deallocate(istack_zonal_grp_rtp)
      deallocate(name_zonal_grp_rtp)
!
      end subroutine deallocate_rtp_zonal_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_rj_r_grp_item
!
      deallocate(item_radial_grp_rj)
      deallocate(istack_radial_grp_rj)
      deallocate(name_radial_grp_rj)
!
      end subroutine deallocate_rj_r_grp_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rj_sphere_grp_item
!
      deallocate(item_sphere_grp_rj)
      deallocate(istack_sphere_grp_rj)
      deallocate(name_sphere_grp_rj)
!
      end subroutine deallocate_rj_sphere_grp_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_rtp_nod_grp_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_bc_grp_rtp', num_bc_grp_rtp
      write(50+my_rank,*) 'ntot_bc_grp_rtp', ntot_bc_grp_rtp
      write(50+my_rank,*) 'istack_bc_grp_rtp',istack_bc_grp_rtp
      do i = 1, num_radial_grp_rj
        write(50+my_rank,*) trim(name_bc_grp_rtp(i))
        ist = istack_bc_grp_rtp(i-1)+1
        ied = istack_bc_grp_rtp(i)
        write(50+my_rank,'(5i10)') item_bc_grp_rtp(ist:ied)
      end do
!
      end subroutine check_rtp_nod_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine check_rtp_r_grp_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_radial_grp_rtp', num_radial_grp_rtp
      write(50+my_rank,*) 'ntot_radial_grp_rtp', ntot_radial_grp_rtp
      write(50+my_rank,*) 'istack_radial_grp_rtp',istack_radial_grp_rtp
      do i = 1, num_radial_grp_rj
        write(50+my_rank,*) trim(name_radial_grp_rtp(i))
        ist = istack_radial_grp_rtp(i-1)+1
        ied = istack_radial_grp_rtp(i)
        write(50+my_rank,'(5i10)') item_radial_grp_rtp(ist:ied)
      end do
!
      end subroutine check_rtp_r_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine check_rtp_theta_grp_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_theta_grp_rtp', num_theta_grp_rtp
      write(50+my_rank,*) 'ntot_theta_grp_rtp', ntot_theta_grp_rtp
      write(50+my_rank,*) 'istack_theta_grp_rtp', istack_theta_grp_rtp
      do i = 1, num_radial_grp_rj
        write(50+my_rank,*) trim(name_theta_grp_rtp(i))
        ist = istack_theta_grp_rtp(i-1)+1
        ied = istack_theta_grp_rtp(i)
        write(50+my_rank,'(5i10)') item_theta_grp_rtp(ist:ied)
      end do
!
      end subroutine check_rtp_theta_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine check_rtp_zonal_grp_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_zonal_grp_rtp', num_zonal_grp_rtp
      write(50+my_rank,*) 'ntot_zonal_grp_rtp', ntot_zonal_grp_rtp
      write(50+my_rank,*) 'istack_zonal_grp_rtp', istack_zonal_grp_rtp
      do i = 1, num_radial_grp_rj
        write(50+my_rank,*) trim(name_zonal_grp_rtp(i))
        ist = istack_zonal_grp_rtp(i-1)+1
        ied = istack_zonal_grp_rtp(i)
        write(50+my_rank,'(5i10)') item_zonal_grp_rtp(ist:ied)
      end do
!
      end subroutine check_rtp_zonal_grp_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_rj_r_grp_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_radial_grp_rj', num_radial_grp_rj
      write(50+my_rank,*) 'ntot_radial_grp_rj', ntot_radial_grp_rj
      write(50+my_rank,*) 'istack_radial_grp_rj', istack_radial_grp_rj
      do i = 1, num_radial_grp_rj
        write(50+my_rank,*) trim(name_radial_grp_rj(i))
        ist = istack_radial_grp_rj(i-1)+1
        ied = istack_radial_grp_rj(i)
        write(50+my_rank,'(5i10)') item_sphere_grp_rj(ist:ied)
      end do
!
      end subroutine check_rj_r_grp_stack
!
! -----------------------------------------------------------------------
!
      subroutine check_rj_sph_grp_stack(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: i, ist, ied
!
      write(50+my_rank,*) 'num_sphere_grp_rj',  num_sphere_grp_rj
      write(50+my_rank,*) 'ntot_sphere_grp_rj', ntot_sphere_grp_rj
      write(50+my_rank,*) 'istack_sphere_grp_rj', istack_sphere_grp_rj
      do i = 1, num_radial_grp_rj
        write(50+my_rank,*) trim(name_sphere_grp_rj(i))
        ist = istack_sphere_grp_rj(i-1)+1
        ied = istack_sphere_grp_rj(i)
        write(50+my_rank,'(5i10)') item_sphere_grp_rj(ist:ied)
      end do
!
      end subroutine check_rj_sph_grp_stack
!
! -----------------------------------------------------------------------!
      end module m_group_data_sph_specr
