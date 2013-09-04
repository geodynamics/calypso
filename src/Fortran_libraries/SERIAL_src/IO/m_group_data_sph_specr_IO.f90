!m_group_data_sph_specr_IO.f90
!      module m_group_data_sph_specr_IO
!
!     Written by H. Matsui on July, 2007
!
!
!      subroutine allocate_rtp_nod_grp_IO_stack
!      subroutine allocate_rtp_r_grp_IO_stack
!      subroutine allocate_rtp_t_grp_IO_stack
!      subroutine allocate_rtp_p_grp_IO_stack
!      subroutine allocate_rj_r_grp_IO_stack
!      subroutine allocate_rj_j_grp_IO_stack
!
!      subroutine allocate_rtp_nod_grp_IO_item
!      subroutine allocate_rtp_r_grp_IO_item
!      subroutine allocate_rtp_t_grp_IO_item
!      subroutine allocate_rtp_p_grp_IO_item
!      subroutine allocate_rj_r_grp_IO_item
!      subroutine allocate_rj_j_grp_IO_item
!
!      subroutine deallocate_rtp_nod_grp_IO_item
!      subroutine deallocate_rtp_r_grp_IO_item
!      subroutine deallocate_rtp_t_grp_IO_item
!      subroutine deallocate_rtp_p_grp_IO_item
!      subroutine deallocate_rj_r_grp_IO_item
!      subroutine deallocate_rj_j_grp_IO_item
!
      module m_group_data_sph_specr_IO
!
      use m_precision
!
      implicit none
!
!
!      groups for grid space
!
      integer(kind = kint) :: num_bc_grp_rtp_IO
      integer(kind = kint) :: ntot_bc_grp_rtp_IO
      integer(kind = kint), allocatable :: istack_bc_grp_rtp_IO(:)
      integer(kind = kint), allocatable :: item_bc_grp_rtp_IO(:)
      character(len = kchara), allocatable :: name_bc_grp_rtp_IO(:)
!
      integer(kind = kint) :: num_radial_grp_rtp_IO
      integer(kind = kint) :: ntot_radial_grp_rtp_IO
      integer(kind = kint), allocatable :: istack_radial_grp_rtp_IO(:)
      integer(kind = kint), allocatable :: item_radial_grp_rtp_IO(:)
      character(len = kchara), allocatable :: name_radial_grp_rtp_IO(:)
!
      integer(kind = kint) :: num_theta_grp_rtp_IO
      integer(kind = kint) :: ntot_theta_grp_rtp_IO
      integer(kind = kint), allocatable :: istack_theta_grp_rtp_IO(:)
      integer(kind = kint), allocatable :: item_theta_grp_rtp_IO(:)
      character(len = kchara), allocatable :: name_theta_grp_rtp_IO(:)
!
      integer(kind = kint) :: num_zonal_grp_rtp_IO
      integer(kind = kint) :: ntot_zonal_grp_rtp_IO
      integer(kind = kint), allocatable :: istack_zonal_grp_rtp_IO(:)
      integer(kind = kint), allocatable :: item_zonal_grp_rtp_IO(:)
      character(len = kchara), allocatable :: name_zonal_grp_rtp_IO(:)
!
!      groups for sphectral space
!
      integer(kind = kint) :: num_radial_grp_rj_IO
      integer(kind = kint) :: ntot_radial_grp_rj_IO
      integer(kind = kint), allocatable :: istack_radial_grp_rj_IO(:)
      integer(kind = kint), allocatable :: item_radial_grp_rj_IO(:)
      character(len = kchara), allocatable :: name_radial_grp_rj_IO(:)
!
      integer(kind = kint) :: num_sphere_grp_rj_IO
      integer(kind = kint) :: ntot_sphere_grp_rj_IO
      integer(kind = kint), allocatable :: istack_sphere_grp_rj_IO(:)
      integer(kind = kint), allocatable :: item_sphere_grp_rj_IO(:)
      character(len = kchara), allocatable :: name_sphere_grp_rj_IO(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_nod_grp_IO_stack
!
      allocate(istack_bc_grp_rtp_IO(0:num_bc_grp_rtp_IO))
      allocate(name_bc_grp_rtp_IO(num_bc_grp_rtp_IO))
      istack_bc_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_nod_grp_IO_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_r_grp_IO_stack
!
      allocate(istack_radial_grp_rtp_IO(0:num_radial_grp_rtp_IO))
      allocate(name_radial_grp_rtp_IO(num_radial_grp_rtp_IO))
      istack_radial_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_r_grp_IO_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_t_grp_IO_stack
!
      allocate(istack_theta_grp_rtp_IO(0:num_theta_grp_rtp_IO))
      allocate(name_theta_grp_rtp_IO(num_theta_grp_rtp_IO))
      istack_theta_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_t_grp_IO_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_p_grp_IO_stack
!
      allocate(istack_zonal_grp_rtp_IO(0:num_zonal_grp_rtp_IO))
      allocate(name_zonal_grp_rtp_IO(num_zonal_grp_rtp_IO))
      istack_zonal_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_p_grp_IO_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_r_grp_IO_stack
!
      allocate(istack_radial_grp_rj_IO(0:num_radial_grp_rj_IO))
      allocate(name_radial_grp_rj_IO(num_radial_grp_rj_IO))
      istack_radial_grp_rj_IO = 0
!
      end subroutine allocate_rj_r_grp_IO_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_j_grp_IO_stack
!
      allocate(istack_sphere_grp_rj_IO(0:num_sphere_grp_rj_IO))
      allocate(name_sphere_grp_rj_IO(num_sphere_grp_rj_IO))
      istack_sphere_grp_rj_IO = 0
!
      end subroutine allocate_rj_j_grp_IO_stack
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_nod_grp_IO_item
!
      allocate(item_bc_grp_rtp_IO(ntot_bc_grp_rtp_IO))
      item_bc_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_nod_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_r_grp_IO_item
!
      allocate(item_radial_grp_rtp_IO(ntot_radial_grp_rtp_IO))
      item_radial_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_r_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_t_grp_IO_item
!
      allocate(item_theta_grp_rtp_IO(ntot_theta_grp_rtp_IO))
      item_theta_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_t_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rtp_p_grp_IO_item
!
      allocate(item_zonal_grp_rtp_IO(ntot_zonal_grp_rtp_IO))
      item_zonal_grp_rtp_IO = 0
!
      end subroutine allocate_rtp_p_grp_IO_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_r_grp_IO_item
!
      allocate(item_radial_grp_rj_IO(ntot_radial_grp_rj_IO))
      item_radial_grp_rj_IO = 0
!
      end subroutine allocate_rj_r_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rj_j_grp_IO_item
!
      allocate(item_sphere_grp_rj_IO(ntot_sphere_grp_rj_IO))
      item_sphere_grp_rj_IO = 0
!
      end subroutine allocate_rj_j_grp_IO_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_nod_grp_IO_item
!
      deallocate(item_bc_grp_rtp_IO)
      deallocate(istack_bc_grp_rtp_IO)
      deallocate(name_bc_grp_rtp_IO)
!
      end subroutine deallocate_rtp_nod_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_r_grp_IO_item
!
      deallocate(item_radial_grp_rtp_IO)
      deallocate(istack_radial_grp_rtp_IO)
      deallocate(name_radial_grp_rtp_IO)
!
      end subroutine deallocate_rtp_r_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_t_grp_IO_item
!
      deallocate(item_theta_grp_rtp_IO)
      deallocate(istack_theta_grp_rtp_IO)
      deallocate(name_theta_grp_rtp_IO)
!
      end subroutine deallocate_rtp_t_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rtp_p_grp_IO_item
!
      deallocate(item_zonal_grp_rtp_IO)
      deallocate(istack_zonal_grp_rtp_IO)
      deallocate(name_zonal_grp_rtp_IO)
!
      end subroutine deallocate_rtp_p_grp_IO_item
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_rj_r_grp_IO_item
!
      deallocate(item_radial_grp_rj_IO)
      deallocate(istack_radial_grp_rj_IO)
      deallocate(name_radial_grp_rj_IO)
!
      end subroutine deallocate_rj_r_grp_IO_item
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rj_j_grp_IO_item
!
      deallocate(item_sphere_grp_rj_IO)
      deallocate(istack_sphere_grp_rj_IO)
      deallocate(name_sphere_grp_rj_IO)
!
      end subroutine deallocate_rj_j_grp_IO_item
!
! -----------------------------------------------------------------------
!
      end module m_group_data_sph_specr_IO
