!>@file   m_ctl_data_4_divide_sphere.f90
!!@brief  module m_ctl_data_4_divide_sphere
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine deallocate_ndomain_rtp_ctl
!!      subroutine deallocate_ndomain_rtm_ctl
!!      subroutine deallocate_ndomain_rj_ctl
!!
!!      subroutine read_ctl_ndomain_4_shell
!!
!!  ---------------------------------------------------------------------
!!    example of control data
!!
!!  begin num_domain_ctl
!!    array  num_domain_sph_grid   2
!!      num_domain_sph_grid    radial       2   end
!!      num_domain_sph_grid   meridional    2   end
!!    end array num_domain_sph_grid
!!
!!    array num_domain_legendre   2
!!      num_domain_legendre   radial        2   end
!!      num_domain_legendre   zonal         2   end
!!    end array num_domain_legendre
!!
!!    array num_domain_spectr     1
!!      num_domain_spectr     modes         4   end
!!    end array num_domain_spectr
!!  end num_domain_ctl
!!
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module m_ctl_data_4_divide_sphere
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint) :: ndir_domain_sph_grid
      integer(kind = kint) :: ndir_domain_legendre
      integer(kind = kint) :: ndir_domain_spectr
!
      character(len=kchara), allocatable :: dir_domain_sph_grid_ctl(:)
      character(len=kchara), allocatable :: dir_domain_legendre_ctl(:)
      character(len=kchara), allocatable :: dir_domain_spectr_ctl(:)
!
      integer(kind = kint), allocatable :: num_domain_sph_grid_ctl(:)
      integer(kind = kint), allocatable :: num_domain_legendre_ctl(:)
      integer(kind = kint), allocatable :: num_domain_spectr_ctl(:)
!
!    label for group entry
!
      character(len=kchara), parameter                                  &
     &      :: hd_domains_sph = 'num_domain_ctl'
      integer(kind = kint) :: i_domains_sph = 0
!
!   labels for subdomain define for spherical shell
!
      character(len=kchara), parameter                                  &
     &      :: hd_ndomain_rtp =  'num_domain_sph_grid'
      character(len=kchara), parameter                                  &
     &       :: hd_ndomain_rtm = 'num_domain_legendre'
      character(len=kchara), parameter                                  &
     &       :: hd_ndomain_rj  = 'num_domain_spectr'
!
      integer(kind = kint) :: i_ndomain_rtp = 0
      integer(kind = kint) :: i_ndomain_rtm = 0
      integer(kind = kint) :: i_ndomain_rj =  0
!
      private :: hd_domains_sph, i_domains_sph
      private :: hd_ndomain_rtp, hd_ndomain_rtm, hd_ndomain_rj
!
      private :: allocate_ndomain_rtp_ctl, allocate_ndomain_rtm_ctl
      private ::  allocate_ndomain_rj_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ndomain_rtp_ctl
!
      allocate(dir_domain_sph_grid_ctl(ndir_domain_sph_grid))
      allocate(num_domain_sph_grid_ctl(ndir_domain_sph_grid))
      num_domain_sph_grid_ctl = 0
!
      end subroutine allocate_ndomain_rtp_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ndomain_rtm_ctl
!
      allocate(dir_domain_legendre_ctl(ndir_domain_legendre))
      allocate(num_domain_legendre_ctl(ndir_domain_legendre))
      num_domain_legendre_ctl = 0
!
      end subroutine allocate_ndomain_rtm_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ndomain_rj_ctl
!
      allocate(dir_domain_spectr_ctl(ndir_domain_spectr))
      allocate(num_domain_spectr_ctl(ndir_domain_spectr))
      num_domain_spectr_ctl = 0
!
      end subroutine allocate_ndomain_rj_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ndomain_rtp_ctl
!
      deallocate(dir_domain_sph_grid_ctl)
      deallocate(num_domain_sph_grid_ctl)
!
      end subroutine deallocate_ndomain_rtp_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ndomain_rtm_ctl
!
      deallocate(dir_domain_legendre_ctl)
      deallocate(num_domain_legendre_ctl)
!
      end subroutine deallocate_ndomain_rtm_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ndomain_rj_ctl
!
      deallocate(dir_domain_spectr_ctl)
      deallocate(num_domain_spectr_ctl)
!
      end subroutine deallocate_ndomain_rj_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_ndomain_4_shell
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_domains_sph) .eq. 0) return
      if (i_domains_sph .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_domains_sph, i_domains_sph)
        if(i_domains_sph .gt. 0) exit
!
!
        call find_control_array_flag(hd_ndomain_rtp,                    &
     &      ndir_domain_sph_grid)
        if(ndir_domain_sph_grid.gt.0 .and. i_ndomain_rtp.eq.0) then
          call allocate_ndomain_rtp_ctl
          call read_control_array_int_v_list(hd_ndomain_rtp,            &
     &        ndir_domain_sph_grid, i_ndomain_rtp,                      &
     &        dir_domain_sph_grid_ctl, num_domain_sph_grid_ctl)
        end if
!
        call find_control_array_flag(hd_ndomain_rtm,                    &
     &      ndir_domain_legendre)
        if(ndir_domain_legendre.gt.0 .and. i_ndomain_rtm.eq.0) then
          call allocate_ndomain_rtm_ctl
          call read_control_array_int_v_list(hd_ndomain_rtm,            &
     &        ndir_domain_legendre, i_ndomain_rtm,                      &
     &        dir_domain_legendre_ctl, num_domain_legendre_ctl)
        end if
!
        call find_control_array_flag(hd_ndomain_rj, ndir_domain_spectr)
        if(ndir_domain_spectr.gt.0 .and. i_ndomain_rj.eq.0) then
          call allocate_ndomain_rj_ctl
          call read_control_array_int_v_list(hd_ndomain_rj,             &
     &        ndir_domain_spectr, i_ndomain_rj,                         &
     &        dir_domain_spectr_ctl, num_domain_spectr_ctl)
        end if
      end do
!
      end subroutine read_ctl_ndomain_4_shell
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_divide_sphere
