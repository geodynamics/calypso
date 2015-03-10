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
      use t_read_control_arrays
!
      implicit  none
!
!
!>      Structure for domain decompostion for spherical grid
!!@n      ndomain_sph_grid_ctl%c_tbl:  Direction of decomposition
!!@n      ndomain_sph_grid_ctl%ivec:   Number of subdomains
      type(ctl_array_ci), save :: ndomain_sph_grid_ctl
!>      Structure for domain decompostion for Legendre transform
!!@n      ndomain_legendre_ctl%c_tbl:  Direction of decomposition
!!@n      ndomain_legendre_ctl%ivec:   Number of subdomains
      type(ctl_array_ci), save :: ndomain_legendre_ctl
!>      Structure for domain decompostion for spherical harmonics
!!@n      ndomain_spectr_ctl%c_tbl:  Direction of decomposition
!!@n      ndomain_spectr_ctl%ivec:   Number of subdomains
      type(ctl_array_ci), save :: ndomain_spectr_ctl
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
      private :: hd_domains_sph, i_domains_sph
      private :: hd_ndomain_rtp, hd_ndomain_rtm, hd_ndomain_rj
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ndomain_rtp_ctl
!
      call dealloc_control_array_c_i(ndomain_sph_grid_ctl)
!
      end subroutine deallocate_ndomain_rtp_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ndomain_rtm_ctl
!
      call dealloc_control_array_c_i(ndomain_legendre_ctl)
!
      end subroutine deallocate_ndomain_rtm_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ndomain_rj_ctl
!
      call dealloc_control_array_c_i(ndomain_spectr_ctl)
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
        call read_control_array_c_i                                     &
     &     (hd_ndomain_rtp, ndomain_sph_grid_ctl)
        call read_control_array_c_i                                     &
     &     (hd_ndomain_rtm, ndomain_legendre_ctl)
        call read_control_array_c_i(hd_ndomain_rj, ndomain_spectr_ctl)
      end do
!
      end subroutine read_ctl_ndomain_4_shell
!
!  ---------------------------------------------------------------------
!
      end module m_ctl_data_4_divide_sphere
