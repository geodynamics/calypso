!>@file   set_control_sph_subdomains.f90
!!@brief  module set_control_sph_subdomains
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition
!!        for spherical transform
!!
!!@verbatim
!!      subroutine set_subdomains_4_sph_shell(nprocs, ierr, e_message)
!!@endverbatim
!
      module set_control_sph_subdomains
!
      use m_precision
!
      use m_ctl_data_4_divide_sphere
      use m_spheric_global_ranks
!
      implicit  none
!
      character(len=kchara), parameter :: radius1 = 'r'
      character(len=kchara), parameter :: radius2 = 'radial'
!
      character(len=kchara), parameter :: theta1 = 'theta'
      character(len=kchara), parameter :: theta2 = 'meridional'
!
      character(len=kchara), parameter :: phi1 = 'phi'
      character(len=kchara), parameter :: phi2 = 'zonal'
!
      character(len=kchara), parameter :: mode1 = 'degree_order'
      character(len=kchara), parameter :: mode2 = 'modes'
!
      private :: radius1, theta1, phi1, mode1
      private :: radius2, theta2, phi2, mode2
!
      private :: simple_subdomains_4_sph_shell
      private :: full_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_subdomains_4_sph_shell(nprocs, ierr, e_message)
!
      use m_error_IDs
!
      integer(kind = kint), intent(in) :: nprocs
      integer(kind = kint), intent(inout) :: ierr
      character(len = kchara), intent(inout) :: e_message
!
      integer(kind = kint) :: iflag_f, iflag_s
!
!
      ierr = 0
      iflag_f = ndomain_sph_grid_ctl%num * ndomain_legendre_ctl%num     &
     &         * ndomain_spectr_ctl%num
      iflag_s = num_radial_domain_ctl%iflag*num_horiz_domain_ctl%iflag
!
      if(iflag_s .gt. 0) then
        call simple_subdomains_4_sph_shell
      else if(iflag_f .gt. 0) then
        call full_subdomains_4_sph_shell
      else
        write(e_message,'(a)') 'Set parallelization information'
        ierr = ierr_mesh
        return
      end if
!
      if (ndomain_sph_grid_ctl%num .gt. 0) then
        call deallocate_ndomain_rtp_ctl
      end if
      if (ndomain_legendre_ctl%num .gt. 0) then
        call deallocate_ndomain_rtm_ctl
      end if
      if(ndomain_spectr_ctl%num .gt. 0) then
        call deallocate_ndomain_rj_ctl
      end if
!
      call check_sph_domains(nprocs, ierr, e_message)
!
      end subroutine set_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      subroutine full_subdomains_4_sph_shell
!
      use skip_comment_f
!
      integer(kind = kint) :: i
!
!
      ndomain_rtp(1:3) = 1
      if (ndomain_sph_grid_ctl%num .gt. 0) then
        do i = 1, ndomain_sph_grid_ctl%num
          if     (cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), radius1)   &
     &       .or. cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), radius2)   &
     &           ) then
            ndomain_rtp(1) = ndomain_sph_grid_ctl%ivec(i)
          else if (cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), theta1)   &
     &        .or. cmp_no_case(ndomain_sph_grid_ctl%c_tbl(i), theta2)   &
     &           ) then
            ndomain_rtp(2) = ndomain_sph_grid_ctl%ivec(i)
          end if
        end do
      end if
!
      ndomain_rtm(1:3) = 1
      if (ndomain_legendre_ctl%num .gt. 0) then
        do i = 1, ndomain_legendre_ctl%num
          if     (cmp_no_case(ndomain_legendre_ctl%c_tbl(i), radius1)   &
     &       .or. cmp_no_case(ndomain_legendre_ctl%c_tbl(i), radius2)   &
     &           ) then
            ndomain_rtm(1) = ndomain_legendre_ctl%ivec(i)
          else if (cmp_no_case(ndomain_legendre_ctl%c_tbl(i), phi1)     &
     &        .or. cmp_no_case(ndomain_legendre_ctl%c_tbl(i), phi2)     &
     &           ) then
            ndomain_rtm(3) = ndomain_legendre_ctl%ivec(i)
           end if
        end do
      end if
!
      ndomain_rlm(1) = ndomain_rtm(1)
      ndomain_rlm(2) = ndomain_rtm(3)
!
      ndomain_rj(1:2) = 1
      if (ndomain_spectr_ctl%num .gt. 0) then
        do i = 1, ndomain_spectr_ctl%num
          if     (cmp_no_case(ndomain_spectr_ctl%c_tbl(i), mode1)       &
     &       .or. cmp_no_case(ndomain_spectr_ctl%c_tbl(i), mode2)       &
     &      ) ndomain_rj(2) = ndomain_spectr_ctl%ivec(i)
        end do
      end if
!
      end subroutine full_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      subroutine simple_subdomains_4_sph_shell
!
!
      ndomain_rtp(1) = num_radial_domain_ctl%intvalue
      ndomain_rtp(2) = num_horiz_domain_ctl%intvalue
      ndomain_rtp(3) = 1
!
      ndomain_rtm(1) = num_radial_domain_ctl%intvalue
      ndomain_rtm(2) = 1
      ndomain_rtm(3) = num_horiz_domain_ctl%intvalue
!
      ndomain_rlm(1) = ndomain_rtm(1)
      ndomain_rlm(2) = ndomain_rtm(3)
!
      ndomain_rj(1) = 1
      ndomain_rj(2) =  num_radial_domain_ctl%intvalue                   &
     &               * num_horiz_domain_ctl%intvalue
!
      end subroutine simple_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      end module set_control_sph_subdomains
