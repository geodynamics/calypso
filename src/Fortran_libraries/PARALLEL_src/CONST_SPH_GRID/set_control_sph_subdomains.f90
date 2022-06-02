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
!!      subroutine set_inner_loop_4_sph_shell(sdctl, s3d_ranks)
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(spheric_global_rank), intent(inout) :: s3d_ranks
!!      subroutine set_subdomains_4_sph_shell                           &
!!     &         (nprocs_check, sdctl, s3d_ranks, ierr, e_message)
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(spheric_global_rank), intent(inout) :: s3d_ranks
!!@endverbatim
!
      module set_control_sph_subdomains
!
      use m_precision
!
      use t_spheric_global_ranks
      use t_ctl_data_4_divide_sphere
!
      implicit  none
!
      character(len=kchara), parameter :: radius1 = 'r'
      character(len=kchara), parameter :: radius2 = 'radial'
      character(len=kchara), parameter :: radius3 = 'radius'
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
      character(len=kchara), parameter :: horiz1 = 'horizontal'
!
!
      character(len = kchara), parameter :: original =    'original'
      character(len = kchara), parameter :: simple =      'simple'
!
      character(len = kchara), parameter                                &
     &                    :: cyclic_mode = 'cyclic_eq_mode'
      character(len = kchara), parameter                                &
     &                    :: cyclic_trns = 'cyclic_eq_trans'
!
      character(len = kchara), parameter                                &
     &                    :: cyclic_neib_mode = 'cyclic_neib_eq_mode'
      character(len = kchara), parameter                                &
     &                    :: cyclic_neib_trns = 'cyclic_neib_eq_trans'
!
      character(len = kchara), parameter                                &
     &                        :: test_ditribution = 'test'
!
      character(len = kchara), parameter                                &
     &                        :: hd_ver_1 = 'Ver_1'
      character(len = kchara), parameter                                &
     &                        :: hd_ver_2 = 'Ver_2'
!
      private :: radius1, theta1, phi1, mode1
      private :: radius2, theta2, phi2, mode2, horiz1
!
      private :: simple, cyclic_mode, cyclic_trns, original
      private :: cyclic_neib_mode, cyclic_neib_trns, test_ditribution
      private :: hd_ver_1, hd_ver_2
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
      subroutine set_inner_loop_4_sph_shell(sdctl, s3d_ranks)
!
      use skip_comment_f
      use select_zonal_4_legendre
!
      type(sphere_domain_control), intent(in) :: sdctl
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      character(len = kchara) :: tmpchara
!
!
      s3d_ranks%rj_rin_flag =  .FALSE.
!
      s3d_ranks%rlm_rin_flag = .TRUE.
      if(sdctl%rlm_inner_loop_ctl%iflag .gt. 0) then
        tmpchara = sdctl%rlm_inner_loop_ctl%charavalue
        if(     cmp_no_case(tmpchara, radius1)                          &
     &     .or. cmp_no_case(tmpchara, radius2)                          &
     &     .or. cmp_no_case(tmpchara, radius3)) then
          s3d_ranks%rlm_rin_flag = .TRUE.
        else if(     cmp_no_case(tmpchara, horiz1)) then
          s3d_ranks%rlm_rin_flag = .FALSE.
        end if
      end if
!
      s3d_ranks%rtm_rin_flag = .TRUE.
      if(sdctl%rtm_inner_loop_ctl%iflag .gt. 0) then
        tmpchara = sdctl%rtm_inner_loop_ctl%charavalue
        if(     cmp_no_case(tmpchara, radius1)                          &
     &     .or. cmp_no_case(tmpchara, radius2)                          &
     &     .or. cmp_no_case(tmpchara, radius3)) then
          s3d_ranks%rtm_rin_flag = .TRUE.
        else if(     cmp_no_case(tmpchara, horiz1)) then
          s3d_ranks%rtm_rin_flag = .FALSE.
        end if
      end if
!
      s3d_ranks%rtp_rin_flag = .FALSE.
      if(sdctl%rtp_inner_loop_ctl%iflag .gt. 0) then
        tmpchara = sdctl%rtp_inner_loop_ctl%charavalue
        if(     cmp_no_case(tmpchara, radius1)                          &
     &     .or. cmp_no_case(tmpchara, radius2)                          &
     &     .or. cmp_no_case(tmpchara, radius3)) then
          s3d_ranks%rtp_rin_flag = .TRUE.
        else if(cmp_no_case(tmpchara, phi1)                             &
     &     .or. cmp_no_case(tmpchara, phi2)) then
          s3d_ranks%rtp_rin_flag = .FALSE.
        end if
      end if
!
      s3d_ranks%iflag_rlm_distribute = id_cyclic_eq_mode
      if(sdctl%rlm_distibution_ctl%iflag .gt. 0) then
        tmpchara = sdctl%rlm_distibution_ctl%charavalue
        if(     cmp_no_case(tmpchara, simple)) then
          s3d_ranks%iflag_rlm_distribute = id_simple_rlm_distribute
!
        else if(cmp_no_case(tmpchara, cyclic_mode)) then
          s3d_ranks%iflag_rlm_distribute = id_cyclic_eq_mode
!        else if(cmp_no_case(tmpchara, cyclic_trns)) then
!          s3d_ranks%iflag_rlm_distribute = id_cyclic_eq_transform
!
!        else if(cmp_no_case(tmpchara, cyclic_neib_mode)) then
!          s3d_ranks%iflag_rlm_distribute = id_cyclic_eq_mode_neib
!        else if(cmp_no_case(tmpchara, cyclic_neib_trns)) then
!          s3d_ranks%iflag_rlm_distribute = id_cyclic_eq_trans_neib
!
        else if(cmp_no_case(tmpchara, test_ditribution)) then
          s3d_ranks%iflag_rlm_distribute = id_test_distribute
!
        else if(cmp_no_case(tmpchara, original)) then
          s3d_ranks%iflag_rlm_distribute = id_V1_rlm_distribute
        end if
      end if
!
      if(sdctl%indices_ordering_set%iflag .gt. 0) then
        tmpchara = sdctl%indices_ordering_set%charavalue
        if(     cmp_no_case(tmpchara, hd_ver_1)) then
          s3d_ranks%radial_inner_domain_flag = .FALSE.
!
          s3d_ranks%rlm_rin_flag = .FALSE.
          s3d_ranks%rtm_rin_flag = .FALSE.
          s3d_ranks%rtp_rin_flag = .TRUE.
          s3d_ranks%iflag_rlm_distribute = id_V1_rlm_distribute
        else if(cmp_no_case(tmpchara, hd_ver_2)) then
          s3d_ranks%radial_inner_domain_flag = .FALSE.
!
          s3d_ranks%rlm_rin_flag = .TRUE.
          s3d_ranks%rtm_rin_flag = .TRUE.
          s3d_ranks%rtp_rin_flag = .FALSE.
          s3d_ranks%iflag_rlm_distribute = id_cyclic_eq_mode
        end if
      end if
!
      s3d_ranks%flag_simple_r_decomp = .FALSE.
      if(sdctl%simple_r_decomp_ctl%iflag .gt. 0) then
        s3d_ranks%flag_simple_r_decomp                                  &
     &        = yes_flag(sdctl%simple_r_decomp_ctl%charavalue)
      end if
!
      end subroutine set_inner_loop_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      subroutine set_subdomains_4_sph_shell                             &
     &         (nprocs_check, sdctl, s3d_ranks, ierr, e_message)
!
      use m_error_IDs
      use skip_comment_f
!
      integer, intent(in) :: nprocs_check
      type(sphere_domain_control), intent(in) :: sdctl
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      integer(kind = kint), intent(inout) :: ierr
      character(len = kchara), intent(inout) :: e_message
!
      integer(kind = kint) :: iflag_f, iflag_s
!
!
      ierr = 0
      iflag_f =  sdctl%ndomain_sph_grid_ctl%num                         &
     &         * sdctl%ndomain_legendre_ctl%num                         &
     &         * sdctl%ndomain_spectr_ctl%num
      iflag_s =  sdctl%num_radial_domain_ctl%iflag                      &
     &         * sdctl%num_horiz_domain_ctl%iflag
!
      if(iflag_s .gt. 0) then
        call simple_subdomains_4_sph_shell(sdctl, s3d_ranks)
      else if(iflag_f .gt. 0) then
        call full_subdomains_4_sph_shell(sdctl, s3d_ranks)
      else
        write(e_message,'(a)') 'Set parallelization information'
        ierr = ierr_mesh
        return
      end if
!
      s3d_ranks%radial_inner_domain_flag = .FALSE.
      if(sdctl%inner_decomp_ctl%iflag .gt. 0) then
        if(cmp_no_case(sdctl%inner_decomp_ctl%charavalue, radius1)      &
     &    .or. cmp_no_case(sdctl%inner_decomp_ctl%charavalue, radius2)  &
     &    .or. cmp_no_case(sdctl%inner_decomp_ctl%charavalue, radius3)) &
     &   s3d_ranks%radial_inner_domain_flag = .TRUE.
      end if
!
      call check_sph_domains(nprocs_check, s3d_ranks, ierr, e_message)
!
      end subroutine set_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      subroutine full_subdomains_4_sph_shell(sdctl, s3d_ranks)
!
      use skip_comment_f
!
      type(sphere_domain_control), intent(in) :: sdctl
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
      integer(kind = kint) :: i
!
!
      s3d_ranks%ndomain_rtp(1:3) = 1
      if (sdctl%ndomain_sph_grid_ctl%num .gt. 0) then
        do i = 1, sdctl%ndomain_sph_grid_ctl%num
          if     (cmp_no_case(sdctl%ndomain_sph_grid_ctl%c_tbl(i),      &
     &                        radius1)                                  &
     &       .or. cmp_no_case(sdctl%ndomain_sph_grid_ctl%c_tbl(i),      &
     &                        radius2)) then
            s3d_ranks%ndomain_rtp(1)                                    &
     &           = sdctl%ndomain_sph_grid_ctl%ivec(i)
          else if (cmp_no_case(sdctl%ndomain_sph_grid_ctl%c_tbl(i),     &
     &                         theta1)                                  &
     &        .or. cmp_no_case(sdctl%ndomain_sph_grid_ctl%c_tbl(i),     &
     &                         theta2)) then
            s3d_ranks%ndomain_rtp(2)                                    &
     &           = sdctl%ndomain_sph_grid_ctl%ivec(i)
          end if
        end do
      end if
!
      s3d_ranks%ndomain_rtm(1:3) = 1
      if (sdctl%ndomain_legendre_ctl%num .gt. 0) then
        do i = 1, sdctl%ndomain_legendre_ctl%num
          if     (cmp_no_case(sdctl%ndomain_legendre_ctl%c_tbl(i),      &
     &                        radius1)                                  &
     &       .or. cmp_no_case(sdctl%ndomain_legendre_ctl%c_tbl(i),      &
     &                        radius2)) then
            s3d_ranks%ndomain_rtm(1)                                    &
     &        = sdctl%ndomain_legendre_ctl%ivec(i)
          else if (cmp_no_case(sdctl%ndomain_legendre_ctl%c_tbl(i),     &
     &                         phi1)                                    &
     &        .or. cmp_no_case(sdctl%ndomain_legendre_ctl%c_tbl(i),     &
     &                         phi2)) then
            s3d_ranks%ndomain_rtm(3)                                    &
     &        = sdctl%ndomain_legendre_ctl%ivec(i)
           end if
        end do
      end if
!
      s3d_ranks%ndomain_rlm(1) = s3d_ranks%ndomain_rtm(1)
      s3d_ranks%ndomain_rlm(2) = s3d_ranks%ndomain_rtm(3)
!
      s3d_ranks%ndomain_rj(1:2) = 1
      if (sdctl%ndomain_spectr_ctl%num .gt. 0) then
        do i = 1, sdctl%ndomain_spectr_ctl%num
          if     (cmp_no_case(sdctl%ndomain_spectr_ctl%c_tbl(i), mode1) &
     &       .or. cmp_no_case(sdctl%ndomain_spectr_ctl%c_tbl(i), mode2) &
     &      ) then
            s3d_ranks%ndomain_rj(2) = sdctl%ndomain_spectr_ctl%ivec(i)
          end if
        end do
      end if
!
      end subroutine full_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      subroutine simple_subdomains_4_sph_shell(sdctl, s3d_ranks)
!
      type(sphere_domain_control), intent(in) :: sdctl
      type(spheric_global_rank), intent(inout) :: s3d_ranks
!
!
      s3d_ranks%ndomain_rtp(1) = sdctl%num_radial_domain_ctl%intvalue
      s3d_ranks%ndomain_rtp(2) = sdctl%num_horiz_domain_ctl%intvalue
      s3d_ranks%ndomain_rtp(3) = 1
!
      s3d_ranks%ndomain_rtm(1) = sdctl%num_radial_domain_ctl%intvalue
      s3d_ranks%ndomain_rtm(2) = 1
      s3d_ranks%ndomain_rtm(3) = sdctl%num_horiz_domain_ctl%intvalue
!
      s3d_ranks%ndomain_rlm(1) = s3d_ranks%ndomain_rtm(1)
      s3d_ranks%ndomain_rlm(2) = s3d_ranks%ndomain_rtm(3)
!
      s3d_ranks%ndomain_rj(1) = 1
      s3d_ranks%ndomain_rj(2) =  sdctl%num_radial_domain_ctl%intvalue   &
     &                         * sdctl%num_horiz_domain_ctl%intvalue
!
      end subroutine simple_subdomains_4_sph_shell
!
!  ---------------------------------------------------------------------
!
      end module set_control_sph_subdomains
