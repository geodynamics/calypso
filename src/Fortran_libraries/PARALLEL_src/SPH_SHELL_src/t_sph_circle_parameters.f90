!>@file   t_sph_circle_parameters.f90
!!@brief  module t_sph_circle_parameters
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine set_control_circle_def(meq_ctl, circle)
!!         type(mid_equator_control), intent(in) :: meq_ctl
!!         type(circle_parameters), intent(inout) :: circle
!!      subroutine set_ctl_circle_for_dbench(dbench_ctl, circle)
!!         type(dynamobench_control), intent(in) :: dbench_ctl
!!        type(circle_parameters), intent(inout) :: circle
!!
!!      subroutine alloc_circle_field(mphi_rtp, circle, d_circle)
!!      subroutine dealloc_circle_field(circle, d_circle)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!
      module t_sph_circle_parameters
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
!
!>      Use spherical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_sph = 1
!>      Use Cylindrical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_cyl = 2
!
!>      Structure to make fields on circle
      type circle_parameters
!>        file name for field data on a circle
        character(len=kchara) :: circle_field_file_prefix
!>        file name for spectr power data on a circle
        character(len=kchara) :: circle_spectr_file_prefix
!>        compress flag for benchmark output file
        logical :: gzip_flag_circle = .FALSE.
!
!>        Flag for coordinate system for circle data
        integer(kind = kint) :: iflag_circle_coord = iflag_circle_sph
!
!>        Number of gird points for a circle
        integer(kind = kint) :: mphi_circle
!>        cylindrical radius for a circle to pick
        real(kind = kreal) :: s_circle
!>        vartical position for a circle to pick
        real(kind = kreal) :: z_circle
!>        radius for a circle to pick
        real(kind = kreal) :: r_circle
!>        colatitude for a circle to pick
        real(kind = kreal) :: colat_circle
!
!>        Inner closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_in
!>        Outer closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_out
!>        Inner closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_in
!>        Outer closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_out
      end type circle_parameters
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_circle_def(meq_ctl, circle)
!
      use t_ctl_data_mid_equator
      use t_control_array_character3
      use t_multi_flag_labels
      use m_file_format_labels
      use skip_comment_f
!
      type(mid_equator_control), intent(in) :: meq_ctl
      type(circle_parameters), intent(inout) :: circle
!
      character(len = kchara) :: tmpchara
!
!
!
      circle%circle_field_file_prefix = 'NO_FILE'
      if(meq_ctl%circle_field_file_ctl%iflag .gt. 0) then
        circle%circle_field_file_prefix                                 &
     &                 = meq_ctl%circle_field_file_ctl%charavalue
      end if
!
      circle%circle_spectr_file_prefix = 'NO_FILE'
      if(meq_ctl%circle_spectr_file_ctl%iflag .gt. 0) then
        circle%circle_spectr_file_prefix                                &
     &                 = meq_ctl%circle_spectr_file_ctl%charavalue
      end if
!
      circle%gzip_flag_circle = .FALSE.
      if(meq_ctl%circle_file_format_ctl%iflag .gt. 0) then
        tmpchara = meq_ctl%circle_file_format_ctl%charavalue
        if(check_mul_flags(tmpchara, gzip_flags))                       &
     &                     circle%gzip_flag_circle = .TRUE.
      end if
!
!
      circle%iflag_circle_coord = iflag_circle_sph
      if (meq_ctl%pick_circle_coord_ctl%iflag .ne. 0) then
        tmpchara = meq_ctl%pick_circle_coord_ctl%charavalue
        if(    cmp_no_case(tmpchara,'spherical')                        &
     &    .or. cmp_no_case(tmpchara,'rtp')) then
          circle%iflag_circle_coord = iflag_circle_sph
        else if(cmp_no_case(tmpchara,'cyrindrical')                     &
      &    .or. cmp_no_case(tmpchara,'spz')) then
          circle%iflag_circle_coord = iflag_circle_cyl
        end if
      end if
!
      circle%mphi_circle = -1
      if(meq_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = meq_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      circle%s_circle = 7.0d0/13.0d0 + 0.5d0
      if(meq_ctl%pick_s_ctl%iflag .gt. 0) then
        circle%s_circle = meq_ctl%pick_s_ctl%realvalue
      end if
!
      circle%z_circle = 0.0d0
      if(meq_ctl%pick_z_ctl%iflag .gt. 0) then
        circle%z_circle = meq_ctl%pick_z_ctl%realvalue
      end if
!
      circle%r_circle = sqrt(circle%s_circle + circle%z_circle**2)
      circle%colat_circle = acos(circle%z_circle / circle%r_circle)
!
      end subroutine set_control_circle_def
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_circle_for_dbench(dbench_ctl, circle)
!
      use t_ctl_data_dynamobench
      use t_control_array_character3
      use skip_comment_f
!
      type(dynamobench_control), intent(in) :: dbench_ctl
      type(circle_parameters), intent(inout) :: circle
!
!
      circle%iflag_circle_coord = iflag_circle_sph
      circle%mphi_circle = -1
      if(dbench_ctl%nphi_mid_eq_ctl%iflag .gt. 0) then
        circle%mphi_circle = dbench_ctl%nphi_mid_eq_ctl%intvalue
      end if
!
      circle%s_circle = 7.0d0/13.0d0 + 0.5d0
      circle%z_circle = 0.0d0
      circle%r_circle = circle%s_circle
      circle%colat_circle = acos(0.0d0)
!
      end subroutine set_ctl_circle_for_dbench
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_circle_field(mphi_rtp, circle, d_circle)
!
      integer(kind = kint), intent(in) :: mphi_rtp
      type(circle_parameters), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
!
      if(circle%mphi_circle .le. izero) then
        circle%mphi_circle = mphi_rtp
      end if
      call alloc_phys_data(circle%mphi_circle, d_circle)
!
      end subroutine alloc_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_field(d_circle)
!
      type(phys_data), intent(inout) :: d_circle
!
!
      call dealloc_phys_data(d_circle)
      call dealloc_phys_name(d_circle)
!
      end subroutine dealloc_circle_field
!
! ----------------------------------------------------------------------
!
     end module t_sph_circle_parameters
